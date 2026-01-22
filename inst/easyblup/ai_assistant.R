aiAssistantUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # dialog-style response window (single white window, no bubbles) - always visible
    shiny::div(class = "message-list dialog-window ai-response-window", style = "min-height:200px; max-height:300px; overflow-y:auto; margin-bottom:8px; background:#ffffff; padding:12px; border:1px solid #e0e0e0; border-radius:8px;",
      shiny::verbatimTextOutput(ns("resp")),
      shiny::uiOutput(ns("loading_ui"))
    ),
    # Option buttons placeholder (will be filled by assistant responses when appropriate)
    shiny::div(id = ns("option_list"), class = "option-list"),

    # Input area: single-line prompt (no send button)
    shiny::div(class = "ai-chat-input", style = "margin-top:8px;",
      shiny::textInput(ns("user_prompt"), label = NULL, placeholder = "Ask me a question", width = "100%")
    ),

    # Controls row: Ask / Apply / Test / Python config
    shiny::div(style = "display:flex; gap:8px; margin-top:8px;",
      shiny::actionButton(ns("ask"), "Ask", class = "btn-primary btn-sm"),
      shiny::actionButton(ns("apply"), "Apply", class = "btn-secondary btn-sm"),
      shiny::actionButton(ns("test"), "Test", class = "btn-light btn-sm"),
      shiny::actionButton(ns("py_config"), "Python Config", class = "btn-outline btn-sm")
    ),

    # Option to include the current parameter file when making an AI request
    shiny::div(style = "margin-top:6px; margin-bottom:6px;", shiny::checkboxInput(ns("include_param"), "Include current parameter file in AI context", value = TRUE)),
    shiny::hr()
  )
}

# Minimal AI server: rule-based stub that produces param_text suggestion using current data
aiAssistantServer <- function(
  id,
  data_reactive,
  apply_callback,
  app_context,
  ai_settings = shiny::reactive(NULL),
  tool_specs = shiny::reactive(NULL),
  tool_call = NULL
) {
  shiny::moduleServer(id, function(input, output, session) {
    # ns <- session$ns  # Keep for potential future use
    rv <- shiny::reactiveValues(resp = "", pending = FALSE, chat = list(), cooldown_until = as.numeric(Sys.time()) - 1)

    `%||%` <- function(x, y) if (is.null(x) || identical(x, "")) y else x
    rtrim_slash <- function(x) sub("/+$$", "", x %||% "")
    normalize_base_url <- function(x) {
      # Many OpenAI-compatible servers require the base URL to include /v1.
      # Users often paste "https://api.openai.com" which yields 404 for /models, etc.
      b <- rtrim_slash(x %||% "")
      if (!nzchar(b)) return("")
      # If user already provided a versioned path (/v1, /v1beta, /v2...), keep it.
      if (grepl("/v\\d+(\\.|/|$)", b) || grepl("/v1beta(\\.|/|$)", b, ignore.case = TRUE)) return(b)
      # Otherwise, append /v1.
      paste0(b, "/v1")
    }
    num_or <- function(x, default, cast = as.numeric) {
      val <- suppressWarnings(cast(x))
      if (!length(val) || is.na(val) || !is.finite(val)) default else val
    }

    # Gemini support removed; this module now targets OpenAI-compatible APIs only.

    call_openai_compatible <- function(s, msgs, tools = NULL) {
      # Allow empty base (use default OpenAI host) — do not treat missing base as an error.
      base <- normalize_base_url(s$base_url %||% "")

      # Check for Python openai package via reticulate (not R package)
      if (!requireNamespace("reticulate", quietly = TRUE)) {
        return(list(ok = FALSE, error = "reticulate 未安装：请运行 install.packages('reticulate') 并配置 Python 环境。"))
      }

      temp_val <- num_or(s$temperature, 0.2, as.numeric)
      max_tokens <- num_or(s$max_tokens, 2048L, as.integer)
      model <- s$model %||% "gpt-4o-mini"

      # Temporarily set environment vars so the openai package uses the configured key/base/org
      old_key <- Sys.getenv("OPENAI_API_KEY", "")
      old_base <- Sys.getenv("OPENAI_API_BASE", "")
      old_org <- Sys.getenv("OPENAI_ORGANIZATION", "")
      on.exit({
        Sys.setenv(OPENAI_API_KEY = old_key)
        Sys.setenv(OPENAI_API_BASE = old_base)
        Sys.setenv(OPENAI_ORGANIZATION = old_org)
      }, add = TRUE)
      Sys.setenv(OPENAI_API_KEY = s$api_key %||% "")
      Sys.setenv(OPENAI_API_BASE = base)
      if (nzchar(s$organization %||% "")) Sys.setenv(OPENAI_ORGANIZATION = s$organization)

        # Use Python OpenAI client via reticulate. This path is used to test the given Python API.
        py_available <- tryCatch(reticulate::py_module_available("openai"), error = function(e) FALSE)
        if (!isTRUE(py_available)) {
          return(list(ok = FALSE, error = "Python openai 包在当前 Python 环境中不可用，请在对应 Python 环境中运行 pip install openai。"))
        }

        # Import python openai module
        openai_py <- tryCatch(reticulate::import("openai", convert = FALSE), error = function(e) NULL)
        if (is.null(openai_py)) return(list(ok = FALSE, error = "无法导入 Python openai 模块"))

        client <- tryCatch({
          # Only pass base_url when it is non-empty; passing an empty string causes the
          # Python client to construct an invalid request URL (missing scheme).
          if (nzchar(base)) {
            openai_py$OpenAI(api_key = s$api_key %||% Sys.getenv("OPENAI_API_KEY", ""), base_url = base)
          } else {
            openai_py$OpenAI(api_key = s$api_key %||% Sys.getenv("OPENAI_API_KEY", ""))
          }
        }, error = function(e) {
          NULL
        })
        if (is.null(client)) return(list(ok = FALSE, error = "创建 Python OpenAI 客户端失败（请检查 key/base_url）。"))

        # Convert messages to Python objects
        py_msgs <- tryCatch(reticulate::r_to_py(msgs), error = function(e) NULL)
        if (is.null(py_msgs)) return(list(ok = FALSE, error = "无法将消息转换为 Python 对象"))

        # Convert tools (if provided)
        py_tools <- NULL
        if (!is.null(tools)) {
          py_tools <- tryCatch(reticulate::r_to_py(tools), error = function(e) NULL)
          if (is.null(py_tools)) return(list(ok = FALSE, error = "无法将 tools 转换为 Python 对象"))
        }

        # Call the Python client's chat completions create (with optional tools)
        res_py <- tryCatch({
          if (!is.null(py_tools)) {
            client$chat$completions$create(
              model = model,
              messages = py_msgs,
              tools = py_tools,
              tool_choice = "auto",
              temperature = as.numeric(temp_val),
              max_tokens = as.integer(max_tokens)
            )
          } else {
            client$chat$completions$create(
              model = model,
              messages = py_msgs,
              temperature = as.numeric(temp_val),
              max_tokens = as.integer(max_tokens)
            )
          }
        }, error = function(e) list(.py_err = TRUE, message = conditionMessage(e)))

        if (is.list(res_py) && isTRUE(res_py$.py_err)) return(list(ok = FALSE, error = paste0("Python 请求失败：", res_py$message)))

        # Convert python response to R and extract text
        res_r <- tryCatch(reticulate::py_to_r(res_py), error = function(e) NULL)
        if (is.null(res_r)) return(list(ok = FALSE, error = "无法解析 Python 响应为 R 对象"))

        # Extract message content and tool calls (if any)
        msg <- NULL
        if (!is.null(res_r$choices) && length(res_r$choices) > 0) {
          msg <- res_r$choices[[1]]$message %||% NULL
        }
        txt <- NULL
        tool_calls <- NULL
        try({
          if (!is.null(msg)) {
            if (!is.null(msg$content)) txt <- msg$content
            if (!is.null(msg$tool_calls)) tool_calls <- msg$tool_calls
          }
          if (is.null(txt) && !is.null(res_r$choices) && length(res_r$choices) > 0 && !is.null(res_r$choices[[1]]$text)) {
            txt <- res_r$choices[[1]]$text
          }
        }, silent = TRUE)

        # It's valid to return empty text when tool_calls exist
        if ((is.null(txt) || !nzchar(as.character(txt))) && (is.null(tool_calls) || length(tool_calls) == 0)) {
          return(list(ok = FALSE, error = "空响应（Python openai 返回空结果）"))
        }
        list(ok = TRUE, text = if (is.null(txt)) "" else as.character(txt), tool_calls = tool_calls)
      }

    .get_tools <- function() {
      ts_fun <- tool_specs
      if (!is.function(ts_fun)) return(NULL)
      tryCatch(ts_fun(), error = function(e) NULL) # nolint
    }

    .parse_tool_args <- function(x) {
      # OpenAI returns function.arguments as a JSON string
      if (is.null(x)) return(list())
      if (is.list(x)) return(x)
      s <- as.character(x)
      if (!nzchar(s)) return(list())
      if (requireNamespace("jsonlite", quietly = TRUE)) {
        tryCatch(jsonlite::fromJSON(s, simplifyVector = TRUE), error = function(e) list())
      } else {
        list()
      }
    }

    .tool_result_to_text <- function(obj) {
      if (requireNamespace("jsonlite", quietly = TRUE)) {
        jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null")
      } else {
        paste(capture.output(str(obj, max.level = 3)), collapse = "\n")
      }
    }

    call_llm <- function(user_text, context_text) {
      s <- tryCatch(ai_settings(), error = function(e) NULL)
      # Require API key but allow empty base (use default OpenAI host)
      if (is.null(s) || !nzchar(s$api_key %||% "")) {
        return(list(ok = FALSE, error = "API 未配置（API Key 为空）"))
      }
  sys_prompt <- s$system_prompt %||% (tryCatch(app_context()$rules$system_prompt, error = function(e) ""))

      msgs <- list(list(role = "system", content = if (nzchar(sys_prompt)) sys_prompt else "You are an assistant."))
      if (nzchar(context_text)) {
        msgs <- append(msgs, list(list(role = "system", content = paste0("Context: ", context_text))))
      }
      if (length(rv$chat) > 0) {
        keep <- tail(seq_along(rv$chat), 10)
        for (i in keep) msgs <- append(msgs, list(rv$chat[[i]]))
      }
      msgs <- append(msgs, list(list(role = "user", content = user_text)))

      tools <- .get_tools()
      has_tools <- !is.null(tools) && length(tools) > 0 && is.function(tool_call)

      # If no tools available, do a normal completion
      if (!isTRUE(has_tools)) {
        return(call_openai_compatible(s, msgs, tools = NULL))
      }

      executed_any <- FALSE
      executed_count <- 0L

      # Tool loop (max 5 rounds)
      for (i in seq_len(5)) {
        res <- call_openai_compatible(s, msgs, tools = tools)
        if (!isTRUE(res$ok)) return(res)

        tc <- res$tool_calls
        if (is.null(tc) || length(tc) == 0) {
          final_txt <- res$text %||% ""
          if (executed_any && !nzchar(final_txt)) {
            final_txt <- paste0("✅ 已执行工具调用并更新界面（", executed_count, " 次）。")
          }
          return(list(ok = TRUE, text = final_txt))
        }

        # Execute tool calls in order
        for (j in seq_along(tc)) {
          call_j <- tc[[j]]
          fn <- call_j$`function` %||% list()
          tool_name <- fn$name %||% ""
          tool_id <- call_j$id %||% paste0("tool_", i, "_", j)
          args <- .parse_tool_args(fn$arguments)

          out <- tryCatch(tool_call(tool_name, args), error = function(e) list(ok = FALSE, error = conditionMessage(e)))
          out_txt <- .tool_result_to_text(out)
          executed_any <- TRUE
          executed_count <- executed_count + 1L

          # Append tool result as a "tool" message for the model
          msgs <- append(msgs, list(list(role = "tool", tool_call_id = tool_id, content = out_txt)))
        }
        # Continue loop; model will see tool outputs and (hopefully) return a final message
      }

      list(ok = FALSE, error = "工具调用循环超时（超过最大轮数）")
    }

    test_connection <- function() {
      s <- tryCatch(ai_settings(), error = function(e) NULL)
      if (is.null(s)) return(list(ok = FALSE, msg = "No settings"))

  base <- normalize_base_url(s$base_url %||% "")
  key <- s$api_key %||% ""
  # Allow empty base (use default OpenAI host); require API key
  if (!nzchar(key)) return(list(ok = FALSE, msg = "API Key 为空"))

      check_api_key_format <- function(key_value) {
        if (!nzchar(key_value)) return(list(ok = FALSE, msg = "API Key 为空"))
        if (grepl("^Bearer\\s+", key_value, ignore.case = TRUE)) return(list(ok = FALSE, msg = "API Key 包含 'Bearer ' 前缀，请只填写密钥本体"))
        if (grepl("\\s", key_value)) return(list(ok = FALSE, msg = "API Key 包含空白字符（空格或换行），请去除"))
        n <- nchar(key_value)
        pre <- if (n >= 4) substr(key_value, 1, 4) else substr(key_value, 1, n)
        suf <- if (n > 3) substr(key_value, max(1, n - 2), n) else ""
        list(ok = TRUE, masked = paste0(pre, "...", suf))
      }

      fmt <- check_api_key_format(key)
      if (!isTRUE(fmt$ok)) return(list(ok = FALSE, msg = fmt$msg))

      # Use Python openai via reticulate for the connection test
      if (!requireNamespace("reticulate", quietly = TRUE)) {
        return(list(ok = FALSE, msg = "reticulate 未安装：请运行 install.packages('reticulate') 并配置 Python 环境。"))
      }
      py_ok <- tryCatch(reticulate::py_module_available("openai"), error = function(e) FALSE)
      if (!isTRUE(py_ok)) return(list(ok = FALSE, msg = "Python openai 包在当前 Python 环境中不可用，请安装 openai。"))

      # Try import and a light models call if available
      out <- tryCatch({
        openai_py <- reticulate::import("openai", convert = FALSE)
        # Attempt to instantiate client (best-effort)
        # Only pass base_url when provided; an empty base_url will lead to malformed request URLs
        client <- tryCatch({
          if (nzchar(base)) {
            openai_py$OpenAI(api_key = key, base_url = base)
          } else {
            openai_py$OpenAI(api_key = key)
          }
        }, error = function(e) NULL)

        if (is.null(client)) {
          return(list(ok = FALSE, msg = "无法创建 Python openai 客户端"))
        }

        # Prefer checking methods on the client object (not the module) and avoid
        # attempting to call non-callable attributes which causes 'attempt to apply non-function'.
        # Try common patterns used by the OpenAI Python client:
        #  - client.models.list()
        #  - client.models.list_models()
        #  - openai.list_models() (module-level legacy)

        # 1) client.models.list()
        if (reticulate::py_has_attr(client, "models")) {
          models_obj <- tryCatch(client$models, error = function(e) NULL)
          if (!is.null(models_obj)) {
            if (reticulate::py_has_attr(models_obj, "list")) {
              invisible(models_obj$list())
              return(list(ok = TRUE, msg = "Python openai models.list() OK"))
            }
            if (reticulate::py_has_attr(models_obj, "list_models")) {
              invisible(models_obj$list_models())
              return(list(ok = TRUE, msg = "Python openai models.list_models() OK"))
            }
            # if models_obj itself is callable, try calling it safely
            if (reticulate::py_is_callable(models_obj)) {
              invisible(models_obj())
              return(list(ok = TRUE, msg = "Python openai models() callable OK"))
            }
          }
        }

        # 2) module-level helpers (legacy)
        if (reticulate::py_has_attr(openai_py, "list_models")) {
          invisible(openai_py$list_models())
          return(list(ok = TRUE, msg = "Python openai list_models() OK"))
        }
        if (reticulate::py_has_attr(openai_py, "models") && reticulate::py_is_callable(openai_py$models)) {
          invisible(openai_py$models())
          return(list(ok = TRUE, msg = "Python openai module models() OK"))
        }

        # If we couldn't exercise a models/list call, treat successful client creation as success
        list(ok = TRUE, msg = "Python openai 客户端创建成功（无法运行 models 测试）")
      }, error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
      # Improve guidance for common 404 (missing /v1)
      if (!isTRUE(out$ok) && nzchar(base) && grepl("404|Not Found|<html>|nginx", out$msg, ignore.case = TRUE)) {
        out$msg <- paste0(
          out$msg,
          "\n\n可能原因：Base URL 缺少版本路径。请尝试把 Base URL 设置为类似：",
          "\n- OpenAI: https://api.openai.com/v1",
          "\n- 其他兼容服务：https://<host>/v1"
        )
      }
      out
    }

    build_param_from_rules <- function(dat, ctx) {
      # Very conservative: reuse current_param if present; otherwise echo minimal header
      base <- dat$current_param %||% "# PARAMETER FILE (AI suggested)\n"
      # Ensure method line aligns with rules default if provided
      if (!is.null(dat$opt$method) && dat$opt$method == "VCE") {
        base <- base
      }
      list(param_text = base)
    }

    shiny::observeEvent(input$ask, {
      # Respect cooldown to avoid repeated 429
      now_ts <- as.numeric(Sys.time())
      if (now_ts < (rv$cooldown_until %||% 0)) {
        wait_s <- max(1, round((rv$cooldown_until - now_ts)))
        shiny::showNotification(paste0('频率受限，请 ', wait_s, ' 秒后再试'), type = 'warning', duration = 3)
        return()
      }
      # Prevent overlapping requests
      if (isTRUE(rv$pending)) {
        shiny::showNotification('请求正在进行中，请稍候...', type = 'warning', duration = 2)
        return()
      }
      rv$pending <- TRUE
      on.exit({ rv$pending <- FALSE }, add = TRUE)

      dat <- tryCatch(data_reactive(), error = function(e) list())
      ctx <- tryCatch(app_context(), error = function(e) list(app_name = "unknown", locale = "zh", rules = list()))

      # Build context text
      ctx_text <- paste0(
        "App: ", (ctx$app_name %||% "unknown"), " | Locale: ", (ctx$locale %||% "zh"), "\n",
        "Traits: ", paste(dat$traits %||% character(), collapse = ", "), "\n",
        "Fixed: ", paste(dat$fixed %||% character(), collapse = ", "), "\n",
        "Random: ", paste(dat$random %||% character(), collapse = ", ")
      )

      # Optionally append the current parameter file to the context so the AI can suggest edits
      try({
        if (isTRUE(input$include_param) && !is.null(dat$current_param) && nzchar(dat$current_param)) {
          ctx_text <- paste0(ctx_text, "\n\nPARAMETER_FILE:\n", dat$current_param)
        }
      }, silent = TRUE)

      # Append user message to local history
      rv$chat <- append(rv$chat, list(list(role = "user", content = input$user_prompt %||% "")))

      # Check AI settings before attempting API call
      s <- tryCatch(ai_settings(), error = function(e) NULL)
      cat("[AI Assistant] ai_settings: ", !!(!is.null(s)), " base_url='", if (!is.null(s)) (s$base_url %||% "") else "", "' key_set=", if (!is.null(s)) nzchar(s$api_key %||% "") else FALSE, "\n")
      # Only require API key; base_url is optional (will use default OpenAI host if empty)
      if (is.null(s) || !nzchar(s$api_key %||% "")) {
        shiny::showNotification("API 未配置或 API Key 为空，请打开 AI 设置并保存 API Key。Base URL 可选（留空则使用默认 OpenAI）。", type = "warning", duration = 5)
      } else {
        # Try real API first
        res <- try(call_llm(input$user_prompt %||% "", ctx_text), silent = TRUE)
        if (!inherits(res, "try-error") && is.list(res) && isTRUE(res$ok)) {
          # Save assistant reply into history
          rv$chat <- append(rv$chat, list(list(role = "assistant", content = res$text)))
          rv$resp <- res$text
          # Store a reasonable param_text for Apply:
          # prefer current_param from data snapshot (may be updated by tools), otherwise use assistant text.
          dat2 <- tryCatch(data_reactive(), error = function(e) list())
          rv$last <- list(param_text = as.character(dat2$current_param %||% res$text))
          return()
        }
        # If there was an error from the API attempt, surface it to the user
        if (!inherits(res, "try-error") && is.list(res) && !isTRUE(res$ok)) {
          # If the call_llm returned retry info, surface it
          if (!is.null(res$retry_after) && !is.na(res$retry_after)) {
            shiny::showNotification(paste0("API 被限流：请等待 ", res$retry_after, " 秒后重试"), type = "error", duration = 8)
            # set cooldown
            rv$cooldown_until <- as.numeric(Sys.time()) + as.numeric(res$retry_after)
          } else {
            shiny::showNotification(paste0("API 请求失败：", res$error), type = "error", duration = 6)
            # generic cooldown 10s on failure to avoid hammering
            rv$cooldown_until <- as.numeric(Sys.time()) + 10
          }
          cat("[AI Assistant] call_llm failed: ", res$error, "\n")
        } else if (inherits(res, "try-error")) {
          shiny::showNotification("API 请求期间发生异常（查看 R 控制台以获取详细信息）", type = "error", duration = 6)
          cat("[AI Assistant] call_llm exception: ", res, "\n")
          rv$cooldown_until <- as.numeric(Sys.time()) + 10
        }
      }

      # Fallback to rule-based stub
      suggestion <- build_param_from_rules(dat, ctx)
      rv$resp <- paste0("[Rule-based]\nApp:", ctx$app_name, "\nLocale:", ctx$locale, "\n\nSuggested parameter file prepared.")
      rv$last <- suggestion
    }, ignoreInit = TRUE)

    # Test connection handler with debug output and user feedback
    shiny::observeEvent(input$test, {
      cat("⚙️ [AI Assistant] Test button clicked\n")
      shiny::showNotification("正在测试连接，请稍候...", type = "message", duration = 2)

      r <- tryCatch(test_connection(), error = function(e) list(ok = FALSE, msg = paste("Error:", conditionMessage(e))))

      if (isTRUE(r$ok)) {
        cat("✅ [AI Assistant] Connection success:", r$msg, "\n")
        rv$resp <- paste0("[Connection Test] Success: ", r$msg)
        shiny::showNotification("✅ 连接成功", type = "message", duration = 3)
      } else {
        cat("❌ [AI Assistant] Connection failed:", r$msg, "\n")
        rv$resp <- paste0("[Connection Test] Failed: ", r$msg)
        shiny::showNotification(paste("❌ 连接失败：", r$msg), type = "error", duration = 5)
      }
    })

    shiny::observeEvent(input$apply, {
      if (!is.null(rv$last)) try(apply_callback(rv$last), silent = TRUE)
    })

    # Generate a Python OpenAI client snippet using current AI settings
    shiny::observeEvent(input$py_config, {
      s <- tryCatch(ai_settings(), error = function(e) NULL)
      base <- rtrim_slash(if (!is.null(s)) s$base_url %||% "" else "")
      key <- if (!is.null(s)) s$api_key %||% "" else ""
      model <- if (!is.null(s)) s$model %||% "gpt-4o-mini" else "gpt-4o-mini"

      # Prefer instructing use of env var; if key present, show masked preview and placeholder
      key_line <- 'api_key="YOUR_API_KEY"'
      if (nzchar(key)) {
        # do not leak full key in UI; show placeholder and note how to use env var
        key_line <- 'api_key="YOUR_API_KEY"  # (replace with your key or use env)'
      }

      base_line <- if (nzchar(base)) paste0('base_url="', base, '"') else 'base_url="https://api.openai.com/v1"'

      snippet_lines <- c(
        "from openai import OpenAI",
        "",
        "# Option A: read key from environment (recommended)",
        "# import os",
        "# client = OpenAI(api_key=os.getenv('OPENAI_API_KEY'), base_url='https://api.openai.com/v1')",
        "",
        "# Option B: pass key directly (not recommended to hard-code in source)",
        paste0('client = OpenAI(', key_line, ',', ' ', base_line, ')'),
        "",
        paste0("# example usage: response = client.chat.completions.create(model=\"", model, "\", messages=[{'role':'user','content':'Hello'}])")
      )

      snippet <- paste(snippet_lines, collapse = "\n")
      rv$resp <- paste0("[Python Config]\n", snippet)
      shiny::showNotification("已生成 Python 配置片段，可在响应区域复制。", type = "message", duration = 4)
    })

    # Render the conversation window. Prefer showing the chat history if present,
    # otherwise fall back to the single rv$resp text (used by connection/test snippets).
    # Always show something (placeholder if empty) so the window is always visible.
    output$resp <- shiny::renderText({
      if (length(rv$chat) > 0) {
        parts <- lapply(rv$chat, function(m) {
          role_label <- if (isTRUE(m$role == "user")) "你" else "AI"
          paste0(role_label, ": ", m$content)
        })
        paste(unlist(parts), collapse = "\n\n")
      } else if (nzchar(rv$resp %||% "")) {
        rv$resp
      } else {
        "等待你的提问...\n\n在这里输入问题，AI 将在这里回答。"
      }
    })

    # Loading UI shown while rv$pending is TRUE (during API call)
    output$loading_ui <- shiny::renderUI({
      if (isTRUE(rv$pending)) {
        shiny::tags$div(class = "ai-loading-wrapper",
          shiny::tags$div(class = "ai-loading-bar"),
          shiny::tags$div(class = "ai-loading-text", "对话正在进行中...")
        )
      } else {
        NULL
      }
    })
  })
}

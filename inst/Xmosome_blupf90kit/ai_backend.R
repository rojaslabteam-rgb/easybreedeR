# Xmosome_blupf90kit — AI assistant backend (OpenAI-compatible chat API)
# Author defaults from env: XMOSOME_AI_API_KEY, XMOSOME_AI_BASE_URL, XMOSOME_AI_MODEL
# Author credentials and endpoint form one indivisible configuration.
# Author-funded chat is rate-limited per client IP (default 10 questions / day).

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || identical(x, "")) y else x
}

# Optional local author secrets (gitignored). Used for shinyapps.io where
# deployApp(envVars=...) is not supported. Never commit real keys.
.ai_load_author_env_file <- function() {
  candidates <- c(
    "ai_author_env.local.R",
    file.path("inst", "Xmosome_blupf90kit", "ai_author_env.local.R"),
    file.path(getwd(), "ai_author_env.local.R"),
    file.path(getwd(), "inst", "Xmosome_blupf90kit", "ai_author_env.local.R")
  )
  pkg_file <- tryCatch(
    system.file("Xmosome_blupf90kit", "ai_author_env.local.R", package = "easybreedeR", mustWork = FALSE),
    error = function(e) ""
  )
  if (nzchar(pkg_file)) candidates <- c(candidates, pkg_file)
  for (f in unique(candidates)) {
    if (nzchar(f) && file.exists(f)) {
      try(sys.source(f, envir = globalenv()), silent = TRUE)
      return(invisible(f))
    }
  }
  invisible(NULL)
}
.ai_load_author_env_file()

ai_env_config <- function() {
  list(
    api_key  = Sys.getenv("XMOSOME_AI_API_KEY", unset = ""),
    base_url = Sys.getenv("XMOSOME_AI_BASE_URL", unset = "https://api.openai.com/v1"),
    model    = Sys.getenv("XMOSOME_AI_MODEL", unset = "gpt-4o-mini"),
    has_default = nzchar(Sys.getenv("XMOSOME_AI_API_KEY", unset = "")),
    daily_limit = ai_author_daily_limit()
  )
}

ai_author_daily_limit <- function() {
  lim <- suppressWarnings(as.integer(Sys.getenv("XMOSOME_AI_DAILY_LIMIT", unset = "10")))
  if (is.na(lim) || lim < 0L) 10L else lim
}

ai_quota_dir <- function() {
  dir <- Sys.getenv("XMOSOME_AI_QUOTA_DIR", unset = "")
  if (!nzchar(dir)) {
    dir <- file.path(tempdir(), "xmosome_ai_quota")
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

# Number of reverse proxies between the internet and this R process.
# 0 (direct/local): forwarded headers are client-controlled and are ignored.
# n >= 1: only the n right-most X-Forwarded-For entries were written by
# infrastructure we trust; everything to their left is supplied by the client.
ai_trusted_proxy_hops <- function() {
  raw <- Sys.getenv("XMOSOME_AI_TRUSTED_PROXIES", unset = "")
  if (nzchar(raw)) {
    n <- suppressWarnings(as.integer(raw))
    if (!is.na(n) && n >= 0L) return(n)
  }
  hosted <- identical(Sys.getenv("R_CONFIG_ACTIVE", ""), "shinyapps") ||
    any(nzchar(Sys.getenv(c("RSTUDIO_CONTENT_GUID", "RS_CONNECT_START_TIME", "RSC_VERSION", "RSC_INSTANCE"), "")))
  if (hosted) 1L else 0L
}

.ai_clean_ip <- function(x) {
  x <- trimws(as.character(x %||% ""))
  if (length(x) != 1L || is.na(x) || !nzchar(x)) return("")
  x <- sub("^\\[([0-9A-Fa-f:.]+)\\](:[0-9]+)?$", "\\1", x)          # [v6]:port
  if (grepl("^[0-9.]+:[0-9]+$", x)) x <- sub(":[0-9]+$", "", x)      # v4:port
  if (grepl("^[0-9]{1,3}(\\.[0-9]{1,3}){3}$", x) || (grepl(":", x, fixed = TRUE) && grepl("^[0-9A-Fa-f:.]+$", x))) {
    tolower(x)
  } else {
    ""
  }
}

ai_client_ip <- function(session, trusted_hops = ai_trusted_proxy_hops()) {
  if (is.null(session) || is.null(session$request)) return("unknown")
  req <- session$request
  headers <- req$HEADERS
  if (is.null(headers)) headers <- list()
  names(headers) <- tolower(names(headers))
  remote <- .ai_clean_ip(req$REMOTE_ADDR %||% headers[["remote_addr"]] %||% "")
  trusted_hops <- suppressWarnings(as.integer(trusted_hops)[1])
  if (is.na(trusted_hops) || trusted_hops < 1L) {
    return(if (nzchar(remote)) remote else "unknown")
  }
  xff <- as.character(req$HTTP_X_FORWARDED_FOR %||% headers[["x-forwarded-for"]] %||% "")
  parts <- trimws(strsplit(paste(xff, collapse = ","), ",", fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  if (length(parts)) {
    # Count from the right: the left-most entries can be forged by the client.
    ip <- .ai_clean_ip(parts[max(1L, length(parts) - trusted_hops + 1L)])
    if (nzchar(ip)) return(ip)
  }
  xr <- .ai_clean_ip(req$HTTP_X_REAL_IP %||% headers[["x-real-ip"]] %||% "")
  if (nzchar(xr)) return(xr)
  if (nzchar(remote)) remote else "unknown"
}

ai_quota_key <- function(ip, day = format(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%d")) {
  ip <- trimws(as.character(ip %||% "unknown"))
  if (!nzchar(ip)) ip <- "unknown"
  digest::digest(paste0("xmosome-ai-quota|", day, "|", ip), algo = "sha256")
}

ai_quota_read <- function(key) {
  path <- file.path(ai_quota_dir(), paste0(key, ".rds"))
  if (!file.exists(path)) return(0L)
  val <- tryCatch(readRDS(path), error = function(e) 0L)
  val <- suppressWarnings(as.integer(val)[1])
  if (is.na(val) || val < 0L) 0L else val
}

ai_quota_write <- function(key, n) {
  path <- file.path(ai_quota_dir(), paste0(key, ".rds"))
  tmp <- paste0(path, ".", Sys.getpid(), ".tmp")
  saveRDS(as.integer(n), tmp)
  ok <- file.rename(tmp, path)
  if (!isTRUE(ok)) {
    # Fallback for platforms where rename cannot overwrite.
    unlink(path)
    file.rename(tmp, path)
  }
  invisible(n)
}

ai_quota_status <- function(ip, limit = ai_author_daily_limit()) {
  limit <- as.integer(limit)
  used <- ai_quota_read(ai_quota_key(ip))
  remaining <- max(0L, limit - used)
  list(
    used = used,
    limit = limit,
    remaining = remaining,
    allowed = used < limit,
    ip_bucket = substr(ai_quota_key(ip), 1, 8)
  )
}

# Reserve one author-funded question for this IP. Own-key traffic should not call this.
ai_quota_consume <- function(ip, limit = ai_author_daily_limit()) {
  limit <- as.integer(limit)
  key <- ai_quota_key(ip)
  lock_path <- file.path(ai_quota_dir(), paste0(key, ".lock"))
  # Best-effort exclusive lock across concurrent sessions on the same host.
  got_lock <- FALSE
  for (i in seq_len(40)) {
    fd <- tryCatch(file(lock_path, open = "w+x"), error = function(e) NULL)
    if (!is.null(fd)) {
      close(fd)
      got_lock <- TRUE
      break
    }
    Sys.sleep(0.025)
  }
  on.exit({
    if (got_lock) unlink(lock_path, force = TRUE)
  }, add = TRUE)
  used <- ai_quota_read(key)
  if (used >= limit) {
    return(list(
      ok = FALSE,
      used = used,
      limit = limit,
      remaining = 0L,
      error = sprintf(
        "Author AI daily limit reached (%d/%d questions for this IP). Add your own API key in Settings, or try again tomorrow (UTC).",
        used, limit
      )
    ))
  }
  used <- ai_quota_write(key, used + 1L)
  list(
    ok = TRUE,
    used = used,
    limit = limit,
    remaining = max(0L, limit - used)
  )
}

ai_scalar <- function(x, name) {
  if (is.null(x)) return("")
  if (!is.character(x) || length(x) != 1L || is.na(x) || grepl("[[:cntrl:]]", x)) {
    stop(paste("Invalid AI setting:", name), call. = FALSE)
  }
  trimws(x)
}

ai_validate_base_url <- function(base, allowed = character()) {
  base <- sub("/+$", "", ai_scalar(base, "base_url"))
  # Only administrator-approved public HTTPS endpoints are callable. This also
  # excludes credentials, query strings, fragments, IP literals and local ports.
  if (!grepl("^https://[A-Za-z0-9][A-Za-z0-9.-]*[.][A-Za-z]{2,}(:443)?(/[A-Za-z0-9._~/-]*)?$", base) ||
      !base %in% allowed) {
    stop("AI endpoint is not approved. Use an approved HTTPS provider or contact the administrator.", call. = FALSE)
  }
  base
}

ai_allowed_endpoints <- function() {
  extra <- trimws(strsplit(Sys.getenv("XMOSOME_AI_ALLOWED_BASE_URLS", ""), ",", fixed = TRUE)[[1]])
  unique(sub("/+$", "", c(
    "https://api.openai.com/v1", "https://openrouter.ai/api/v1",
    "https://api.deepseek.com/v1", "https://api.groq.com/openai/v1",
    "https://api.together.xyz/v1", extra[nzchar(extra)]
  )))
}

ai_resolve_config <- function(user_cfg = list()) {
  if (!is.list(user_cfg)) stop("Invalid AI settings.", call. = FALSE)
  env <- ai_env_config()
  key <- ai_scalar(user_cfg$api_key, "api_key")
  base <- ai_scalar(user_cfg$base_url, "base_url")
  model <- ai_scalar(user_cfg$model, "model")
  preset <- ai_scalar(user_cfg$api_preset, "api_preset")
  source <- if (nzchar(key)) "user" else "default"
  host_base <- sub("/+$", "", env$base_url)
  if (source == "default") {
    if ((nzchar(base) && sub("/+$", "", base) != host_base) ||
        (nzchar(preset) && preset != "host")) {
      stop("Custom AI settings require your own API key. The author key can only use the author configuration.", call. = FALSE)
    }
    key <- env$api_key
    base <- host_base
    model <- env$model
    preset <- "host"
  } else {
    if (!nzchar(base)) base <- host_base
    if (!nzchar(model)) model <- env$model
  }
  base <- ai_validate_base_url(base, c(ai_allowed_endpoints(), host_base))
  base <- sub("/+$", "", base)
  # OpenRouter requires vendor/model ids
  if (grepl("openrouter\\.ai", base, ignore.case = TRUE) || identical(preset, "openrouter")) {
    model <- ai_normalize_openrouter_model(model)
  }
  list(
    api_key = key,
    base_url = base,
    model = model,
    api_preset = preset,
    source = source
  )
}

ai_normalize_openrouter_model <- function(model) {
  model <- trimws(as.character(model %||% ""))
  if (!nzchar(model)) return("openai/gpt-4o-mini")
  if (grepl("/", model, fixed = TRUE)) return(model)
  map <- c(
    "gpt-4o-mini" = "openai/gpt-4o-mini",
    "gpt-4o" = "openai/gpt-4o",
    "gpt-4.1-mini" = "openai/gpt-4.1-mini",
    "gpt-4.1" = "openai/gpt-4.1",
    "o4-mini" = "openai/o4-mini",
    "deepseek-chat" = "deepseek/deepseek-chat",
    "deepseek-reasoner" = "deepseek/deepseek-reasoner",
    "llama-3.3-70b-versatile" = "meta-llama/llama-3.3-70b-instruct"
  )
  if (model %in% names(map)) return(unname(map[[model]]))
  # Heuristic fallbacks
  if (grepl("^gpt-|^o[0-9]", model)) return(paste0("openai/", model))
  if (grepl("^deepseek", model)) return(paste0("deepseek/", model))
  model
}

ai_strip_ansi <- function(x) {
  gsub("\033\\[[0-9;]*[A-Za-z]|\\x1b\\[[0-9;]*[A-Za-z]", "", as.character(x %||% ""), perl = TRUE)
}

ai_format_http_error <- function(status, res_txt, model = "", base_url = "") {
  raw <- ai_strip_ansi(res_txt %||% "")
  msg <- NULL
  parsed <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.null(parsed)) {
    msg <- parsed$error$message %||% parsed$message %||% parsed$error %||% NULL
    if (is.list(msg)) msg <- msg$message %||% jsonlite::toJSON(msg, auto_unbox = TRUE)
  }
  if (is.null(msg) || !nzchar(as.character(msg))) {
    msg <- trimws(substr(raw, 1, 280))
  }
  msg <- as.character(msg %||% "")
  status <- as.integer(status)
  tip <- ""
  if (identical(status, 429L) || grepl("retry shortly|rate.?limit|verify available credits", msg, ignore.case = TRUE)) {
    tip <- " Tip: temporary OpenRouter rate/credits-check delay — wait a few seconds and send again (not a model-id error)."
  } else if (identical(status, 402L) || grepl("Payment Required|insufficient.?credit|no credits|out of credits|billing", msg, ignore.case = TRUE)) {
    tip <- " Tip: OpenRouter/OpenAI account has no credits — add balance, or switch provider / use a free model."
  } else if (identical(status, 400L) && grepl("openrouter", base_url, ignore.case = TRUE)) {
    tip <- paste0(" Tip: OpenRouter model ids look like `openai/gpt-4o-mini` or `deepseek/deepseek-chat` (got `", model, "`).")
  } else if (identical(status, 401L)) {
    tip <- " Tip: API key invalid — paste a valid key in ⚙ settings."
  }
  paste0("API HTTP ", status, if (nzchar(model)) paste0(" · model ", model) else "", ": ", msg, tip)
}

ai_chat_request <- function(messages, cfg, timeout_sec = 90, max_tokens = NULL) {
  # Revalidate at the network boundary, including callers other than the UI.
  valid <- tryCatch({
    host <- ai_env_config()
    ai_validate_base_url(cfg$base_url, c(ai_allowed_endpoints(), sub("/+$", "", host$base_url)))
    if (nzchar(host$api_key) && identical(cfg$api_key, host$api_key) &&
        !identical(cfg$base_url, sub("/+$", "", host$base_url))) {
      stop("The author key can only use the author configuration.")
    }
    TRUE
  }, error = function(e) FALSE)
  if (!valid) return(list(ok = FALSE, error = "Invalid AI connection configuration."))
  if (!nzchar(cfg$api_key)) {
    return(list(ok = FALSE, error = "No API key. Set your key in AI Settings, or ask the author to configure XMOSOME_AI_API_KEY."))
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    return(list(ok = FALSE, error = "Package jsonlite is required for the AI assistant."))
  }

  url <- paste0(cfg$base_url, "/chat/completions")
  body <- list(
    model = cfg$model,
    messages = messages,
    temperature = 0.2
  )
  if (!is.null(max_tokens) && is.finite(as.numeric(max_tokens))) {
    body$max_tokens <- as.integer(max_tokens)
  }
  payload <- jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")

  is_openrouter <- grepl("openrouter\\.ai", cfg$base_url, ignore.case = TRUE)
  hdrs <- list(
    Authorization = paste("Bearer", cfg$api_key),
    `Content-Type` = "application/json"
  )
  if (is_openrouter) {
    hdrs[["HTTP-Referer"]] <- "https://xmosome-blupf90kit.local"
    hdrs[["X-Title"]] <- "Xmosome_blupf90kit"
  }

  res_txt <- NULL
  status <- NA_integer_
  err <- NULL

  do_post <- function() {
    out <- list(err = NULL, status = NA_integer_, body = NULL)
    if (requireNamespace("httr2", quietly = TRUE)) {
      req <- httr2::request(url) |>
        httr2::req_headers(!!!hdrs) |>
        httr2::req_options(followlocation = FALSE) |>
        httr2::req_body_raw(payload, "application/json") |>
        httr2::req_timeout(timeout_sec) |>
        httr2::req_error(is_error = function(resp) FALSE)
      resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
      if (inherits(resp, "error")) {
        out$err <- ai_strip_ansi(conditionMessage(resp))
      } else {
        out$status <- httr2::resp_status(resp)
        out$body <- httr2::resp_body_string(resp)
      }
    } else if (requireNamespace("httr", quietly = TRUE)) {
      resp <- tryCatch(
        httr::POST(
          url,
          httr::add_headers(.headers = unlist(hdrs)),
          body = payload,
          encode = "raw",
          httr::timeout(timeout_sec),
          httr::config(followlocation = FALSE)
        ),
        error = function(e) e
      )
      if (inherits(resp, "error")) {
        out$err <- ai_strip_ansi(conditionMessage(resp))
      } else {
        out$status <- httr::status_code(resp)
        out$body <- httr::content(resp, as = "text", encoding = "UTF-8")
      }
    } else {
      out$err <- "Install httr2 or httr to use the AI assistant."
    }
    out
  }

  result <- do_post()
  # One automatic retry on transient OpenRouter 429 / credits-check lag
  if (is.null(result$err) && identical(as.integer(result$status), 429L)) {
    Sys.sleep(1.8)
    result <- do_post()
  }

  err <- result$err
  status <- result$status
  res_txt <- result$body

  if (!is.null(err)) return(list(ok = FALSE, error = "AI connection failed or timed out. Check the approved provider and try again."))
  if (is.na(status) || status < 200 || status >= 300) {
    # Do not echo untrusted response bodies: a remote endpoint may echo secrets.
    return(list(ok = FALSE, error = ai_format_http_error(status, "", cfg$model, cfg$base_url)))
  }

  parsed <- tryCatch(jsonlite::fromJSON(res_txt, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(parsed)) return(list(ok = FALSE, error = "Could not parse API JSON response."))
  content <- tryCatch(parsed$choices[[1]]$message$content, error = function(e) NULL)
  if (is.null(content) || !nzchar(content)) {
    return(list(ok = FALSE, error = "Empty model response."))
  }
  list(ok = TRUE, content = content, raw = parsed, model = cfg$model)
}

# Lightweight provider ping used by Settings → Test provider.
ai_test_provider <- function(cfg, timeout_sec = 25) {
  t0 <- proc.time()[["elapsed"]]
  result <- ai_chat_request(
    messages = list(list(role = "user", content = "Reply with the single word: ok")),
    cfg = cfg,
    timeout_sec = timeout_sec,
    max_tokens = 8L
  )
  elapsed_ms <- as.integer(round((proc.time()[["elapsed"]] - t0) * 1000))
  if (!isTRUE(result$ok)) {
    return(list(
      ok = FALSE,
      error = result$error %||% "Provider test failed.",
      ms = elapsed_ms,
      model = cfg$model %||% ""
    ))
  }
  model_id <- result$model %||% cfg$model %||% ""
  list(
    ok = TRUE,
    message = paste0("Provider OK · ", model_id, " · ", elapsed_ms, " ms"),
    model = model_id,
    ms = elapsed_ms
  )
}

# Embedded domain guidance travels with every AI request; no external skill path required.
ai_genetic_parameter_guidance <- function() {
  paste(c(
    "Genetic parameters and uncertainty (BLUPF90 skill: blupf90plus_reml.md, renumf90.md, gibbs.md):",
    "- When asked to add genetic parameters or their SE, generate actual OPTION se_covar_function lines, not only mathematical explanations. When drafting a VCE card with auto_se_covar enabled, include the applicable functions unless the user asks otherwise.",
    "- Syntax: OPTION se_covar_function unique_label expression. Expressions contain NO spaces; use **0.5 for square roots. G_e1_e2_t1_t2 uses EFFECT numbers in the final renf90.par and trait indices 1..n, NOT raw data columns. R_t1_t2 is the residual covariance.",
    "- Example: TRAITS 7 8 means formula trait indices 1 and 2. It NEVER means G_a_a_7_8. Count fixed classes, fixed covariates and random effects when determining effect indices.",
    "- Derive the effect map from the proposed card, not merely the UI template or previous formulas. RENUMF90 optional expansion is animal, mat, mpe, pe (include only enabled effects). Verify against generated renf90.par. If RR, nested structures or manual application cards make indices uncertain, identify exactly what is missing; do not invent indices.",
    "- Before returning a revised card, audit existing se_covar_function lines too: remove duplicate labels and replace obsolete indices/denominators when the model changes. Preserve unrelated options and fitted covariance values.",
    "- For an ordinary animal model with independent additional random effects, P_t = sum of all relevant random variances + R_t_t; h2_t = G_a_a_t_t/P_t. Include litter, PE and other modeled variance components in P_t, not just animal plus residual.",
    "- Repeated records with animal and permanent environment: repeatability_t=(G_a_a_t_t+G_pe_pe_t_t)/P_t. With other shared effects, define which effects are shared between repeated records before adding them to the numerator.",
    "- Pairwise additive genetic correlation: G_a_a_i_j/(G_a_a_i_i*G_a_a_j_j)**0.5; residual correlation: R_i_j/(R_i_i*R_j_j)**0.5. Generate every requested trait pair. Include maternal correlations only when the relevant covariance is fitted.",
    "- Ordinary direct-maternal convention from the skill: P_t = V_A + V_M + C_AM + V_MPE + V_PE + other applicable variances + V_E. The coefficient of C_AM is 1 under the usual animal/dam relationship assumption, not automatically 2. State this assumption and adapt if the design implies a different relationship.",
    "- Maternal model: direct h2=V_A/P; maternal m2=V_M/P; total h2=(V_A+1.5*C_AM+0.5*V_M)/P; direct-maternal correlation=C_AM/(V_A*V_M)**0.5. Distinguish direct, maternal and total heritability in labels and explanations.",
    "- Phenotypic correlations require the full model-implied covariance, including all relevant random components and direct-maternal relationship coefficients. Do not reuse a generic animal-only formula for maternal, RR or heterogeneous-residual models. For RR or heterogeneous residuals specify the evaluation point/basis or stratum before claiming a single h2.",
    "- SE example ONLY (animal effect 4, PE effect 5, trait 1): OPTION se_covar_function h2_1 G_4_4_1_1/(G_4_4_1_1+G_5_5_1_1+R_1_1)",
    "- Repeated-record example with the same indices: OPTION se_covar_function rep_1 (G_4_4_1_1+G_5_5_1_1)/(G_4_4_1_1+G_5_5_1_1+R_1_1)",
    "- Two-trait example ONLY (animal effect 4): OPTION se_covar_function rg_1_2 G_4_4_1_2/(G_4_4_1_1*G_4_4_2_2)**0.5",
    "- Add OPTION samples_se_covar_function 10000 and OPTION out_se_covar_function once when requesting REML sampling-based uncertainty, unless already present or the user supplied a different sample count. This is not a Gibbs chain-length setting.",
    "- REML: use OPTION method VCE with AI information for SE. Pure EM supplies no AI matrix for these SE; explain the conflict and propose EM-REML AI or AI conv as appropriate, rather than claiming pure EM produces SE. Do not silently change a requested fixed-variance BLUP into variance estimation.",
    "- OPTION sol se concerns solution/EBV standard errors, NOT SE of h2 or rg. Formula outputs require execution; never fabricate numerical SE from a parameter card. Explain Sample Mean and Sample SD (approximate SE) for REML sampling output.",
    "- Gibbs: use posterior functions in postgibbsf90 after burn-in/convergence checks. Report posterior SD and credible/HPD intervals, distinguish them from frequentist REML SE. fixed_var does not estimate variance-component uncertainty.",
    "- In the reply, briefly give the effect/trait map, define each parameter and its denominator, and state the run needed to obtain uncertainty. If editing, place the complete updated card including all formula OPTION lines inside the existing proposal markers."
  ), collapse = "\n")
}

ai_skill_system_prompt <- function() {
  paste(collapse = "\n", c(
    "You are the BLUPF90 skill assistant inside Xmosome_blupf90kit (a renumf90 parameter-card builder).",
    "Follow the BLUPF90 skill: generate, explain, and check renumf90 instruction files (renum.par) and renf90.par.",
    "Reply language (apply independently on EVERY turn):",
    "- Use the language of the latest actual user question: English question -> English reply; Chinese question -> Chinese reply. Match Simplified or Traditional Chinese when evident.",
    "- If the latest question explicitly requests a response language, follow that request instead.",
    "- Determine language from the user's natural-language request, not from parameter cards, code, filenames, column names, quoted material, or the automatically supplied application context.",
    "- For mixed Chinese/English questions, use the main language of the request. Chinese prose containing English BLUPF90 terms still requires a Chinese reply.",
    "- If the latest message has no identifiable natural-language request (for example, only code or numbers), use the most recent identifiable user-question language; default to English if none exists.",
    "- Switch languages immediately when the user switches. Earlier assistant replies and the application interface language must not override the current question's language.",
    "- Apply this to all explanatory prose, headings, summaries, follow-up questions, and proposal explanations. Do not add a bilingual translation unless requested.",
    "- Preserve BLUPF90 keywords, parameter syntax, proposal markers, file paths, variable names, and exact UI button labels unchanged.",
    "",
    "Card rules (critical):",
    "- Keywords at column 1, fixed order; value on the next line.",
    "- renumf90 order: DATAFILE → TRAITS → FIELDS_PASSED TO OUTPUT → WEIGHT(S) → RESIDUAL_VARIANCE → EFFECT → RANDOM → OPTIONAL → FILE → FILE_POS → SNP_FILE → PED_DEPTH → UPG_TYPE → … → (CO)VARIANCES → OPTION.",
    "- OPTIONAL effects: pe / mat / mpe (mpe needs mat). Missing parents = 0. OPTION missing must be integer.",
    "- Templates T1–T17 exist in the UI; prefer matching the user's selected template when sensible.",
    "",
    ai_genetic_parameter_guidance(),
    "",
    "When you propose a full updated parameter card, wrap it EXACTLY like this (no other fences for the card):",
    "<<<PAR_PROPOSAL>>>",
    "...full renum.par or renf90.par text...",
    "<<<END_PAR_PROPOSAL>>>",
    "Only mention Review changes / Accept & insert when you actually emit a PAR_PROPOSAL block in that same reply.",
    "If you are still asking clarifying questions or only listing a few OPTION lines, do NOT mention Review changes and do NOT emit a PAR_PROPOSAL.",
    "When the user asks you to build/update/export a card and enough information is available, emit the complete card in a PAR_PROPOSAL block instead of only describing it.",
    "Outside the proposal markers, write a short explanation.",
    "If only checking / explaining, do NOT emit a PAR_PROPOSAL block.",
    "You can see the current card, phenotype header, and mapped roles in the context message."
  ))
}

ai_build_context <- function(param_text, pheno_header, mapping, template_label, extras = list()) {
  lines <- c(
    "## App context",
    paste0("Selected template: ", template_label %||% "(unknown)"),
    "",
    "## Phenotype header (column names / positions)",
    if (length(pheno_header)) {
      paste0(seq_along(pheno_header), ": ", pheno_header, collapse = "\n")
    } else {
      "(no phenotype uploaded yet)"
    },
    "",
    "## Current column mapping",
    sprintf("traits: %s", { x <- paste(mapping$traits %||% character(), collapse = ", "); if (nzchar(x)) x else "(none)" }),
    sprintf("fixed: %s", { x <- paste(mapping$fixed %||% character(), collapse = ", "); if (nzchar(x)) x else "(none)" }),
    sprintf("random: %s", { x <- paste(mapping$random %||% character(), collapse = ", "); if (nzchar(x)) x else "(none)" }),
    sprintf("animal: %s", { x <- paste(mapping$animal %||% character(), collapse = ", "); if (nzchar(x)) x else "(none)" }),
    sprintf("OPTIONAL: pe=%s mat=%s mpe=%s",
            isTRUE(extras$pe), isTRUE(extras$mat), isTRUE(extras$mpe)),
    "",
    "## Genetic-parameter preferences (card settings take precedence if UI is stale)",
    sprintf("UI method: %s; include genetic-parameter functions: %s; pure EM: %s; Gibbs: %s",
            extras$method %||% "unspecified", isTRUE(extras$auto_se_covar),
            isTRUE(extras$em_pure), isTRUE(extras$gibbs)),
    "Raw trait column positions are NOT formula trait indices. Inspect the actual card effect order before building G_e1_e2_t1_t2 functions.",
    "",
    "## Current parameter card",
    "```",
    if (nzchar(param_text %||% "")) param_text else "(empty)",
    "```"
  )
  paste(lines, collapse = "\n")
}

ai_extract_proposal <- function(text) {
  text <- as.character(text %||% "")
  m <- regexec("<<<PAR_PROPOSAL>>>\\s*([\\s\\S]*?)\\s*<<<END_PAR_PROPOSAL>>>", text, perl = TRUE)
  r <- regmatches(text, m)[[1]]
  if (length(r) >= 2) {
    proposal <- trimws(r[2])
    reply <- trimws(sub("<<<PAR_PROPOSAL>>>[\\s\\S]*<<<END_PAR_PROPOSAL>>>", "", text, perl = TRUE))
    if (!nzchar(reply)) {
      reply <- "I prepared an updated parameter card. Click **Review changes** to preview the diff, then Accept to apply it."
    }
    return(list(reply = reply, proposal = proposal))
  }
  # Fallback: fenced markdown block that looks like a renumf90/renf90 card.
  fence <- regexec("```(?:[A-Za-z0-9_-]+)?\\s*\\n([\\s\\S]*?)```", text, perl = TRUE)
  fr <- regmatches(text, fence)[[1]]
  if (length(fr) >= 2) {
    cand <- trimws(fr[2])
    if (grepl("\\bDATAFILE\\b", cand) && grepl("\\bTRAITS\\b", cand) && grepl("\\bEFFECT\\b", cand)) {
      reply <- trimws(sub("```(?:[A-Za-z0-9_-]+)?\\s*\\n[\\s\\S]*?```", "", text, perl = TRUE))
      if (!nzchar(reply)) {
        reply <- "I prepared an updated parameter card. Click **Review changes** to preview the diff, then Accept to apply it."
      }
      return(list(reply = reply, proposal = cand))
    }
  }
  list(reply = text, proposal = NULL)
}

# Simple unified line diff (old → new) for the UI
ai_unified_diff <- function(old, new) {
  a <- strsplit(old %||% "", "\n", fixed = TRUE)[[1]]
  b <- strsplit(new %||% "", "\n", fixed = TRUE)[[1]]
  # Myers-ish LCS DP (fine for typical par files)
  n <- length(a); m <- length(b)
  if (n > 4000 || m > 4000) {
    return(list(
      rows = list(
        list(type = "del", text = paste(a, collapse = "\n")),
        list(type = "add", text = paste(b, collapse = "\n"))
      )
    ))
  }
  L <- matrix(0L, n + 1, m + 1)
  if (n && m) {
    for (i in seq_len(n)) {
      for (j in seq_len(m)) {
        if (identical(a[i], b[j])) L[i + 1, j + 1] <- L[i, j] + 1L
        else L[i + 1, j + 1] <- max(L[i + 1, j], L[i, j + 1])
      }
    }
  }
  rows <- list()
  i <- n; j <- m
  stack <- list()
  while (i > 0 || j > 0) {
    if (i > 0 && j > 0 && identical(a[i], b[j])) {
      stack[[length(stack) + 1]] <- list(type = "ctx", text = a[i])
      i <- i - 1; j <- j - 1
    } else if (j > 0 && (i == 0 || L[i + 1, j] >= L[i, j + 1])) {
      stack[[length(stack) + 1]] <- list(type = "add", text = b[j])
      j <- j - 1
    } else if (i > 0) {
      stack[[length(stack) + 1]] <- list(type = "del", text = a[i])
      i <- i - 1
    }
  }
  list(rows = rev(stack))
}

## MCP-style tools for easyblup (Shiny)
## These tools are designed to be used by an LLM via OpenAI "tools" (function calling),
## and executed inside the Shiny server to update UI inputs and internal state.

`%||%` <- function(x, y) if (is.null(x) || identical(x, "")) y else x

.easyblup_allowed_inputs <- function() {
  # Map inputId -> type.
  # Types: checkbox, text, textarea, numeric, select, select_multiple,
  #        selectize, selectize_multiple, radio, directory
  list(
    # Left panel: model builder selectors (selectize)
    geno_format = "selectize",
    traits_select = "selectize_multiple",
    fixed_select = "selectize_multiple",
    animal_select = "selectize",
    random_select = "selectize_multiple",

    # Optional effects
    opt_pe = "checkbox",
    opt_mat = "checkbox",
    opt_mpe = "checkbox",

    # Right panel - basic options
    opt_remove_all_missing = "checkbox",
    opt_missing_in_weights = "checkbox",
    opt_no_basic_statistics = "checkbox",
    opt_missing_value = "text",

    # Analysis method options
    opt_method = "select",
    opt_sol_se = "checkbox",
    opt_conv_crit_val = "text",
    opt_em_reml_rounds = "numeric",
    opt_em_reml_pure = "checkbox",
    opt_em_reml_ai_conv = "checkbox",
    opt_use_yams = "checkbox",
    opt_tuned_g2 = "checkbox",
    opt_maxrounds_val = "numeric",
    opt_solv_method = "select",
    opt_r_factor = "numeric",
    opt_blksize = "numeric",
    opt_residual_out = "checkbox",
    opt_stdresidual_out = "checkbox",
    opt_prior_solutions = "checkbox",
    opt_set_eig = "text",
    opt_auto_se_covar = "checkbox",

    # Solution output
    opt_origID = "checkbox",

    # Accuracy / reliability
    opt_store_accuracy = "checkbox",
    opt_store_accuracy_orig = "checkbox",
    opt_acctype = "select",
    opt_correct_acc_inb_direct0 = "checkbox",

    # Genomic ssGBLUP
    opt_snp_p_value = "checkbox",
    opt_omit_ainv = "checkbox",
    opt_TauOmega = "text",
    opt_AlphaBeta = "text",

    # Heterogeneous residual weights
    opt_hetres_pos = "select_multiple",
    opt_hetres_pol_preset = "select",

    # RENUMF90
    opt_ped_search_complete = "checkbox",
    opt_use_ped_depth = "checkbox",
    opt_ped_depth = "numeric",
    opt_inbreeding_method = "select",

    # Genotype conversion modal (directory selection)
    geno_convert_direction = "radio",
    geno_convert_output_dir_blup = "directory",
    geno_convert_output_dir = "directory",

    # AI settings panel (selectize/text/numeric/textarea)
    ai_model = "selectize",
    ai_api_base = "text",
    ai_api_key = "text",
    ai_organization = "text",
    ai_temperature = "numeric",
    ai_max_tokens = "numeric",
    ai_system_prompt = "textarea"
  )
}

easyblup_mcp_tool_specs <- function() {
  list(
    list(
      name = "get_app_state",
      description = "Get current easyblup state: selected columns and key options.",
      inputSchema = list(type = "object", properties = list(), additionalProperties = FALSE)
    ),
    list(
      name = "set_columns",
      description = "Set column selections in the model builder (traits/fixed/random/animal).",
      inputSchema = list(
        type = "object",
        properties = list(
          traits = list(type = "array", items = list(type = "string")),
          fixed = list(type = "array", items = list(type = "string")),
          random = list(type = "array", items = list(type = "string")),
          animal = list(type = "string")
        ),
        additionalProperties = FALSE
      )
    ),
    list(
      name = "set_optional_effects",
      description = "Toggle optional effects (pe/mat/mpe).",
      inputSchema = list(
        type = "object",
        properties = list(
          pe = list(type = "boolean"),
          mat = list(type = "boolean"),
          mpe = list(type = "boolean")
        ),
        additionalProperties = FALSE
      )
    ),
    list(
      name = "set_inputs",
      description = "Set one or more Shiny inputs (whitelisted). Provide a named object {inputId: value}.",
      inputSchema = list(
        type = "object",
        properties = list(
          inputs = list(type = "object", additionalProperties = TRUE)
        ),
        required = list("inputs"),
        additionalProperties = FALSE
      )
    ),
    list(
      name = "set_parameter_file",
      description = "Replace the parameter editor content with the provided text.",
      inputSchema = list(
        type = "object",
        properties = list(
          content = list(type = "string")
        ),
        required = list("content"),
        additionalProperties = FALSE
      )
    )
  )
}

easyblup_openai_tools <- function() {
  # Convert MCP-style specs into OpenAI tool schema
  lapply(easyblup_mcp_tool_specs(), function(t) {
    list(
      type = "function",
      `function` = list(
        name = t$name,
        description = t$description,
        parameters = t$inputSchema
      )
    )
  })
}

.easyblup_safe_num <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  if (!length(v) || is.na(v) || !is.finite(v)) NULL else v
}

.easyblup_safe_bool <- function(x) {
  if (is.logical(x) && length(x) == 1) return(isTRUE(x))
  if (is.numeric(x) && length(x) == 1) return(isTRUE(x != 0))
  if (is.character(x) && length(x) == 1) return(tolower(x) %in% c("true", "t", "1", "yes", "y"))
  NULL
}

.easyblup_safe_chr <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.character(x)) return(as.character(x))
  as.character(x)
}

.easyblup_set_one_input <- function(session, inputId, type, value) {
  if (type == "checkbox") {
    b <- .easyblup_safe_bool(value)
    if (!is.null(b)) shiny::updateCheckboxInput(session, inputId, value = b)
    return(invisible(TRUE))
  }
  if (type == "text") {
    shiny::updateTextInput(session, inputId, value = as.character(value %||% ""))
    return(invisible(TRUE))
  }
  if (type == "textarea") {
    if (isTRUE(exists("updateTextAreaInput", where = asNamespace("shiny"), mode = "function"))) {
      shiny::updateTextAreaInput(session, inputId, value = as.character(value %||% ""))
    } else {
      shiny::updateTextInput(session, inputId, value = as.character(value %||% ""))
    }
    return(invisible(TRUE))
  }
  if (type == "numeric") {
    n <- .easyblup_safe_num(value)
    if (!is.null(n)) shiny::updateNumericInput(session, inputId, value = n)
    return(invisible(TRUE))
  }
  if (type == "select") {
    shiny::updateSelectInput(session, inputId, selected = as.character(value %||% ""))
    return(invisible(TRUE))
  }
  if (type == "select_multiple") {
    vals <- .easyblup_safe_chr(value)
    if (!is.null(vals)) {
      shiny::updateSelectInput(session, inputId, selected = vals)
    }
    return(invisible(TRUE))
  }
  if (type == "selectize") {
    shiny::updateSelectizeInput(session, inputId, selected = as.character(value %||% ""))
    return(invisible(TRUE))
  }
  if (type == "selectize_multiple") {
    vals <- .easyblup_safe_chr(value)
    shiny::updateSelectizeInput(session, inputId, selected = vals %||% character(0))
    return(invisible(TRUE))
  }
  if (type == "radio") {
    shiny::updateRadioButtons(session, inputId, selected = as.character(value %||% ""))
    return(invisible(TRUE))
  }
  if (type == "directory") {
    # If shinyDirectoryInput is available and the directoryInput widget is used, update it properly.
    v <- as.character(value %||% "")
    if (requireNamespace("shinyDirectoryInput", quietly = TRUE) &&
        isTRUE(exists("updateDirectoryInput", where = asNamespace("shinyDirectoryInput"), mode = "function"))) {
      shinyDirectoryInput::updateDirectoryInput(session, inputId, value = v)
    } else {
      shiny::updateTextInput(session, inputId, value = v)
    }
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

easyblup_mcp_call_tool <- function(name, arguments, session, input, values) {
  # Returns list(ok=TRUE, result=...) or list(ok=FALSE, error=...)
  tryCatch({
    if (identical(name, "get_app_state")) {
      out <- list(
        traits = values$traits,
        fixed = values$fixed,
        random = values$random,
        animal = values$animal,
        opt = list(
          method = input$opt_method %||% NULL,
          pe = isTRUE(input$opt_pe),
          mat = isTRUE(input$opt_mat),
          mpe = isTRUE(input$opt_mpe)
        ),
        current_param = values$current_param %||% ""
      )
      return(list(ok = TRUE, result = out))
    }

    if (identical(name, "set_columns")) {
      if (!is.null(arguments$traits)) shiny::updateSelectizeInput(session, "traits_select", selected = .easyblup_safe_chr(arguments$traits))
      if (!is.null(arguments$fixed))  shiny::updateSelectizeInput(session, "fixed_select", selected = .easyblup_safe_chr(arguments$fixed))
      if (!is.null(arguments$random)) shiny::updateSelectizeInput(session, "random_select", selected = .easyblup_safe_chr(arguments$random))
      if (!is.null(arguments$animal)) shiny::updateSelectizeInput(session, "animal_select", selected = as.character(arguments$animal))
      return(list(ok = TRUE, result = list(message = "columns updated")))
    }

    if (identical(name, "set_optional_effects")) {
      if (!is.null(arguments$pe))  shiny::updateCheckboxInput(session, "opt_pe", value = isTRUE(arguments$pe))
      if (!is.null(arguments$mat)) shiny::updateCheckboxInput(session, "opt_mat", value = isTRUE(arguments$mat))
      if (!is.null(arguments$mpe)) shiny::updateCheckboxInput(session, "opt_mpe", value = isTRUE(arguments$mpe))
      return(list(ok = TRUE, result = list(message = "optional effects updated")))
    }

    if (identical(name, "set_inputs")) {
      inputs <- arguments$inputs
      if (is.null(inputs) || !is.list(inputs)) {
        return(list(ok = FALSE, error = "inputs must be an object"))
      }
      allowed <- .easyblup_allowed_inputs()
      changed <- list()
      skipped <- list()
      for (k in names(inputs)) {
        if (!k %in% names(allowed)) {
          skipped[[k]] <- "not allowed"
          next
        }
        .easyblup_set_one_input(session, k, allowed[[k]], inputs[[k]])
        changed[[k]] <- inputs[[k]]
      }
      return(list(ok = TRUE, result = list(changed = changed, skipped = skipped)))
    }

    if (identical(name, "set_parameter_file")) {
      txt <- as.character(arguments$content %||% "")
      values$ai_applied <- TRUE
      values$current_param <- txt
      session$sendCustomMessage("update_textarea", list(content = values$current_param))
      return(list(ok = TRUE, result = list(message = "parameter editor updated", nchar = nchar(txt))))
    }

    list(ok = FALSE, error = paste0("Unknown tool: ", name))
  }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
}


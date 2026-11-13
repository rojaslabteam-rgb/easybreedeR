# easybreedeR usage report: how many people used the deployed app, and for how long.
#
# The deployed app writes one "EB_USAGE {json}" line when a session starts and
# one when it ends (see eb_track_session() in inst/Runtime.R). Visitors are
# anonymous salted hashes; no addresses or uploaded data are recorded.
#
#   source("scripts/usage_report.R")
#   usage_report(account = "myaccount")                 # shinyapps.io logs
#   usage_report(log_dir = "/var/lib/easybreeder/usage") # EASYBREEDER_USAGE_DIR on your own server
#
# shinyapps.io only keeps recent log lines. Each run therefore merges what it
# finds into a local archive (scripts/usage_archive.jsonl, gitignored), so run
# it regularly (e.g. weekly) and the history accumulates on your computer.
#
# Platform totals that need no code at all:
#   rsconnect::showUsage(appName = "easybreeder-suite", account = "myaccount", from = "30d")  # active hours
#   or the "Metrics" tab of the app on https://www.shinyapps.io

.usage_archive_default <- function() {
  root <- if (exists("PROJECT_ROOT", inherits = TRUE)) get("PROJECT_ROOT", inherits = TRUE) else getwd()
  file.path(root, "scripts", "usage_archive.jsonl")
}

# Pull the JSON payloads out of arbitrary log text.
usage_extract <- function(lines) {
  lines <- unlist(lapply(lines, as.character), use.names = FALSE)
  hits <- regmatches(lines, regexpr("EB_USAGE \\{.*\\}", lines))
  sub("^EB_USAGE ", "", hits)
}

usage_parse <- function(json_lines) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' is required.", call. = FALSE)
  rows <- lapply(unique(json_lines), function(x) tryCatch(jsonlite::fromJSON(x), error = function(e) NULL))
  rows <- rows[!vapply(rows, is.null, logical(1))]
  field <- function(name, default) vapply(rows, function(r) {
    v <- r[[name]]
    if (is.null(v) || length(v) != 1L) default else v
  }, default)
  data.frame(
    event = field("event", NA_character_), app = field("app", NA_character_),
    sid = field("sid", NA_character_), visitor = field("visitor", NA_character_),
    ts = as.POSIXct(field("ts", NA_character_), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    duration_sec = field("duration_sec", NA_real_),
    stringsAsFactors = FALSE
  )
}

# One row per session; sessions whose "end" line is missing have NA duration.
usage_sessions <- function(events) {
  events <- events[!is.na(events$sid) & !is.na(events$ts), , drop = FALSE]
  if (!nrow(events)) {
    return(data.frame(sid = character(), app = character(), visitor = character(),
                      start = as.POSIXct(character(), tz = "UTC"), duration_min = numeric()))
  }
  key <- paste(events$app, events$sid)
  do.call(rbind, lapply(split(events, key), function(e) {
    end <- e[e$event == "end", , drop = FALSE]
    dur <- if (nrow(end)) end$duration_sec[1] else NA_real_
    start <- if (any(e$event == "start")) min(e$ts[e$event == "start"]) else min(e$ts) - ifelse(is.na(dur), 0, dur)
    data.frame(sid = e$sid[1], app = e$app[1], visitor = e$visitor[1], start = start,
               duration_min = dur / 60, stringsAsFactors = FALSE, row.names = NULL)
  }))
}

#' Summarise app usage
#' @param account,appName shinyapps.io account and app; omit `account` to skip fetching platform logs
#' @param entries how many recent log lines to request from the platform
#' @param log_dir optional directory of usage-YYYY-MM.jsonl files (EASYBREEDER_USAGE_DIR)
#' @param archive local file the collected events are merged into; NULL to disable
#' @param from,until optional Date range (UTC) for the summary
#' @return invisibly, a list with `overall`, `by_app`, `by_day` and `sessions`
usage_report <- function(account = NULL, appName = "easybreeder-suite", entries = 5000,
                         log_dir = NULL, archive = .usage_archive_default(),
                         from = NULL, until = NULL) {
  json <- character()
  if (!is.null(archive) && file.exists(archive)) json <- readLines(archive, warn = FALSE)
  if (!is.null(log_dir)) {
    for (f in list.files(log_dir, pattern = "^usage-.*\\.jsonl$", full.names = TRUE)) {
      json <- c(json, readLines(f, warn = FALSE))
    }
  }
  if (!is.null(account)) {
    if (!requireNamespace("rsconnect", quietly = TRUE)) stop("Package 'rsconnect' is required.", call. = FALSE)
    fetched <- rsconnect::getLogs(appName = appName, account = account, entries = entries)
    json <- c(json, usage_extract(fetched))
  }
  json <- unique(json[nzchar(json)])
  if (!is.null(archive) && length(json)) writeLines(json, archive)

  sessions <- usage_sessions(usage_parse(json))
  if (!is.null(from)) sessions <- sessions[as.Date(sessions$start) >= as.Date(from), , drop = FALSE]
  if (!is.null(until)) sessions <- sessions[as.Date(sessions$start) <= as.Date(until), , drop = FALSE]
  if (!nrow(sessions)) {
    message("No usage events found yet.")
    return(invisible(list(overall = NULL, by_app = NULL, by_day = NULL, sessions = sessions)))
  }

  summarise <- function(d) data.frame(
    visitors = length(unique(d$visitor)),
    sessions = nrow(d),
    total_hours = round(sum(d$duration_min, na.rm = TRUE) / 60, 2),
    median_min = round(stats::median(d$duration_min, na.rm = TRUE), 1),
    mean_min = round(mean(d$duration_min, na.rm = TRUE), 1)
  )
  by <- function(d, f, name) {
    out <- do.call(rbind, lapply(split(d, f), summarise))
    out <- cbind(setNames(data.frame(rownames(out), stringsAsFactors = FALSE), name), out)
    rownames(out) <- NULL
    out
  }
  # A visit to the Studio is one person using the app; embedded tools are counted per tool.
  top <- sessions[sessions$app == "studio", , drop = FALSE]
  if (!nrow(top)) top <- sessions
  result <- list(
    overall = cbind(from = min(as.Date(top$start)), until = max(as.Date(top$start)), summarise(top)),
    by_app = by(sessions, sessions$app, "app"),
    by_day = by(top, as.character(as.Date(top$start)), "day"),
    sessions = sessions
  )
  cat("== Overall (visits to the app) ==\n"); print(result$overall, row.names = FALSE)
  cat("\n== By tool ==\n"); print(result$by_app, row.names = FALSE)
  cat("\n== By day (UTC) ==\n"); print(result$by_day, row.names = FALSE)
  invisible(result)
}

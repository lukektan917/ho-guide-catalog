# 00_setup.R ─────────────────────────────────────────────────────────────────
# Shared setup for the Ho Family Student Guide tour catalog.
# Run everything from the project root (ho_guide_catalog/) so "data/..." resolves.

library(httr2)
library(rvest)
library(tidyr)
library(jsonlite)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(readr)

# ── Paths ───────────────────────────────────────────────────────────────────
DATA_DIR  <- "data"
CACHE_DIR <- file.path(DATA_DIR, "html_cache")
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

# ── Politeness ──────────────────────────────────────────────────────────────
# harvardartmuseums.org/robots.txt is ~110 named AI crawler user-agents
# (ClaudeBot, anthropic-ai, GPTBot, CCBot, ...) followed by `Disallow: /`.
# There is NO `User-agent: *` rule, so a self-identifying script like this one
# is not covered by any Disallow directive there -- but an AI assistant fetching
# on your behalf IS. That asymmetry is the whole reason this is a script you run
# rather than something an assistant runs for you.
#
# Keep the contact address real. It is the thing that gets you an email from a
# museum sysadmin instead of a silent IP block.
USER_AGENT <- paste0(
  "HoGuideCatalog/0.1 (Harvard student project; ",
  "your.email@example.com)"
)

REQ_PER_SEC <- 1   # ~210 pages at 1/sec is under 4 minutes. Do not raise this.

# ── Fetch with on-disk caching ──────────────────────────────────────────────
# Every page is cached under data/html_cache/ keyed by URL hash, so re-running
# the scrape (or iterating on the parser, which you will) costs zero requests.
# Delete the cache dir to force a genuine refetch.
cache_path <- function(url) {
  file.path(CACHE_DIR, paste0(substr(rlang::hash(url), 1, 16), ".html"))
}

fetch_html_raw <- function(url, refresh = FALSE) {
  cf <- cache_path(url)
  if (!refresh && file.exists(cf) && file.size(cf) > 0) {
    return(list(html = read_file(cf), status = 200L, cached = TRUE))
  }

  resp <- tryCatch(
    request(url) |>
      req_user_agent(USER_AGENT) |>
      req_throttle(rate = REQ_PER_SEC) |>
      req_timeout(30) |>
      req_retry(max_tries = 3, backoff = ~ 2^.x) |>
      # Do not throw on 404 -- a dead tour page is data (it tells us to fall
      # back to the Wayback snapshot), not an error worth aborting the run for.
      req_error(is_error = function(resp) FALSE) |>
      req_perform(),
    error = function(e) NULL
  )

  if (is.null(resp)) return(list(html = NA_character_, status = NA_integer_, cached = FALSE))

  st <- resp_status(resp)
  if (st != 200L) return(list(html = NA_character_, status = st, cached = FALSE))

  body <- resp_body_string(resp)
  write_file(body, cf)
  list(html = body, status = st, cached = FALSE)
}

# Wayback raw-content fallback for pages the live site has since removed.
# The `id_` suffix asks the Wayback Machine for the original bytes without its
# own injected toolbar/banner markup, which would otherwise pollute parsing.
wayback_url <- function(url, timestamp) {
  sprintf("https://web.archive.org/web/%sid_/%s", timestamp, url)
}

# ── Locale-independent date parsing ─────────────────────────────────────────
# Deliberately NOT as.Date(x, format = "%B %d, %Y") -- %B depends on LC_TIME,
# so that silently returns NA on a machine with a non-English locale. Explicit
# month lookup always works.
MONTH_NUM <- c(
  January = 1L, February = 2L, March     = 3L,  April   = 4L,  May      = 5L,  June     = 6L,
  July    = 7L, August   = 8L, September = 9L,  October = 10L, November = 11L, December = 12L
)

parse_page_date <- function(txt) {
  m <- str_match(
    txt,
    paste0("\\b(", paste(names(MONTH_NUM), collapse = "|"), ")\\s+(\\d{1,2}),\\s+(\\d{4})\\b")
  )
  if (is.na(m[1, 1])) return(as.Date(NA))
  as.Date(sprintf("%s-%02d-%02d", m[1, 4], MONTH_NUM[[m[1, 2]]], as.integer(m[1, 3])))
}

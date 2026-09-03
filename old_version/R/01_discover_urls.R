# 01_discover_urls.R ─────────────────────────────────────────────────────────
# Build the list of tour pages to scrape.
#
# WHY THE WAYBACK MACHINE AND NOT THE MUSEUM'S OWN CALENDAR:
# harvardartmuseums.org/calendar is JS-rendered and, as far as could be found,
# forward-looking only -- there is no past-event archive, no date filter, and
# no pagination backwards. /sitemap.xml returns an error page. So the live site
# will not tell you what tours happened in 2023. The Internet Archive will:
# its CDX API lists every URL it ever crawled under /calendar/, which recovers
# tour slugs that are no longer linked from anywhere on the live site.
#
# IMPORTANT CAVEAT: this is a FLOOR, not a census. Wayback only has what it
# happened to crawl, so a tour that ran once and was never crawled is invisible
# here. That is one of the reasons to also ask DAPP for the program's own
# records -- treat this list as "what the web remembers", not "what happened".
#
# Output: data/tour_urls.csv, data/discovery_summary.txt

source("R/00_setup.R")

CDX_ENDPOINT <- "http://web.archive.org/cdx/search/cdx"
CDX_RAW_FILE <- file.path(DATA_DIR, "cdx_calendar_raw.txt")
REFRESH_CDX  <- FALSE   # TRUE to re-query the archive instead of using the cache

# The CDX API throttles hard and answers 503 when it feels like it -- this query
# asks for tens of thousands of rows, so expect to be told to wait. Retry with a
# generous backoff, and cache the raw response so a rerun (or a crash in the
# parsing below) never costs another 503-and-wait cycle.
if (!REFRESH_CDX && file.exists(CDX_RAW_FILE) && file.size(CDX_RAW_FILE) > 0) {
  message("Using cached CDX response (", CDX_RAW_FILE, "). ",
          "Set REFRESH_CDX <- TRUE to re-query.")
  cdx_raw <- read_file(CDX_RAW_FILE)
} else {
  message("Querying Wayback CDX for harvardartmuseums.org/calendar* ",
          "(this is slow, and 503s are normal -- it retries) ...")

  resp <- request(CDX_ENDPOINT) |>
    req_user_agent(USER_AGENT) |>
    req_url_query(
      url          = "harvardartmuseums.org/calendar*",
      output       = "text",
      fl           = "original,timestamp",
      collapse     = "urlkey",          # one row per distinct URL
      filter       = "statuscode:200",
      limit        = 200000
    ) |>
    req_timeout(300) |>
    req_retry(
      max_tries    = 6,
      backoff      = ~ min(120, 10 * 2^.x),
      is_transient = function(resp) resp_status(resp) %in% c(429, 500, 502, 503, 504)
    ) |>
    req_perform()

  cdx_raw <- resp_body_string(resp)
  write_file(cdx_raw, CDX_RAW_FILE)
}

if (!nzchar(str_trim(cdx_raw))) {
  stop("CDX returned an empty response. The archive is likely throttling -- ",
       "wait a few minutes and rerun.")
}

cdx.df <- tibble(line = str_split_1(cdx_raw, "\n")) |>
  filter(line != "") |>
  separate_wider_delim(line, delim = " ", names = c("url", "timestamp"), too_many = "drop")

message(nrow(cdx.df), " distinct archived /calendar URLs found.")

# ── Normalize to one row per real page ──────────────────────────────────────
# Two kinds of duplication in the raw list:
#   1. utm_* tracking query strings. The museum posts the same event to the
#      college calendar, the Boston calendar, etc., each with its own utm tags,
#      so one page shows up 3-5 times. Stripping the query string collapses them.
#   2. Repeat runs of the SAME tour get numeric slug suffixes (-2, -3, -4 ...),
#      because guides give their tour several times over a term.
#
# The suffix strip has to dodge class years, which are also trailing digits:
# "...-with-devi-sharma-27" ends in the class year 27, while
# "...-with-devi-sharma-27-2" ends in run number 2. Run numbers realistically
# stay under 20 and class years sit at 24-30, so stripping a trailing -N only
# when N <= 19 separates them cleanly. If the program ever gives one tour 20+
# times, revisit this.
tours.df <- cdx.df |>
  mutate(
    path = str_remove(url, "\\?.*$"),
    path = str_remove(path, "^https?://"),
    path = str_remove(path, "^www\\."),
    slug = str_match(path, "^harvardartmuseums\\.org/(?:index\\.php/)?calendar/([A-Za-z0-9-]+)$")[, 2],
    slug = str_to_lower(slug)
  ) |>
  filter(!is.na(slug)) |>
  mutate(
    kind = case_when(
      str_starts(slug, "spotlight-tour-")             ~ "spotlight",
      str_starts(slug, "virtual-student-guide-tour")  ~ "virtual_generic",
      str_starts(slug, "student-guide-tour")          ~ "weekly_generic",
      TRUE                                            ~ "other"
    )
  ) |>
  filter(kind != "other") |>
  # Canonical slug = the tour identity, with the run number removed.
  mutate(slug_canonical = str_remove(slug, "-(?:[1-9]|1[0-9])$")) |>
  group_by(slug) |>
  slice_min(timestamp, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(
    slug,
    slug_canonical,
    kind,
    url_live       = paste0("https://harvardartmuseums.org/calendar/", slug),
    wayback_stamp  = timestamp,
    url_wayback    = wayback_url(paste0("https://harvardartmuseums.org/calendar/", slug), timestamp)
  ) |>
  arrange(kind, slug)

write_csv(tours.df, file.path(DATA_DIR, "tour_urls.csv"))

# ── Summary ─────────────────────────────────────────────────────────────────
summary_txt <- c(
  paste0("Wayback CDX discovery run: ", Sys.Date()),
  paste0("Archived /calendar URLs returned: ", nrow(cdx.df)),
  "",
  "Pages to scrape (one row per run / event instance):",
  capture.output(print(count(tours.df, kind))),
  "",
  "Distinct tours (run-number suffix collapsed):",
  capture.output(print(tours.df |> distinct(kind, slug_canonical) |> count(kind))),
  "",
  "NOTE: 'weekly_generic' and 'virtual_generic' pages carry a date but no",
  "guide name, theme, or object list -- the museum used a single boilerplate",
  "listing for the weekly tours. They are kept as deliberate stub records so",
  "the timeline shows its own holes; they are not scraping failures."
)
write_lines(summary_txt, file.path(DATA_DIR, "discovery_summary.txt"))
cat(paste(summary_txt, collapse = "\n"), "\n")
message("Wrote ", file.path(DATA_DIR, "tour_urls.csv"), " (", nrow(tours.df), " pages).")

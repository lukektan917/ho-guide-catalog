# catalog.R ──────────────────────────────────────────────────────────────────
#
# Builds a spreadsheet of every Ho Family Student Guide "Spotlight Tour":
# the theme, who gave it, when, and the three artworks they chose.
#
#   Run:     Rscript catalog.R          (from this folder)
#   Reads:   data/tour_urls.csv         (the tour list, already assembled)
#   Writes:  data/catalog.csv           (import this into your Google Sheet)
#
# Needs your Harvard Art Museums API key. Put this line in ~/.Renviron, then
# restart R:
#     HAM_API_KEY=your_key_here
#
# Two things this script does on purpose, both worth leaving alone:
#   1. It saves every page and API reply into data/ the first time it sees
#      them. Run it again and it reuses those files instead of re-downloading,
#      so fixing something costs zero traffic. To force fresh downloads,
#      delete the data/html_cache and data/api_cache folders.
#   2. It waits one second between downloads, so it behaves like a person
#      browsing rather than a burst of traffic.

library(httr2)      # downloading pages and calling the API
library(rvest)      # reading HTML
library(dplyr)      # tables
library(tidyr)      # reshaping tables
library(stringr)    # text patterns
library(purrr)      # looping over things
library(readr)      # reading and writing csv
library(jsonlite)   # reading the API's replies

HTML_CACHE <- "data/html_cache"
API_CACHE  <- "data/api_cache"
dir.create(HTML_CACHE, recursive = TRUE, showWarnings = FALSE)
dir.create(API_CACHE,  recursive = TRUE, showWarnings = FALSE)

# Identify yourself honestly. This is what lets a museum sysadmin email you
# with a question instead of just blocking you.
USER_AGENT <- "HoGuideCatalog/1.0 (Harvard student project; your.email@example.com)"

api_key <- Sys.getenv("HAM_API_KEY")
if (!nzchar(api_key)) stop("HAM_API_KEY is not set. Add it to ~/.Renviron and restart R.")


# ── Download something, reusing a saved copy if we already have one ─────────
download_cached <- function(url, cache_file) {
  if (file.exists(cache_file) && file.size(cache_file) > 0) {
    return(read_file(cache_file))
  }

  response <- tryCatch(
    request(url) |>
      req_user_agent(USER_AGENT) |>
      req_throttle(rate = 1) |>            # at most 1 download per second
      req_timeout(30) |>
      req_retry(max_tries = 3) |>
      req_error(is_error = function(resp) FALSE) |>   # handle 404s ourselves
      req_perform(),
    error = function(e) NULL
  )

  if (is.null(response) || resp_status(response) != 200) return(NA_character_)

  body <- resp_body_string(response)
  write_file(body, cache_file)
  body
}


# ── Pull the theme and guide names out of a page title ──────────────────────
# Page titles look like:
#   "Spotlight Tour: Colour and Memory, with Alex Rivera '28 | Harvard Art Museums"
#   "Spotlight Tour: A Noble Craft, with Sam Okonkwo '25 and Jo Bennett '25 | ..."
#   "Spotlight Tour: Breaking and Mending, with Maria Torres Vega '26 (in Spanish) | ..."
read_title <- function(page_title) {
  blank <- list(theme = NA_character_, guides = NA_character_,
                class_years = NA_character_, language = "English")
  if (is.na(page_title)) return(blank)

  text <- str_trim(str_remove(page_title, "\\s*\\|\\s*Harvard Art Museums\\s*$"))

  # Take "(in Spanish)" off the end first, so it isn't mistaken for a surname.
  found_language <- str_match(text, "\\((?:in\\s+)?([A-Za-z]+)\\)\\s*$")[, 2]
  if (!is.na(found_language)) {
    blank$language <- found_language
    text <- str_trim(str_remove(text, "\\s*\\((?:in\\s+)?[A-Za-z]+\\)\\s*$"))
  }

  parts <- str_match(text, "^Spotlight Tour:\\s*(.+?),\\s*with\\s+(.+)$")
  if (is.na(parts[1, 1])) return(blank)

  blank$theme <- str_trim(parts[1, 2])

  # "A '25 and B '25"  ->  c("A '25", "B '25")
  people <- str_split_1(parts[1, 3], "\\s*,\\s*and\\s+|\\s+and\\s+|\\s*,\\s*")
  people <- str_trim(people[people != ""])

  # Split each "Name '28" into the name and the two-digit class year.
  # Handles both a curly apostrophe and a straight one.
  split_people <- str_match(people, "^(.*?)\\s*[’']\\s*(\\d{2})\\s*$")
  names_only <- ifelse(is.na(split_people[, 2]), people, str_trim(split_people[, 2]))
  years_only <- ifelse(is.na(split_people[, 3]), "", split_people[, 3])

  blank$guides      <- paste(names_only, collapse = "; ")
  blank$class_years <- paste(years_only, collapse = "; ")
  blank
}


# ── Find the date on a page ─────────────────────────────────────────────────
# Written out by hand rather than using R's built-in month parsing, which
# quietly returns nothing on a computer set to a non-English language.
MONTHS <- c(January = 1, February = 2, March = 3, April = 4, May = 5, June = 6,
            July = 7, August = 8, September = 9, October = 10, November = 11, December = 12)

read_date <- function(page_text) {
  found <- str_match(
    page_text,
    paste0("\\b(", paste(names(MONTHS), collapse = "|"), ")\\s+(\\d{1,2}),\\s+(\\d{4})\\b")
  )
  if (is.na(found[1, 1])) return(as.Date(NA))
  as.Date(sprintf("%s-%02d-%02d", found[1, 4], MONTHS[[found[1, 2]]], as.integer(found[1, 3])))
}


# ── Read one tour page ──────────────────────────────────────────────────────
read_tour_page <- function(slug, live_url, archive_url) {
  html <- download_cached(live_url, file.path(HTML_CACHE, paste0(slug, ".html")))

  # Older tours get taken down. Fall back to the Internet Archive's copy.
  if (is.na(html)) {
    html <- download_cached(archive_url, file.path(HTML_CACHE, paste0(slug, "_archived.html")))
  }
  if (is.na(html)) {
    return(tibble(slug = slug, theme = NA_character_, guides = NA_character_,
                  class_years = NA_character_, language = NA_character_,
                  date = as.Date(NA), description = NA_character_,
                  artwork_ids = NA_character_, n_artworks = 0L))
  }

  page  <- read_html(html)
  title <- html_text2(html_element(page, "title"))
  bits  <- read_title(title)

  # The museum fills in a short summary for search engines; that's the
  # cleanest place to get the tour description from.
  description <- html_attr(html_element(page, "meta[name='description']"), "content")

  # The three artworks, found by looking for links that point into the
  # collection. Looking at where a link GOES is much sturdier than looking at
  # how it's styled, which changes whenever the site is redesigned.
  ids <- html_elements(page, "a[href*='/collections/object/']") |>
    html_attr("href") |>
    str_match("/collections/object/(\\d+)") |>
    (\(m) m[, 2])()
  ids <- unique(ids[!is.na(ids)])

  tibble(
    slug        = slug,
    theme       = bits$theme,
    guides      = bits$guides,
    class_years = bits$class_years,
    language    = bits$language,
    date        = read_date(html_text2(page)),
    description = description,
    artwork_ids = if (length(ids)) paste(ids, collapse = ";") else NA_character_,
    n_artworks  = length(ids)
  )
}


# ── Look one artwork up in the museum's API ─────────────────────────────────
# This is the part that genuinely saves you hours: it turns a bare ID number
# into the artist, date, medium, accession number and image link, all spelled
# and formatted consistently.
read_artwork <- function(artwork_id) {
  cache_file <- file.path(API_CACHE, paste0(artwork_id, ".json"))
  url <- paste0("https://api.harvardartmuseums.org/object/", artwork_id,
                "?apikey=", api_key)

  raw <- download_cached(url, cache_file)
  if (is.na(raw)) return(tibble(artwork_id = artwork_id))

  info <- fromJSON(raw, simplifyVector = FALSE)

  # The API leaves fields out rather than leaving them empty, so every lookup
  # needs a fallback or the whole row collapses.
  field <- function(name) {
    value <- info[[name]]
    if (is.null(value) || length(value) == 0) NA_character_ else as.character(value)[1]
  }

  # An artwork can list several people (artist, printer, former owner...).
  # Prefer whoever is credited as the artist.
  artist <- NA_character_
  if (!is.null(info$people) && length(info$people) > 0) {
    roles <- vapply(info$people, function(p) {
      if (is.null(p$role)) NA_character_ else as.character(p$role)[1]
    }, character(1))
    pick <- if (any(roles == "Artist", na.rm = TRUE)) which(roles == "Artist")[1] else 1
    artist <- if (is.null(info$people[[pick]]$displayname)) NA_character_
              else as.character(info$people[[pick]]$displayname)[1]
  }

  tibble(
    artwork_id   = artwork_id,
    artwork      = field("title"),
    artist       = artist,
    artwork_date = field("dated"),
    medium       = field("medium"),
    accession_no = field("objectnumber"),
    artwork_url  = field("url"),
    image_url    = field("primaryimageurl")
  )
}


# ─────────────────────────────────────────────────────────────────────────────
# Step 1: the tour list.
#
# Only the "Spotlight" pages carry a theme, a guide's name and the artworks.
# The old weekly listings were one reused page with nothing on it but a date,
# so they are left out entirely -- they would just be empty rows.
#
# A guide gives the same tour several times a term, and each showing has its
# own page. Those are all the same tour, so keep one and record how many
# times it ran.
# ─────────────────────────────────────────────────────────────────────────────
tours <- read_csv("data/tour_urls.csv", show_col_types = FALSE) |>
  filter(kind == "spotlight") |>
  group_by(slug_canonical) |>
  summarise(
    times_given  = n(),
    slug         = first(slug),
    live_url     = first(url_live),
    archive_url  = first(url_wayback),
    .groups = "drop"
  )

message("Reading ", nrow(tours), " tours (1 second apart; saved copies are instant)...")

tour_details <- pmap_dfr(
  list(tours$slug, tours$live_url, tours$archive_url),
  read_tour_page,
  .progress = TRUE
) |>
  left_join(select(tours, slug, slug_canonical, times_given, live_url), by = "slug")


# ── Step 2: look up every artwork mentioned ─────────────────────────────────
artwork_list <- tour_details |>
  filter(!is.na(artwork_ids)) |>
  select(slug, artwork_ids) |>
  separate_longer_delim(artwork_ids, delim = ";") |>
  rename(artwork_id = artwork_ids) |>
  group_by(slug) |>
  mutate(stop = row_number()) |>          # the order the guide chose
  ungroup()

message("Looking up ", n_distinct(artwork_list$artwork_id), " artworks in the API...")

artworks <- map_dfr(sort(unique(artwork_list$artwork_id)), read_artwork, .progress = TRUE)


# ── Step 3: one row per tour, three artworks across the columns ─────────────
artworks_across <- artwork_list |>
  left_join(artworks, by = "artwork_id") |>
  filter(stop <= 3) |>
  select(slug, stop, artwork, artist, artwork_date, accession_no, artwork_url) |>
  pivot_wider(names_from = stop, values_from = -c(slug, stop),
              names_glue = "artwork{stop}_{.value}")

catalog <- tour_details |>
  select(theme, guides, class_years, date, times_given, language,
         description, n_artworks, slug, live_url) |>
  left_join(artworks_across, by = "slug") |>
  mutate(added_by = "scraped") |>         # so hand-typed rows stay tellable apart
  arrange(desc(date))

write_csv(catalog, "data/catalog.csv")

message("\nWrote data/catalog.csv -- ", nrow(catalog), " tours.")
needs_look <- filter(catalog, is.na(theme) | is.na(date) | n_artworks != 3)
if (nrow(needs_look) > 0) {
  message(nrow(needs_look), " row(s) worth checking by hand (missing a theme or ",
          "date, or not exactly 3 artworks):")
  print(select(needs_look, theme, date, n_artworks, live_url))
}

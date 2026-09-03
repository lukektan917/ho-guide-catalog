# 02_scrape_tours.R ──────────────────────────────────────────────────────────
# Scrape each tour page discovered by 01 into structured records.
#
# PARSER DESIGN NOTE -- read this before you tweak selectors:
# Every field below is pulled from something layout-independent rather than
# from a CSS class, because CSS classes are exactly what breaks on a site
# redesign and would silently turn a 44-tour catalog into 44 blank rows:
#   * title       -> <title> tag
#   * theme/guide -> regex over that title string, not any page element
#   * description -> <meta name="description">, which the CMS fills for SEO
#   * tour stops  -> a[href*="/collections/object/"], i.e. the link TARGET
#                    pattern, not the markup wrapped around it
# So this should survive a reskin. It has NOT, however, been run against the
# live site (see the robots.txt note in 00_setup.R), so validate on one page
# first: set TEST_SLUG below and run this script -- it prints the parsed record
# and stops without touching the other ~200 pages.
#
# Output: data/tours_scraped.csv  (one row per run/event instance)
#         data/tour_stops.csv     (long: one row per tour x object stop)

source("R/00_setup.R")

# Set to a slug string to parse exactly one page and stop. NULL = full run.
TEST_SLUG <- NULL
# e.g. TEST_SLUG <- "spotlight-tour-colour-and-memory-with-alex-rivera-28"

tour_urls.df <- read_csv(file.path(DATA_DIR, "tour_urls.csv"), show_col_types = FALSE)

if (!is.null(TEST_SLUG)) {
  tour_urls.df <- filter(tour_urls.df, slug == TEST_SLUG)
  if (nrow(tour_urls.df) == 0) stop("TEST_SLUG not found in tour_urls.csv: ", TEST_SLUG)
}

# ── Title -> theme, guide names, class years, language ──────────────────────
# Title format observed: "Spotlight Tour: Colour and Memory, with Alex Rivera '28 | Harvard Art Museums"
# Multi-guide:           "Spotlight Tour: A Noble Craft, with Sam Okonkwo '25 and Jo Bennett '25 | ..."
# Translated run:        "... with Maria Torres Vega '26 (in Spanish) | ..."
parse_title <- function(title) {
  out <- list(theme = NA_character_, guides = NA_character_,
              class_years = NA_character_, language = "English")
  if (is.na(title)) return(out)

  bare <- str_trim(str_remove(title, "\\s*\\|\\s*Harvard Art Museums\\s*$"))

  # Pull off a trailing parenthetical like "(in Spanish)" before name parsing,
  # so it does not get swallowed into the last guide's name.
  lang <- str_match(bare, "\\((?:in\\s+)?([A-Za-z]+)\\)\\s*$")[, 2]
  if (!is.na(lang)) {
    out$language <- lang
    bare <- str_trim(str_remove(bare, "\\s*\\((?:in\\s+)?[A-Za-z]+\\)\\s*$"))
  }

  m <- str_match(bare, "^Spotlight Tour:\\s*(.+?),\\s*with\\s+(.+)$")
  if (is.na(m[1, 1])) {
    # Generic weekly/virtual listing -- no theme or guide in the title. Expected
    # for ~164 of the ~210 pages, not a parse failure.
    return(out)
  }
  out$theme <- str_trim(m[1, 2])

  # "A '25 and B '25" -> c("A '25", "B '25"). Also tolerates an Oxford-comma list.
  people <- str_split_1(m[1, 3], "\\s*,\\s*and\\s+|\\s+and\\s+|\\s*,\\s*")
  people <- str_trim(people[people != ""])

  # Class year is a curly OR straight apostrophe plus two digits: '28 / '28
  nm <- str_match(people, "^(.*?)\\s*[’']\\s*(\\d{2})\\s*$")
  names_only <- ifelse(is.na(nm[, 2]), people,      str_trim(nm[, 2]))
  years_only <- ifelse(is.na(nm[, 3]), NA_character_, nm[, 3])

  out$guides      <- paste(names_only, collapse = "; ")
  out$class_years <- paste(ifelse(is.na(years_only), "", years_only), collapse = "; ")
  out
}

# ── Scrape one page, with Wayback fallback ──────────────────────────────────
scrape_one <- function(slug, kind, url_live, url_wayback) {
  got    <- fetch_html_raw(url_live)
  source <- "live"

  if (is.na(got$html)) {
    # Live page gone (older tours get unpublished). Fall back to the snapshot.
    got    <- fetch_html_raw(url_wayback)
    source <- "wayback"
  }
  if (is.na(got$html)) {
    return(tibble(slug = slug, kind = kind, fetch_source = "failed",
                  http_status = got$status, title = NA_character_,
                  theme = NA_character_, guides = NA_character_,
                  class_years = NA_character_, language = NA_character_,
                  tour_date = as.Date(NA), description = NA_character_,
                  object_ids = NA_character_, n_objects = 0L))
  }

  doc  <- read_html(got$html)
  txt  <- html_text2(doc)

  title <- html_element(doc, "title") |> html_text2()
  if (is.na(title) || title == "") {
    title <- html_element(doc, "h1") |> html_text2()
  }
  parsed <- parse_title(title)

  desc <- html_element(doc, "meta[name='description']") |> html_attr("content")
  if (is.na(desc)) {
    desc <- html_element(doc, "meta[property='og:description']") |> html_attr("content")
  }

  # Tour stops, in document order. Dedupe but PRESERVE order -- the sequence is
  # the tour itself, and the guide chose it.
  obj_ids <- html_elements(doc, "a[href*='/collections/object/']") |>
    html_attr("href") |>
    str_match("/collections/object/(\\d+)") |>
    (\(m) m[, 2])() |>
    (\(x) x[!is.na(x)])() |>
    unique()

  tibble(
    slug         = slug,
    kind         = kind,
    fetch_source = source,
    http_status  = got$status,
    title        = title,
    theme        = parsed$theme,
    guides       = parsed$guides,
    class_years  = parsed$class_years,
    language     = parsed$language,
    tour_date    = parse_page_date(txt),
    description  = desc,
    object_ids   = if (length(obj_ids)) paste(obj_ids, collapse = ";") else NA_character_,
    n_objects    = length(obj_ids)
  )
}

message("Scraping ", nrow(tour_urls.df), " pages at ", REQ_PER_SEC, " req/sec ",
        "(cached pages are free) ...")

scraped.df <- purrr::pmap_dfr(
  list(tour_urls.df$slug, tour_urls.df$kind,
       tour_urls.df$url_live, tour_urls.df$url_wayback),
  scrape_one,
  .progress = TRUE
)

scraped.df <- scraped.df |>
  left_join(select(tour_urls.df, slug, slug_canonical, url_live), by = "slug") |>
  # Flag rows a human should eyeball rather than silently trusting.
  mutate(
    needs_review = case_when(
      fetch_source == "failed"                    ~ "fetch failed",
      kind == "spotlight" & is.na(theme)          ~ "spotlight page but no theme parsed from title",
      kind == "spotlight" & n_objects != 3        ~ paste0("expected 3 object stops, found ", n_objects),
      is.na(tour_date)                            ~ "no date found on page",
      TRUE                                        ~ NA_character_
    )
  ) |>
  relocate(slug_canonical, .after = slug) |>
  arrange(kind, tour_date, slug)

if (!is.null(TEST_SLUG)) {
  message("\n── TEST_SLUG parse result ─────────────────────────────────────")
  print(as.data.frame(t(scraped.df[1, ])))
  message("\nSet TEST_SLUG <- NULL to run the full scrape.")
} else {
  write_csv(scraped.df, file.path(DATA_DIR, "tours_scraped.csv"))

  # Long form: one row per (tour, object stop), with stop order preserved.
  stops.df <- scraped.df |>
    filter(!is.na(object_ids)) |>
    select(slug, slug_canonical, kind, theme, guides, tour_date, object_ids) |>
    separate_longer_delim(object_ids, delim = ";") |>
    rename(objectid = object_ids) |>
    group_by(slug) |>
    mutate(stop_order = row_number()) |>
    ungroup()

  write_csv(stops.df, file.path(DATA_DIR, "tour_stops.csv"))

  message("Wrote tours_scraped.csv (", nrow(scraped.df), " rows) and ",
          "tour_stops.csv (", nrow(stops.df), " rows).")
  message("Rows flagged for review: ", sum(!is.na(scraped.df$needs_review)))
  if (any(!is.na(scraped.df$needs_review))) {
    print(count(filter(scraped.df, !is.na(needs_review)), needs_review, sort = TRUE))
  }
}

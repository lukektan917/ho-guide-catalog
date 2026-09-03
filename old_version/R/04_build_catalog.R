# 04_build_catalog.R ─────────────────────────────────────────────────────────
# Collapse runs -> tours, attach artwork metadata, and write the catalog.
#
# Two grains, because both are genuinely useful:
#   data/catalog_tours.csv -> one row per distinct tour, three stops widened
#                             into columns. This is the human-readable catalog
#                             and the thing to seed the Google Sheet with.
#   data/catalog_long.csv  -> one row per tour x stop. Use this for any
#                             analysis ("which objects get toured most?",
#                             "which divisions are over/under-represented?").
#
# Output: data/catalog_tours.csv, data/catalog_long.csv, data/gaps_report.txt

source("R/00_setup.R")

runs.df    <- read_csv(file.path(DATA_DIR, "tours_scraped.csv"), show_col_types = FALSE)
stops.df   <- read_csv(file.path(DATA_DIR, "tour_stops.csv"),    show_col_types = FALSE)
objects.df <- read_csv(file.path(DATA_DIR, "objects.csv"),       show_col_types = FALSE)

first_present <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x)) x[1] else NA_character_
}

# ── Runs -> one row per distinct tour ───────────────────────────────────────
# A guide gives the same tour several times a term, so the run-level rows are
# repeats of one intellectual object. Collapse on slug_canonical and keep the
# date RANGE plus a run count, rather than throwing the repeats away -- how
# often a tour ran is real information about the program.
tour_level.df <- runs.df |>
  group_by(slug_canonical) |>
  summarise(
    kind         = first_present(kind),
    theme        = first_present(theme),
    guides       = first_present(guides),
    class_years  = first_present(class_years),
    language     = first_present(language),
    description  = first_present(description),
    n_runs       = n(),
    first_run    = suppressWarnings(min(tour_date, na.rm = TRUE)),
    last_run     = suppressWarnings(max(tour_date, na.rm = TRUE)),
    # Representative run for object stops: whichever run parsed the most stops.
    # Ties break to the earliest, so the choice is deterministic across reruns.
    best_slug    = slug[order(-n_objects, tour_date)][1],
    needs_review = first_present(needs_review),
    .groups = "drop"
  ) |>
  mutate(across(c(first_run, last_run), \(d) as.Date(ifelse(is.infinite(d), NA, d)))) |>
  mutate(
    record_quality = if_else(kind == "spotlight" & !is.na(theme), "full", "stub"),
    url            = paste0("https://harvardartmuseums.org/calendar/", slug_canonical)
  )

# ── Long grain: tour x stop x artwork ───────────────────────────────────────
catalog_long.df <- tour_level.df |>
  select(slug_canonical, theme, guides, class_years, language, kind,
         record_quality, n_runs, first_run, last_run, best_slug) |>
  inner_join(
    stops.df |> select(best_slug = slug, objectid, stop_order),
    by = "best_slug"
  ) |>
  left_join(objects.df, by = "objectid") |>
  select(-best_slug) |>
  arrange(first_run, slug_canonical, stop_order)

write_csv(catalog_long.df, file.path(DATA_DIR, "catalog_long.csv"))

# ── Wide grain: three stops as columns ──────────────────────────────────────
stops_wide.df <- catalog_long.df |>
  filter(stop_order <= 3) |>
  select(slug_canonical, stop_order, obj_title, artist, dated, accession_no, museum_url) |>
  pivot_wider(
    names_from  = stop_order,
    values_from = c(obj_title, artist, dated, accession_no, museum_url),
    names_glue  = "stop{stop_order}_{.value}"
  )

catalog_tours.df <- tour_level.df |>
  select(-best_slug) |>
  left_join(stops_wide.df, by = "slug_canonical") |>
  mutate(source = "scraped", added_on = as.character(Sys.Date())) |>
  relocate(record_quality, .after = kind) |>
  arrange(desc(first_run), slug_canonical)

write_csv(catalog_tours.df, file.path(DATA_DIR, "catalog_tours.csv"))

# ── Gaps report ─────────────────────────────────────────────────────────────
# "Everything, gaps visible" -- so state the holes plainly rather than letting
# a reader assume the blank rows are scraping bugs.
gaps <- c(
  paste0("Ho Family Student Guide tour catalog -- build ", Sys.Date()),
  "",
  paste0("Distinct tours: ", nrow(catalog_tours.df)),
  capture.output(print(count(catalog_tours.df, kind, record_quality))),
  "",
  paste0("Date range: ", min(catalog_tours.df$first_run, na.rm = TRUE),
         " to ", max(catalog_tours.df$last_run, na.rm = TRUE)),
  paste0("Total run/event instances behind those tours: ", sum(catalog_tours.df$n_runs)),
  "",
  "KNOWN GAPS, in descending order of how much they should bother you:",
  "",
  "1. Stub records. The weekly and virtual listings used one boilerplate page",
  "   with no guide name, theme, or object list, so those rows carry a date and",
  "   nothing else. This is not recoverable from the web at any effort level --",
  "   the information was never published. Only DAPP's internal records or the",
  "   guides themselves can fill these in.",
  "",
  "2. Wayback coverage is a floor, not a census. 01 discovers tours from what",
  "   the Internet Archive happened to crawl. A tour that ran once and was",
  "   never crawled does not appear here at all -- and unlike the stubs, you",
  "   cannot see that it is missing. This is the strongest argument for",
  "   reconciling against DAPP's own list.",
  "",
  "3. Object stops come from link targets on the page, so a page that links a",
  "   related work outside the tour will over-count. Rows where the count was",
  "   not 3 are flagged in needs_review in tours_scraped.csv.",
  "",
  paste0("Rows flagged for review: ",
         sum(!is.na(catalog_tours.df$needs_review)), " of ", nrow(catalog_tours.df))
)
write_lines(gaps, file.path(DATA_DIR, "gaps_report.txt"))
cat(paste(gaps, collapse = "\n"), "\n")

# ── Seeding the Google Sheet (the part new guides write into) ───────────────
# Left commented on purpose: it creates something in your Drive, and it should
# be YOUR deliberate one-time action rather than a side effect of a rebuild.
#
# The Sheet -- not this repo -- is the system of record once it exists. These
# scripts produce the historical seed; guides append to the Sheet through a
# Google Form whose fields match docs/submission_form_fields.md; and the
# `source` column ("scraped" vs "submitted") keeps the provenance legible so a
# future rescrape never silently clobbers a hand-entered row.
#
# library(googlesheets4)
# gs4_auth(email = "your.email@example.com")
#
# # ONE TIME -- create the Sheet and seed it with scraped history:
# ss <- gs4_create("Ho Family Student Guide Tour Catalog",
#                  sheets = list(tours = catalog_tours.df))
# print(ss)   # save this Sheet ID somewhere
#
# # LATER -- read back what guides have submitted, and combine:
# SHEET_ID <- "paste_your_sheet_id_here"
# submitted.df <- read_sheet(SHEET_ID, sheet = "Form Responses 1")
# combined.df  <- bind_rows(catalog_tours.df, mutate(submitted.df, source = "submitted"))
#
# NOTE: do not re-run gs4_create on a rebuild -- it makes a second Sheet.
# To refresh only the scraped rows in place, filter to source == "scraped",
# replace those, and leave the submitted rows untouched.

message("Wrote catalog_tours.csv (", nrow(catalog_tours.df), " tours) and ",
        "catalog_long.csv (", nrow(catalog_long.df), " tour-stop rows).")

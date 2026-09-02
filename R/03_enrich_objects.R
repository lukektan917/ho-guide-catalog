# 03_enrich_objects.R ────────────────────────────────────────────────────────
# Turn the bare object IDs scraped in 02 into real artwork records via the
# Harvard Art Museums API. This is the half of the project the API is actually
# good for: the API has no tours endpoint and is read-only, so it cannot store
# or serve the catalog -- but /object/:id gives you artist, date, medium,
# accession number, credit line, gallery and IIIF image URLs in consistent
# form, which is a lot of transcription you do not want to do by hand.
#
# API KEY: never hardcode it. Put this line in ~/.Renviron (or a project-level
# .Renviron) and restart R:
#     HAM_API_KEY=your_key_here
#
# Output: data/objects.csv
#
# Note: each object is fetched individually and cached. The API does support
# query syntax that might allow batching several IDs per call, but that was not
# verified against the live docs, and 130-odd cached single lookups run in about
# two minutes -- not worth guessing at a parameter that might silently return
# the wrong thing.

source("R/00_setup.R")

HAM_API_KEY <- Sys.getenv("HAM_API_KEY")
if (!nzchar(HAM_API_KEY)) {
  stop("HAM_API_KEY is not set. Add `HAM_API_KEY=...` to ~/.Renviron and restart R.")
}

JSON_CACHE <- file.path(DATA_DIR, "api_cache")
dir.create(JSON_CACHE, recursive = TRUE, showWarnings = FALSE)

stops.df <- read_csv(file.path(DATA_DIR, "tour_stops.csv"), show_col_types = FALSE)
object_ids <- sort(unique(stops.df$objectid))
message(length(object_ids), " distinct objects to fetch.")

# ── Safe extraction helpers ─────────────────────────────────────────────────
# The API omits fields rather than returning null for them, so every pull needs
# a default or the whole row silently becomes a zero-length vector.
pluck_chr <- function(x, key) {
  v <- x[[key]]
  if (is.null(v) || length(v) == 0) return(NA_character_)
  as.character(v)[1]
}

primary_artist <- function(x) {
  ppl <- x[["people"]]
  if (is.null(ppl) || length(ppl) == 0) return(list(name = NA_character_, role = NA_character_))
  roles <- vapply(ppl, function(p) pluck_chr(p, "role"), character(1))
  i <- which(roles == "Artist")
  i <- if (length(i)) i[1] else 1L
  list(name = pluck_chr(ppl[[i]], "displayname"), role = roles[i])
}

fetch_object <- function(objectid) {
  cf <- file.path(JSON_CACHE, paste0("object_", objectid, ".json"))

  if (file.exists(cf) && file.size(cf) > 0) {
    js <- jsonlite::fromJSON(cf, simplifyVector = FALSE)
  } else {
    resp <- tryCatch(
      request("https://api.harvardartmuseums.org/object") |>
        req_url_path_append(objectid) |>
        req_url_query(apikey = HAM_API_KEY) |>
        req_user_agent(USER_AGENT) |>
        req_throttle(rate = REQ_PER_SEC) |>
        req_timeout(30) |>
        req_retry(max_tries = 3, backoff = ~ 2^.x) |>
        req_error(is_error = function(resp) FALSE) |>
        req_perform(),
      error = function(e) NULL
    )
    if (is.null(resp) || resp_status(resp) != 200L) {
      return(tibble(objectid = objectid, api_ok = FALSE))
    }
    write_file(resp_body_string(resp), cf)
    js <- jsonlite::fromJSON(cf, simplifyVector = FALSE)
  }

  art <- primary_artist(js)

  tibble(
    objectid        = objectid,
    api_ok          = TRUE,
    obj_title       = pluck_chr(js, "title"),
    artist          = art$name,
    artist_role     = art$role,
    dated           = pluck_chr(js, "dated"),
    culture         = pluck_chr(js, "culture"),
    classification  = pluck_chr(js, "classification"),
    medium          = pluck_chr(js, "medium"),
    technique       = pluck_chr(js, "technique"),
    division        = pluck_chr(js, "division"),
    accession_no    = pluck_chr(js, "objectnumber"),
    creditline      = pluck_chr(js, "creditline"),
    gallery         = pluck_chr(js, "gallery"),
    image_url       = pluck_chr(js, "primaryimageurl"),
    iiif_base       = pluck_chr(js, "iiifbaseuri"),
    museum_url      = pluck_chr(js, "url")
  )
}

objects.df <- purrr::map_dfr(object_ids, fetch_object, .progress = TRUE)

write_csv(objects.df, file.path(DATA_DIR, "objects.csv"))

n_fail <- sum(!objects.df$api_ok)
message("Wrote objects.csv (", nrow(objects.df), " objects; ", n_fail, " failed).")
if (n_fail > 0) {
  message("Failed object IDs (likely deaccessioned, restricted, or the link was ",
          "not actually a tour stop): ",
          paste(objects.df$objectid[!objects.df$api_ok], collapse = ", "))
}

> **Archived.** Superseded by `catalog.R` in the project root. Kept for
> reference only.
>
> These scripts no longer run as written: they `source("R/00_setup.R")` and
> read `data/...` as paths relative to wherever you started R, which assumed
> this folder *was* the project root. To run one, copy the `R/` folder back to
> the root first. Nothing here is needed for normal use.

# Ho Family Student Guide Tour Catalog

Two halves:

1. **Historical catalog** — recover past Student Guide tours (theme, guide,
   date, the three objects) from the museum's own event pages, enriched with
   artwork metadata from the Harvard Art Museums API.
2. **Ongoing additions** — a Google Form → Sheet that new guides append to,
   using the same schema so history and new entries stay one table.

## Run order

```bash
Rscript R/01_discover_urls.R     # Wayback CDX -> data/tour_urls.csv (~210 pages)
Rscript R/02_scrape_tours.R      # scrape pages -> tours_scraped.csv, tour_stops.csv
Rscript R/03_enrich_objects.R    # HAM API      -> objects.csv        (needs HAM_API_KEY)
Rscript R/04_build_catalog.R     # join         -> catalog_tours.csv, catalog_long.csv
```

Run from the project root so `data/...` resolves.

**Validate the parser before the full scrape.** Open `R/02_scrape_tours.R`, set
`TEST_SLUG` to one slug from `data/tour_urls.csv`, and run it — it parses that
single page, prints the record, and stops. Confirm `theme`, `guides`,
`tour_date` and a 3-element `object_ids` all look right, then set `TEST_SLUG`
back to `NULL`. This script has never been run against the live site (see
below), so budget for one round of selector fixes.

## Setup

```r
install.packages(c("httr2", "rvest", "dplyr", "tidyr", "stringr",
                   "purrr", "tibble", "readr", "jsonlite", "googlesheets4"))
```

API key — put this in `~/.Renviron` and restart R. Never hardcode it, and never
commit it:

```
HAM_API_KEY=your_key_here
```

## Why a script you run, and not an assistant

`harvardartmuseums.org/robots.txt` is ~110 named AI-crawler user-agents
(`ClaudeBot`, `anthropic-ai`, `GPTBot`, `CCBot`, …) followed by `Disallow: /`.
There is **no** `User-agent: *` rule, so an ordinary self-identifying script is
not covered by any disallow directive — but an AI assistant fetching on your
behalf is. Hence: Claude wrote this; you run it. Keep the contact address in
`USER_AGENT` real, keep `REQ_PER_SEC` at 1, and the cache means a rerun costs
no requests at all.

## Why the Wayback Machine for discovery

The live calendar is JS-rendered and forward-looking only — no past-event
archive, no backwards pagination, and `/sitemap.xml` errors out. So the live
site cannot tell you what ran in 2023. The Internet Archive's CDX API lists
every `/calendar/` URL it ever crawled, which recovers slugs that are no longer
linked anywhere.

## What the data looks like

| Page type | Distinct tours | What's on the page |
|---|---|---|
| `spotlight-tour-*` | ~44 | Theme, guide name(s) + class year, date, description, all 3 object links |
| `student-guide-tour*` | ~119 | Date only — one boilerplate listing reused weekly |
| `virtual-student-guide-tour*` | ~45 | Date only |

The ~44 Spotlight tours are the real catalog. The other ~164 become deliberate
**stub** rows (`record_quality == "stub"`) so the timeline shows its own holes.
Those holes are *not* scraper bugs — the guide and theme were never published
for the weekly tours. Only DAPP's internal records or the guides themselves can
fill them.

See `data/gaps_report.txt` after a build for the honest limitations, especially
this one: **Wayback coverage is a floor, not a census.** A tour that ran once
and was never crawled is absent with no trace, which is the strongest reason to
reconcile this against DAPP's own list rather than treating it as complete.

## The Sheet is the system of record

Once seeded, the Google Sheet — not this repo — is authoritative. These scripts
produce the historical seed; guides append via the Form
(`docs/submission_form_fields.md`); the `source` column (`scraped` vs
`submitted`) keeps provenance legible so a rescrape never clobbers a
hand-entered row. Sheet creation is left commented in
`R/04_build_catalog.R` — it should be your deliberate one-time action, not a
side effect of a rebuild.

## Before publishing

This catalog is other students' research under their names. Worth clearing with
DAPP before it goes anywhere public.

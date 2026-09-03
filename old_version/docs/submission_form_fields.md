# Google Form fields for new guide submissions

The point of matching these to the scraper's output is that a submitted tour and
a scraped tour end up as the same shape of row, so the catalog stays one table
instead of two that have to be reconciled by hand later.

Create the Form, set its destination to the catalog Sheet, then rename the
generated response columns to match the names in the right-hand column.

| Form question | Type | Required | Maps to CSV column |
|---|---|---|---|
| Your name(s) — as you want it credited | Short answer | yes | `guides` (semicolon-separate co-guides) |
| Class year(s) — two digits, e.g. 28 | Short answer | yes | `class_years` |
| Tour title / theme | Short answer | yes | `theme` |
| One-sentence description | Paragraph | yes | `description` |
| Date first given | Date | yes | `first_run` |
| Language | Multiple choice (English / other) | yes | `language` |
| Stop 1 — collection URL or object ID | Short answer | yes | `stop1_museum_url` |
| Stop 2 — collection URL or object ID | Short answer | yes | `stop2_museum_url` |
| Stop 3 — collection URL or object ID | Short answer | yes | `stop3_museum_url` |
| Anything else worth recording | Paragraph | no | `notes` |

## Two design choices worth keeping

**Ask for the object URL, not the artwork title.** A guide pasting
`harvardartmuseums.org/collections/object/304115` gives you an ID that
`03_enrich_objects.R` turns into title, artist, date, medium, accession number
and image automatically. A guide typing "the Van Gogh one" gives you a
reconciliation problem forever. The paste is less work for them *and* less work
for you.

**Do not ask for anything the API already knows.** No artist, date, medium, or
accession fields on the form. Every field you add is a field a tired
undergraduate can get wrong, and all four are derivable from the object ID.

## Set `source = "submitted"` on these rows

The scraper's rows carry `source = "scraped"`. Keeping the two distinguishable
is what lets you re-run the scrape later without clobbering hand-entered
records — see the note at the bottom of `R/04_build_catalog.R`.

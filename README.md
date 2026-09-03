# Ho Family Student Guide Tour Catalog

A spreadsheet of every Spotlight Tour: the theme, who gave it, when, and the
three artworks they chose.

## Running it

**One-time setup.** Install the packages (once, in R):

```r
install.packages(c("httr2", "rvest", "dplyr", "tidyr", "stringr",
                   "purrr", "readr", "jsonlite"))
```

Then put your museum API key where R can find it. Open `~/.Renviron` (create
it if it isn't there), add this line, and restart R (either `HAM_APIKEY`
or `HAM_API_KEY` works):

```
HAM_APIKEY=your_key_here
```

**Then just:**

```bash
Rscript catalog.R
```

It reads the tour list in `data/tour_urls.csv` and writes `data/catalog.csv`.
Takes a few minutes the first time — it pauses a second between downloads to
be a polite visitor. Runs after that are near-instant, because it keeps the
pages and API replies it already fetched.

## What you get

One row per tour, with these columns:

| Column | Example |
|---|---|
| `theme` | Colour and Memory |
| `guides` | Alex Rivera |
| `class_years` | 28 |
| `date` | 2026-09-27 |
| `times_given` | 3 |
| `language` | English |
| `description` | one-sentence summary |
| `artwork1_artwork` … `artwork3_*` | title, artist, date, accession no., link |
| `added_by` | `scraped` |

## Putting it in a Google Sheet

1. Open a new Google Sheet.
2. **File → Import** → upload `data/catalog.csv`.
3. That's your catalog. From here **the Sheet is the real copy**, not this
   folder.

New guides add their tour by typing a row straight into the Sheet. Set their
`added_by` to something other than `scraped` — `submitted` works — so you can
always tell hand-typed rows from scraped ones.

**Careful with re-running.** If you run `catalog.R` again and re-import over
the Sheet, you'd wipe out any rows guides typed in. Import into a *new tab*
instead and copy across what you need.

A guide filling in a row really only needs `theme`, `guides`, `class_years`,
`date`, and the three artwork links. The artist/date/medium details are
looked up automatically for *scraped* tours, but not for hand-typed ones — so
either they type those in too, or leave them blank for now. (Happy to add a
small step that fills them in from the links if it becomes annoying.)

## What's deliberately not in here

The museum ran generic weekly and virtual Student Guide tours for years using
one reused listing page that showed only a date — no guide, no theme, no
artworks. About 164 of those exist. They're left out, because they'd be
completely empty rows. That information was never published anywhere, so the
only way to recover those years is from the program's own records or from the
guides themselves.

Also worth knowing: the tour list came from the Internet Archive's saved
copies of the museum's calendar, because the live calendar only shows upcoming
events. So it's "what the web remembers" rather than a guaranteed complete
list — a tour that ran once and was never saved wouldn't appear at all.

## A note on why you run this and not Claude

Websites publish a file called `robots.txt` listing which automated programs
they'd rather not have visiting — a posted request, not a lock. The museum's
asks about 110 AI companies' crawlers, Claude's included, to stay off the
whole site. It says nothing about ordinary scripts like this one. So Claude
wrote it; you run it. Keep the email address near the top of `catalog.R`
accurate, and leave the one-second pause alone.

## `old_version/`

The first version of this project, as five separate scripts. It does more —
it can rediscover the tour list from scratch, keeps a second table with one
row per artwork, and has its own test file. `catalog.R` replaces all of it for
normal use. Kept only for reference; nothing needs it.

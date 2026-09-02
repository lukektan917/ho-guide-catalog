# tests/test_parsers.R ───────────────────────────────────────────────────────
# Offline unit tests for the two pieces of logic most likely to be wrong and
# least likely to announce it: the title regex (theme + guide extraction) and
# the run-suffix-vs-class-year heuristic. Neither touches the network.
# Run from the project root: Rscript tests/test_parsers.R

source("R/00_setup.R")

# parse_title() lives in 02; source just that definition without running the scrape.
local({
  lines <- readLines("R/02_scrape_tours.R")
  start <- grep("^parse_title <- function", lines)
  end   <- grep("^\\}$", lines)
  end   <- end[end > start][1]
  eval(parse(text = paste(lines[start:end], collapse = "\n")), envir = globalenv())
})

pass <- 0L; fail <- 0L
check <- function(label, got, want) {
  ok <- identical(as.character(got), as.character(want))
  if (ok) pass <<- pass + 1L else fail <<- fail + 1L
  cat(sprintf("%-4s %s\n", if (ok) "ok" else "FAIL", label))
  if (!ok) cat(sprintf("       got:  %s\n       want: %s\n", got, want))
}

cat("\n── parse_title() ────────────────────────────────────────────────\n")

t1 <- parse_title("Spotlight Tour: Colour and Memory, with Alex Rivera ’28 | Harvard Art Museums")
check("single guide: theme",       t1$theme,       "Colour and Memory")
check("single guide: name",        t1$guides,      "Alex Rivera")
check("single guide: class year",  t1$class_years, "28")
check("single guide: language",    t1$language,    "English")

t2 <- parse_title("Spotlight Tour: A Noble Craft, with Sam Okonkwo ’25 and Jo Bennett ’25 | Harvard Art Museums")
check("two guides: theme",         t2$theme,       "A Noble Craft")
check("two guides: names",         t2$guides,      "Sam Okonkwo; Jo Bennett")
check("two guides: class years",   t2$class_years, "25; 25")

t3 <- parse_title("Spotlight Tour: Breaking and Mending, with Maria Torres Vega ’26 (in Spanish) | Harvard Art Museums")
check("spanish run: language",     t3$language,    "Spanish")
check("spanish run: name clean",   t3$guides,      "Maria Torres Vega")
check("spanish run: theme",        t3$theme,       "Breaking and Mending")

# "and" inside the theme must not be mistaken for a guide separator, and the
# lazy quantifier must still find the real ", with" boundary.
t4 <- parse_title("Spotlight Tour: Ink and Order, with Chris Delgado ’25 | Harvard Art Museums")
check("'and' in theme",            t4$theme,       "Ink and Order")
check("'and' in theme: one guide", t4$guides,      "Chris Delgado")

# A comma inside the theme: backtracking should extend the theme, not truncate it.
t5 <- parse_title("Spotlight Tour: Food, Glorious Food, with Someone Here ’27 | Harvard Art Museums")
check("comma in theme",            t5$theme,       "Food, Glorious Food")
check("comma in theme: guide",     t5$guides,      "Someone Here")

t6 <- parse_title("Spotlight Tour: Inside/Outside, with Robin Ashford ’25 | Harvard Art Museums")
check("slash in theme",            t6$theme,       "Inside/Outside")

t7 <- parse_title("Spotlight Tour: Quiet Rooms, with Taylor Quinn '25 | Harvard Art Museums")
check("straight apostrophe",       t7$class_years, "25")

t8 <- parse_title("Student Guide Tour | Harvard Art Museums")
check("generic listing: no theme", is.na(t8$theme),  TRUE)
check("generic listing: no guide", is.na(t8$guides), TRUE)

cat("\n── parse_page_date() ────────────────────────────────────────────\n")
check("date from page text",
      parse_page_date("Spotlight Tour\nSunday, September 27, 2026\n11:00am - 11:50am"),
      as.Date("2026-09-27"))
check("date, single-digit day",
      parse_page_date("Saturday, September 5, 2026"),
      as.Date("2026-09-05"))
check("no date present",
      is.na(parse_page_date("no date anywhere in this string")),
      TRUE)

cat("\n── run-suffix vs class-year heuristic ───────────────────────────\n")
canon <- function(slug) stringr::str_remove(slug, "-(?:[1-9]|1[0-9])$")

check("class year 27 preserved",
      canon("spotlight-tour-vessels-with-devi-sharma-27"),
      "spotlight-tour-vessels-with-devi-sharma-27")
check("run suffix -2 stripped",
      canon("spotlight-tour-vessels-with-devi-sharma-27-2"),
      "spotlight-tour-vessels-with-devi-sharma-27")
check("run suffix -5 stripped",
      canon("spotlight-tour-ink-and-order-with-chris-delgado-25-5"),
      "spotlight-tour-ink-and-order-with-chris-delgado-25")
check("run suffix -4 after two class years",
      canon("spotlight-tour-a-noble-craft-with-sam-okonkwo-25-and-jo-bennett-25-4"),
      "spotlight-tour-a-noble-craft-with-sam-okonkwo-25-and-jo-bennett-25")
check("non-numeric suffix preserved",
      canon("spotlight-tour-breaking-and-mending-with-maria-torres-vega-26-in-spanish"),
      "spotlight-tour-breaking-and-mending-with-maria-torres-vega-26-in-spanish")

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0) quit(status = 1)

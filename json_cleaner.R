# WEBSITE.de — JSONL Cleaning
# Input:  WEBSITE_data/articles_full.jsonl
# Output: WEBSITE_data/articles_clean.csv + articles_clean.rds
#
# Pakete installieren (einmalig):
# install.packages(c("jsonlite", "xml2", "dplyr", "readr", "purrr"))

library(jsonlite)
library(xml2)
library(dplyr)
library(readr)
library(purrr)

# ── Config ────────────────────────────────────────────────────────────────────

IN_FILE  <- "WEBSITE_data/articles_full.jsonl"
OUT_CSV  <- "WEBSITE_data/articles_clean.csv"
OUT_RDS  <- "WEBSITE_data/articles_clean.rds"

# ── Helper ────────────────────────────────────────────────────────────────────

# HTML-Tags entfernen, Whitespace normalisieren
strip_html <- function(html_str) {
  if (is.null(html_str) || is.na(html_str) || html_str == "") return("")
  tryCatch({
    doc  <- xml2::read_html(paste0("<div>", html_str, "</div>"))
    text <- xml2::xml_text(doc)
    # Whitespace normalisieren
    text <- gsub("\\s+", " ", text)
    trimws(text)
  }, error = function(e) "")
}

# content-data.frame → einen Fließtext zusammenführen
extract_text <- function(full) {
  tryCatch({
    content <- full[["data"]][["content"]]
    if (is.null(content) || !is.data.frame(content)) return("")
    # Nur HTML-Blöcke, iframes etc. rausfiltern
    html_blocks <- content$data[content$type == "HTML"]
    # Jeden Block bereinigen und zusammenkleben
    texts <- sapply(html_blocks, strip_html)
    paste(texts, collapse = " ")
  }, error = function(e) "")
}

# Autoren: Liste → kommaseparierter String
extract_authors <- function(authors) {
  tryCatch({
    if (is.null(authors) || length(authors) == 0) return("")
    if (is.data.frame(authors)) return(paste(authors$name, collapse = "; "))
    paste(sapply(authors, function(a) a$name), collapse = "; ")
  }, error = function(e) "")
}

# Tags: Vektor → String
extract_tags <- function(tags) {
  tryCatch({
    if (is.null(tags) || length(tags) == 0) return("")
    paste(unlist(tags), collapse = "; ")
  }, error = function(e) "")
}

# Kategorien: Vektor → String
extract_categories <- function(cats) {
  tryCatch({
    if (is.null(cats) || length(cats) == 0) return("")
    paste(unlist(cats), collapse = "; ")
  }, error = function(e) "")
}

# ── Einlesen ──────────────────────────────────────────────────────────────────

message("Lese JSONL: ", IN_FILE)
lines <- readLines(IN_FILE, warn = FALSE, encoding = "UTF-8")
lines <- lines[nzchar(trimws(lines))]  # leere Zeilen raus
message(sprintf("%d Artikel gefunden", length(lines)))

# ── Parsen + Bereinigen ───────────────────────────────────────────────────────

message("Verarbeite Artikel...")

clean_list <- vector("list", length(lines))

for (i in seq_along(lines)) {
  if (i %% 500 == 0) message(sprintf("  %d / %d", i, length(lines)))

  tryCatch({
    art <- fromJSON(lines[[i]], simplifyVector = TRUE)
    full <- art[["_full"]]

    clean_list[[i]] <- tibble(
      id           = art$id          %||% NA_character_,
      slug         = art$slug        %||% NA_character_,
      title        = art$title       %||% NA_character_,
      intro        = art$intro       %||% NA_character_,
      authors      = extract_authors(art$authors),
      categories   = extract_categories(art$categories),
      tags         = extract_tags(art$tags),
      published_at = art$publishedAt %||% NA_character_,
      created      = art$created     %||% NA_character_,
      modified     = art$lastModified %||% NA_character_,
      is_paid      = isTRUE(art$isPaidContent),
      category_ref = art$categoryRef %||% NA_character_,
      article_ref  = art$articleRef  %||% NA_character_,
      body_text    = extract_text(full),
      body_chars   = nchar(extract_text(full)),
      has_body     = !is.null(full) && !is.null(full[["data"]][["content"]])
    )
  }, error = function(e) {
    message(sprintf("  [!] Zeile %d: %s", i, conditionMessage(e)))
    clean_list[[i]] <- NULL
  })
}

# ── Zusammenführen ────────────────────────────────────────────────────────────

df <- bind_rows(compact(clean_list))

# Timestamps parsen
df <- df |>
  mutate(
    published_at = as.POSIXct(published_at, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    created      = as.POSIXct(created,      format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    modified     = as.POSIXct(modified,     format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  )

# ── Überblick ─────────────────────────────────────────────────────────────────

message("\n── Überblick ──────────────────────────────")
message(sprintf("Artikel gesamt:      %d", nrow(df)))
message(sprintf("Mit Volltext:        %d", sum(df$has_body, na.rm = TRUE)))
message(sprintf("Paid Content:        %d", sum(df$is_paid,  na.rm = TRUE)))
message(sprintf("Ø Textlänge (Zeichen): %.0f", mean(df$body_chars[df$body_chars > 0], na.rm = TRUE)))
message(sprintf("Zeitraum: %s bis %s",
        format(min(df$published_at, na.rm = TRUE), "%Y-%m-%d"),
        format(max(df$published_at, na.rm = TRUE), "%Y-%m-%d")))
message("\nKategorien:")
print(sort(table(df$categories), decreasing = TRUE))

# ── Speichern ─────────────────────────────────────────────────────────────────

write_csv(df, OUT_CSV)
saveRDS(df, OUT_RDS)

message(sprintf("\nCSV  → %s", OUT_CSV))
message(sprintf("RDS  → %s  (schneller in R)", OUT_RDS))
message("Fertig.")

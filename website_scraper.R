# WEBSITE.de Scraper v3 — robust + Comments
# Fixes: Resume baut IDs direkt aus vorhandener JSONL
# Neu:   Kommentar-Endpunkte (comment, comment/total, comment/preview)
#
# install.packages(c("httr2", "jsonlite"))

library(httr2)
library(jsonlite)

# ── Config ────────────────────────────────────────────────────────────────────

API_BASE     <- "https://api.WEBSITE.de"
PAGE_SIZE    <- 20
DELAY        <- 1.2
BODY_DELAY   <- 0.8
FETCH_BODY   <- TRUE
FETCH_COMMENTS <- TRUE   # Kommentardaten holen
MAX_ARTICLES <- Inf      # Zum Testen z.B. auf 40 setzen

OUT_DIR      <- "nius_data"
META_FILE    <- file.path(OUT_DIR, "articles_meta.jsonl")
FULL_FILE    <- file.path(OUT_DIR, "articles_full.jsonl")
COMMENT_FILE <- file.path(OUT_DIR, "comments.jsonl")
LOG_FILE     <- file.path(OUT_DIR, "scraper.log")
IDS_FILE     <- file.path(OUT_DIR, "fetched_ids.txt")

HEADERS <- list(
  "User-Agent"      = "Mozilla/5.0 (compatible; ResearchBot/1.0; university-research)",
  "Accept"          = "application/json",
  "Accept-Language" = "de-DE,de;q=0.9",
  "Referer"         = "https://www.WEBSITE.de/"
)

# ── Setup ─────────────────────────────────────────────────────────────────────

dir.create(OUT_DIR, showWarnings = FALSE)

log_msg <- function(msg) {
  line <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg)
  message(line)
  cat(line, "\n", file = LOG_FILE, append = TRUE)
}

append_jsonl <- function(path, obj) {
  cat(toJSON(obj, auto_unbox = TRUE, null = "null"), "\n",
      file = path, append = TRUE)
}

# ── Resume: IDs aus vorhandener JSONL rebuilden ───────────────────────────────
# Verlässt sich NICHT auf fetched_ids.txt — liest direkt aus der JSONL.
# Dadurch ist Resume robust gegen Absturz, Neustart, gelöschte txt-Datei.

build_fetched_ids <- function() {
  ids <- character(0)

  # Aus JSONL lesen (primäre Quelle)
  for (f in c(META_FILE, FULL_FILE)) {
    if (!file.exists(f)) next
    lines <- readLines(f, warn = FALSE)
    lines <- lines[nzchar(trimws(lines))]
    parsed_ids <- sapply(lines, function(l) {
      tryCatch(fromJSON(l)$id, error = function(e) NA_character_)
    })
    ids <- c(ids, parsed_ids[!is.na(parsed_ids)])
  }

  # Aus txt als Ergänzung (falls vorhanden)
  if (file.exists(IDS_FILE)) {
    ids <- c(ids, readLines(IDS_FILE, warn = FALSE))
  }

  unique(ids[nzchar(ids)])
}

mark_fetched <- function(id) {
  cat(id, "\n", file = IDS_FILE, append = TRUE)
}

# ── HTTP ──────────────────────────────────────────────────────────────────────

api_get <- function(path, params = list(), retries = 3) {
  url <- paste0(API_BASE, path)
  for (attempt in seq_len(retries)) {
    tryCatch({
      resp <- request(url) |>
        req_headers(!!!HEADERS) |>
        req_url_query(!!!params) |>
        req_timeout(15) |>
        req_perform()
      return(resp_body_json(resp, simplifyVector = FALSE))
    }, error = function(e) {
      wait <- 10 * attempt
      log_msg(sprintf("  [!] %s Versuch %d/%d: %s — warte %ds",
                      path, attempt, retries, conditionMessage(e), wait))
      Sys.sleep(wait)
    })
  }
  return(NULL)
}

# ── Volltext ──────────────────────────────────────────────────────────────────

fetch_body <- function(article_id) {
  api_get(paste0("/article/", article_id))
}

# ── Kommentare ────────────────────────────────────────────────────────────────
# Endpunkte werden beim ersten Aufruf automatisch geprobt.
# Ergebnis wird gecacht damit nicht bei jedem Artikel neu geprobt wird.

.comment_endpoint <- NULL  # gecachter Endpunkt

probe_comment_endpoint <- function(sample_id) {
  log_msg("Probe: Kommentar-Endpunkte...")
  candidates <- list(
    list(path = "/comment",         params = list(articleId = sample_id)),
    list(path = "/comment/preview", params = list(articleId = sample_id)),
    list(path = "/comment/total",   params = list(articleId = sample_id)),
    list(path = "/comment",         params = list(id = sample_id)),
    list(path = paste0("/comment/", sample_id), params = list())
  )
  for (c in candidates) {
    result <- api_get(c$path, c$params)
    if (!is.null(result)) {
      log_msg(sprintf("  [+] Funktioniert: %s params=%s",
                      c$path, toJSON(c$params, auto_unbox = TRUE)))
      log_msg(sprintf("       Keys: %s", paste(names(result), collapse = ", ")))
      return(list(path = c$path, params_template = names(c$params)))
    }
    Sys.sleep(0.5)
  }
  log_msg("  [-] Kein Kommentar-Endpunkt gefunden")
  return(NULL)
}

fetch_comments <- function(article_id) {
  if (is.null(.comment_endpoint)) return(NULL)
  params <- setNames(list(article_id), .comment_endpoint$params_template)
  result <- api_get(.comment_endpoint$path, params)
  if (!is.null(result)) {
    append_jsonl(COMMENT_FILE, list(article_id = article_id, comments = result))
  }
  result
}

# ── Duplikate aus JSONL entfernen ─────────────────────────────────────────────

dedup_jsonl <- function(path) {
  if (!file.exists(path)) return(invisible())
  lines  <- readLines(path, warn = FALSE)
  lines  <- lines[nzchar(trimws(lines))]
  ids    <- sapply(lines, function(l) tryCatch(fromJSON(l)$id, error = function(e) NA))
  keep   <- !duplicated(ids) & !is.na(ids)
  dupes  <- sum(!keep)
  if (dupes > 0) {
    writeLines(lines[keep], path)
    log_msg(sprintf("  Dedupliziert %s: %d Duplikate entfernt", basename(path), dupes))
  } else {
    log_msg(sprintf("  %s: keine Duplikate", basename(path)))
  }
}

# ── Scraper ───────────────────────────────────────────────────────────────────

scrape <- function(fetch_body_flag   = FETCH_BODY,
                   fetch_comment_flag = FETCH_COMMENTS,
                   max_articles      = MAX_ARTICLES) {

  # Resume: IDs aus vorhandenen Dateien rebuilden
  log_msg("Baue Resume-Index aus vorhandenen Dateien...")
  fetched_ids <- build_fetched_ids()
  log_msg(sprintf("  %d bereits geholte Artikel gefunden", length(fetched_ids)))

  # Duplikate in vorhandenen JSONL bereinigen
  log_msg("Prüfe auf Duplikate in vorhandenen Dateien...")
  dedup_jsonl(META_FILE)
  if (fetch_body_flag) dedup_jsonl(FULL_FILE)

  # Gesamtzahl
  first_page <- api_get("/articles", list(skip = 0))
  if (is.null(first_page)) { log_msg("[!] API nicht erreichbar"); return(invisible()) }
  total <- first_page$count
  target <- min(total, max_articles)
  log_msg(sprintf("API meldet %d Artikel | Ziel: %d | Noch offen: ~%d",
                  total, target, max(0, target - length(fetched_ids))))

  # Kommentar-Endpunkt proben
  if (fetch_comment_flag) {
    sample_id <- first_page$results[[1]]$id
    ep <- probe_comment_endpoint(sample_id)
    assign(".comment_endpoint", ep, envir = .GlobalEnv)
    Sys.sleep(DELAY)
  }

  new_count  <- 0L
  skip_count <- 0L
  err_count  <- 0L
  skip       <- 0L

  while ((new_count + skip_count) < target) {
    data <- api_get("/articles", list(skip = skip))

    if (is.null(data)) {
      log_msg(sprintf("[!] skip=%d endgültig fehlgeschlagen", skip))
      err_count <- err_count + 1L
      skip <- skip + PAGE_SIZE
      next
    }

    results <- data$results
    if (length(results) == 0) {
      log_msg(sprintf("skip=%d: leere Seite — Ende", skip))
      break
    }

    for (article in results) {
      article_id <- article$id

      if (article_id %in% fetched_ids) {
        skip_count <- skip_count + 1L
        next
      }

      # Metadaten
      append_jsonl(META_FILE, article)

      # Volltext
      if (fetch_body_flag) {
        Sys.sleep(BODY_DELAY)
        body   <- fetch_body(article_id)
        merged <- c(article, list(`_full` = body))
        append_jsonl(FULL_FILE, merged)
      }

      # Kommentare
      if (fetch_comment_flag && !is.null(.comment_endpoint)) {
        Sys.sleep(BODY_DELAY)
        fetch_comments(article_id)
      }

      # Als geholt markieren (in-memory + Datei)
      fetched_ids <- c(fetched_ids, article_id)
      mark_fetched(article_id)
      new_count <- new_count + 1L

      if (new_count >= max_articles) {
        log_msg(sprintf("Limit %d erreicht", max_articles))
        goto_end <- TRUE
        break
      }
    }

    if (exists("goto_end") && goto_end) break

    pct <- min(100, (skip + PAGE_SIZE) / target * 100)
    log_msg(sprintf("skip=%5d | %2d Einträge | %5d neu | %d übersprungen | %.1f%%",
                    skip, length(results), new_count, skip_count, pct))

    skip <- skip + PAGE_SIZE
    Sys.sleep(DELAY)
  }

  log_msg(strrep("=", 50))
  log_msg(sprintf("Fertig. Neu: %d | Übersprungen: %d | Fehler: %d",
                  new_count, skip_count, err_count))
  log_msg(sprintf("Metadaten -> %s", META_FILE))
  if (fetch_body_flag)    log_msg(sprintf("Volltext  -> %s", FULL_FILE))
  if (fetch_comment_flag) log_msg(sprintf("Kommentare-> %s", COMMENT_FILE))
}

# ── Start ─────────────────────────────────────────────────────────────────────

log_msg(strrep("=", 50))
log_msg(sprintf("WEBSITE.de Scraper v3 | %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
log_msg(sprintf("Volltext: %s | Kommentare: %s | Limit: %s",
        ifelse(FETCH_BODY, "ja", "nein"),
        ifelse(FETCH_COMMENTS, "ja", "nein"),
        ifelse(is.infinite(MAX_ARTICLES), "kein Limit", MAX_ARTICLES)))
log_msg(strrep("=", 50))

scrape()

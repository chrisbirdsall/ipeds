# Downloading and caching -------------------------------------------------------

#' Location of the local IPEDS file cache
#'
#' Downloaded zip files, extracted CSV files and parsed dictionaries are stored
#' in a per-user cache directory so each file is only downloaded once. The
#' location is, in order of precedence, the `ipeds.cache_dir` option, the
#' `IPEDS_CACHE_DIR` environment variable, or
#' `tools::R_user_dir("ipeds", "cache")`.
#'
#' Cached files are re-checked against the NCES server once they are older
#' than `getOption("ipeds.max_age", 30)` days, so provisional releases are
#' replaced automatically once NCES publishes revised (final) data. See the
#' `refresh` argument of [ipeds_read()] to control this per call.
#'
#' @return The path of the cache directory (it may not exist yet).
#' @seealso [ipeds_cache_info()], [ipeds_cache_clear()]
#' @examples
#' ipeds_cache_dir()
#' @export
ipeds_cache_dir <- function() {
	d <- getOption("ipeds.cache_dir")
	if (is.null(d) || !nzchar(d)) d <- Sys.getenv("IPEDS_CACHE_DIR", "")
	if (!nzchar(d)) d <- tools::R_user_dir("ipeds", which = "cache")
	normalizePath(d, winslash = "/", mustWork = FALSE)
}

#' List or clear cached IPEDS files
#'
#' @param cache_dir the cache directory.
#' @param type which part of the cache to clear: `"all"`, `"data"`,
#'   `"dictionary"` (parsed dictionaries and dictionary/program zips).
#' @return `ipeds_cache_info()` returns a data frame of cached files with their
#'   size in megabytes and modification time. `ipeds_cache_clear()` invisibly
#'   returns the paths removed.
#' @export
ipeds_cache_info <- function(cache_dir = ipeds_cache_dir()) {
	files <- list.files(cache_dir, recursive = TRUE, full.names = TRUE)
	files <- files[!grepl("\\.meta\\.rds$", files)]
	if (length(files) == 0) {
		return(data.frame(file = character(), type = character(), size_mb = numeric(),
						  modified = as.POSIXct(character())))
	}
	fi <- file.info(files)
	data.frame(file = basename(files),
			   type = basename(dirname(files)),
			   size_mb = round(fi$size / 1024^2, 2),
			   modified = fi$mtime,
			   stringsAsFactors = FALSE)
}

#' @rdname ipeds_cache_info
#' @export
ipeds_cache_clear <- function(type = c("all", "data", "dictionary"), cache_dir = ipeds_cache_dir()) {
	type <- match.arg(type)
	dirs <- switch(type,
				   all = file.path(cache_dir, c(names(.suffix), "parsed")),
				   data = file.path(cache_dir, "data"),
				   dictionary = file.path(cache_dir, c("dictionary", "stata", "parsed")))
	dirs <- dirs[dir.exists(dirs)]
	unlink(dirs, recursive = TRUE)
	invisible(dirs)
}

.base_url <- function() {
	u <- getOption("ipeds.base_url", "https://nces.ed.gov/ipeds/datacenter/data/")
	if (!grepl("/$", u)) u <- paste0(u, "/")
	u
}

.suffix <- c(data = "", dictionary = "_Dict", stata = "_Stata", data_stata = "_Data_Stata",
			 spss = "_SPS", sas = "_SAS")

#' URL of an IPEDS data center file
#'
#' @param file an IPEDS file name as returned by [ipeds_file_name()].
#' @param type `"data"` for the complete data file, `"dictionary"` for the
#'   data dictionary, `"stata"` for the Stata program (`.do`) file, or
#'   `"data_stata"`, `"spss"`, `"sas"`.
#' @return A character vector of URLs.
#' @examples
#' ipeds_url(ipeds_file_name("HD", 2023))
#' ipeds_url("C2023_A", "dictionary")
#' @export
ipeds_url <- function(file, type = "data") {
	type <- match.arg(type, names(.suffix))
	paste0(.base_url(), file, .suffix[[type]], ".zip")
}

.is_file_url <- function(url) grepl("^file://", url)

.file_url_path <- function(url) {
	p <- sub("^file://", "", url)
	if (grepl("^/[A-Za-z]:", p)) p <- substring(p, 2)
	utils::URLdecode(p)
}

.is_zip <- function(path) {
	if (!file.exists(path) || file.info(path)$size < 4) return(FALSE)
	con <- file(path, "rb")
	on.exit(close(con))
	sig <- readBin(con, "raw", n = 4)
	identical(sig, as.raw(c(0x50, 0x4b, 0x03, 0x04))) ||
		identical(sig, as.raw(c(0x50, 0x4b, 0x05, 0x06)))
}

# Query a remote file without downloading it. Returns exists = TRUE/FALSE, or
# NA when the server could not be reached.
.remote_info <- function(url) {
	if (.is_file_url(url)) {
		path <- .file_url_path(url)
		if (!file.exists(path)) return(list(exists = FALSE, status = 404L))
		fi <- file.info(path)
		return(list(exists = TRUE, status = 200L,
					last_modified = format(fi$mtime, "%Y-%m-%d %H:%M:%S"),
					content_length = as.character(fi$size),
					content_type = "application/zip"))
	}
	if (!isTRUE(capabilities("libcurl"))) return(list(exists = NA, status = NA_integer_))
	args <- list(url = url, redirect = TRUE, verify = TRUE)
	if ("timeout" %in% names(formals(curlGetHeaders))) args$timeout <- 60L
	h <- tryCatch(do.call(curlGetHeaders, args), error = function(e) NULL)
	if (is.null(h)) return(list(exists = NA, status = NA_integer_))
	status <- attr(h, "status")
	starts <- which(grepl("^HTTP/", h))
	if (length(starts)) h <- h[max(starts):length(h)]
	get <- function(key) {
		l <- grep(paste0("^", key, ":"), h, ignore.case = TRUE, value = TRUE)
		if (length(l) == 0) return(NA_character_)
		trimws(sub("^[^:]+:", "", l[length(l)]))
	}
	ct <- get("content-type")
	list(exists = isTRUE(status == 200) && !isTRUE(grepl("text/html", ct, ignore.case = TRUE)),
		 status = status, last_modified = get("last-modified"),
		 content_length = get("content-length"), content_type = ct)
}

.refresh_mode <- function(refresh) {
	if (is.null(refresh)) refresh <- getOption("ipeds.refresh", "auto")
	if (isTRUE(refresh)) return("always")
	if (identical(refresh, FALSE)) return("never")
	match.arg(refresh, c("auto", "check", "never", "always"))
}

.download <- function(url, dest, quiet = FALSE) {
	.msg("Downloading ", url, quiet = quiet)
	tmp <- tempfile(fileext = ".zip")
	on.exit(unlink(tmp), add = TRUE)
	old <- options(timeout = max(getOption("ipeds.timeout", 600), getOption("timeout")))
	on.exit(options(old), add = TRUE)
	tries <- max(1L, as.integer(getOption("ipeds.retries", 3)))
	err <- NULL
	ok <- FALSE
	for (i in seq_len(tries)) {
		err <- NULL
		res <- tryCatch(suppressWarnings(utils::download.file(url, tmp, mode = "wb", quiet = TRUE)),
						error = function(e) e)
		if (inherits(res, "error")) {
			err <- conditionMessage(res)
		} else if (!.is_zip(tmp)) {
			err <- "the server did not return a zip file (the file may not exist yet)"
		} else {
			ok <- TRUE
			break
		}
		if (grepl("404|410|not found|no such file|does not exist|not return a zip", err, ignore.case = TRUE)) break
		if (i < tries) Sys.sleep(2^i)
	}
	if (!ok) stop(sprintf("Could not download %s: %s", url, err), call. = FALSE)
	dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
	if (!file.copy(tmp, dest, overwrite = TRUE)) stop("Could not write ", dest, call. = FALSE)
	if (!.is_file_url(url)) Sys.sleep(getOption("ipeds.pause", 0.5))
	invisible(dest)
}

# Return the local path of a cached zip, downloading or refreshing it as
# needed.
.fetch <- function(file, type = "data", cache_dir = ipeds_cache_dir(), refresh = NULL,
				   quiet = FALSE) {
	refresh <- .refresh_mode(refresh)
	url <- ipeds_url(file, type)
	dest <- file.path(cache_dir, type, paste0(file, .suffix[[type]], ".zip"))
	meta_path <- paste0(dest, ".meta.rds")
	meta <- if (file.exists(meta_path)) tryCatch(readRDS(meta_path), error = function(e) NULL) else NULL
	have <- .is_zip(dest)
	save_meta <- function(m) {
		m$checked <- Sys.time()
		saveRDS(m, meta_path)
	}
	if (have && refresh == "never") return(dest)
	if (have && refresh == "auto") {
		checked <- meta$checked %||% file.info(dest)$mtime
		age <- as.numeric(difftime(Sys.time(), checked, units = "days"))
		if (age < getOption("ipeds.max_age", 30)) return(dest)
	}
	if (have && refresh != "always") {
		info <- .remote_info(url)
		if (!isTRUE(info$exists)) {
			if (is.na(info$exists)) {
				.msg("Could not reach ", url, " to check for updates; using the cached copy.", quiet = quiet)
			}
			save_meta(meta %||% list(url = url))
			return(dest)
		}
		if (!is.null(meta) && identical(info$last_modified, meta$last_modified) &&
			identical(info$content_length, meta$content_length)) {
			save_meta(meta)
			return(dest)
		}
		.msg("NCES has updated ", basename(dest), "; downloading the new version.", quiet = quiet)
	}
	.download(url, dest, quiet = quiet)
	info <- .remote_info(url)
	save_meta(list(url = url, downloaded = Sys.time(),
				   last_modified = info$last_modified %||% NA_character_,
				   content_length = info$content_length %||% NA_character_))
	dest
}

#' Download IPEDS files to the local cache
#'
#' Downloads (or refreshes) complete data files and/or their dictionaries for
#' any combination of components and years. Files already in the cache are
#' reused. Failures (for example a file that has not been released yet) are
#' reported in the result rather than stopping the whole batch.
#'
#' @param component one or more component ids (see [ipeds_components()]).
#' @param years one or more years.
#' @param type any of `"data"`, `"dictionary"`, `"stata"` (Stata program
#'   file, which carries variable and value labels), `"data_stata"`, `"spss"`
#'   and `"sas"`.
#' @param cache_dir cache directory, see [ipeds_cache_dir()].
#' @param refresh `"auto"` (default; re-check files older than
#'   `getOption("ipeds.max_age", 30)` days), `"check"` (always ask the server
#'   whether a newer version exists), `"never"` or `"always"` (force a new
#'   download). `TRUE`/`FALSE` are shorthand for `"always"`/`"never"`.
#' @param quiet suppress progress messages.
#' @return A data frame with one row per file and columns `component`,
#'   `year`, `file`, `type`, `path` (`NA` when the download failed) and
#'   `error`.
#' @examples
#' \dontrun{
#' ipeds_download(c("HD", "EFA"), 2021:2023)
#' }
#' @export
ipeds_download <- function(component, years, type = "data", cache_dir = ipeds_cache_dir(),
						   refresh = NULL, quiet = FALSE) {
	grid <- expand.grid(component = component, year = as.integer(years), type = type,
						stringsAsFactors = FALSE)
	grid$file <- mapply(ipeds_file_name, grid$component, grid$year)
	grid$path <- NA_character_
	grid$error <- NA_character_
	for (i in seq_len(nrow(grid))) {
		res <- tryCatch(.fetch(grid$file[i], grid$type[i], cache_dir, refresh, quiet),
						error = function(e) e)
		if (inherits(res, "error")) {
			grid$error[i] <- conditionMessage(res)
			.msg(grid$error[i], quiet = quiet)
		} else {
			grid$path[i] <- res
		}
	}
	grid[, c("component", "year", "file", "type", "path", "error")]
}

#' Check which IPEDS files are available from NCES
#'
#' Queries the NCES data center (without downloading) to see whether the
#' complete data file for each component and year has been released.
#'
#' @inheritParams ipeds_download
#' @return A data frame with columns `component`, `year`, `file`, `url`,
#'   `available` (`TRUE`, `FALSE`, or `NA` when the server could not be
#'   reached), `last_modified` and `size_mb`.
#' @examples
#' \dontrun{
#' ipeds_available(c("HD", "EFA", "C_A"), 2022:2025)
#' }
#' @export
ipeds_available <- function(component, years) {
	grid <- expand.grid(component = component, year = as.integer(years), stringsAsFactors = FALSE)
	grid$file <- mapply(ipeds_file_name, grid$component, grid$year)
	grid$url <- ipeds_url(grid$file)
	info <- lapply(grid$url, .remote_info)
	grid$available <- vapply(info, function(x) as.logical(x$exists), logical(1))
	grid$last_modified <- vapply(info, function(x) x$last_modified %||% NA_character_, character(1))
	grid$size_mb <- vapply(info, function(x) round(.as_num(x$content_length %||% NA) / 1024^2, 2),
						   numeric(1))
	grid
}

#' Most recent year released for IPEDS components
#'
#' Probes the NCES data center, starting from next year and working
#' backwards, to find the most recent year for which each component's
#' complete data file has been published. Use this to build panels that
#' always include the newest data, e.g.
#' `ipeds_panel(years = 2010:ipeds_latest_year("HD"))`.
#'
#' @param component one or more component ids.
#' @param max_year the first (latest) year to try.
#' @param lookback how many years to go back before giving up.
#' @return A named integer vector of years (`NA` if none was found).
#' @examples
#' \dontrun{
#' ipeds_latest_year(c("HD", "EFA", "C_A", "SFA", "F_F1A"))
#' }
#' @export
ipeds_latest_year <- function(component = "HD", max_year = .current_year() + 1L, lookback = 6L) {
	out <- vapply(component, function(comp) {
		for (y in seq.int(max_year, max_year - lookback)) {
			info <- .remote_info(ipeds_url(ipeds_file_name(comp, y)))
			if (is.na(info$exists)) {
				stop("Could not reach the NCES data center (", .base_url(), ").", call. = FALSE)
			}
			if (isTRUE(info$exists)) return(as.integer(y))
		}
		NA_integer_
	}, integer(1))
	names(out) <- component
	out
}

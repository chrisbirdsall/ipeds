# Internal helpers --------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

.msg <- function(..., quiet = FALSE) {
	if (!isTRUE(quiet)) message(...)
	invisible(NULL)
}

.as_num <- function(x) {
	if (is.numeric(x)) return(as.numeric(x))
	suppressWarnings(as.numeric(as.character(x)))
}

# Row-wise sum of the given columns, treating missing values as zero unless
# every value in the row is missing (in which case the result is NA).
.rsum <- function(cols, n) {
	cols <- Filter(Negate(is.null), cols)
	if (length(cols) == 0) return(rep(NA_real_, n))
	m <- do.call(cbind, lapply(cols, .as_num))
	out <- rowSums(m, na.rm = TRUE)
	out[rowSums(!is.na(m)) == 0] <- NA_real_
	out
}

# First non-missing value across the given vectors.
.coalesce <- function(...) {
	args <- Filter(Negate(is.null), list(...))
	if (length(args) == 0) return(NULL)
	out <- args[[1]]
	for (a in args[-1]) {
		idx <- is.na(out)
		out[idx] <- a[idx]
	}
	out
}

.safe_div <- function(num, den) {
	num <- .as_num(num)
	den <- .as_num(den)
	out <- num / den
	out[!is.finite(out) | is.na(den) | den <= 0] <- NA_real_
	out
}

# Lower-case, whitespace-normalized version of a label for keyword matching.
# Typographic quotes and dashes are replaced byte-wise so this works in any
# locale (including C/POSIX).
.typographic <- list(
	"'" = c("\u2018", "\u2019", "\u201a", "\u201b"),
	"\"" = c("\u201c", "\u201d", "\u201e"),
	"-" = c("\u2013", "\u2014"))

.norm_label <- function(x) {
	x <- as.character(x)
	for (to in names(.typographic)) {
		for (from in .typographic[[to]]) x <- gsub(from, to, x, fixed = TRUE, useBytes = TRUE)
	}
	x <- gsub("([A-Z]+)", "\\L\\1", x, perl = TRUE, useBytes = TRUE)
	x <- gsub("[[:space:]]+", " ", x, useBytes = TRUE)
	gsub("^ | $", "", x, useBytes = TRUE)
}

# Remove a leading UTF-8 byte order mark.
.strip_bom <- function(x) {
	vapply(x, function(s) {
		if (is.na(s)) return(s)
		r <- charToRaw(s)
		if (length(r) < 3 || !identical(r[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) return(s)
		out <- rawToChar(r[-(1:3)])
		Encoding(out) <- "UTF-8"
		out
	}, character(1), USE.NAMES = FALSE)
}

# Does a label satisfy a keyword specification? `all` is a vector of regular
# expressions that must all match and `none` a vector that must not match.
# Matching is case-insensitive (labels are normalized to lower case).
.label_match <- function(label, all = character(), none = character()) {
	if (length(label) != 1 || is.na(label) || !nzchar(label)) return(FALSE)
	l <- .norm_label(label)
	ok_all <- vapply(all, function(p) grepl(p, l, perl = TRUE), logical(1))
	ok_none <- vapply(none, function(p) grepl(p, l, perl = TRUE), logical(1))
	all(ok_all) && !any(ok_none)
}

.spec <- function(all = character(), none = character()) {
	list(all = all, none = none)
}

# Bind a list of data frames by row, filling missing columns with NA.
.rbind_fill <- function(dfs) {
	dfs <- Filter(function(d) !is.null(d) && nrow(d) > 0, dfs)
	if (length(dfs) == 0) return(data.frame())
	if (requireNamespace("data.table", quietly = TRUE)) {
		out <- data.table::rbindlist(dfs, use.names = TRUE, fill = TRUE)
		return(as.data.frame(out, stringsAsFactors = FALSE))
	}
	cols <- unique(unlist(lapply(dfs, names)))
	dfs <- lapply(dfs, function(d) {
		for (m in setdiff(cols, names(d))) d[[m]] <- rep(NA, nrow(d))
		d[, cols, drop = FALSE]
	})
	out <- do.call(rbind, dfs)
	rownames(out) <- NULL
	out
}

# Sum numeric columns within groups, keeping NA when every value in a group is
# NA. Groups are returned in order of first appearance.
.sum_by <- function(group, values) {
	values <- as.data.frame(values)
	if (length(group) == 0) {
		out <- data.frame(group = character(), stringsAsFactors = FALSE)
		for (nm in names(values)) out[[nm]] <- numeric()
		return(out)
	}
	grp <- factor(group, levels = unique(group))
	out <- data.frame(group = levels(grp), stringsAsFactors = FALSE)
	for (nm in names(values)) {
		v <- .as_num(values[[nm]])
		s <- rowsum(ifelse(is.na(v), 0, v), grp, reorder = FALSE)[, 1]
		n <- rowsum(as.numeric(!is.na(v)), grp, reorder = FALSE)[, 1]
		s[n == 0] <- NA_real_
		out[[nm]] <- unname(s)
	}
	out
}

.sum_by_unitid <- function(unitid, values) {
	out <- .sum_by(unitid, values)
	names(out)[1] <- "unitid"
	out$unitid <- as.integer(out$unitid)
	out
}

.current_year <- function() as.integer(format(Sys.Date(), "%Y"))

.check_years <- function(years) {
	if (missing(years) || is.null(years) || length(years) == 0) {
		stop("Please specify `years`, e.g. years = 2015:2023 or ",
			 "years = ipeds_latest_year() - 4:0.", call. = FALSE)
	}
	years <- as.integer(years)
	if (anyNA(years)) stop("`years` must be numeric.", call. = FALSE)
	sort(unique(years))
}

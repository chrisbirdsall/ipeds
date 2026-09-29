# Reading complete data files ----------------------------------------------------

# Columns that must stay character even when they look numeric (codes with
# leading zeros, identifiers, free text).
.char_vars <- c("cipcode", "opeid", "zip", "ein", "duns", "ueis", "gentele", "faxtele",
				"countycd", "cngdstcd", "webaddr", "adminurl", "faidurl", "applurl",
				"npricurl", "veturl", "athurl", "disaurl", "instnm", "ialias", "addr",
				"city", "stabbr", "chfnm", "chftitle", "f1sysnam", "countynm", "ope6id",
				"opeid6")

.is_flag <- function(nm, all_names) {
	startsWith(nm, "x") & substring(nm, 2) %in% all_names
}

# Locate the data CSV inside a cached zip, preferring the revised ("_rv")
# release when NCES has published one.
.data_csv <- function(file, cache_dir = ipeds_cache_dir(), refresh = NULL, quiet = FALSE,
					  revised = TRUE) {
	zip <- .fetch(file, "data", cache_dir, refresh, quiet)
	members <- utils::unzip(zip, list = TRUE)$Name
	csvs <- members[grepl("\\.csv$", members, ignore.case = TRUE)]
	if (length(csvs) == 0) stop("No CSV file found in ", basename(zip), call. = FALSE)
	rv <- grepl("_rv\\.csv$", csvs, ignore.case = TRUE)
	chosen <- if (revised && any(rv)) csvs[rv][1] else if (any(!rv)) csvs[!rv][1] else csvs[1]
	outdir <- file.path(dirname(zip), "csv")
	out <- file.path(outdir, basename(chosen))
	if (!file.exists(out) || file.info(out)$mtime < file.info(zip)$mtime) {
		dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
		utils::unzip(zip, files = chosen, exdir = outdir, junkpaths = TRUE, overwrite = TRUE)
		Sys.setFileTime(out, Sys.time())
	}
	list(path = out, member = chosen, revised = grepl("_rv\\.csv$", chosen, ignore.case = TRUE),
		 zip = zip, n_csv = length(csvs))
}

.detect_encoding <- function(path) {
	n <- min(file.info(path)$size, 4e6)
	con <- file(path, "rb")
	on.exit(close(con))
	raw <- readBin(con, "raw", n = n)
	if (length(raw) >= 3 && identical(raw[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) return("UTF-8")
	nl <- which(raw == as.raw(0x0a))
	if (length(nl)) raw <- raw[seq_len(max(nl))]
	raw <- raw[raw != as.raw(0)]
	if (validUTF8(rawToChar(raw))) "UTF-8" else "latin1"
}

.read_header <- function(path, encoding) {
	con <- file(path, "r", encoding = if (encoding == "UTF-8") "UTF-8-BOM" else "latin1")
	on.exit(close(con))
	h <- readLines(con, n = 1, warn = FALSE)
	h <- .strip_bom(h)
	nm <- scan(text = h, what = "", sep = ",", quote = "\"", quiet = TRUE, strip.white = TRUE)
	tolower(trimws(nm))
}

.read_csv <- function(path, keep, header, encoding) {
	use_dt <- requireNamespace("data.table", quietly = TRUE) &&
		isTRUE(getOption("ipeds.use_data_table", TRUE))
	if (use_dt) {
		df <- data.table::fread(path, colClasses = "character", select = which(keep),
								encoding = if (encoding == "UTF-8") "UTF-8" else "Latin-1",
								na.strings = c("", "."), showProgress = FALSE,
								data.table = FALSE, strip.white = TRUE, fill = TRUE)
	} else {
		cc <- ifelse(keep, "character", "NULL")
		# mark (rather than re-encode) strings so this works in any locale
		df <- utils::read.csv(path, colClasses = cc, na.strings = c("", "."), check.names = FALSE,
							  strip.white = TRUE, stringsAsFactors = FALSE,
							  encoding = if (encoding == "UTF-8") "UTF-8" else "latin1")
	}
	names(df) <- header[keep]
	for (j in seq_along(df)) {
		if (is.character(df[[j]])) df[[j]] <- enc2utf8(df[[j]])
	}
	df
}

.convert_types <- function(df) {
	nms <- names(df)
	for (nm in nms) {
		x <- df[[nm]]
		if (!is.character(x)) next
		x[!is.na(x) & !nzchar(trimws(x))] <- NA_character_
		if (nm %in% .char_vars || .is_flag(nm, nms)) {
			df[[nm]] <- x
			next
		}
		y <- utils::type.convert(x, as.is = TRUE)
		if (is.numeric(y)) {
			nn <- x[!is.na(x)]
			if (any(grepl("^0[0-9]", nn))) y <- x
		} else if (!is.character(y)) {
			y <- x
		}
		df[[nm]] <- y
	}
	if ("unitid" %in% nms) df$unitid <- as.integer(df$unitid)
	df
}

# Read one data file. `select` is NULL (all columns), a character vector of
# column names, or a function taking the header and returning a logical
# vector.
.read_data <- function(file, select = NULL, imputation_flags = TRUE, cache_dir = ipeds_cache_dir(),
					   refresh = NULL, quiet = FALSE, revised = TRUE) {
	csv <- .data_csv(file, cache_dir, refresh, quiet, revised)
	enc <- .detect_encoding(csv$path)
	header <- .read_header(csv$path, enc)
	keep <- if (is.null(select)) {
		rep(TRUE, length(header))
	} else if (is.function(select)) {
		as.logical(select(header))
	} else {
		sel <- tolower(select)
		header %in% c("unitid", sel, if (imputation_flags) paste0("x", sel))
	}
	if (!imputation_flags) keep <- keep & !.is_flag(header, header)
	keep[header == "unitid"] <- TRUE
	df <- .read_csv(csv$path, keep, header, enc)
	df <- .convert_types(df)
	attr(df, "ipeds") <- list(file = file, revised = csv$revised, source = csv$member,
							  url = ipeds_url(file), encoding = enc)
	df
}

.apply_labels <- function(df, dict) {
	if (is.null(dict)) return(df)
	vl <- dict$variables
	vals <- split(dict$values, dict$values$varname)
	for (nm in names(df)) {
		lab <- vl$label[match(nm, vl$varname)]
		if (length(lab) == 1 && !is.na(lab)) attr(df[[nm]], "label") <- lab
		v <- vals[[nm]]
		if (!is.null(v) && nrow(v) > 0) {
			if (is.numeric(df[[nm]])) {
				codes <- .as_num(v$code)
				ok <- !is.na(codes)
				if (any(ok)) attr(df[[nm]], "labels") <- stats::setNames(codes[ok], v$label[ok])
			} else {
				attr(df[[nm]], "labels") <- stats::setNames(as.character(v$code), v$label)
			}
		}
	}
	df
}

#' Read an IPEDS complete data file
#'
#' Downloads (if necessary) and reads one IPEDS complete data file. The
#' revised release (`*_rv.csv`) is used when NCES has published one. Column
#' names are converted to lower case, text is converted to UTF-8, numeric
#' columns are converted to numbers while codes with leading zeros (CIP codes,
#' ZIP codes, OPEIDs) are kept as text, and, when `labels = TRUE`, each column
#' receives a `"label"` attribute (the variable title) and, for coded
#' variables, a `"labels"` attribute (named vector of value labels) taken from
#' the official IPEDS dictionary. Use [ipeds_as_factor()] to turn a coded
#' column into a factor.
#'
#' @param component a component id from [ipeds_components()] (e.g. `"HD"`,
#'   `"EFA"`, `"C_A"`, `"SFA"`).
#' @param year the data year (see [ipeds_components()]).
#' @param file a literal IPEDS file name such as `"EF99_ANR"`, for files not
#'   covered by the catalog.
#' @param vars optional character vector of variables to read (`unitid` is
#'   always included). Reading a subset is much faster for large files.
#' @param labels attach variable and value labels from the IPEDS dictionary.
#' @param imputation_flags keep the imputation flag columns (the `x*`
#'   columns that accompany most numeric variables).
#' @param revised prefer the revised (`_rv`) data file when available.
#' @inheritParams ipeds_download
#' @return A data frame. The attribute `"ipeds"` records the file name,
#'   source member, URL and whether the revised release was used.
#' @examples
#' \dontrun{
#' hd <- ipeds_read("HD", 2023)
#' attr(hd$sector, "label")
#' table(ipeds_as_factor(hd$sector))
#' ef <- ipeds_read("EFA", 2023, vars = c("efalevel", "eftotlt"))
#' }
#' @export
ipeds_read <- function(component, year, vars = NULL, labels = TRUE, imputation_flags = TRUE,
					   revised = TRUE, cache_dir = ipeds_cache_dir(), refresh = NULL,
					   quiet = FALSE) {
	if (length(year) != 1) stop("`year` must be a single year; use ipeds_stack() for several years.",
								call. = FALSE)
	file <- ipeds_file_name(component, year)
	df <- ipeds_read_file(file, vars = vars, labels = labels, imputation_flags = imputation_flags,
						  revised = revised, cache_dir = cache_dir, refresh = refresh, quiet = quiet)
	info <- attr(df, "ipeds")
	info$component <- .component(component)$id
	info$year <- as.integer(year)
	attr(df, "ipeds") <- info
	df
}

#' @rdname ipeds_read
#' @export
ipeds_read_file <- function(file, vars = NULL, labels = TRUE, imputation_flags = TRUE,
							revised = TRUE, cache_dir = ipeds_cache_dir(), refresh = NULL,
							quiet = FALSE) {
	df <- .read_data(file, select = vars, imputation_flags = imputation_flags,
					 cache_dir = cache_dir, refresh = refresh, quiet = quiet, revised = revised)
	info <- attr(df, "ipeds")
	if (!is.null(vars)) {
		missing <- setdiff(tolower(vars), names(df))
		if (length(missing)) warning(file, " does not contain: ", paste(missing, collapse = ", "),
									 call. = FALSE)
	}
	if (isTRUE(labels)) {
		dict <- tryCatch(ipeds_dictionary_file(file, cache_dir = cache_dir, refresh = refresh,
											   quiet = TRUE),
						 error = function(e) {
						 	.msg("Labels unavailable for ", file, ": ", conditionMessage(e), quiet = quiet)
						 	NULL
						 })
		df <- .apply_labels(df, dict)
	}
	attr(df, "ipeds") <- info
	df
}

#' Stack one IPEDS component across several years
#'
#' Reads the same component for several years and binds the results by row,
#' adding a `year` column. Columns that exist only in some years are filled
#' with `NA`. No harmonization is done; see [ipeds_panel()] and the module
#' functions for harmonized data.
#'
#' @inheritParams ipeds_read
#' @param years the years to read. Years whose file is unavailable are skipped
#'   with a warning.
#' @return A data frame with `unitid`, `year` and the requested variables.
#' @examples
#' \dontrun{
#' ef <- ipeds_stack("EFD", 2015:2023, vars = c("ret_pcf", "ret_pcp"))
#' }
#' @export
ipeds_stack <- function(component, years, vars = NULL, labels = FALSE, imputation_flags = FALSE,
						revised = TRUE, cache_dir = ipeds_cache_dir(), refresh = NULL,
						quiet = FALSE) {
	years <- .check_years(years)
	parts <- lapply(years, function(y) {
		df <- tryCatch(suppressWarnings(
			ipeds_read(component, y, vars = vars, labels = labels,
					   imputation_flags = imputation_flags, revised = revised,
					   cache_dir = cache_dir, refresh = refresh, quiet = quiet)),
			error = function(e) {
				warning(ipeds_file_name(component, y), ": ", conditionMessage(e), call. = FALSE)
				NULL
			})
		if (is.null(df)) return(NULL)
		df$year <- rep(as.integer(y), nrow(df))
		df[, c("unitid", "year", setdiff(names(df), c("unitid", "year"))), drop = FALSE]
	})
	out <- .rbind_fill(parts)
	if (nrow(out) && isTRUE(labels)) {
		# re-apply the most recent labels (rbind drops attributes)
		last <- Filter(Negate(is.null), parts)
		last <- last[[length(last)]]
		for (nm in intersect(names(out), names(last))) {
			for (a in c("label", "labels")) {
				if (!is.null(attr(last[[nm]], a))) attr(out[[nm]], a) <- attr(last[[nm]], a)
			}
		}
	}
	out
}

#' Convert a labelled IPEDS column to a factor
#'
#' @param x a column read with [ipeds_read()] carrying a `"labels"` attribute.
#' @param drop_unused drop value labels that do not occur in `x`.
#' @return A factor. Codes without a value label keep their code as the level.
#' @export
ipeds_as_factor <- function(x, drop_unused = TRUE) {
	labs <- attr(x, "labels")
	if (is.null(labs)) return(factor(x))
	codes <- unname(labs)
	extra <- setdiff(sort(unique(x[!is.na(x)])), codes)
	lev <- c(codes, extra)
	lab <- c(names(labs), as.character(extra))
	keep <- !duplicated(lev)
	f <- factor(x, levels = lev[keep], labels = make.unique(lab[keep]))
	if (drop_unused) f <- droplevels(f)
	f
}

# Data dictionaries --------------------------------------------------------------

#' IPEDS data dictionary for a file
#'
#' Returns the variable titles and value labels for an IPEDS complete data
#' file. By default the labels are taken from the Stata program file that NCES
#' publishes alongside every data file (`<FILE>_Stata.zip`), which is plain
#' text and has the same layout in every year. If that fails, the Excel
#' dictionary (`<FILE>_Dict.zip`) is used, which requires the \pkg{readxl}
#' package. Parsed dictionaries are cached.
#'
#' @inheritParams ipeds_read
#' @param source `"auto"` (Stata program file, then Excel dictionary),
#'   `"stata"` or `"dict"`.
#' @return An object of class `ipeds_dictionary`: a list with elements
#'   `file`, `source`, `variables` (data frame with `varname` and `label`)
#'   and `values` (data frame with `varname`, `code` and `label`).
#' @examples
#' \dontrun{
#' d <- ipeds_dictionary("HD", 2023)
#' head(d$variables)
#' subset(d$values, varname == "sector")
#' }
#' @export
ipeds_dictionary <- function(component, year, source = c("auto", "stata", "dict"),
							 cache_dir = ipeds_cache_dir(), refresh = NULL, quiet = FALSE) {
	ipeds_dictionary_file(ipeds_file_name(component, year), source = source,
						  cache_dir = cache_dir, refresh = refresh, quiet = quiet)
}

#' @rdname ipeds_dictionary
#' @export
ipeds_dictionary_file <- function(file, source = c("auto", "stata", "dict"),
								  cache_dir = ipeds_cache_dir(), refresh = NULL, quiet = FALSE) {
	source <- match.arg(source)
	parsed <- file.path(cache_dir, "parsed", paste0(file, "_", source, ".rds"))
	errs <- character()
	try_source <- function(type, parser) {
		zip <- tryCatch(.fetch(file, type, cache_dir, refresh, quiet), error = function(e) {
			errs <<- c(errs, conditionMessage(e))
			NULL
		})
		if (is.null(zip)) return(NULL)
		if (file.exists(parsed) && file.info(parsed)$mtime >= file.info(zip)$mtime) {
			d <- tryCatch(readRDS(parsed), error = function(e) NULL)
			if (inherits(d, "ipeds_dictionary") && identical(d$source, type)) return(d)
		}
		d <- tryCatch(parser(zip, file), error = function(e) {
			errs <<- c(errs, conditionMessage(e))
			NULL
		})
		if (!is.null(d) && nrow(d$variables) > 0) {
			dir.create(dirname(parsed), recursive = TRUE, showWarnings = FALSE)
			saveRDS(d, parsed)
			return(d)
		}
		NULL
	}
	d <- NULL
	if (source %in% c("auto", "stata")) d <- try_source("stata", .dict_from_stata)
	if (is.null(d) && source %in% c("auto", "dict")) d <- try_source("dictionary", .dict_from_excel)
	if (is.null(d)) {
		stop("No dictionary could be loaded for ", file, ": ", paste(unique(errs), collapse = "; "),
			 call. = FALSE)
	}
	d
}

#' @export
print.ipeds_dictionary <- function(x, ...) {
	cat("<ipeds_dictionary> ", x$file, " (source: ", x$source, ")\n", sep = "")
	cat(nrow(x$variables), "variables,", length(unique(x$values$varname)),
		"with value labels\n")
	print(utils::head(x$variables, 10), row.names = FALSE)
	if (nrow(x$variables) > 10) cat("...\n")
	invisible(x)
}

.new_dictionary <- function(file, source, variables, values) {
	variables <- variables[!is.na(variables$varname) & nzchar(variables$varname), , drop = FALSE]
	variables$varname <- tolower(trimws(variables$varname))
	variables <- variables[!duplicated(variables$varname), , drop = FALSE]
	if (is.null(values) || nrow(values) == 0) {
		values <- data.frame(varname = character(), code = character(), label = character(),
							 stringsAsFactors = FALSE)
	}
	values$varname <- tolower(trimws(values$varname))
	values$code <- trimws(as.character(values$code))
	values <- values[!duplicated(values[, c("varname", "code")], fromLast = TRUE), , drop = FALSE]
	rownames(variables) <- rownames(values) <- NULL
	structure(list(file = file, source = source,
				   variables = variables[, c("varname", "label")],
				   values = values[, c("varname", "code", "label")]),
			  class = "ipeds_dictionary")
}

# Read a text file whose encoding may be UTF-8 or Windows-1252/latin1.
.read_lines_any <- function(path) {
	raw <- readBin(path, "raw", n = file.info(path)$size)
	raw <- raw[raw != as.raw(0)]
	txt <- rawToChar(raw)
	if (!validUTF8(txt)) {
		txt <- iconv(txt, from = "windows-1252", to = "UTF-8", sub = "?")
		if (is.na(txt)) txt <- iconv(rawToChar(raw), from = "latin1", to = "UTF-8", sub = "?")
	} else {
		Encoding(txt) <- "UTF-8"
	}
	txt <- .strip_bom(txt)
	strsplit(txt, "\r?\n")[[1]]
}

.extract_member <- function(zip, pattern) {
	members <- utils::unzip(zip, list = TRUE)$Name
	m <- members[grepl(pattern, members, ignore.case = TRUE)]
	if (length(m) == 0) return(NULL)
	# prefer revised program files when present
	rv <- grepl("_rv\\.", m, ignore.case = TRUE)
	m <- if (any(rv)) m[rv][1] else m[1]
	exdir <- tempfile("ipeds")
	utils::unzip(zip, files = m, exdir = exdir, junkpaths = TRUE)
	file.path(exdir, basename(m))
}

.dict_from_stata <- function(zip, file) {
	path <- .extract_member(zip, "\\.do$")
	if (is.null(path)) stop("no Stata .do file in ", basename(zip), call. = FALSE)
	on.exit(unlink(dirname(path), recursive = TRUE))
	.parse_stata_do(.read_lines_any(path), file)
}

# Parse the `label variable`, `label define` and `label values` statements of
# an IPEDS Stata program file.
.parse_stata_do <- function(lines, file = NA_character_) {
	lines <- sub("\r$", "", lines)
	rx_var <- "^\\s*label\\s+var(?:iable)?\\s+([A-Za-z_][A-Za-z0-9_]*)\\s+\"(.*)\"\\s*$"
	rx_def <- paste0("^\\s*\\*?\\s*label\\s+def(?:ine)?\\s+([A-Za-z_][A-Za-z0-9_]*)\\s+",
					 "(-?[0-9]+(?:\\.[0-9]+)?|\"[^\"]*\"|[^\\s\"]+)\\s+\"(.*)\"",
					 "\\s*(?:,\\s*(?:add|modify|replace)\\s*)?$")
	rx_val <- "^\\s*label\\s+val(?:ues)?\\s+(.+?)\\s*$"
	grab <- function(rx) {
		m <- regmatches(lines, regexec(rx, lines, perl = TRUE))
		m[lengths(m) > 0]
	}
	vm <- grab(rx_var)
	variables <- data.frame(varname = vapply(vm, `[`, "", 2),
							label = trimws(vapply(vm, `[`, "", 3)),
							stringsAsFactors = FALSE)
	dm <- grab(rx_def)
	defs <- data.frame(lblname = vapply(dm, `[`, "", 2),
					   code = gsub("^\"|\"$", "", vapply(dm, `[`, "", 3)),
					   label = trimws(vapply(dm, `[`, "", 4)),
					   stringsAsFactors = FALSE)
	lv <- grab(rx_val)
	map <- do.call(rbind, lapply(lv, function(m) {
		tok <- strsplit(trimws(m[2]), "\\s+")[[1]]
		if (length(tok) < 2) return(NULL)
		data.frame(varname = tok[-length(tok)], lblname = tok[length(tok)],
				   stringsAsFactors = FALSE)
	}))
	if (is.null(map)) map <- data.frame(varname = character(), lblname = character())
	# label sets without a `label values` statement map to label_<var>
	orphan <- setdiff(unique(defs$lblname), map$lblname)
	if (length(orphan)) {
		map <- rbind(map, data.frame(varname = sub("^label_", "", orphan), lblname = orphan,
									 stringsAsFactors = FALSE))
	}
	values <- merge(map, defs, by = "lblname")
	values <- values[order(values$varname, suppressWarnings(as.numeric(values$code))), , drop = FALSE]
	.new_dictionary(file, "stata", variables, values[, c("varname", "code", "label")])
}

.dict_from_excel <- function(zip, file) {
	path <- .extract_member(zip, "\\.xlsx?$")
	if (is.null(path)) {
		stop("the dictionary for ", file, " is not an Excel file (older dictionaries are HTML); ",
			 "labels are taken from the Stata program file instead", call. = FALSE)
	}
	on.exit(unlink(dirname(path), recursive = TRUE))
	if (!requireNamespace("readxl", quietly = TRUE)) {
		stop("install the 'readxl' package to read Excel dictionaries", call. = FALSE)
	}
	.parse_excel_dictionary(path, file)
}

.parse_excel_dictionary <- function(path, file = NA_character_) {
	sheets <- readxl::excel_sheets(path)
	read <- function(pattern) {
		s <- sheets[grepl(pattern, sheets, ignore.case = TRUE)]
		if (length(s) == 0) return(NULL)
		d <- as.data.frame(readxl::read_excel(path, sheet = s[1], col_types = "text"))
		names(d) <- tolower(gsub("[^A-Za-z]", "", names(d)))
		d
	}
	col <- function(d, pattern) {
		n <- names(d)[grepl(pattern, names(d))]
		if (length(n) == 0) return(rep(NA_character_, nrow(d)))
		as.character(d[[n[1]]])
	}
	v <- read("^varlist|^variables")
	if (is.null(v)) v <- read("desc")
	if (is.null(v)) stop("no variable list sheet in dictionary for ", file, call. = FALSE)
	variables <- data.frame(varname = col(v, "^varname$"), label = col(v, "title|label"),
							stringsAsFactors = FALSE)
	f <- read("^freq")
	values <- if (is.null(f)) NULL else {
		data.frame(varname = col(f, "^varname$"), code = col(f, "codevalue|^code"),
				   label = col(f, "valuelabel|^label"), stringsAsFactors = FALSE)
	}
	if (!is.null(values)) values <- values[!is.na(values$code) & !is.na(values$varname), ]
	.new_dictionary(file, "dictionary", variables, values)
}

# Dictionary lookups -------------------------------------------------------------

.dict_label <- function(dict, var) {
	if (is.null(dict)) return(NA_character_)
	dict$variables$label[match(var, dict$variables$varname)]
}

.dict_values <- function(dict, var) {
	if (is.null(dict)) return(data.frame(code = character(), label = character()))
	dict$values[dict$values$varname == var, c("code", "label"), drop = FALSE]
}

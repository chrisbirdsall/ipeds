# Inflation adjustment and export -----------------------------------------------------

# CPI-U, U.S. city average, all items (1982-84 = 100), annual averages
# (BLS series CUUR0000SA0).
.cpi_u <- c(
	"2000" = 172.2, "2001" = 177.1, "2002" = 179.9, "2003" = 184.0, "2004" = 188.9,
	"2005" = 195.3, "2006" = 201.6, "2007" = 207.342, "2008" = 215.303, "2009" = 214.537,
	"2010" = 218.056, "2011" = 224.939, "2012" = 229.594, "2013" = 232.957, "2014" = 236.736,
	"2015" = 237.017, "2016" = 240.007, "2017" = 245.120, "2018" = 251.107, "2019" = 255.657,
	"2020" = 258.811, "2021" = 270.970, "2022" = 292.655, "2023" = 304.702, "2024" = 313.689)

#' Consumer price index used for inflation adjustment
#'
#' Annual average CPI-U (U.S. city average, all items, 1982-84 = 100; Bureau
#' of Labor Statistics series CUUR0000SA0) for 2000-2024. For later years, or
#' to use a different index such as HEPI, pass your own data frame with a year
#' column and an index column to [ipeds_real_dollars()] or [ipeds_deflate()].
#'
#' @return A data frame with columns `year` and `cpi_u`.
#' @export
ipeds_cpi <- function() {
	data.frame(year = as.integer(names(.cpi_u)), cpi_u = unname(.cpi_u))
}

#' Convert nominal dollars to constant dollars
#'
#' `ipeds_deflate()` converts a vector; `ipeds_real_dollars()` converts every
#' dollar-denominated column of a panel or module result (those with
#' `dollars = TRUE` in [ipeds_codebook()]) and updates their labels.
#'
#' Values are deflated with the index value of the data `year`. Note that
#' IPEDS periods differ by component (e.g. charges refer to the academic year
#' beginning in `year`, finance to the fiscal year ending in `year`); see the
#' `period` column of the codebook.
#'
#' @param x a numeric vector (`ipeds_deflate()`) or a panel
#'   (`ipeds_real_dollars()`).
#' @param year the year of each value of `x`.
#' @param base_year the year whose dollars the result is expressed in;
#'   defaults to the latest year of the index.
#' @param index a data frame whose first column is the year and second the
#'   price index; defaults to [ipeds_cpi()].
#' @param vars columns to convert; defaults to all dollar columns.
#' @return `x` in constant `base_year` dollars.
#' @examples
#' ipeds_deflate(c(10000, 10000), year = c(2010, 2020), base_year = 2024)
#' @export
ipeds_deflate <- function(x, year, base_year = NULL, index = ipeds_cpi()) {
	idx <- stats::setNames(as.numeric(index[[2]]), as.character(index[[1]]))
	if (is.null(base_year)) base_year <- max(as.integer(names(idx)))
	to <- idx[as.character(base_year)]
	if (is.na(to)) stop("The index has no value for base_year ", base_year, ".", call. = FALSE)
	from <- idx[as.character(year)]
	miss <- unique(year[is.na(from) & !is.na(x)])
	if (length(miss)) {
		warning("No index value for year(s) ", paste(sort(miss), collapse = ", "),
				"; those values are NA. Supply `index` to cover them.", call. = FALSE)
	}
	unname(x * to / from)
}

#' @rdname ipeds_deflate
#' @export
ipeds_real_dollars <- function(x, base_year = NULL, index = ipeds_cpi(), vars = NULL) {
	if (is.null(base_year)) base_year <- max(as.integer(index[[1]]))
	if (is.null(vars)) {
		cb <- .codebook_rows(names(x))
		vars <- cb$variable[cb$dollars]
	}
	for (v in vars) {
		lab <- attr(x[[v]], "label")
		x[[v]] <- ipeds_deflate(x[[v]], x$year, base_year, index)
		if (!is.null(lab)) attr(x[[v]], "label") <- paste0(lab, " (constant ", base_year, " dollars)")
	}
	x
}

#' Write a panel with its labels and codebook
#'
#' Writes a panel or module result to disk, keeping variable labels.
#' The format is chosen from the file extension:
#' \describe{
#'   \item{`.csv`}{data as CSV plus `<name>_codebook.csv` (and
#'     `<name>_audit.csv`) next to it.}
#'   \item{`.dta`}{Stata file with variable and value labels (requires
#'     \pkg{haven}).}
#'   \item{`.sav`}{SPSS file with variable and value labels (requires
#'     \pkg{haven}).}
#'   \item{`.rds`}{R serialized object including all attributes.}
#' }
#'
#' @param x a result of [ipeds_panel()] or a module function.
#' @param path output file path.
#' @return `path`, invisibly.
#' @export
ipeds_write <- function(x, path) {
	ext <- tolower(tools::file_ext(path))
	cb <- ipeds_codebook(x)
	stem <- sub(paste0("\\.", ext, "$"), "", path, ignore.case = TRUE)
	if (ext == "rds") {
		saveRDS(x, path)
	} else if (ext == "csv") {
		y <- as.data.frame(x)
		for (v in names(y)) attributes(y[[v]]) <- attributes(y[[v]])[intersect(names(attributes(y[[v]])), c("levels", "class"))]
		utils::write.csv(y, path, row.names = FALSE, na = "")
		utils::write.csv(cb, paste0(stem, "_codebook.csv"), row.names = FALSE, na = "")
		a <- attr(x, "ipeds_audit")
		if (!is.null(a)) utils::write.csv(a, paste0(stem, "_audit.csv"), row.names = FALSE, na = "")
	} else if (ext %in% c("dta", "sav")) {
		if (!requireNamespace("haven", quietly = TRUE)) {
			stop("Install the 'haven' package to write .", ext, " files.", call. = FALSE)
		}
		y <- as.data.frame(x)
		for (v in names(y)) {
			lab <- cb$label[cb$variable == v]
			if (is.logical(y[[v]])) {
				y[[v]] <- haven::labelled(as.integer(y[[v]]), c(No = 0L, Yes = 1L))
			}
			if (length(lab) && !is.na(lab)) attr(y[[v]], "label") <- substr(lab, 1, 80)
		}
		if (ext == "dta") haven::write_dta(y, path) else haven::write_sav(y, path)
	} else {
		stop("Unsupported file type '.", ext, "'; use .csv, .dta, .sav or .rds.", call. = FALSE)
	}
	invisible(path)
}

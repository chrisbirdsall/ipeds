# Functions kept for compatibility with earlier versions ------------------------------

.superseded <- function(old, new) {
	msg <- sprintf("%s() is superseded by %s() and will be removed in a future version.", old, new)
	if (!isTRUE(getOption("ipeds.silence_superseded"))) message(msg)
}

.strings_as_factors <- function(df) {
	for (v in names(df)) if (is.character(df[[v]])) df[[v]] <- factor(df[[v]])
	df
}

#' Legacy download functions
#'
#' These functions from earlier versions of the package still work but are
#' superseded by [ipeds_read()], [ipeds_download()] and
#' [ipeds_dictionary()]. Files are now stored in [ipeds_cache_dir()] rather
#' than inside the installed package.
#'
#' @param surveyId a component id such as `"HD"`, `"EFA"` or `"C_A"` (see
#'   [ipeds_components()]).
#' @param year the data year.
#' @param dir cache directory (defaults to [ipeds_cache_dir()]).
#' @param stringsAsFactors convert character columns to factors.
#' @param ... ignored.
#' @return `getIPEDSSurvey()` and `downloadIPEDSSurvey()` return a data frame
#'   with lower-case column names (`NULL` if the file could not be
#'   downloaded); `downloadAllSurveys()` returns the result of
#'   [ipeds_download()]; `ipedsHelp()` returns the dictionary.
#' @name legacy
NULL

#' @rdname legacy
#' @export
getIPEDSSurvey <- function(surveyId, year, dir = NULL, stringsAsFactors = FALSE, ...) {
	.superseded("getIPEDSSurvey", "ipeds_read")
	df <- tryCatch(ipeds_read(surveyId, year, labels = FALSE, cache_dir = dir %||% ipeds_cache_dir(),
							  quiet = isTRUE(getOption("ipeds.quiet_legacy"))),
				   error = function(e) {
				   	warning(conditionMessage(e), call. = FALSE)
				   	NULL
				   })
	if (!is.null(df) && stringsAsFactors) df <- .strings_as_factors(df)
	df
}

#' @rdname legacy
#' @export
downloadIPEDSSurvey <- function(surveyId, year, dir = NULL, stringsAsFactors = FALSE, ...) {
	.superseded("downloadIPEDSSurvey", "ipeds_read")
	df <- ipeds_read(surveyId, year, labels = FALSE, cache_dir = dir %||% ipeds_cache_dir(),
					 refresh = "check", quiet = isTRUE(getOption("ipeds.quiet_legacy")))
	if (stringsAsFactors) df <- .strings_as_factors(df)
	df
}

#' @rdname legacy
#' @export
downloadAllSurveys <- function(year, ...) {
	.superseded("downloadAllSurveys", "ipeds_download")
	comps <- ipeds_components()
	comps <- comps[comps$first_year <= year & (is.na(comps$last_year) | comps$last_year >= year), ]
	invisible(ipeds_download(comps$id, year))
}

#' @rdname legacy
#' @export
ipedsHelp <- function(surveyId, year) {
	.superseded("ipedsHelp", "ipeds_dictionary")
	d <- ipeds_dictionary(surveyId, year, quiet = isTRUE(getOption("ipeds.quiet_legacy")))
	if (interactive()) utils::View(d$variables, title = d$file)
	invisible(d)
}

#' ipeds: Download, label and harmonize IPEDS data
#'
#' Tools for working with the Integrated Postsecondary Education Data System
#' (IPEDS) complete data files published by the National Center for Education
#' Statistics.
#'
#' @section Main functions:
#' \itemize{
#'   \item [ipeds_panel()] builds a harmonized, labelled institution-by-year
#'     panel from several survey components.
#'   \item [ipeds_modules] (e.g. [ipeds_enrollment()], [ipeds_completions()])
#'     return one harmonized component at a time.
#'   \item [ipeds_read()], [ipeds_stack()] and [ipeds_dictionary()] give raw
#'     access to any file and its labels.
#'   \item [ipeds_latest_year()] and [ipeds_available()] find the newest data.
#'   \item [ipeds_codebook()], [ipeds_audit()] and [ipeds_validate()] document
#'     and check the harmonization.
#' }
#'
#' @section Options:
#' \describe{
#'   \item{`ipeds.cache_dir`}{where downloaded files are stored (see
#'     [ipeds_cache_dir()]).}
#'   \item{`ipeds.max_age`}{days after which cached files are re-checked
#'     against the NCES server (default 30).}
#'   \item{`ipeds.refresh`}{default refresh policy: `"auto"`, `"check"`,
#'     `"never"` or `"always"`.}
#'   \item{`ipeds.base_url`}{base URL of the NCES data files (default
#'     `https://nces.ed.gov/ipeds/datacenter/data/`).}
#'   \item{`ipeds.timeout`, `ipeds.retries`, `ipeds.pause`}{download timeout
#'     in seconds (default 600), number of attempts (3) and pause between
#'     downloads in seconds (0.5).}
#'   \item{`ipeds.use_data_table`}{use \pkg{data.table} to read files when it
#'     is installed (default `TRUE`; much faster for large files).}
#' }
#' @keywords internal
"_PACKAGE"

#' IPEDS survey components (legacy)
#'
#' The component catalog in the format used by earlier versions of the
#' package. New code should use [ipeds_components()].
#'
#' @format A data frame with columns `SurveyID`, `Survey`, `Title`,
#'   `DataFilePre`, `DataFilePost` and `YearFormat` (4 = four digit year,
#'   2 = two digit year, 22 = two digit prior and current year).
#' @source National Center for Education Statistics, IPEDS Data Center,
#'   \url{https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx}
#' @keywords datasets
"surveys"

#' CIP 2010 codes (legacy)
#'
#' Classification of Instructional Programs (CIP) 2010 codes, titles and
#' definitions. Codes are stored as numbers, so the 2-, 4- and 6-digit levels
#' of codes ending in zeros cannot be distinguished (e.g. `1` is used for
#' `01`, `01.00` and `01.0000`); use [ipeds_cip_format()] to format codes read
#' from IPEDS files.
#'
#' @format A data frame with columns `CIPFamily`, `CIPCode`, `Action`,
#'   `TextChange`, `CIPTitle`, `CIPDefinition`, `CrossReferences` and
#'   `Examples`.
#' @source National Center for Education Statistics,
#'   \url{https://nces.ed.gov/ipeds/cipcode/}
#' @keywords datasets
"cipcodes"

#' CIP 2000 to CIP 2010 crosswalk (legacy)
#'
#' Crosswalk between the 2000 and 2010 editions of the Classification of
#' Instructional Programs. Codes are stored as numbers (see [cipcodes]).
#'
#' @format A data frame with columns `Original.code`, `Original.title`,
#'   `Action`, `Text.change`, `Current.code` and `Current.title`.
#' @source National Center for Education Statistics,
#'   \url{https://nces.ed.gov/ipeds/cipcode/}
#' @keywords datasets
"crosswalk"

.onLoad <- function(libname, pkgname) {
	op <- options()
	defaults <- list(
		ipeds.base_url = "https://nces.ed.gov/ipeds/datacenter/data/",
		ipeds.max_age = 30,
		ipeds.refresh = "auto",
		ipeds.timeout = 600,
		ipeds.retries = 3,
		ipeds.pause = 0.5,
		ipeds.use_data_table = TRUE)
	toset <- !(names(defaults) %in% names(op))
	if (any(toset)) options(defaults[toset])
	invisible()
}

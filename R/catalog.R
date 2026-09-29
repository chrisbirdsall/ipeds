# Catalog of IPEDS complete data files ------------------------------------------
#
# File names follow the conventions of the NCES data center
# (https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx). Templates use:
#   {YYYY} four digit year, {YY} two digit year, {PY} two digit prior year.
# Years before a component's regular naming scheme began are handled with
# explicit overrides.

.catalog_rows <- list(
	# id, group, title, template, first_year, last_year, period
	c("HD", "Institutional Characteristics", "Directory information", "HD{YYYY}", 2002, NA,
	  "Institution as of fall {year} (the {year}-{year1} collection)"),
	c("IC", "Institutional Characteristics", "Educational offerings, organization, services, athletic associations and (through 2013) admissions", "IC{YYYY}", 2000, NA,
	  "Fall {year}"),
	c("IC_AY", "Institutional Characteristics", "Student charges for academic year programs", "IC{YYYY}_AY", 2000, NA,
	  "Academic year {year}-{year1} (current year) and prior years"),
	c("IC_PY", "Institutional Characteristics", "Student charges by program (vocational programs)", "IC{YYYY}_PY", 2000, NA,
	  "Academic year {year}-{year1}"),
	c("IC_CAMPUSES", "Institutional Characteristics", "Institutional campuses", "IC{YYYY}_CAMPUSES", 2021, NA,
	  "Fall {year}"),
	c("IC_MISSION", "Institutional Characteristics", "Mission statements", "IC{YYYY}Mission", 2004, NA,
	  "Fall {year}"),
	c("FLAGS", "Response status", "Response status, imputation and parent/child reporting flags for all survey components", "FLAGS{YYYY}", 2004, NA,
	  "{year} data collection"),
	c("ADM", "Admissions", "Admission considerations, applications, admissions, enrollees and test scores", "ADM{YYYY}", 2014, NA,
	  "First-time students entering in fall {year}"),
	c("EFFY", "12-month Enrollment", "12-month unduplicated headcount by race/ethnicity, gender and level of student", "EFFY{YYYY}", 2002, NA,
	  "12 months from July 1, {year0} to June 30, {year}"),
	c("EFFY_DIST", "12-month Enrollment", "12-month unduplicated headcount by distance education status", "EFFY{YYYY}_DIST", 2020, NA,
	  "12 months from July 1, {year0} to June 30, {year}"),
	c("EFFY_HS", "12-month Enrollment", "12-month unduplicated headcount of high school students enrolled for credit", "EFFY{YYYY}_HS", 2023, NA,
	  "12 months from July 1, {year0} to June 30, {year}"),
	c("EFIA", "12-month Enrollment", "12-month instructional activity (credit/contact hours and FTE)", "EFIA{YYYY}", 2002, NA,
	  "12 months from July 1, {year0} to June 30, {year}"),
	c("EFA", "Fall Enrollment", "Fall enrollment by race/ethnicity, gender, attendance status and level of student", "EF{YYYY}A", 2000, NA,
	  "Fall {year}"),
	c("EFA_DIST", "Fall Enrollment", "Fall enrollment by distance education status", "EF{YYYY}A_DIST", 2012, NA,
	  "Fall {year}"),
	c("EFB", "Fall Enrollment", "Fall enrollment by age category, gender, attendance status and level of student", "EF{YYYY}B", 2000, NA,
	  "Fall {year}"),
	c("EFC", "Fall Enrollment", "Residence and migration of first-time freshmen", "EF{YYYY}C", 2000, NA,
	  "Fall {year}"),
	c("EFCP", "Fall Enrollment", "Fall enrollment by major field of study (even years)", "EF{YYYY}CP", 2000, NA,
	  "Fall {year}"),
	c("EFD", "Fall Enrollment", "Total entering class, retention rates and student-to-faculty ratio", "EF{YYYY}D", 2002, NA,
	  "Fall {year} (retention of the fall {year0} cohort)"),
	c("EFEST", "Fall Enrollment", "Estimated fall enrollment", "EFEST{YYYY}", 2007, 2009,
	  "Fall {year}"),
	c("EFANR", "Fall Enrollment", "Fall enrollment by race/ethnicity, gender, attendance status and level (1995-1999 format)", "EF{YY}_ANR", 1995, 1999,
	  "Fall {year}"),
	c("C_A", "Completions", "Awards/degrees conferred by program (6-digit CIP code), award level, race/ethnicity and gender", "C{YYYY}_A", 2000, NA,
	  "Awards conferred July 1, {year0} to June 30, {year}"),
	c("C_B", "Completions", "Number of students receiving awards/degrees by race/ethnicity and gender", "C{YYYY}_B", 2012, NA,
	  "July 1, {year0} to June 30, {year}"),
	c("C_C", "Completions", "Number of students receiving awards/degrees by award level and age", "C{YYYY}_C", 2012, NA,
	  "July 1, {year0} to June 30, {year}"),
	c("C_DEP", "Completions", "Number of programs offered and programs offered via distance education", "C{YYYY}DEP", 2013, NA,
	  "July 1, {year0} to June 30, {year}"),
	c("SFA", "Student Financial Aid", "Student financial aid and net price", "SFA{PY}{YY}", 2002, NA,
	  "Academic year {year0}-{yy} (first-time students entering fall {year0})"),
	c("SFAV", "Student Financial Aid", "Military servicemembers and veteran's benefits", "SFAV{PY}{YY}", 2014, NA,
	  "Academic year {year0}-{yy}"),
	c("GR", "Graduation Rates", "Graduation rate data, 150% of normal time to complete", "GR{YYYY}", 1997, NA,
	  "Status on August 31, {year} of the cohort entering fall {year6} (4-year) or fall {year3} (2-year)"),
	c("GR_L2", "Graduation Rates", "Graduation rate data for less-than-2-year institutions", "GR{YYYY}_L2", 1997, NA,
	  "Cohort entering fall {year3}"),
	c("GR200", "Graduation Rates", "Graduation rate data, 200% of normal time to complete", "GR200_{YY}", 2009, NA,
	  "Cohort entering fall {year8} (4-year) or fall {year4} (2-year)"),
	c("GR_PELL_SSL", "Graduation Rates", "Graduation rates by Pell Grant and Subsidized Stafford Loan status", "GR{YYYY}_PELL_SSL", 2016, NA,
	  "Cohort entering fall {year6}"),
	c("OM", "Outcome Measures", "Award and enrollment outcomes of degree/certificate-seeking cohorts", "OM{YYYY}", 2015, NA,
	  "Cohort entering {year8}-{year7}"),
	c("F_F1A", "Finance", "Public institutions using GASB 34/35 standards", "F{PY}{YY}_F1A", 2002, NA,
	  "Fiscal year ending in {year}"),
	c("F_F1", "Finance", "Public institutions using pre-GASB 34/35 standards", "F{PY}{YY}_F1", 2000, 2003,
	  "Fiscal year ending in {year}"),
	c("F_F2", "Finance", "Private not-for-profit institutions and public institutions using FASB standards", "F{PY}{YY}_F2", 2001, NA,
	  "Fiscal year ending in {year}"),
	c("F_F3", "Finance", "Private for-profit institutions", "F{PY}{YY}_F3", 2001, NA,
	  "Fiscal year ending in {year}"),
	c("SAL_IS", "Human Resources", "Salaries of full-time instructional staff", "SAL{YYYY}_IS", 2012, NA,
	  "Academic year {year}-{year1}"),
	c("SAL_NIS", "Human Resources", "Salaries of full-time non-instructional staff", "SAL{YYYY}_NIS", 2012, NA,
	  "Academic year {year}-{year1}"),
	c("SAL_A", "Human Resources", "Salaries of full-time instructional faculty by contract length, gender and rank (pre-2012 form)", "SAL{YYYY}_A", 2002, 2011,
	  "Academic year {year}-{year1}"),
	c("SAL_B", "Human Resources", "Fringe benefits of full-time instructional faculty (pre-2012 form)", "SAL{YYYY}_B", 2002, 2010,
	  "Academic year {year}-{year1}"),
	c("SAL_FACULTY", "Human Resources", "Tenure status of full-time instructional faculty (pre-2012 form)", "SAL{YYYY}_FACULTY", 2006, 2011,
	  "Academic year {year}-{year1}"),
	c("SAL_A_LT9", "Human Resources", "Full-time instructional faculty with less than 9-month contracts (pre-2012 form)", "SAL{YYYY}_A_LT9", 2005, 2011,
	  "Academic year {year}-{year1}"),
	c("S_IS", "Human Resources", "Full-time instructional staff by rank, tenure, race/ethnicity and gender", "S{YYYY}_IS", 2012, NA,
	  "November 1, {year}"),
	c("S_NH", "Human Resources", "New hires by occupational category, race/ethnicity and gender", "S{YYYY}_NH", 2012, NA,
	  "November 1, {year}"),
	c("S_OC", "Human Resources", "Staff by occupational category, race/ethnicity and gender", "S{YYYY}_OC", 2012, NA,
	  "November 1, {year}"),
	c("S_SIS", "Human Resources", "Instructional staff by faculty and tenure status", "S{YYYY}_SIS", 2012, NA,
	  "November 1, {year}"),
	c("S_ABD", "Human Resources", "Employees by primary occupation, salary, race/ethnicity and gender (pre-2012 form)", "S{YYYY}_ABD", 2001, 2011,
	  "Fall {year}"),
	c("S_F", "Human Resources", "Full-time instruction/research/public service staff by tenure and rank (pre-2012 form)", "S{YYYY}_F", 2001, 2011,
	  "Fall {year}"),
	c("S_G", "Human Resources", "New hires by primary occupation (pre-2012 form)", "S{YYYY}_G", 2001, 2011,
	  "Fall {year}"),
	c("S_CN", "Human Resources", "Employees at smaller and non-degree-granting institutions (pre-2012 form)", "S{YYYY}_CN", 2001, 2011,
	  "Fall {year}"),
	c("EAP", "Human Resources", "Employees by assigned position", "EAP{YYYY}", 2001, NA,
	  "November 1, {year}"),
	c("AL", "Academic Libraries", "Academic libraries", "AL{YYYY}", 2014, NA,
	  "Fiscal year ending in {year}"),
	c("DRVIC", "Derived variables", "Derived variables: institutional characteristics", "DRVIC{YYYY}", 2020, NA,
	  "Fall {year}"),
	c("DRVADM", "Derived variables", "Derived variables: admissions", "DRVADM{YYYY}", 2020, NA,
	  "Fall {year}"),
	c("DRVEF", "Derived variables", "Derived variables: fall enrollment", "DRVEF{YYYY}", 2020, NA,
	  "Fall {year}"),
	c("DRVEF12", "Derived variables", "Derived variables: 12-month enrollment", "DRVEF12{YYYY}", 2020, NA,
	  "July 1, {year0} to June 30, {year}"),
	c("DRVC", "Derived variables", "Derived variables: completions", "DRVC{YYYY}", 2020, NA,
	  "July 1, {year0} to June 30, {year}"),
	c("DRVGR", "Derived variables", "Derived variables: graduation rates", "DRVGR{YYYY}", 2020, NA,
	  "See GR"),
	c("DRVOM", "Derived variables", "Derived variables: outcome measures", "DRVOM{YYYY}", 2020, NA,
	  "See OM"),
	c("DRVF", "Derived variables", "Derived variables: finance", "DRVF{YYYY}", 2020, NA,
	  "Fiscal year ending in {year}"),
	c("DRVHR", "Derived variables", "Derived variables: human resources", "DRVHR{YYYY}", 2020, NA,
	  "November 1, {year}"),
	c("DRVAL", "Derived variables", "Derived variables: academic libraries", "DRVAL{YYYY}", 2020, NA,
	  "Fiscal year ending in {year}")
)

.catalog <- local({
	m <- do.call(rbind, .catalog_rows)
	data.frame(id = m[, 1], group = m[, 2], title = m[, 3], template = m[, 4],
			   first_year = as.integer(m[, 5]), last_year = suppressWarnings(as.integer(m[, 6])),
			   period = m[, 7], stringsAsFactors = FALSE)
})

# Irregular file names used before the regular scheme started (from the NCES
# data center listing).
.overrides <- list(
	HD = c("1999" = "IC99_HD", "2000" = "FA2000HD", "2001" = "FA2001HD"),
	IC = c("1999" = "IC99ABCF"),
	IC_AY = c("1999" = "IP1999AY"),
	IC_PY = c("1999" = "IP1999PY"),
	EFFY = c("2001" = "EF2001D1"),
	EFIA = c("2001" = "EF2001D2"),
	C_A = c("1995" = "C9495_A", "1996" = "C9596_A", "1997" = "C9697_A",
			"1998" = "C9798_A", "1999" = "C9899_A"),
	SFA = c("2000" = "SFA9900S", "2001" = "SFA0001S"),
	F_F1 = c("2000" = "F9900_F1"),
	F_F2 = c("2000" = "F9900F2"),
	F_F3 = c("2000" = "F9900F3"),
	SAL_A = c("2001" = "SAL2001_A_S"),
	SAL_B = c("2001" = "SAL2001_B_S")
)

# Older component identifiers accepted for backwards compatibility.
.aliases <- c(EFD1 = "EFFY", EFD2 = "EFIA", F1A = "F_F1A", F2 = "F_F2", F3 = "F_F3",
			  EF = "EFA", CDEP = "C_DEP", GR_PELL = "GR_PELL_SSL")

#' IPEDS components (survey files) known to the package
#'
#' Returns the catalog of IPEDS complete data files that the package knows how
#' to locate. Each component is identified by an `id` (for example `"HD"` for
#' the directory file or `"C_A"` for completions) that is used by
#' [ipeds_read()], [ipeds_download()] and friends.
#'
#' The `period` column describes the reference period of the data in terms of
#' the `year` argument used throughout the package. IPEDS names most files by
#' the year of the fall term (e.g. `HD2023`, `EF2023A`) or by the year in
#' which an academic/fiscal year ends (e.g. `C2023_A` covers July 2022 to June
#' 2023 and `SFA2223`/`F2223_F1A` cover 2022-23). The package always uses the
#' four digit year that appears in, or is implied by, the file name, so
#' `year = 2023` refers to `HD2023`, `C2023_A`, `SFA2223` and `F2223_F1A`.
#'
#' @param group optional survey group to filter on (e.g. `"Finance"`).
#' @return A data frame with columns `id`, `group`, `title`, `template`,
#'   `first_year`, `last_year` (`NA` when still collected) and `period`.
#' @examples
#' ipeds_components()
#' ipeds_components("Finance")
#' @export
ipeds_components <- function(group = NULL) {
	out <- .catalog
	if (!is.null(group)) out <- out[tolower(out$group) %in% tolower(group), , drop = FALSE]
	rownames(out) <- NULL
	out
}

.component <- function(component) {
	if (length(component) != 1 || is.na(component)) {
		stop("`component` must be a single IPEDS component id; see ipeds_components().", call. = FALSE)
	}
	id <- toupper(component)
	if (id %in% names(.aliases)) id <- .aliases[[id]]
	row <- .catalog[.catalog$id == id, , drop = FALSE]
	if (nrow(row) == 0) {
		stop("Unknown IPEDS component '", component, "'. See ipeds_components() for valid ids, ",
			 "or use ipeds_read_file() with a literal file name such as 'EF99_ANR'.", call. = FALSE)
	}
	as.list(row)
}

#' Build IPEDS data center file names
#'
#' @param component a component id from [ipeds_components()].
#' @param year one or more years (see [ipeds_components()] for the meaning of
#'   `year` for each component).
#' @return A character vector of file names (without extension) such as
#'   `"HD2023"`, `"C2023_A"` or `"SFA2223"`.
#' @examples
#' ipeds_file_name("HD", 2023)
#' ipeds_file_name("SFA", 2020:2023)
#' ipeds_file_name("F_F1A", 2023)
#' @export
ipeds_file_name <- function(component, year) {
	comp <- .component(component)
	year <- as.integer(year)
	ov <- .overrides[[comp$id]]
	vapply(year, function(y) {
		if (!is.null(ov) && as.character(y) %in% names(ov)) return(ov[[as.character(y)]])
		t <- comp$template
		t <- gsub("{YYYY}", sprintf("%04d", y), t, fixed = TRUE)
		t <- gsub("{YY}", sprintf("%02d", y %% 100L), t, fixed = TRUE)
		t <- gsub("{PY}", sprintf("%02d", (y - 1L) %% 100L), t, fixed = TRUE)
		t
	}, character(1))
}

# Human readable reference period for a component and year.
.period <- function(component, year) {
	comp <- .component(component)
	p <- comp$period
	subs <- list("{year}" = year, "{year0}" = year - 1L, "{year1}" = year + 1L,
				 "{year3}" = year - 3L, "{year4}" = year - 4L, "{year6}" = year - 6L,
				 "{year7}" = year - 7L, "{year8}" = year - 8L,
				 "{yy}" = sprintf("%02d", year %% 100L))
	for (k in names(subs)) p <- gsub(k, subs[[k]], p, fixed = TRUE)
	p
}

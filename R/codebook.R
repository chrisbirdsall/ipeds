# Codebook: labels and documentation for harmonized variables ------------------------

.module_info <- list(
	keys = list(source = "", period = ""),
	directory = list(source = "HD", period = "Fall of `year` (HD<year>)"),
	enrollment = list(source = "EFA", period = "Fall of `year` (EF<year>A)"),
	enrollment_12m = list(source = "EFFY, EFIA",
						  period = "July 1 of `year`-1 to June 30 of `year` (EFFY<year>, EFIA<year>)"),
	retention = list(source = "EFD",
					 period = "Fall of `year`; retention of students who entered in fall `year`-1 (EF<year>D)"),
	completions = list(source = "C_A",
					   period = "Awards conferred July 1 of `year`-1 to June 30 of `year` (C<year>_A)"),
	admissions = list(source = "IC (through 2013), ADM (2014 onward)",
					  period = "First-time students entering in fall of `year`"),
	charges = list(source = "IC_AY", period = "Academic year `year` to `year`+1 (IC<year>_AY)"),
	aid = list(source = "SFA", period = "Academic year `year`-1 to `year` (SFA<yy-1><yy>)"),
	grad_rates = list(source = "GR",
					  period = paste("Status on August 31 of `year` of students who entered in fall",
					  			   "`year`-6 (bachelor's cohort) or `year`-3 (2-year cohort) (GR<year>)")),
	finance = list(source = "F_F1A, F_F2, F_F3", period = "Fiscal year ending in `year`"),
	flags = list(source = "FLAGS", period = "`year` data collection")
)

.cb <- function(variable, label, module, notes = "", dollars = FALSE) {
	data.frame(variable = variable, label = label, module = module, notes = notes,
			   dollars = dollars, stringsAsFactors = FALSE)
}

.codebook_static <- function() {
	tuit <- expand.grid(k = c("ug_indistrict", "ug_instate", "ug_outstate", "grad_instate",
							  "grad_outstate"), what = c("tuition", "fees", "tuitfee"),
						stringsAsFactors = FALSE)
	tuit_lab <- c(ug_indistrict = "undergraduate, in-district", ug_instate = "undergraduate, in-state",
				  ug_outstate = "undergraduate, out-of-state", grad_instate = "graduate, in-state",
				  grad_outstate = "graduate, out-of-state")
	what_lab <- c(tuition = "Average tuition", fees = "Required fees", tuitfee = "Tuition and required fees")
	rbind(
		.cb("unitid", "IPEDS unit ID of the institution", "keys"),
		.cb("year", "IPEDS data year (see the period of each module)", "keys"),
		.cb("instnm", "Institution name", "directory"),
		.cb("ialias", "Institution name alias", "directory"),
		.cb("city", "City", "directory"),
		.cb("stabbr", "State abbreviation", "directory"),
		.cb("fips", "State FIPS code", "directory"),
		.cb("countycd", "County FIPS code", "directory"),
		.cb("countynm", "County name", "directory"),
		.cb("zip5", "ZIP code (first 5 digits)", "directory"),
		.cb("latitude", "Latitude", "directory"),
		.cb("longitude", "Longitude", "directory"),
		.cb("cbsa", "Core-based statistical area code", "directory"),
		.cb("cngdstcd", "Congressional district code", "directory"),
		.cb("webaddr", "Institution web address", "directory"),
		.cb("opeid", "Office of Postsecondary Education ID", "directory"),
		.cb("sector", "Sector of institution", "directory"),
		.cb("control", "Control of institution", "directory"),
		.cb("iclevel", "Level of institution", "directory"),
		.cb("obereg", "Bureau of Economic Analysis region", "directory"),
		.cb("instsize", "Institution size category (total enrollment)", "directory"),
		.cb("hloffer", "Highest level of offering (IPEDS code)", "directory"),
		.cb("hdegofr1", "Highest degree offered (IPEDS code)", "directory"),
		.cb("deggrant", "Degree-granting institution", "directory"),
		.cb("hbcu", "Historically Black college or university", "directory"),
		.cb("tribal", "Tribal college", "directory"),
		.cb("landgrnt", "Land-grant institution", "directory"),
		.cb("medical", "Grants a medical degree", "directory"),
		.cb("openpubl", "Open to the general public", "directory"),
		.cb("pset4flg", "Postsecondary and Title IV institution indicator (IPEDS code)", "directory"),
		.cb("instcat", "Institutional category (IPEDS code)", "directory"),
		.cb("cyactive", "Institution active in the current year", "directory"),
		.cb("newid", "UNITID of the institution this one merged into", "directory"),
		.cb("deathyr", "Year the institution was deleted from IPEDS", "directory"),
		.cb("closedat", "Date the institution closed", "directory"),
		.cb("f1systyp", "Part of a multi-institution or multi-campus system (IPEDS code)", "directory"),
		.cb("f1sysnam", "Name of multi-institution system", "directory"),
		.cb("locale", "Locale (IPEDS code)", "directory",
			"Metro-centric codes (1-8) through 2005 and urban-centric codes (11-43) from 2006; use locale4 for a comparable series"),
		.cb("locale4", "Locale: city, suburb, town or rural (harmonized)", "directory",
			"Urban-centric codes 11-13/21-23/31-33/41-43; before 2006 metro-centric codes 1-2 city, 3-4 urban fringe (suburb), 5-6 town, 7-8 rural"),
		.cb("carnegie_basic", "Carnegie basic classification code", "directory",
			"Newest edition in the file each year; edition in carnegie_edition. Codes are not comparable across editions"),
		.cb("carnegie_edition", "Edition of the Carnegie classification in carnegie_basic", "directory"),
		.cb("carnegie_group", "Carnegie classification group (harmonized)", "directory",
			"Coarse groups comparable across the 2000 and 2005-2021 editions; 2015+ special-focus two-year and baccalaureate/associate's colleges are grouped with associate's"),
		.cb("enr_total", "Fall enrollment: all students", "enrollment"),
		.cb("enr_ug", "Fall enrollment: undergraduates", "enrollment"),
		.cb("enr_grad", "Fall enrollment: graduate and first-professional students", "enrollment",
			"Total minus undergraduate, so first-professional students (reported separately before 2010) are included in every year"),
		.cb("enr_ft", "Fall enrollment: full-time students", "enrollment"),
		.cb("enr_pt", "Fall enrollment: part-time students", "enrollment"),
		.cb("enr_ug_ft", "Fall enrollment: full-time undergraduates", "enrollment"),
		.cb("enr_ug_pt", "Fall enrollment: part-time undergraduates", "enrollment"),
		.cb("enr_grad_ft", "Fall enrollment: full-time graduate and first-professional students", "enrollment"),
		.cb("enr_grad_pt", "Fall enrollment: part-time graduate and first-professional students", "enrollment"),
		.cb("enr_ug_firsttime", "Fall enrollment: first-time degree/certificate-seeking undergraduates", "enrollment"),
		.cb("enr_ug_ft_firsttime", "Fall enrollment: full-time first-time degree/certificate-seeking undergraduates", "enrollment"),
		.cb("enr_race_scheme", "Race/ethnicity categories reported (fall enrollment)", "enrollment",
			"7 categories (1977 standards) or 9 categories (1997 standards); institutions switched between 2008 and 2010"),
		.cb("e12_total", "12-month unduplicated headcount: all students", "enrollment_12m"),
		.cb("e12_ug", "12-month unduplicated headcount: undergraduates", "enrollment_12m"),
		.cb("e12_grad", "12-month unduplicated headcount: graduate and first-professional students", "enrollment_12m",
			"Total minus undergraduate"),
		.cb("e12_race_scheme", "Race/ethnicity categories reported (12-month enrollment)", "enrollment_12m"),
		.cb("fte_ug", "Full-time-equivalent undergraduate enrollment", "enrollment_12m",
			"Reported FTE when available, otherwise the IPEDS estimate"),
		.cb("fte_grad", "Full-time-equivalent graduate enrollment", "enrollment_12m"),
		.cb("fte_dpp", "Full-time-equivalent doctor's professional practice enrollment", "enrollment_12m"),
		.cb("fte_total", "Full-time-equivalent enrollment, total", "enrollment_12m", "fte_ug + fte_grad + fte_dpp"),
		.cb("credit_hours_ug", "Undergraduate credit hours", "enrollment_12m"),
		.cb("clock_hours_ug", "Undergraduate clock (contact) hours", "enrollment_12m"),
		.cb("credit_hours_grad", "Graduate credit hours", "enrollment_12m"),
		.cb("ret_ft_pct", "Full-time retention rate (percent)", "retention"),
		.cb("ret_pt_pct", "Part-time retention rate (percent)", "retention"),
		.cb("stu_fac_ratio", "Student-to-faculty ratio", "retention"),
		.cb("awards_total", "Awards conferred, all levels (first majors)", "completions"),
		.cb(paste0("awards_", names(.award_group_labels)),
			paste0("Awards conferred: ", .award_group_labels, " (first majors)"), "completions",
			ifelse(names(.award_group_labels) == "doctor",
				   "Sum of doctor's research/scholarship, professional practice, other, pre-2010 doctor's and first-professional degrees",
				   ifelse(names(.award_group_labels) %in% c("doctor_pre2010", "first_prof"),
				   	   "Category retired after 2009-10; missing in later years",
				   	   ifelse(names(.award_group_labels) %in% c("doctor_research", "doctor_prof", "doctor_other"),
				   	   	   "Missing before the new doctoral categories were used; see awards_doctor for a comparable series",
				   	   	   "")))),
		.cb("bach_race_scheme", "Race/ethnicity categories reported (completions)", "completions"),
		.cb("awards", "Awards conferred (first majors)", "completions"),
		.cb("cip", "CIP code of the program", "completions",
			"In the CIP edition given by cip_edition; 2-digit families are the most comparable across editions"),
		.cb("cip_edition", "Edition of the Classification of Instructional Programs", "completions"),
		.cb("award_level", "Harmonized award level", "completions",
			"See the awards_* variables of ipeds_completions(); unmapped IPEDS codes appear as awlevel_<code>"),
		.cb("race_scheme", "Race/ethnicity categories reported", "completions"),
		.cb("applicants", "Applicants (first-time degree/certificate-seeking)", "admissions"),
		.cb("applicants_men", "Applicants, men", "admissions"),
		.cb("applicants_women", "Applicants, women", "admissions"),
		.cb("admits", "Admitted applicants", "admissions"),
		.cb("admits_men", "Admitted applicants, men", "admissions"),
		.cb("admits_women", "Admitted applicants, women", "admissions"),
		.cb("enrolled", "Admitted applicants who enrolled", "admissions"),
		.cb("enrolled_ft", "Admitted applicants who enrolled full-time", "admissions"),
		.cb("admit_rate", "Admission rate (admits / applicants, proportion)", "admissions"),
		.cb("yield_rate", "Yield rate (enrolled / admits, proportion)", "admissions"),
		.cb("sat_submit_pct", "Percent of enrollees submitting SAT scores", "admissions"),
		.cb("act_submit_pct", "Percent of enrollees submitting ACT scores", "admissions"),
		.cb("sat_rw_25", "SAT critical reading / evidence-based reading and writing, 25th percentile", "admissions",
			"The SAT was redesigned in March 2016; scores are not directly comparable across sat_version"),
		.cb("sat_rw_75", "SAT critical reading / evidence-based reading and writing, 75th percentile", "admissions",
			"See sat_version"),
		.cb("sat_math_25", "SAT math, 25th percentile", "admissions", "See sat_version"),
		.cb("sat_math_75", "SAT math, 75th percentile", "admissions", "See sat_version"),
		.cb("act_comp_25", "ACT composite, 25th percentile", "admissions"),
		.cb("act_comp_75", "ACT composite, 75th percentile", "admissions"),
		.cb("sat_version", "SAT version in effect for the entering class", "admissions"),
		.cb("test_policy", "Admission test score policy (harmonized)", "admissions",
			"Category wording changed over time; 'Considered but not required' is only available in recent years"),
		.cb("open_admission", "Open admission policy", "admissions"),
		.cb(paste0(tuit$what, "_", tuit$k), paste0(what_lab[tuit$what], ", ", tuit_lab[tuit$k]),
			"charges", "Nominal dollars; see ipeds_real_dollars()", dollars = TRUE),
		.cb("room_board_oncampus", "On-campus room and board", "charges", "Nominal dollars", dollars = TRUE),
		.cb("books_supplies", "Books and supplies", "charges", "Nominal dollars", dollars = TRUE),
		.cb("pell_pct_ug", "Percent of undergraduates awarded Pell grants", "aid"),
		.cb("pell_pct_ftft", "Percent of full-time first-time undergraduates awarded Pell grants", "aid"),
		.cb("fedloan_pct_ug", "Percent of undergraduates awarded federal student loans", "aid"),
		.cb("fedloan_pct_ftft", "Percent of full-time first-time undergraduates awarded federal loans", "aid"),
		.cb("grant_avg_ftft", "Average grant aid, full-time first-time undergraduates", "aid", "Nominal dollars", TRUE),
		.cb("inst_grant_avg_ftft", "Average institutional grant aid, full-time first-time undergraduates", "aid",
			"Nominal dollars", TRUE),
		.cb("net_price", "Average net price, full-time first-time students awarded grant aid", "aid",
			"In-state net price for public institutions; nominal dollars", TRUE),
		.cb("net_price_inc_0_30k", "Average net price, family income $0-30,000", "aid", "Nominal dollars", TRUE),
		.cb("gr_bach_cohort", "Bachelor's degree-seeking cohort (adjusted), 4-year institutions", "grad_rates"),
		.cb("gr_bach_completers_150", "Completers within 150% of normal time, bachelor's cohort", "grad_rates"),
		.cb("gr_2yr_cohort", "Degree/certificate-seeking cohort (adjusted), 2-year institutions", "grad_rates"),
		.cb("gr_2yr_completers_150", "Completers within 150% of normal time, 2-year cohort", "grad_rates"),
		.cb("gr_race_scheme", "Race/ethnicity categories reported (graduation rates)", "grad_rates"),
		.cb("fin_form", "Finance reporting form (GASB, FASB or for-profit)", "finance",
			"Definitions differ between accounting standards; compare within form"),
		.cb("rev_total", "Total revenues (and other additions / investment return)", "finance",
			"Experimental; nominal dollars", TRUE),
		.cb("rev_tuition", "Tuition and fees revenue", "finance", "Experimental; nominal dollars", TRUE),
		.cb("rev_state_approp", "State appropriations", "finance", "Experimental; nominal dollars", TRUE),
		.cb("exp_total", "Total expenses (and deductions)", "finance", "Experimental; nominal dollars", TRUE),
		.cb("exp_instruction", "Instruction expenses", "finance", "Experimental; nominal dollars", TRUE),
		.cb("endowment_eoy", "Value of endowment assets at the end of the fiscal year", "finance",
			"Experimental; nominal dollars", TRUE)
	)
}

.race_prefixes <- list(
	enr = list(label = "Fall enrollment, all students", module = "enrollment"),
	enr_ug = list(label = "Fall enrollment, undergraduates", module = "enrollment"),
	e12 = list(label = "12-month unduplicated headcount", module = "enrollment_12m"),
	bach = list(label = "Bachelor's degrees awarded (first majors)", module = "completions"),
	awards = list(label = "Awards conferred (first majors)", module = "completions"),
	gr_bach_rate_150 = list(label = "150% graduation rate, bachelor's cohort (proportion)", module = "grad_rates"),
	gr_2yr_rate_150 = list(label = "150% graduation rate, 2-year cohort (proportion)", module = "grad_rates")
)

# Labels for race/ethnicity and sex columns, e.g. enr_ug_asian_pi_women.
.codebook_race <- function(v) {
	for (p in names(.race_prefixes)[order(-nchar(names(.race_prefixes)))]) {
		if (!startsWith(v, paste0(p, "_")) && v != p) next
		rest <- if (v == p) "total" else substring(v, nchar(p) + 2)
		bridged <- grepl("_bridged$", rest)
		rest <- sub("_bridged$", "", rest)
		sex <- regmatches(rest, regexpr("_(men|women)$", rest))
		key <- sub("_(men|women)$", "", rest)
		if (!(key %in% names(.race_labels))) next
		lab <- paste0(.race_prefixes[[p]]$label, ": ", .race_labels[[key]])
		if (length(sex)) lab <- paste0(lab, ", ", sub("_", "", sex))
		notes <- ""
		if (key %in% c("asian", "nhpi", "two_more")) {
			notes <- "Only reported under the 9-category standards (from 2008-2010); missing earlier"
		} else if (key == "asian_pi") {
			notes <- "Asian plus Native Hawaiian/Other Pacific Islander; equals Asian/Pacific Islander under the 7-category standards"
		} else if (key %in% c("white", "black", "aian")) {
			notes <- "Series has a break when institutions adopted the 9-category standards (Two or more races added)"
		}
		if (bridged) {
			lab <- paste(lab, "(bridged)")
			notes <- "Two or more races allocated proportionally to White, Black, American Indian/Alaska Native and Asian/Pacific Islander in 9-category years"
		}
		return(.cb(v, lab, .race_prefixes[[p]]$module, notes))
	}
	NULL
}

.codebook_rows <- function(vars) {
	static <- .codebook_static()
	rows <- lapply(vars, function(v) {
		r <- static[static$variable == v, , drop = FALSE]
		if (nrow(r)) return(r[1, , drop = FALSE])
		r <- .codebook_race(v)
		if (!is.null(r)) return(r)
		if (grepl("^gr_(bach|2yr)_rate_150", v)) {
			return(.cb(v, paste("150% graduation rate", v), "grad_rates"))
		}
		if (grepl("^prch_.*_child$", v)) {
			return(.cb(v, paste0("Child record for ", toupper(sub("^prch_(.*)_child$", "\\1", v)),
								 " (data reported with a parent institution)"), "flags"))
		}
		if (grepl("^(prch|idx)_", v)) {
			return(.cb(v, paste0(if (startsWith(v, "prch")) "Parent/child indicator (IPEDS code): "
								 else "UNITID of parent institution: ", toupper(sub("^(prch|idx)_", "", v))),
					   "flags"))
		}
		.cb(v, v, NA_character_)
	})
	out <- do.call(rbind, rows)
	out$source <- vapply(out$module, function(m) .module_info[[m]]$source %||% NA_character_, character(1))
	out$period <- vapply(out$module, function(m) .module_info[[m]]$period %||% NA_character_, character(1))
	rownames(out) <- NULL
	out[, c("variable", "label", "module", "source", "period", "notes", "dollars")]
}

# Attach "label" attributes from the codebook.
.apply_codebook <- function(df) {
	if (ncol(df) == 0) return(df)
	cb <- .codebook_rows(names(df))
	for (i in seq_len(nrow(cb))) attr(df[[cb$variable[i]]], "label") <- cb$label[i]
	df
}

#' Codebook of a harmonized IPEDS panel
#'
#' Describes every variable of a panel or module result: label, source module
#' and IPEDS component, reference period, harmonization notes, whether it is
#' a dollar amount, the years in which it is available and the number of
#' non-missing values.
#'
#' @param x a result of [ipeds_panel()] or of a module function.
#' @return A data frame with one row per variable.
#' @examples
#' \dontrun{
#' p <- ipeds_panel(2019:2023, modules = c("directory", "enrollment"))
#' ipeds_codebook(p)
#' }
#' @export
ipeds_codebook <- function(x) {
	cb <- .codebook_rows(names(x))
	yrs <- x$year
	cb$years <- vapply(cb$variable, function(v) {
		y <- sort(unique(yrs[!is.na(x[[v]])]))
		.year_ranges(y)
	}, character(1))
	cb$n_nonmissing <- vapply(cb$variable, function(v) sum(!is.na(x[[v]])), integer(1))
	cb
}

.year_ranges <- function(y) {
	if (length(y) == 0) return("")
	br <- c(0, which(diff(y) != 1), length(y))
	paste(vapply(seq_len(length(br) - 1), function(i) {
		a <- y[br[i] + 1]
		b <- y[br[i + 1]]
		if (a == b) as.character(a) else paste0(a, "-", b)
	}, character(1)), collapse = ", ")
}

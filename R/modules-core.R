# Shared machinery for harmonized modules -----------------------------------------

.opts <- function(cache_dir = ipeds_cache_dir(), refresh = NULL, quiet = FALSE, verify = TRUE,
				  revised = TRUE, race_bridge = FALSE, race_by_sex = TRUE, audit = .audit_new()) {
	list(cache_dir = cache_dir, refresh = refresh, quiet = quiet, verify = verify,
		 revised = revised, race_bridge = race_bridge, race_by_sex = race_by_sex, audit = audit)
}

.drop_flags <- function(header) !.is_flag(header, header)

# Load one component-year for a module: data, dictionary and audit context.
# Returns NULL (and records the reason) when the file is unavailable.
.load <- function(component, year, opts, module, select = .drop_flags) {
	file <- ipeds_file_name(component, year)
	ctx <- list(module = module, component = component, year = year, file = file,
				audit = opts$audit, dict = NULL)
	df <- tryCatch(.read_data(file, select = select, imputation_flags = FALSE,
							  cache_dir = opts$cache_dir, refresh = opts$refresh,
							  quiet = opts$quiet, revised = opts$revised),
				   error = function(e) e)
	if (inherits(df, "error")) {
		.audit(ctx, "file", file, "file_unavailable", conditionMessage(df))
		.msg("  ", file, " unavailable: ", conditionMessage(df), quiet = opts$quiet)
		return(NULL)
	}
	if (isTRUE(opts$verify)) {
		ctx$dict <- tryCatch(
			ipeds_dictionary_file(file, cache_dir = opts$cache_dir, refresh = opts$refresh,
								  quiet = TRUE),
			error = function(e) {
				.audit(ctx, "dictionary", file, "note",
					   paste("dictionary unavailable, labels not verified:", conditionMessage(e)))
				NULL
			})
	}
	info <- attr(df, "ipeds")
	.audit(ctx, "file", file, "loaded",
		   c(sprintf("%d rows", nrow(df)),
		     if (isTRUE(info$revised)) "revised release (_rv)" else "original release"))
	df <- df[!is.na(df$unitid), , drop = FALSE]
	list(df = df, ctx = ctx)
}

.run_module <- function(module, years, opts, fun, ...) {
	years <- .check_years(years)
	parts <- lapply(years, function(y) {
		.msg(sprintf("[%s] %d", module, y), quiet = opts$quiet)
		res <- if (isTRUE(getOption("ipeds.debug"))) fun(y, opts, ...) else
			tryCatch(fun(y, opts, ...), error = function(e) {
				.audit(list(audit = opts$audit, module = module, year = y), "module", NA,
					   "check_failed", paste("error:", conditionMessage(e)))
				warning(sprintf("%s (%d): %s", module, y, conditionMessage(e)), call. = FALSE)
				NULL
			})
		if (is.null(res) || nrow(res) == 0) return(NULL)
		res$year <- rep(as.integer(y), nrow(res))
		res[, c("unitid", "year", setdiff(names(res), c("unitid", "year"))), drop = FALSE]
	})
	out <- .rbind_fill(parts)
	if (nrow(out) == 0) out <- data.frame(unitid = integer(), year = integer())
	out <- out[order(out$unitid, out$year), , drop = FALSE]
	rownames(out) <- NULL
	out <- .apply_codebook(out)
	attr(out, "ipeds_audit") <- .audit_table(opts$audit)
	out
}

# Keep one row per unitid (the first), recording duplicates in the audit.
.one_per_unitid <- function(df, ctx, concept) {
	dup <- duplicated(df$unitid)
	if (any(dup)) {
		.audit(ctx, concept, NA, "note",
			   sprintf("%d duplicate unitid rows dropped", sum(dup)))
		df <- df[!dup, , drop = FALSE]
	}
	df
}

.yes_no <- function(x) {
	if (is.null(x)) return(NULL)
	x <- .as_num(x)
	out <- rep(NA, length(x))
	out[x %in% 1] <- TRUE
	out[x %in% c(0, 2)] <- FALSE
	out
}

.factor_codes <- function(x, map) {
	x <- .as_num(x)
	factor(unname(map[match(x, as.numeric(names(map)))]), levels = unique(unname(map)))
}

# Common documentation for module functions -------------------------------------

#' Harmonized IPEDS modules
#'
#' Each module reads one IPEDS survey component for a range of years and
#' returns one row per institution (`unitid`) and `year` with harmonized,
#' labelled measures. [ipeds_panel()] combines several modules into a single
#' panel; the module functions can also be used on their own.
#'
#' \describe{
#'   \item{`ipeds_directory()`}{Directory information (HD): name, location,
#'     sector, control, level, HBCU/tribal/land-grant status, size, a
#'     harmonized four-category locale (the 2006 switch from the old
#'     metro-centric codes to urban-centric codes is bridged), the most recent
#'     Carnegie basic classification in each year with a coarse group that is
#'     comparable across the 2000-2021 editions, and closure/merger fields.}
#'   \item{`ipeds_enrollment()`}{Fall enrollment (EF A): totals by level and
#'     attendance status, first-time students, and race/ethnicity by sex with
#'     the 2008-2010 change in categories harmonized. Graduate enrollment is
#'     derived as total minus undergraduate, so it includes first-professional
#'     students in years before they were reclassified as graduate students
#'     (2010).}
#'   \item{`ipeds_enrollment_12m()`}{12-month unduplicated headcount (EFFY) by
#'     level and race/ethnicity, and full-time-equivalent enrollment and
#'     credit hours (EFIA).}
#'   \item{`ipeds_retention()`}{Retention rates and student-to-faculty ratio
#'     (EF D).}
#'   \item{`ipeds_completions()`}{Awards conferred (C A), first majors only, in
#'     harmonized award levels (the 2010 retirement of first-professional and
#'     old doctor's categories and the 2020s split of short certificates are
#'     bridged) and bachelor's degrees by race/ethnicity and sex.}
#'   \item{`ipeds_admissions()`}{Applicants, admits, enrollees, admission and
#'     yield rates, SAT/ACT percentiles and test policy, read from IC through
#'     2013 and from ADM from 2014, with a flag for the 2016 SAT redesign.}
#'   \item{`ipeds_charges()`}{Published tuition and fees (IC AY).}
#'   \item{`ipeds_aid()`}{Pell and federal loan recipients, average grant aid
#'     and average net price (SFA).}
#'   \item{`ipeds_grad_rates()`}{150% graduation rates for the bachelor's
#'     cohort at 4-year institutions and the degree/certificate cohort at
#'     2-year institutions (GR), optionally by race/ethnicity and sex.}
#'   \item{`ipeds_finance()`}{Experimental: core revenue, expense and endowment
#'     measures from the GASB (F1A), FASB (F2) and for-profit (F3) forms.}
#'   \item{`ipeds_flags()`}{Parent/child reporting indicators (FLAGS).}
#' }
#'
#' @inheritParams ipeds_panel
#' @return A data frame with `unitid`, `year` and the module's variables, with
#'   a `"label"` attribute on every column and an `"ipeds_audit"` attribute
#'   (see [ipeds_audit()]). Use [ipeds_codebook()] for variable descriptions.
#' @name ipeds_modules
#' @examples
#' \dontrun{
#' dir <- ipeds_directory(2020:2023)
#' enr <- ipeds_enrollment(2004:2023)
#' ipeds_audit(enr, problems_only = TRUE)
#' }
NULL

.module_opts <- function(cache_dir, refresh, quiet, verify, revised, race_bridge = FALSE,
						 race_by_sex = TRUE) {
	.opts(cache_dir = cache_dir, refresh = refresh, quiet = quiet, verify = verify,
		  revised = revised, race_bridge = race_bridge, race_by_sex = race_by_sex)
}

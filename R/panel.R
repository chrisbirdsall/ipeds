# Harmonized institution-by-year panel ------------------------------------------------

.module_funs <- list(
	directory = .directory_year,
	enrollment = .enrollment_year,
	enrollment_12m = .enrollment_12m_year,
	retention = .retention_year,
	completions = .completions_year,
	admissions = .admissions_year,
	charges = .charges_year,
	aid = .aid_year,
	grad_rates = .grad_rates_year,
	finance = .finance_year,
	flags = .flags_year
)

.all_modules <- names(.module_funs)
.default_modules <- c("directory", "enrollment", "enrollment_12m", "retention", "completions",
					  "admissions", "charges", "aid", "grad_rates")

# Parent/child flag governing each module's data.
.child_flag <- c(enrollment = "prch_ef", enrollment_12m = "prch_e12", retention = "prch_ef",
				 completions = "prch_c", aid = "prch_sfa", grad_rates = "prch_gr",
				 finance = "prch_f", admissions = "prch_adm")

#' Build a harmonized, labelled IPEDS panel
#'
#' Downloads the IPEDS components needed for the requested modules and years,
#' harmonizes each measure across changes in IPEDS definitions, and merges
#' everything into one data frame with one row per institution (`unitid`)
#' and `year`. Every column carries a `"label"` attribute; see
#' [ipeds_codebook()] for labels, sources, reference periods and
#' harmonization notes, and [ipeds_audit()] for a record of which source
#' variable/code was used for each measure in each year and whether it was
#' verified against the official IPEDS dictionary for that file.
#'
#' **Year convention.** `year` is the year in (or implied by) the IPEDS file
#' name, so the panel row for 2023 combines fall 2023 directory and
#' enrollment data (HD2023, EF2023A) with 2022-23 completions (C2023_A),
#' 12-month enrollment (EFFY2023), financial aid (SFA2223) and fiscal year
#' 2023 finance (F2223). The `period` column of [ipeds_codebook()] documents
#' the reference period of every variable.
#'
#' **Harmonization.** Race/ethnicity is harmonized across the 2008-2010
#' change from 7 to 9 categories (see `race_bridge`); award levels across the
#' 2010 change in doctoral categories; admissions across the 2014 move from
#' IC to ADM; locale across the 2006 change in locale codes; Carnegie
#' classification into coarse groups comparable across editions. Graduate
#' enrollment is total minus undergraduate enrollment so it is not affected by
#' the reclassification of first-professional students. Breaks that cannot
#' be bridged (e.g. the 2016 SAT redesign) are flagged with indicator columns.
#'
#' **Newest data.** Revised (`_rv`) releases are used when NCES has published
#' them, and cached files are re-checked against the NCES server once they
#' are older than `getOption("ipeds.max_age", 30)` days, so provisional data
#' are replaced when final data are released. Use [ipeds_latest_year()] to
#' find the most recent year available.
#'
#' @param years the years to include, e.g. `2004:2023`.
#' @param modules which modules to include; any of `"directory"`,
#'   `"enrollment"`, `"enrollment_12m"`, `"retention"`, `"completions"`,
#'   `"admissions"`, `"charges"`, `"aid"`, `"grad_rates"`, `"finance"`
#'   (experimental) and `"flags"`. See [ipeds_modules] for their contents.
#' @param race_bridge also add `*_bridged` race columns in which students of
#'   two or more races (a category introduced with the 9-category standards)
#'   are allocated proportionally to the White, Black, American Indian/Alaska
#'   Native and Asian/Pacific Islander categories, approximating the
#'   7-category scheme.
#' @param race_by_sex include race/ethnicity-by-sex columns (e.g.
#'   `enr_black_women`) in addition to race/ethnicity totals.
#' @param grad_rates_by_race compute graduation rates by race/ethnicity and
#'   sex.
#' @param child_records how to treat records of institutions whose data are
#'   reported together with a parent institution: `"keep"` (default; these
#'   records typically contain zeros or missing values, and the `prch_*`
#'   flags are added when the `"flags"` module is included) or `"na"` (set the
#'   affected module's values to `NA`).
#' @param verify verify every source variable and code against the labels in
#'   the IPEDS dictionary for that file (recommended). Graduation rates
#'   require dictionaries.
#' @param revised prefer revised (`_rv`) releases.
#' @inheritParams ipeds_download
#' @return A data frame of class `ipeds_panel` with columns `unitid`, `year`
#'   and the harmonized variables of each module. Attributes `"ipeds_audit"`
#'   and `"ipeds_info"` hold the audit trail and build information.
#' @seealso [ipeds_codebook()], [ipeds_audit()], [ipeds_write()],
#'   [ipeds_real_dollars()], [ipeds_latest_year()]
#' @examples
#' \dontrun{
#' latest <- ipeds_latest_year("HD")
#' p <- ipeds_panel(years = 2010:latest)
#' ipeds_codebook(p)
#' ipeds_audit(p, problems_only = TRUE)
#' ipeds_write(p, "ipeds_panel.dta")
#' }
#' @export
ipeds_panel <- function(years, modules = .default_modules, race_bridge = FALSE, race_by_sex = TRUE,
						grad_rates_by_race = FALSE, child_records = c("keep", "na"), verify = TRUE,
						revised = TRUE, cache_dir = ipeds_cache_dir(), refresh = NULL, quiet = FALSE) {
	years <- .check_years(years)
	modules <- unique(match.arg(modules, .all_modules, several.ok = TRUE))
	child_records <- match.arg(child_records)
	if (min(years) < 2000) {
		warning("Harmonized modules are designed for 2000 onward; earlier files use different ",
				"layouts. Use ipeds_read_file() for raw access to older files.", call. = FALSE)
	}
	opts <- .opts(cache_dir = cache_dir, refresh = refresh, quiet = quiet, verify = verify,
				  revised = revised, race_bridge = race_bridge, race_by_sex = race_by_sex)
	run <- modules
	if (child_records == "na" && !"flags" %in% run) run <- c(run, "flags")
	parts <- list()
	for (m in run) {
		.msg("== ", m, " ==", quiet = quiet)
		fun <- .module_funs[[m]]
		parts[[m]] <- if (m == "grad_rates") {
			.run_module(m, years, opts, fun, by_race = grad_rates_by_race)
		} else {
			.run_module(m, years, opts, fun)
		}
	}
	if (child_records == "na") {
		fl <- parts[["flags"]]
		for (m in intersect(names(.child_flag), modules)) {
			flag <- paste0(.child_flag[[m]], "_child")
			d <- parts[[m]]
			if (is.null(fl) || !flag %in% names(fl) || is.null(d) || nrow(d) == 0) next
			key <- paste(d$unitid, d$year)
			child <- key %in% paste(fl$unitid, fl$year)[fl[[flag]] %in% TRUE]
			cols <- setdiff(names(d), c("unitid", "year"))
			for (cn in cols) d[[cn]][child] <- NA
			parts[[m]] <- d
			.audit(list(audit = opts$audit, module = m), "child records", flag, "note",
				   sprintf("%d institution-years set to NA", sum(child)))
		}
		if (!"flags" %in% modules) parts[["flags"]] <- NULL
	}
	parts <- Filter(function(d) !is.null(d) && nrow(d) > 0, parts)
	if (length(parts) == 0) stop("No data could be loaded; see warnings.", call. = FALSE)
	panel <- Reduce(function(a, b) merge(a, b, by = c("unitid", "year"), all = TRUE, sort = FALSE),
					parts)
	panel <- panel[order(panel$unitid, panel$year), , drop = FALSE]
	rownames(panel) <- NULL
	panel <- .apply_codebook(panel)
	attr(panel, "ipeds_audit") <- .audit_table(opts$audit)
	attr(panel, "ipeds_info") <- list(
		years = years, modules = modules, race_bridge = race_bridge,
		child_records = child_records, verify = verify, revised = revised,
		created = Sys.time(), package_version = as.character(utils::packageVersion("ipeds")))
	class(panel) <- c("ipeds_panel", "data.frame")
	panel
}

#' @export
print.ipeds_panel <- function(x, ...) {
	info <- attr(x, "ipeds_info")
	a <- attr(x, "ipeds_audit")
	cat("<ipeds_panel>", format(nrow(x), big.mark = ","), "institution-years,",
		length(unique(x$unitid)), "institutions,", ncol(x), "variables\n")
	if (!is.null(info)) {
		cat("Years:", .year_ranges(info$years), "  Modules:", paste(info$modules, collapse = ", "), "\n")
	}
	if (!is.null(a)) {
		probs <- sum(a$status %in% .audit_problem_status)
		cat("Audit:", sum(a$status == "verified"), "verified,", sum(a$status == "relabeled"),
			"relabeled,", sum(a$status == "unverified"), "unverified,", probs, "problems",
			"(see ipeds_audit())\n")
	}
	cat("Codebook: ipeds_codebook(x)\n\n")
	y <- x
	class(y) <- "data.frame"
	print(utils::head(y[, seq_len(min(8, ncol(y))), drop = FALSE], 6), row.names = FALSE)
	invisible(x)
}

#' Check harmonization against live IPEDS files
#'
#' Builds the requested modules and summarizes the audit trail by module and
#' concept: for how many years each measure was verified against the IPEDS
#' dictionary, taken from an unverified source, found under a different
#' variable/code than expected ("relabeled"), or could not be produced. Run
#' this after new IPEDS releases to confirm that every harmonized measure is
#' still picked up correctly.
#'
#' @inheritParams ipeds_panel
#' @return A data frame with one row per module and concept and counts of
#'   years by audit status, plus the distinct sources used.
#' @examples
#' \dontrun{
#' v <- ipeds_validate(2004:2023)
#' subset(v, problem > 0 | relabeled > 0)
#' }
#' @export
ipeds_validate <- function(years, modules = .all_modules, cache_dir = ipeds_cache_dir(),
						   refresh = NULL, quiet = TRUE) {
	p <- ipeds_panel(years, modules = modules, verify = TRUE, cache_dir = cache_dir,
					 refresh = refresh, quiet = quiet)
	a <- ipeds_audit(p)
	a <- a[!a$status %in% c("loaded", "note", "derived"), , drop = FALSE]
	key <- paste(a$module, a$concept, sep = "\r")
	out <- do.call(rbind, lapply(split(a, factor(key, levels = unique(key))), function(d) {
		data.frame(module = d$module[1], concept = d$concept[1],
				   years = .year_ranges(sort(unique(d$year))),
				   verified = sum(d$status %in% c("verified", "check_passed")),
				   unverified = sum(d$status == "unverified"),
				   relabeled = sum(d$status == "relabeled"),
				   problem = sum(d$status %in% .audit_problem_status),
				   sources = paste(unique(d$source[nzchar(d$source)]), collapse = " | "),
				   stringsAsFactors = FALSE)
	}))
	rownames(out) <- NULL
	out
}

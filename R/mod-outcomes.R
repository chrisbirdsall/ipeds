# Graduation rates (GR) --------------------------------------------------------------
#
# GR files have one row per institution and cohort/status combination. The
# codes of the cohort, status (chrtstat) and combined (grtype) variables have
# changed over the years, so rows are selected by their value labels in the
# dictionary of each file rather than by fixed codes. Graduation rates are
# therefore only computed when a dictionary is available (verify = TRUE).

.gr_specs <- list(
	bach = .spec("bachelor", c("other", "\\+")),
	two = .spec(c("degree|certif", "2-yr|2 yr|2-year|two-year|less than 4|less-than-4"), "bachelor"),
	adjusted = .spec("adjusted cohort", c("completer", "transfer", "still enrolled", "no longer")),
	comp150 = .spec("completers within 150",
					c("completers of", "less than 2", "2 but less", "4 years", "5 years", "6 years",
					  "100%", "200%"))
)

.gr_rows <- function(df, ctx, concept, cohort_spec, status_spec) {
	nms <- names(df)
	if (all(c("cohort", "chrtstat") %in% nms) &&
		nrow(.dict_values(ctx$dict, "cohort")) > 0 && nrow(.dict_values(ctx$dict, "chrtstat")) > 0) {
		cc <- .pick_code(df, "cohort", ctx, paste0(concept, " (cohort)"), numeric(), cohort_spec)
		sc <- .pick_code(df, "chrtstat", ctx, paste0(concept, " (status)"), numeric(), status_spec)
		if (!is.na(cc) && !is.na(sc)) {
			return(which(.as_num(df$cohort) == cc & .as_num(df$chrtstat) == sc))
		}
	}
	if ("grtype" %in% nms) {
		code <- .pick_code(df, "grtype", ctx, concept, numeric(),
						   .spec(c(cohort_spec$all, status_spec$all), c(cohort_spec$none, status_spec$none)))
		if (!is.na(code)) return(which(.as_num(df$grtype) == code))
	}
	integer()
}

.first_rows <- function(unitid, rows, vals, unitids) {
	u <- unitid[rows]
	vals[rows[match(unitids, u)], , drop = FALSE]
}

.grad_rates_year <- function(year, opts, by_race = FALSE) {
	x <- .load("GR", year, opts, "grad_rates")
	if (is.null(x)) return(NULL)
	df <- x$df
	ctx <- x$ctx
	h <- .harmonize_race(df, ctx, .race_cfg$GR, "g", sex = FALSE, bridge = FALSE)
	res <- data.frame(unitid = unique(df$unitid))
	keys <- c("total", "men", "women", if (by_race) .race_keys)
	for (co in c("bach", "2yr")) {
		cspec <- if (co == "bach") .gr_specs$bach else .gr_specs$two
		lab <- if (co == "bach") "bachelor's cohort" else "2-year cohort"
		ra <- .gr_rows(df, ctx, paste0(lab, ": adjusted cohort"), cspec, .gr_specs$adjusted)
		rc <- .gr_rows(df, ctx, paste0(lab, ": completers within 150%"), cspec, .gr_specs$comp150)
		if (length(ra) == 0 || length(rc) == 0) next
		coh <- .first_rows(df$unitid, ra, h, res$unitid)
		com <- .first_rows(df$unitid, rc, h, res$unitid)
		pre <- paste0("gr_", co)
		res[[paste0(pre, "_cohort")]] <- coh$g_total
		res[[paste0(pre, "_completers_150")]] <- com$g_total
		for (k in keys) {
			r <- .safe_div(com[[paste0("g_", k)]], coh[[paste0("g_", k)]])
			bad <- !is.na(r) & r > 1
			if (any(bad) && k == "total") {
				.audit(ctx, paste(lab, "completers <= cohort"), NA, "note",
					   sprintf("%d institutions with completers > cohort set to NA", sum(bad)))
			}
			r[bad] <- NA_real_
			res[[paste0(pre, "_rate_150", if (k != "total") paste0("_", k))]] <- r
		}
		if (co == "bach") res$gr_race_scheme <- coh$g_race_scheme
	}
	if (ncol(res) == 1) return(NULL)
	res
}

#' @rdname ipeds_modules
#' @param by_race for `ipeds_grad_rates()`, also compute graduation rates by
#'   harmonized race/ethnicity.
#' @export
ipeds_grad_rates <- function(years, by_race = FALSE, verify = TRUE, revised = TRUE,
							 cache_dir = ipeds_cache_dir(), refresh = NULL, quiet = FALSE) {
	opts <- .module_opts(cache_dir, refresh, quiet, verify, revised)
	.run_module("grad_rates", years, opts, .grad_rates_year, by_race = by_race)
}

# Finance (F1A, F2, F3) -- experimental ------------------------------------------------

.instr_none <- c("salar", "wage", "benefit", "depreciation", "interest", "maintenance",
				 "other", "academic", "prior")

.fin_forms <- list(
	F_F1A = list(form = "GASB", measures = list(
		rev_total = list(c("f1d01", "f1b25"), .spec(c("total", "revenue"), c("\\boperating", "nonoperating", "capital"))),
		rev_tuition = list("f1b01", .spec("tuition")),
		rev_state_approp = list("f1b11", .spec("state appropriation", "capital")),
		exp_total = list(c("f1d02", "f1c191"), .spec(c("total", "expense"), c("\\boperating", "nonoperating"))),
		exp_instruction = list("f1c011", .spec("instruction", .instr_none)),
		endowment_eoy = list("f1h02", .spec(c("endowment", "end of")))
	)),
	F_F2 = list(form = "FASB", measures = list(
		rev_total = list("f2d16", .spec(c("total", "revenue"))),
		rev_tuition = list("f2d01", .spec("tuition")),
		rev_state_approp = list("f2d03", .spec("state appropriation")),
		exp_total = list(c("f2e131", "f2b02"), .spec(c("total", "expense"))),
		exp_instruction = list("f2e011", .spec("instruction", .instr_none)),
		endowment_eoy = list("f2h02", .spec(c("endowment", "end of")))
	)),
	F_F3 = list(form = "For-profit", measures = list(
		rev_total = list(c("f3d09", "f3d16"), .spec(c("total", "revenue"))),
		rev_tuition = list("f3d01", .spec("tuition")),
		exp_total = list(c("f3e07", "f3e08"), .spec(c("total", "expense"))),
		exp_instruction = list("f3e011", .spec("instruction", .instr_none))
	))
)

.finance_year <- function(year, opts) {
	parts <- lapply(names(.fin_forms), function(comp) {
		f <- .fin_forms[[comp]]
		x <- .load(comp, year, opts, "finance")
		if (is.null(x)) return(NULL)
		df <- .one_per_unitid(x$df, x$ctx, "unitid")
		res <- data.frame(unitid = df$unitid, fin_form = rep(f$form, nrow(df)),
						  stringsAsFactors = FALSE)
		for (m in names(f$measures)) {
			spec <- f$measures[[m]]
			v <- .money(.get_var(df, x$ctx, m, spec[[1]], spec[[2]], search = TRUE))
			if (!is.null(v)) res[[m]] <- v
		}
		res
	})
	out <- .rbind_fill(parts)
	if (nrow(out) == 0) return(NULL)
	dup <- duplicated(out$unitid)
	if (any(dup)) {
		.audit(list(audit = opts$audit, module = "finance", year = year), "one form per institution",
			   NA, "note", sprintf("%d institutions reported on more than one form; first kept", sum(dup)))
		out <- out[!dup, , drop = FALSE]
	}
	out
}

#' @rdname ipeds_modules
#' @export
ipeds_finance <- function(years, verify = TRUE, revised = TRUE, cache_dir = ipeds_cache_dir(),
						  refresh = NULL, quiet = FALSE) {
	opts <- .module_opts(cache_dir, refresh, quiet, verify, revised)
	.run_module("finance", years, opts, .finance_year)
}

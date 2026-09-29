# Admissions (IC through 2013, ADM from 2014), charges (IC AY), aid (SFA) ----------

.test_policy_levels <- c("Required", "Recommended", "Considered but not required",
						 "Not required or considered")

# Canonical test-score policy from the value label text (codes and wording
# changed over time, e.g. "Neither required nor recommended" became
# "Not considered" and "Considered but not required" was added).
.test_policy <- function(code, dict, var) {
	code <- .as_num(code)
	vl <- .dict_values(dict, var)
	lab <- if (nrow(vl)) vl$label[match(code, .as_num(vl$code))] else {
		c("1" = "Required", "2" = "Recommended",
		  "3" = "Neither required nor recommended")[as.character(code)]
	}
	l <- .norm_label(lab)
	out <- rep(NA_character_, length(code))
	out[grepl("considered but not required", l)] <- "Considered but not required"
	out[is.na(out) & grepl("neither|not considered|not required", l)] <- "Not required or considered"
	out[is.na(out) & grepl("^required", l)] <- "Required"
	out[is.na(out) & grepl("^recommended", l)] <- "Recommended"
	factor(out, levels = .test_policy_levels)
}

.admissions_year <- function(year, opts) {
	comp <- if (year >= 2014) "ADM" else "IC"
	x <- .load(comp, year, opts, "admissions")
	if (is.null(x)) return(NULL)
	df <- .one_per_unitid(x$df, x$ctx, "unitid")
	ctx <- x$ctx
	n <- nrow(df)
	get <- function(concept, cands, spec) .get_var(df, ctx, concept, cands, spec, search = FALSE)
	sexsum <- function(concept, total, men, women, vars) {
		if (is.null(total) && !(is.null(men) && is.null(women))) {
			.audit(ctx, concept, vars, "derived", "sum of men and women")
			total <- .rsum(list(men, women), n)
		}
		total
	}
	res <- data.frame(unitid = df$unitid)
	put <- function(name, v) if (!is.null(v)) res[[name]] <<- v

	app_m <- get("applicants, men", "applcnm", .spec(c("applic", "\\bmen\\b")))
	app_w <- get("applicants, women", "applcnw", .spec(c("applic", "women")))
	app <- sexsum("applicants", get("applicants", "applcn", .spec("applic", c("\\bmen\\b", "women", "fee"))),
				  app_m, app_w, c("applcnm", "applcnw"))
	adm_m <- get("admits, men", "admssnm", .spec(c("admi", "\\bmen\\b")))
	adm_w <- get("admits, women", "admssnw", .spec(c("admi", "women")))
	adm <- sexsum("admits", get("admits", "admssn", .spec("admi", c("\\bmen\\b", "women", "polic", "open", "consider", "test"))),
				  adm_m, adm_w, c("admssnm", "admssnw"))
	enr_m <- get("enrolled, men", "enrlm", .spec(c("enrol", "\\bmen\\b"), c("full-time", "part-time")))
	enr_w <- get("enrolled, women", "enrlw", .spec(c("enrol", "women"), c("full-time", "part-time")))
	enr <- sexsum("enrolled", get("enrolled", "enrlt", .spec("enrol", c("\\bmen\\b", "women", "full-time", "part-time"))),
				  enr_m, enr_w, c("enrlm", "enrlw"))
	put("applicants", app)
	put("applicants_men", app_m)
	put("applicants_women", app_w)
	put("admits", adm)
	put("admits_men", adm_m)
	put("admits_women", adm_w)
	put("enrolled", enr)
	put("enrolled_ft", get("enrolled full-time", "enrlft", .spec(c("enrol", "full-time"), c("\\bmen\\b", "women"))))
	if (!is.null(app) && !is.null(adm)) {
		r <- .safe_div(adm, app)
		r[!is.na(r) & r > 1] <- NA_real_
		res$admit_rate <- r
	}
	if (!is.null(adm) && !is.null(enr)) {
		r <- .safe_div(enr, adm)
		r[!is.na(r) & r > 1] <- NA_real_
		res$yield_rate <- r
	}
	put("sat_submit_pct", get("percent submitting SAT", "satpct", .spec(c("\\bsat\\b", "percent"))))
	put("act_submit_pct", get("percent submitting ACT", "actpct", .spec(c("\\bact\\b", "percent"))))
	put("sat_rw_25", get("SAT reading/writing 25th percentile", "satvr25", .spec(c("\\bsat\\b", "25"), c("math", "\\bact\\b"))))
	put("sat_rw_75", get("SAT reading/writing 75th percentile", "satvr75", .spec(c("\\bsat\\b", "75"), c("math", "\\bact\\b"))))
	put("sat_math_25", get("SAT math 25th percentile", "satmt25", .spec(c("\\bsat\\b", "math", "25"))))
	put("sat_math_75", get("SAT math 75th percentile", "satmt75", .spec(c("\\bsat\\b", "math", "75"))))
	put("act_comp_25", get("ACT composite 25th percentile", "actcm25", .spec(c("\\bact\\b", "composite", "25"))))
	put("act_comp_75", get("ACT composite 75th percentile", "actcm75", .spec(c("\\bact\\b", "composite", "75"))))
	sat_cols <- intersect(c("sat_rw_25", "sat_rw_75", "sat_math_25", "sat_math_75"), names(res))
	if (length(sat_cols)) {
		has_sat <- rowSums(!is.na(res[, sat_cols, drop = FALSE])) > 0
		res$sat_version <- ifelse(has_sat, if (year <= 2015) "Pre-2016 SAT" else if (year == 2016)
			"Transition (old and redesigned SAT)" else "Redesigned SAT (2016)", NA_character_)
	}
	tv <- .pick_var(df, ctx, "test score policy", "admcon7", .spec("test"), search = FALSE)
	if (!is.na(tv)) res$test_policy <- .test_policy(df[[tv]], ctx$dict, tv)

	# Open admission policy is always reported in IC
	ic <- if (comp == "IC") x else .load("IC", year, opts, "admissions",
										 select = function(h) h %in% c("unitid", "openadmp"))
	if (!is.null(ic)) {
		d2 <- .one_per_unitid(ic$df, ic$ctx, "unitid")
		oa <- .get_var(d2, ic$ctx, "open admission policy", "openadmp", .spec("open admission"),
					   search = FALSE)
		if (!is.null(oa)) {
			oa <- .yes_no(oa)
			res <- merge(res, data.frame(unitid = d2$unitid, open_admission = oa), by = "unitid",
						 all = TRUE)
		}
	}
	if (ncol(res) == 1) return(NULL)
	res
}

#' @rdname ipeds_modules
#' @export
ipeds_admissions <- function(years, verify = TRUE, revised = TRUE, cache_dir = ipeds_cache_dir(),
							 refresh = NULL, quiet = FALSE) {
	opts <- .module_opts(cache_dir, refresh, quiet, verify, revised)
	.run_module("admissions", years, opts, .admissions_year)
}

# Student charges (IC AY) ---------------------------------------------------------

.money <- function(x) {
	if (is.null(x)) return(NULL)
	x[!is.na(x) & x < 0] <- NA_real_
	x
}

.charges_year <- function(year, opts) {
	x <- .load("IC_AY", year, opts, "charges")
	if (is.null(x)) return(NULL)
	df <- .one_per_unitid(x$df, x$ctx, "unitid")
	ctx <- x$ctx
	n <- nrow(df)
	get <- function(concept, cands, spec) .money(.get_var(df, ctx, concept, cands, spec, search = FALSE))
	res <- data.frame(unitid = df$unitid)
	pairs <- list(
		ug_indistrict = list(t = "tuition1", f = "fee1", res = "in-district", lvl = "undergraduate"),
		ug_instate = list(t = "tuition2", f = "fee2", res = "in-state", lvl = "undergraduate"),
		ug_outstate = list(t = "tuition3", f = "fee3", res = "out-of-state", lvl = "undergraduate"),
		grad_instate = list(t = "tuition6", f = "fee6", res = "in-state", lvl = "graduate"),
		grad_outstate = list(t = "tuition7", f = "fee7", res = "out-of-state", lvl = "graduate"))
	for (k in names(pairs)) {
		p <- pairs[[k]]
		lvl_spec <- if (p$lvl == "graduate") .spec("\\bgraduate") else .spec(character(), "\\bgraduate")
		t <- get(paste(p$res, "tuition,", p$lvl), p$t,
				 .spec(c("tuition", p$res, lvl_spec$all), lvl_spec$none))
		f <- get(paste(p$res, "fees,", p$lvl), p$f,
				 .spec(c("fee", p$res, lvl_spec$all), lvl_spec$none))
		if (!is.null(t)) res[[paste0("tuition_", k)]] <- t
		if (!is.null(f)) res[[paste0("fees_", k)]] <- f
		if (!is.null(t)) res[[paste0("tuitfee_", k)]] <- .rsum(list(t, f), n)
	}
	rb <- get("on-campus room and board", "chg5ay3", .spec(c("room", "board"), "off"))
	if (!is.null(rb)) res$room_board_oncampus <- rb
	bk <- get("books and supplies", "chg4ay3", .spec("book"))
	if (!is.null(bk)) res$books_supplies <- bk
	if (ncol(res) == 1) return(NULL)
	res
}

#' @rdname ipeds_modules
#' @export
ipeds_charges <- function(years, verify = TRUE, revised = TRUE, cache_dir = ipeds_cache_dir(),
						  refresh = NULL, quiet = FALSE) {
	opts <- .module_opts(cache_dir, refresh, quiet, verify, revised)
	.run_module("charges", years, opts, .charges_year)
}

# Student financial aid and net price (SFA) -----------------------------------------

.aid_year <- function(year, opts) {
	x <- .load("SFA", year, opts, "aid")
	if (is.null(x)) return(NULL)
	df <- .one_per_unitid(x$df, x$ctx, "unitid")
	ctx <- x$ctx
	get <- function(concept, cands, spec) .get_var(df, ctx, concept, cands, spec, search = FALSE)
	pct <- function(concept, cands, spec) .pct_range(get(concept, cands, spec), ctx, concept)
	res <- data.frame(unitid = df$unitid)
	put <- function(name, v) if (!is.null(v)) res[[name]] <<- v
	put("pell_pct_ug", pct("percent of undergraduates with Pell grants", "upgrntp",
						   .spec(c("pell", "percent"), c("first.time", "full-time"))))
	put("pell_pct_ftft", pct("percent of full-time first-time students with Pell grants", "pgrnt_p",
							 .spec(c("pell", "percent", "first.time"))))
	put("fedloan_pct_ug", pct("percent of undergraduates with federal loans", "ufloanp",
							  .spec(c("federal", "loan", "percent"), c("first.time", "full-time"))))
	put("fedloan_pct_ftft", pct("percent of full-time first-time students with federal loans", "floan_p",
								.spec(c("federal", "loan", "percent", "first.time"))))
	put("grant_avg_ftft", .money(get("average grant aid, full-time first-time", "agrnt_a",
									 .spec(c("average", "grant|scholarship", "first.time"),
									 	  c("pell", "^average amount of (federal|state|local|institutional|other) grant")))))
	put("inst_grant_avg_ftft", .money(get("average institutional grant, full-time first-time", "igrnt_a",
										  .spec(c("average", "institutional", "grant")))))
	pub <- .money(get("average net price (public)", "npist2", .spec("net price", "income")))
	priv <- .money(get("average net price (private)", "npgrn2", .spec("net price", "income")))
	put("net_price", .coalesce(pub, priv))
	pub <- .money(get("net price, income $0-30,000 (public)", "npis412", .spec(c("net price", "0.{0,4}30"))))
	priv <- .money(get("net price, income $0-30,000 (private)", "npt412", .spec(c("net price", "0.{0,4}30"))))
	put("net_price_inc_0_30k", .coalesce(pub, priv))
	if (ncol(res) == 1) return(NULL)
	res
}

#' @rdname ipeds_modules
#' @export
ipeds_aid <- function(years, verify = TRUE, revised = TRUE, cache_dir = ipeds_cache_dir(),
					  refresh = NULL, quiet = FALSE) {
	opts <- .module_opts(cache_dir, refresh, quiet, verify, revised)
	.run_module("aid", years, opts, .aid_year)
}

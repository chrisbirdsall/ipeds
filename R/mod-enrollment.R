# Fall enrollment (EF A), 12-month enrollment (EFFY, EFIA), retention (EF D) ------

.efa_levels <- list(
	total = list(code = 1, spec = .spec("^all students",
		c("undergrad", "\\bgraduate", "first", "degree", "professional", "full-time", "part-time"))),
	ug = list(code = 2, spec = .spec(c("^all students", "undergrad"),
		c("degree", "first", "transfer", "continuing", "\\bgraduate"))),
	ft = list(code = 21, spec = .spec("^full-time",
		c("undergrad", "\\bgraduate", "first", "degree", "professional"))),
	ug_ft = list(code = 22, spec = .spec(c("^full-time", "undergrad"),
		c("degree", "first", "transfer", "continuing", "\\bgraduate"))),
	pt = list(code = 41, spec = .spec("^part-time",
		c("undergrad", "\\bgraduate", "first", "degree", "professional"))),
	ug_pt = list(code = 42, spec = .spec(c("^part-time", "undergrad"),
		c("degree", "first", "transfer", "continuing", "\\bgraduate"))),
	ug_firsttime = list(code = 4, spec = .spec(c("^all students", "first.time"),
		c("\\bgraduate", "non-degree"))),
	ug_ft_firsttime = list(code = 24, spec = .spec(c("^full-time", "first.time"),
		c("\\bgraduate", "non-degree")))
)

# Values of `vals` (data frame) for the rows of `df` at level code `code`,
# aligned to `unitids` (first row per institution).
.at_level <- function(df, lvl, code, vals, unitids, ctx, concept) {
	rows <- which(lvl == code)
	u <- df$unitid[rows]
	if (anyDuplicated(u)) {
		.audit(ctx, concept, NA, "note",
			   sprintf("%d institutions have more than one row; the first is used",
			   		length(unique(u[duplicated(u)]))))
	}
	idx <- rows[match(unitids, u)]
	vals[idx, , drop = FALSE]
}

# Apply an identity check; when more than 10% of institutions fail it, the
# columns in `cols` are set to NA.
.identity_check <- function(res, ok, cols, ctx, concept) {
	ok <- ok[!is.na(ok)]
	if (length(ok) == 0) return(res)
	fail <- mean(!ok)
	cols <- intersect(cols, names(res))
	if (fail > 0.10) {
		.audit(ctx, concept, NA, "check_failed",
			   c(sprintf("fails for %.0f%% of institutions", 100 * fail),
			     paste("set to NA:", paste(cols, collapse = ", "))))
		for (cn in cols) res[[cn]] <- NA_real_
	} else {
		.audit(ctx, concept, NA, "check_passed",
			   sprintf("holds for %.1f%% of institutions", 100 * (1 - fail)))
	}
	res
}

.nonneg <- function(x) {
	x[!is.na(x) & x < 0] <- NA_real_
	x
}

.enrollment_year <- function(year, opts) {
	x <- .load("EFA", year, opts, "enrollment")
	if (is.null(x)) return(NULL)
	df <- x$df
	ctx <- x$ctx
	lv <- .pick_var(df, ctx, "student level variable", "efalevel", .spec("level"), search = FALSE)
	if (is.na(lv)) return(NULL)
	lvl <- .as_num(df[[lv]])
	h <- .harmonize_race(df, ctx, .race_cfg$EFA, "enr", sex = opts$race_by_sex,
						 bridge = opts$race_bridge)
	codes <- vapply(names(.efa_levels), function(k) {
		.pick_code(df, lv, ctx, paste("student level:", k), .efa_levels[[k]]$code,
				   .efa_levels[[k]]$spec)
	}, numeric(1))
	res <- data.frame(unitid = unique(df$unitid))
	for (k in names(.efa_levels)) {
		if (is.na(codes[[k]])) next
		v <- .at_level(df, lvl, codes[[k]], h[, "enr_total", drop = FALSE], res$unitid, ctx,
					   paste("student level:", k))
		res[[if (k == "total") "enr_total" else paste0("enr_", k)]] <- v$enr_total
	}
	race_cols <- setdiff(names(h), c("enr_total", "enr_race_scheme"))
	for (k in c("total", "ug")) {
		if (is.na(codes[[k]])) next
		v <- .at_level(df, lvl, codes[[k]], h, res$unitid, ctx, paste("student level:", k))
		out_prefix <- if (k == "total") "enr" else "enr_ug"
		for (cn in race_cols) res[[sub("^enr", out_prefix, cn)]] <- v[[cn]]
		if (k == "total") res$enr_race_scheme <- v$enr_race_scheme
	}
	if (all(c("enr_total", "enr_ug") %in% names(res))) {
		res$enr_grad <- .nonneg(res$enr_total - res$enr_ug)
	}
	if (all(c("enr_ft", "enr_ug_ft") %in% names(res))) {
		res$enr_grad_ft <- .nonneg(res$enr_ft - res$enr_ug_ft)
	}
	if (all(c("enr_pt", "enr_ug_pt") %in% names(res))) {
		res$enr_grad_pt <- .nonneg(res$enr_pt - res$enr_ug_pt)
	}
	# Consistency checks
	if (all(c("enr_total", "enr_ft", "enr_pt") %in% names(res))) {
		ok <- abs(res$enr_ft + res$enr_pt - res$enr_total) <= 1
		res <- .identity_check(res, ok, c("enr_ft", "enr_pt", "enr_ug_ft", "enr_ug_pt",
										  "enr_grad_ft", "enr_grad_pt"),
							   ctx, "full-time + part-time = total")
	}
	if (all(c("enr_total", "enr_ug") %in% names(res))) {
		ok <- res$enr_ug <= res$enr_total + 1
		res <- .identity_check(res, ok, grep("^enr_(ug|grad)", names(res), value = TRUE), ctx,
							   "undergraduate <= total")
	}
	if (all(c("enr_total", "enr_men", "enr_women") %in% names(res))) {
		ok <- abs(res$enr_men + res$enr_women - res$enr_total) <= 1
		res <- .identity_check(res, ok, c("enr_men", "enr_women"), ctx, "men + women = total")
	}
	res
}

#' @rdname ipeds_modules
#' @export
ipeds_enrollment <- function(years, race_bridge = FALSE, race_by_sex = TRUE, verify = TRUE,
							 revised = TRUE, cache_dir = ipeds_cache_dir(), refresh = NULL,
							 quiet = FALSE) {
	opts <- .module_opts(cache_dir, refresh, quiet, verify, revised, race_bridge, race_by_sex)
	.run_module("enrollment", years, opts, .enrollment_year)
}

# 12-month enrollment -------------------------------------------------------------

.effy_levels <- list(
	total = list(code = c(1, 999), spec = .spec("total|all students",
		c("undergrad", "\\bgraduate", "professional", "degree", "first"))),
	ug = list(code = c(2, 1), spec = .spec("undergrad",
		c("degree", "first", "\\bgraduate", "non")))
)

.enrollment_12m_year <- function(year, opts) {
	res <- NULL
	x <- .load("EFFY", year, opts, "enrollment_12m")
	if (!is.null(x)) {
		df <- x$df
		ctx <- x$ctx
		lv <- .pick_var(df, ctx, "student level variable", c("effyalev", "effylev", "lstudy"),
						.spec("level"), search = FALSE)
		if (!is.na(lv)) {
			lvl <- .as_num(df[[lv]])
			h <- .harmonize_race(df, ctx, .race_cfg$EFFY, "e12", sex = opts$race_by_sex,
								 bridge = opts$race_bridge)
			codes <- vapply(names(.effy_levels), function(k) {
				.pick_code(df, lv, ctx, paste("student level:", k), .effy_levels[[k]]$code,
						   .effy_levels[[k]]$spec)
			}, numeric(1))
			res <- data.frame(unitid = unique(df$unitid))
			if (!is.na(codes[["total"]])) {
				v <- .at_level(df, lvl, codes[["total"]], h, res$unitid, ctx, "student level: total")
				for (cn in names(h)) res[[cn]] <- v[[cn]]
			}
			if (!is.na(codes[["ug"]])) {
				v <- .at_level(df, lvl, codes[["ug"]], h[, "e12_total", drop = FALSE], res$unitid,
							   ctx, "student level: ug")
				res$e12_ug <- v$e12_total
			}
			if (all(c("e12_total", "e12_ug") %in% names(res))) {
				res$e12_grad <- .nonneg(res$e12_total - res$e12_ug)
				res <- .identity_check(res, res$e12_ug <= res$e12_total + 1,
									   c("e12_ug", "e12_grad"), ctx, "undergraduate <= total")
			}
		}
	}
	y <- .load("EFIA", year, opts, "enrollment_12m")
	if (!is.null(y)) {
		df <- .one_per_unitid(y$df, y$ctx, "unitid")
		ctx <- y$ctx
		get <- function(concept, cands, spec) .get_var(df, ctx, concept, cands, spec)
		fte <- data.frame(unitid = df$unitid)
		fte_ug <- get("FTE undergraduate", c("fteug", "efteug"),
					  .spec(c("fte|full-time equivalent", "undergrad")))
		fte_gr <- get("FTE graduate", c("ftegd", "eftegd"),
					  .spec(c("fte|full-time equivalent", "\\bgraduate"), c("undergrad", "professional")))
		fte_dpp <- get("FTE doctor's professional practice", c("ftedpp", "eftedpp"),
					   .spec(c("fte|full-time equivalent", "professional")))
		if (!is.null(fte_ug)) fte$fte_ug <- fte_ug
		if (!is.null(fte_gr)) fte$fte_grad <- fte_gr
		if (!is.null(fte_dpp)) fte$fte_dpp <- fte_dpp
		if (ncol(fte) > 1) fte$fte_total <- .rsum(list(fte_ug, fte_gr, fte_dpp), nrow(fte))
		ch <- get("undergraduate credit hours", "cdactua", .spec(c("credit", "undergrad")))
		if (!is.null(ch)) fte$credit_hours_ug <- ch
		cl <- get("undergraduate clock hours", "cnactua", .spec("clock|contact"))
		if (!is.null(cl)) fte$clock_hours_ug <- cl
		cg <- get("graduate credit hours", c("cdactga", "cddacta"),
				  .spec(c("credit", "\\bgraduate"), "undergrad"))
		if (!is.null(cg)) fte$credit_hours_grad <- cg
		res <- if (is.null(res)) fte else merge(res, fte, by = "unitid", all = TRUE)
	}
	res
}

#' @rdname ipeds_modules
#' @export
ipeds_enrollment_12m <- function(years, race_bridge = FALSE, race_by_sex = TRUE, verify = TRUE,
								 revised = TRUE, cache_dir = ipeds_cache_dir(), refresh = NULL,
								 quiet = FALSE) {
	opts <- .module_opts(cache_dir, refresh, quiet, verify, revised, race_bridge, race_by_sex)
	.run_module("enrollment_12m", years, opts, .enrollment_12m_year)
}

# Retention (EF D) ------------------------------------------------------------------

.pct_range <- function(x, ctx, concept) {
	if (is.null(x)) return(NULL)
	bad <- !is.na(x) & (x < 0 | x > 100)
	if (any(bad)) {
		.audit(ctx, concept, NA, "note", sprintf("%d values outside 0-100 set to NA", sum(bad)))
		x[bad] <- NA_real_
	}
	x
}

.retention_year <- function(year, opts) {
	x <- .load("EFD", year, opts, "retention")
	if (is.null(x)) return(NULL)
	df <- .one_per_unitid(x$df, x$ctx, "unitid")
	ctx <- x$ctx
	res <- data.frame(unitid = df$unitid)
	v <- .pct_range(.get_var(df, ctx, "full-time retention rate", "ret_pcf",
							 .spec(c("retention", "full-time"))), ctx, "full-time retention rate")
	if (!is.null(v)) res$ret_ft_pct <- v
	v <- .pct_range(.get_var(df, ctx, "part-time retention rate", "ret_pcp",
							 .spec(c("retention", "part-time"))), ctx, "part-time retention rate")
	if (!is.null(v)) res$ret_pt_pct <- v
	v <- .get_var(df, ctx, "student-to-faculty ratio", "stufacr",
				  .spec("student.{0,6}faculty"))
	if (!is.null(v)) res$stu_fac_ratio <- v
	if (ncol(res) == 1) return(NULL)
	res
}

#' @rdname ipeds_modules
#' @export
ipeds_retention <- function(years, verify = TRUE, revised = TRUE, cache_dir = ipeds_cache_dir(),
							refresh = NULL, quiet = FALSE) {
	opts <- .module_opts(cache_dir, refresh, quiet, verify, revised)
	.run_module("retention", years, opts, .retention_year)
}

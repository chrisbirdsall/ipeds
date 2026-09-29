# Race/ethnicity harmonization ---------------------------------------------------
#
# IPEDS collected race/ethnicity in seven categories based on the 1977 OMB
# standards until the 2007 Department of Education guidance introduced the
# nine categories of the 1997 OMB standards. Institutions could switch in
# 2008-09 or 2009-10 and had to by 2010-11, so the transition-year files
# contain both sets of variables, with each institution reporting in one of
# them.
#
# Old variables are numbered pairs (<prefix>race01 ... race24): 01/02
# nonresident alien men/women, 03/04 Black non-Hispanic, 05/06 American
# Indian/Alaska Native, 07/08 Asian/Pacific Islander, 09/10 Hispanic, 11/12
# White non-Hispanic, 13/14 race unknown, 15/16 grand total men/women,
# 17-23 the totals of the seven categories and 24 the grand total.
#
# New variables are mnemonic: <prefix><stem><t|m|w>, e.g. efwhitt, cbkaam.
#
# Comparable categories across the break: Asian and Native Hawaiian/Other
# Pacific Islander are combined into `asian_pi`; `two_more` exists only
# under the new standards (optionally allocated back proportionally with
# `race_bridge = TRUE`); Hispanic, nonresident, unknown and totals are
# directly comparable.

.race_keys <- c("white", "black", "hispanic", "asian", "nhpi", "asian_pi", "aian",
				"two_more", "unknown", "nonres")

.race_new_stem <- c(aian = "aian", asian = "asia", black = "bkaa", hispanic = "hisp",
					nhpi = "nhpi", white = "whit", two_more = "2mor", unknown = "unkn",
					nonres = "nral")

.race_old_men <- c(nonres = 1, black = 3, aian = 5, asian_pi = 7, hispanic = 9, white = 11,
				   unknown = 13, total = 15)
.race_old_total <- c(nonres = 17, black = 18, aian = 19, asian_pi = 20, hispanic = 21,
					 white = 22, unknown = 23, total = 24)

.race_spec <- list(
	total = .spec("total"),
	white = .spec("white"),
	black = .spec("black"),
	hispanic = .spec("hispanic|latino"),
	asian = .spec("asian", c("pacific", "hawaiian")),
	nhpi = .spec("hawaiian|pacific islander", "^asian"),
	asian_pi = .spec("asian|pacific"),
	aian = .spec("american indian|alaska"),
	two_more = .spec("two or more|multi"),
	unknown = .spec("unknown"),
	nonres = .spec("nonresident|non-resident|alien")
)

.sex_spec <- list(
	t = .spec(character(), c("\\bmen\\b", "women", "\\bmales?\\b", "females?")),
	m = .spec("\\bmen\\b|\\bmales?\\b", c("women", "females?")),
	w = .spec("women|females?")
)

.race_labels <- c(
	total = "Total", men = "Men", women = "Women",
	white = "White", black = "Black or African American", hispanic = "Hispanic or Latino",
	asian = "Asian (9-category years only)",
	nhpi = "Native Hawaiian or Other Pacific Islander (9-category years only)",
	asian_pi = "Asian, Native Hawaiian or Other Pacific Islander (comparable across years)",
	aian = "American Indian or Alaska Native",
	two_more = "Two or more races (9-category years only)",
	unknown = "Race/ethnicity unknown",
	nonres = "U.S. nonresident (nonresident alien)")

# Component-specific variable prefixes.
.race_cfg <- list(
	EFA = list(new = "ef", total_stem = "totl", old = "efrace"),
	C_A = list(new = "c", total_stem = "total", old = "crace"),
	GR = list(new = "gr", total_stem = "totl", old = "grrace"),
	EFFY = list(new = "efy", total_stem = "totl", old = c("fyrace", "efyrace"))
)

.race_scheme_labels <- c(old = "7 categories (1977 standards)",
						 new = "9 categories (1997 standards)")

# Harmonize the race/ethnicity-by-sex columns of `df` (one output row per input
# row). Returns a data frame with columns <prefix>_total, _men, _women,
# _<race> (and _<race>_men/_women when `sex = TRUE`), <prefix>_race_scheme
# and, when `bridge = TRUE`, <prefix>_<race>_bridged.
.harmonize_race <- function(df, ctx, cfg, prefix, sex = TRUE, bridge = FALSE,
							concept = "race/ethnicity and sex") {
	n <- nrow(df)
	nms <- names(df)
	dict <- ctx$dict
	checked <- 0L
	verified <- 0L
	mismatched <- character()
	getv <- function(v, spec) {
		if (!(v %in% nms)) return(NULL)
		checked <<- checked + 1L
		lab <- .dict_label(dict, v)
		if (!is.null(dict) && !is.na(lab)) {
			if (!.label_match(lab, spec$all, spec$none)) {
				mismatched <<- c(mismatched, sprintf("%s ('%s')", v, lab))
				return(NULL)
			}
			verified <<- verified + 1L
		}
		.as_num(df[[v]])
	}
	comb <- function(r, s) .spec(c(r$all, s$all), c(r$none, s$none))

	# New (9-category) variables
	new <- list()
	for (k in c("total", names(.race_new_stem))) {
		stem <- if (k == "total") cfg$total_stem else .race_new_stem[[k]]
		new[[k]] <- lapply(c(t = "t", m = "m", w = "w"), function(s) {
			getv(paste0(cfg$new, stem, s), comb(.race_spec[[k]], .sex_spec[[s]]))
		})
		if (is.null(new[[k]]$t) && !(is.null(new[[k]]$m) && is.null(new[[k]]$w))) {
			new[[k]]$t <- .rsum(list(new[[k]]$m, new[[k]]$w), n)
		}
	}
	has_new <- any(vapply(new, function(x) !all(vapply(x, is.null, logical(1))), logical(1)))

	# Old (7-category) variables
	old <- list()
	old_prefix <- NULL
	for (op in cfg$old) {
		if (any(sprintf("%s%02d", op, 1:24) %in% nms)) {
			old_prefix <- op
			break
		}
	}
	has_old <- !is.null(old_prefix)
	if (has_old) {
		for (k in names(.race_old_men)) {
			i <- .race_old_men[[k]]
			m <- getv(sprintf("%s%02d", old_prefix, i), comb(.race_spec[[k]], .sex_spec$m))
			w <- getv(sprintf("%s%02d", old_prefix, i + 1L), comb(.race_spec[[k]], .sex_spec$w))
			t <- getv(sprintf("%s%02d", old_prefix, .race_old_total[[k]]),
					  comb(.race_spec[[k]], .sex_spec$t))
			if (is.null(t) && !(is.null(m) && is.null(w))) t <- .rsum(list(m, w), n)
			old[[k]] <- list(t = t, m = m, w = w)
		}
	}

	# Decide, row by row, which scheme the institution reported in.
	detail_sum <- function(lst, keys) {
		v <- .rsum(lapply(keys, function(k) lst[[k]]$t), n)
		v[is.na(v)] <- 0
		v
	}
	if (has_new && has_old) {
		nd <- detail_sum(new, setdiff(names(.race_new_stem), character()))
		od <- detail_sum(old, setdiff(names(.race_old_men), "total"))
		use_new <- nd > 0 | od == 0
	} else {
		use_new <- rep(has_new, n)
	}

	pick <- function(nv, ov) {
		res <- rep(NA_real_, n)
		if (!is.null(nv)) res[use_new] <- nv[use_new]
		if (!is.null(ov)) res[!use_new] <- ov[!use_new]
		res
	}
	value <- function(k, s) {
		nv <- if (k == "asian_pi") {
			if (is.null(new$asian[[s]]) && is.null(new$nhpi[[s]])) NULL
			else .rsum(list(new$asian[[s]], new$nhpi[[s]]), n)
		} else new[[k]][[s]]
		ov <- old[[k]][[s]]
		pick(nv, ov)
	}

	out <- list()
	out[[paste0(prefix, "_total")]] <- value("total", "t")
	out[[paste0(prefix, "_men")]] <- value("total", "m")
	out[[paste0(prefix, "_women")]] <- value("total", "w")
	for (k in .race_keys) {
		out[[paste0(prefix, "_", k)]] <- value(k, "t")
		if (sex) {
			out[[paste0(prefix, "_", k, "_men")]] <- value(k, "m")
			out[[paste0(prefix, "_", k, "_women")]] <- value(k, "w")
		}
	}
	scheme <- ifelse(use_new, .race_scheme_labels[["new"]], .race_scheme_labels[["old"]])
	scheme[is.na(out[[paste0(prefix, "_total")]])] <- NA_character_
	out[[paste0(prefix, "_race_scheme")]] <- scheme
	if (bridge) {
		base <- c("white", "black", "aian", "asian_pi")
		b <- lapply(base, function(k) out[[paste0(prefix, "_", k)]])
		names(b) <- base
		denom <- .rsum(b, n)
		tm <- out[[paste0(prefix, "_two_more")]]
		for (k in base) {
			share <- ifelse(!is.na(denom) & denom > 0, b[[k]] / denom, 0)
			add <- ifelse(use_new & !is.na(tm), tm * share, 0)
			out[[paste0(prefix, "_", k, "_bridged")]] <- b[[k]] + add
		}
	}
	out <- as.data.frame(out, stringsAsFactors = FALSE, optional = TRUE)

	status <- if (checked == 0) "not_found"
		else if (length(mismatched)) "check_failed"
		else if (is.null(dict)) "unverified"
		else "verified"
	.audit(ctx, concept,
		   c(if (has_new) paste0(cfg$new, "*"), if (has_old) paste0(old_prefix, "01-24")),
		   status,
		   c(sprintf("%d of %d variables checked against the dictionary", verified, checked),
		     sprintf("%d rows in the 9-category scheme, %d rows in the 7-category scheme",
		     		sum(use_new), sum(!use_new)),
		     if (length(mismatched)) paste("dropped (label mismatch):",
		     							  paste(mismatched, collapse = ", "))))
	out
}

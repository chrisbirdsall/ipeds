# Completions (C A) ----------------------------------------------------------------

# Award level codes and the harmonized group each belongs to. Codes 9-11 were
# retired after 2009-10 (old doctor's degree, first-professional degree and
# certificate) and replaced by 17-19 (doctor's research/scholarship,
# professional practice, other); first-professional certificates were folded
# into post-master's certificates. Codes 20 and 21 split awards of less than
# one academic year into those under 12 weeks and 12 weeks to one year.
.award_levels <- list(
	"1" = list(group = "cert_lt1", spec = .spec("less than (one|1)", c("at least", "12 weeks"))),
	"20" = list(group = "cert_lt1", spec = .spec("(less than|under) 12 weeks")),
	"21" = list(group = "cert_lt1", spec = .spec(c("12 weeks", "less than (one|1)"))),
	"2" = list(group = "cert_1to2", spec = .spec(c("at least (one|1)", "less than (two|2)"))),
	"3" = list(group = "assoc", spec = .spec("associate")),
	"4" = list(group = "cert_2to4", spec = .spec(c("at least (two|2)", "less than (four|4)"))),
	"5" = list(group = "bach", spec = .spec("bachelor", "post")),
	"6" = list(group = "postbacc_cert", spec = .spec("post.?baccalaureate")),
	"7" = list(group = "master", spec = .spec("master", "post")),
	"8" = list(group = "postmaster_cert", spec = .spec("post.?master")),
	"9" = list(group = "doctor_pre2010", spec = .spec("doctor", c("research", "professional", "other"))),
	"10" = list(group = "first_prof", spec = .spec(c("first.?professional", "degree"))),
	"11" = list(group = "postmaster_cert", spec = .spec(c("first.?professional", "certificate"))),
	"17" = list(group = "doctor_research", spec = .spec(c("doctor", "research"))),
	"18" = list(group = "doctor_prof", spec = .spec(c("doctor", "professional"))),
	"19" = list(group = "doctor_other", spec = .spec(c("doctor", "other")))
)

.award_groups <- c("cert_lt1", "cert_1to2", "assoc", "cert_2to4", "bach", "postbacc_cert",
				   "master", "postmaster_cert", "doctor_research", "doctor_prof",
				   "doctor_other", "doctor_pre2010", "first_prof")

.award_group_labels <- c(
	cert_lt1 = "Certificates of less than one academic year",
	cert_1to2 = "Certificates of at least one but less than two academic years",
	assoc = "Associate's degrees",
	cert_2to4 = "Certificates of at least two but less than four academic years",
	bach = "Bachelor's degrees",
	postbacc_cert = "Postbaccalaureate certificates",
	master = "Master's degrees",
	postmaster_cert = "Post-master's certificates (including first-professional certificates before 2010)",
	doctor_research = "Doctor's degrees - research/scholarship (2010 onward; optional 2008-2009)",
	doctor_prof = "Doctor's degrees - professional practice (2010 onward; optional 2008-2009)",
	doctor_other = "Doctor's degrees - other (2010 onward; optional 2008-2009)",
	doctor_pre2010 = "Doctor's degrees (pre-2010 definition)",
	first_prof = "First-professional degrees (before 2010)",
	doctor = "Doctor's degrees, all types including first-professional (comparable across years)")

#' Format CIP codes
#'
#' Converts CIP codes read as numbers or text (e.g. `1.0101`, `"1.0101"`,
#' `"01.0101"`, `'="01.0101"'`) to the standard `"xx.xxxx"` form. Two digit
#' family codes are zero padded and the IPEDS grand total code `"99"` is left
#' as is.
#'
#' @param x CIP codes.
#' @param digits return the 2-, 4- or 6-digit level of the code.
#' @return A character vector.
#' @examples
#' ipeds_cip_format(c(1.0101, 27.0501, 99))
#' ipeds_cip_format("27.0501", digits = 2)
#' @export
ipeds_cip_format <- function(x, digits = 6) {
	x <- trimws(gsub("^=?\"?|\"$", "", as.character(x)))
	out <- x
	six <- grepl("^[0-9]{1,2}\\.[0-9]{1,4}$", x)
	out[six] <- sprintf("%07.4f", as.numeric(x[six]))
	two <- grepl("^[0-9]{1,2}$", x) & x != "99"
	out[two] <- sprintf("%02d", as.integer(x[two]))
	if (digits == 4) out[grepl("^[0-9]{2}\\.[0-9]{4}$", out)] <- substr(out[grepl("^[0-9]{2}\\.[0-9]{4}$", out)], 1, 5)
	if (digits == 2) out[grepl("^[0-9]{2}(\\.|$)", out)] <- substr(out[grepl("^[0-9]{2}(\\.|$)", out)], 1, 2)
	out
}

# CIP edition used by IPEDS completions for a data year.
.cip_edition <- function(year) {
	ifelse(year <= 2001, "1990", ifelse(year <= 2009, "2000", ifelse(year <= 2019, "2010", "2020")))
}

# Read C_A for one year, restricted to first majors, with harmonized race and
# award level group per row.
.completions_rows <- function(year, opts, module) {
	x <- .load("C_A", year, opts, module)
	if (is.null(x)) return(NULL)
	df <- x$df
	ctx <- x$ctx
	cip <- .pick_var(df, ctx, "CIP code", "cipcode", .spec("cip|instructional program"), search = FALSE)
	aw <- .pick_var(df, ctx, "award level", "awlevel", .spec("award|level"), search = FALSE)
	if (is.na(cip) || is.na(aw)) return(NULL)
	mj <- .pick_var(df, ctx, "first or second major", "majornum", .spec("major"), search = FALSE)
	if (!is.na(mj)) {
		m <- .as_num(df[[mj]])
		df <- df[is.na(m) | m == 1, , drop = FALSE]
		.audit(ctx, "first majors only", mj, "note", "awards to second majors are excluded")
	}
	df$.cip <- ipeds_cip_format(df[[cip]])
	df$.awlevel <- .as_num(df[[aw]])
	grp <- rep(NA_character_, nrow(df))
	verified <- unverified <- mismatched <- unknown <- character()
	for (code in sort(unique(df$.awlevel))) {
		def <- .award_levels[[as.character(code)]]
		if (is.null(def)) {
			unknown <- c(unknown, as.character(code))
			next
		}
		ok <- .code_label_ok(ctx$dict, aw, code, def$spec)
		if (isFALSE(ok)) {
			vl <- .dict_values(ctx$dict, aw)
			mismatched <- c(mismatched, sprintf("%s ('%s')", code, vl$label[match(code, .as_num(vl$code))]))
			next
		}
		if (isTRUE(ok)) verified <- c(verified, as.character(code))
		else unverified <- c(unverified, as.character(code))
		grp[df$.awlevel == code] <- def$group
	}
	df$.group <- grp
	.audit(ctx, "award levels", aw,
		   if (length(mismatched)) "check_failed" else if (length(unverified)) "unverified" else "verified",
		   c(if (length(verified)) paste("verified codes:", paste(verified, collapse = ",")),
		     if (length(unverified)) paste("unverified codes:", paste(unverified, collapse = ",")),
		     if (length(mismatched)) paste("excluded, label mismatch:", paste(mismatched, collapse = ", ")),
		     if (length(unknown)) paste("unmapped codes (counted in totals only):",
		     						   paste(unknown, collapse = ","))))
	list(df = df, ctx = ctx)
}

.completions_year <- function(year, opts) {
	x <- .completions_rows(year, opts, "completions")
	if (is.null(x)) return(NULL)
	df <- x$df
	ctx <- x$ctx
	is_total <- df$.cip == "99"
	is_six <- grepl("^[0-9]{2}\\.[0-9]{4}$", df$.cip)
	# grand-total rows (cipcode 99) where an institution has them, otherwise the
	# sum of its 6-digit program rows
	has_total <- df$unitid %in% df$unitid[is_total]
	use <- (is_total & has_total) | (is_six & !has_total)
	.audit(ctx, "institution totals", "cipcode", "note",
		   sprintf("grand-total rows (cipcode 99) used for %d institutions, 6-digit rows summed for %d",
		   		length(unique(df$unitid[has_total])), length(unique(df$unitid[!has_total]))))
	d <- df[use, , drop = FALSE]
	h <- .harmonize_race(d, ctx, .race_cfg$C_A, "c", sex = opts$race_by_sex,
						 bridge = opts$race_bridge)
	res <- data.frame(unitid = unique(df$unitid))
	s <- .sum_by_unitid(d$unitid, data.frame(v = h$c_total))
	res$awards_total <- s$v[match(res$unitid, s$unitid)]
	for (g in .award_groups) {
		rows <- which(d$.group == g)
		if (!any(df$.group == g, na.rm = TRUE)) {
			res[[paste0("awards_", g)]] <- NA_real_
			next
		}
		s <- .sum_by_unitid(d$unitid[rows], data.frame(v = h$c_total[rows]))
		v <- s$v[match(res$unitid, s$unitid)]
		v[is.na(v) & !is.na(res$awards_total)] <- 0
		res[[paste0("awards_", g)]] <- v
	}
	res$awards_doctor <- .rsum(res[paste0("awards_", c("doctor_research", "doctor_prof", "doctor_other",
												  "doctor_pre2010", "first_prof"))], nrow(res))
	# Bachelor's degrees by race/ethnicity and sex
	rows <- which(d$.group == "bach")
	cols <- setdiff(names(h), c("c_total", "c_race_scheme"))
	s <- .sum_by_unitid(d$unitid[rows], h[rows, cols, drop = FALSE])
	idx <- match(res$unitid, s$unitid)
	for (cn in cols) res[[sub("^c_", "bach_", cn)]] <- s[[cn]][idx]
	res$bach_race_scheme <- h$c_race_scheme[rows][match(res$unitid, d$unitid[rows])]
	# Cross-check grand-total rows against the sum of program rows
	if (any(is_total) && any(is_six)) {
		h6 <- .harmonize_race(df[is_six, , drop = FALSE], list(audit = NULL, dict = ctx$dict),
							  .race_cfg$C_A, "c", sex = FALSE)
		six <- .sum_by_unitid(df$unitid[is_six], data.frame(six = h6$c_total))
		cmp <- merge(res[, c("unitid", "awards_total")], six, by = "unitid")
		ok <- abs(cmp$awards_total - cmp$six) <= 1
		.audit(ctx, "grand total = sum of programs", "cipcode", "check_passed",
			   sprintf("agrees for %.1f%% of institutions", 100 * mean(ok, na.rm = TRUE)))
	}
	res
}

#' @rdname ipeds_modules
#' @export
ipeds_completions <- function(years, race_bridge = FALSE, race_by_sex = TRUE, verify = TRUE,
							  revised = TRUE, cache_dir = ipeds_cache_dir(), refresh = NULL,
							  quiet = FALSE) {
	opts <- .module_opts(cache_dir, refresh, quiet, verify, revised, race_bridge, race_by_sex)
	.run_module("completions", years, opts, .completions_year)
}

#' Awards by program (CIP code) and harmonized award level
#'
#' Returns completions in long format: one row per institution, year, CIP
#' code and harmonized award level, counting first majors only. CIP codes are
#' reported in the edition IPEDS used that year (`cip_edition`: 1990 through
#' 2001, 2000 for 2002-2009, 2010 for 2010-2019 and 2020 from 2020). Two-digit
#' families (`cip_digits = 2`) are the most comparable level across CIP
#' editions.
#'
#' @inheritParams ipeds_panel
#' @param cip_digits aggregate to 6-, 4- or 2-digit CIP codes.
#' @param by_race include harmonized race/ethnicity columns.
#' @return A data frame with `unitid`, `year`, `cip`, `cip_edition`,
#'   `award_level` (harmonized group, see [ipeds_codebook()]), `awards`,
#'   `awards_men`, `awards_women` and, optionally, race/ethnicity columns.
#' @examples
#' \dontrun{
#' stem <- ipeds_completions_programs(2015:2023, cip_digits = 2)
#' subset(stem, cip %in% c("11", "14", "26", "27", "40"))
#' }
#' @export
ipeds_completions_programs <- function(years, cip_digits = 6, by_race = FALSE, race_bridge = FALSE,
									   verify = TRUE, revised = TRUE, cache_dir = ipeds_cache_dir(),
									   refresh = NULL, quiet = FALSE) {
	if (!cip_digits %in% c(2, 4, 6)) stop("`cip_digits` must be 2, 4 or 6.", call. = FALSE)
	opts <- .module_opts(cache_dir, refresh, quiet, verify, revised, race_bridge, TRUE)
	fun <- function(year, opts) {
		x <- .completions_rows(year, opts, "completions_programs")
		if (is.null(x)) return(NULL)
		d <- x$df[grepl("^[0-9]{2}\\.[0-9]{4}$", x$df$.cip), , drop = FALSE]
		h <- .harmonize_race(d, x$ctx, .race_cfg$C_A, "c", sex = FALSE, bridge = race_bridge)
		keep <- c("c_total", "c_men", "c_women",
				  if (by_race) setdiff(grep("^c_", names(h), value = TRUE),
				  					 c("c_total", "c_men", "c_women", "c_race_scheme")))
		vals <- h[, keep, drop = FALSE]
		key <- data.frame(unitid = d$unitid, cip = ipeds_cip_format(d$.cip, cip_digits),
						  award_level = ifelse(is.na(d$.group), paste0("awlevel_", d$.awlevel), d$.group),
						  stringsAsFactors = FALSE)
		grp <- paste(key$unitid, key$cip, key$award_level, sep = "\r")
		first <- !duplicated(grp)
		sums <- .sum_by(grp, vals)
		out <- cbind(key[first, , drop = FALSE], sums[, -1, drop = FALSE])
		names(out) <- sub("^c_", "awards_", sub("^c_total$", "awards", names(out)))
		out$cip_edition <- .cip_edition(year)
		if (by_race) out$race_scheme <- h$c_race_scheme[first]
		out
	}
	out <- .run_module("completions_programs", years, opts, fun)
	audit <- attr(out, "ipeds_audit")
	out <- out[order(out$unitid, out$year, out$cip, out$award_level), , drop = FALSE]
	rownames(out) <- NULL
	out <- .apply_codebook(out)
	attr(out, "ipeds_audit") <- audit
	out
}

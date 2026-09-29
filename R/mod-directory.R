# Directory (HD) --------------------------------------------------------------------

.sector_map <- c("0" = "Administrative unit", "1" = "Public, 4-year or above",
				 "2" = "Private nonprofit, 4-year or above",
				 "3" = "Private for-profit, 4-year or above", "4" = "Public, 2-year",
				 "5" = "Private nonprofit, 2-year", "6" = "Private for-profit, 2-year",
				 "7" = "Public, less-than 2-year", "8" = "Private nonprofit, less-than 2-year",
				 "9" = "Private for-profit, less-than 2-year", "99" = "Sector unknown (not active)")
.control_map <- c("1" = "Public", "2" = "Private nonprofit", "3" = "Private for-profit")
.iclevel_map <- c("1" = "Four or more years", "2" = "At least 2 but less than 4 years",
				  "3" = "Less than 2 years (below associate)")
.obereg_map <- c("0" = "U.S. Service schools", "1" = "New England", "2" = "Mid East",
				 "3" = "Great Lakes", "4" = "Plains", "5" = "Southeast", "6" = "Southwest",
				 "7" = "Rocky Mountains", "8" = "Far West", "9" = "Outlying areas")
.instsize_map <- c("1" = "Under 1,000", "2" = "1,000 - 4,999", "3" = "5,000 - 9,999",
				   "4" = "10,000 - 19,999", "5" = "20,000 and above")
.locale4_levels <- c("City", "Suburb", "Town", "Rural")
.carnegie_groups <- c("Doctoral", "Master's", "Baccalaureate", "Associate's",
					  "Special focus", "Tribal", "Not classified")

# Four-category locale from either the urban-centric codes (2006 onward: 11-13
# city, 21-23 suburb, 31-33 town, 41-43 rural) or the earlier metro-centric
# codes (1-2 city, 3-4 urban fringe, 5-6 town, 7-8 rural).
.locale4 <- function(x) {
	x <- .as_num(x)
	out <- rep(NA_character_, length(x))
	uc <- !is.na(x) & x >= 11 & x <= 43
	out[uc] <- .locale4_levels[x[uc] %/% 10]
	mc <- !is.na(x) & x >= 1 & x <= 8
	out[mc] <- .locale4_levels[c(1, 1, 2, 2, 3, 3, 4, 4)[x[mc]]]
	factor(out, levels = .locale4_levels)
}

# Coarse Carnegie group. `edition` is "2000" for the CARNEGIE variable and
# "2005+" for the basic classifications of the 2005-2021 editions, which share
# the same code structure.
.carnegie_group <- function(x, edition) {
	x <- .as_num(x)
	out <- rep(NA_character_, length(x))
	if (edition == "2000") {
		out[x %in% c(15, 16)] <- "Doctoral"
		out[x %in% c(21, 22)] <- "Master's"
		out[x %in% c(31, 32, 33)] <- "Baccalaureate"
		out[x %in% 40] <- "Associate's"
		out[x %in% 51:59] <- "Special focus"
		out[x %in% 60] <- "Tribal"
		out[x %in% c(-3, -2, 0)] <- "Not classified"
	} else if (edition == "2005+") {
		out[x %in% 1:14] <- "Associate's"
		out[x %in% 15:17] <- "Doctoral"
		out[x %in% 18:20] <- "Master's"
		out[x %in% 21:23] <- "Baccalaureate"
		out[x %in% 24:32] <- "Special focus"
		out[x %in% 33] <- "Tribal"
		out[x %in% c(-3, -2, 0)] <- "Not classified"
	}
	factor(out, levels = .carnegie_groups)
}

.directory_year <- function(year, opts) {
	x <- .load("HD", year, opts, "directory")
	if (is.null(x)) return(NULL)
	df <- .one_per_unitid(x$df, x$ctx, "unitid")
	ctx <- x$ctx
	get <- function(concept, cands, spec, numeric = TRUE) {
		.get_var(df, ctx, concept, cands, spec, search = FALSE, numeric = numeric)
	}
	txt <- function(concept, cands, spec) get(concept, cands, spec, numeric = FALSE)
	out <- data.frame(unitid = df$unitid)
	add <- function(name, value) if (!is.null(value)) out[[name]] <<- value

	add("instnm", txt("institution name", "instnm", .spec("name")))
	add("ialias", txt("alias", "ialias", .spec("alias")))
	add("city", txt("city", "city", .spec("city")))
	add("stabbr", txt("state", "stabbr", .spec("state")))
	add("fips", get("state FIPS", "fips", .spec("fips|state")))
	add("countycd", txt("county code", "countycd", .spec("county")))
	add("countynm", txt("county name", "countynm", .spec("county")))
	zip <- txt("zip", "zip", .spec("zip"))
	if (!is.null(zip)) add("zip5", substr(gsub("[^0-9]", "", zip), 1, 5))
	add("latitude", get("latitude", "latitude", .spec("latitude")))
	add("longitude", get("longitude", c("longitud", "longitude"), .spec("longitude")))
	add("cbsa", get("CBSA", "cbsa", .spec("cbsa|core based")))
	add("cngdstcd", txt("congressional district", "cngdstcd", .spec("congressional")))
	add("webaddr", txt("web address", "webaddr", .spec("web|internet|url")))
	add("opeid", txt("OPEID", "opeid", .spec("ope")))

	sector <- get("sector", "sector", .spec("sector"))
	if (!is.null(sector)) {
		chk <- .code_label_ok(ctx$dict, "sector", 1, .spec(c("public", "4-year|four")))
		if (isFALSE(chk)) .audit(ctx, "sector codes", "sector", "check_failed",
								 "code 1 is not labelled 'Public, 4-year or above'")
		add("sector", .factor_codes(sector, .sector_map))
	}
	control <- get("control", "control", .spec("control"))
	if (!is.null(control)) add("control", .factor_codes(control, .control_map))
	iclevel <- get("level of institution", "iclevel", .spec("level"))
	if (!is.null(iclevel)) add("iclevel", .factor_codes(iclevel, .iclevel_map))
	obereg <- get("region", "obereg", .spec("region"))
	if (!is.null(obereg)) add("obereg", .factor_codes(obereg, .obereg_map))
	instsize <- get("size category", "instsize", .spec("size"))
	if (!is.null(instsize)) add("instsize", .factor_codes(instsize, .instsize_map))
	add("hloffer", get("highest level of offering", "hloffer", .spec("highest level")))
	add("hdegofr1", get("highest degree offered", "hdegofr1", .spec("highest degree")))
	add("deggrant", .yes_no(get("degree-granting status", "deggrant", .spec("degree.?grant"))))
	add("hbcu", .yes_no(get("HBCU", "hbcu", .spec("historically black"))))
	add("tribal", .yes_no(get("tribal college", "tribal", .spec("tribal"))))
	add("landgrnt", .yes_no(get("land grant", "landgrnt", .spec("land.?grant"))))
	add("medical", .yes_no(get("medical degree", "medical", .spec("medical"))))
	add("openpubl", .yes_no(get("open to the public", "openpubl", .spec("open"))))
	add("pset4flg", get("postsecondary/Title IV flag", "pset4flg", .spec("postsecondary|title iv")))
	add("instcat", get("institutional category", "instcat", .spec("categor")))
	add("cyactive", .yes_no(get("active in current year", "cyactive", .spec("active"))))
	add("newid", get("unitid of merged institution", "newid", .spec("unitid|merge|new")))
	add("deathyr", get("year institution deleted", "deathyr", .spec("year|delet|clos")))
	add("closedat", txt("closing date", "closedat", .spec("clos")))
	add("f1systyp", get("multi-institution system", "f1systyp", .spec("system")))
	add("f1sysnam", txt("system name", "f1sysnam", .spec("system")))

	loc <- get("locale", "locale", .spec("locale|urbani"))
	if (!is.null(loc)) {
		add("locale", loc)
		add("locale4", .locale4(loc))
	}

	# Carnegie: newest basic classification available in the file
	cc <- grep("^c[0-9]{2}basic$", names(df), value = TRUE)
	cc <- cc[order(-as.integer(substr(cc, 2, 3)))]
	cands <- c(cc, "ccbasic", "carnegie")
	cv <- .pick_var(df, ctx, "Carnegie classification", cands, .spec("carnegie|basic|classification"),
					search = FALSE)
	if (!is.na(cv)) {
		code <- .as_num(df[[cv]])
		lab <- .dict_label(ctx$dict, cv)
		ver <- if (grepl("^c[0-9]{2}basic$", cv)) paste0("20", substr(cv, 2, 3))
			else if (cv == "ccbasic") "2005/2010" else if (cv == "carnegie") "2000"
			else regmatches(lab, regexpr("(19|20)[0-9]{2}", lab))[1]
		edition <- if (cv == "carnegie") "2000"
			else if (cv %in% c("ccbasic", "c15basic", "c18basic", "c21basic")) "2005+"
			else "unknown"
		if (edition == "2005+") {
			ok <- .code_label_ok(ctx$dict, cv, 15, .spec("doctoral|research"))
			if (isFALSE(ok)) edition <- "unknown"
		}
		add("carnegie_basic", code)
		add("carnegie_edition", rep(ver[1], nrow(df)))
		grp <- .carnegie_group(code, edition)
		if (edition == "unknown") {
			.audit(ctx, "Carnegie group", cv, "not_found",
				   "unrecognized Carnegie edition; carnegie_group left missing")
		}
		add("carnegie_group", grp)
	}
	out
}

#' @rdname ipeds_modules
#' @export
ipeds_directory <- function(years, verify = TRUE, revised = TRUE, cache_dir = ipeds_cache_dir(),
							refresh = NULL, quiet = FALSE) {
	opts <- .module_opts(cache_dir, refresh, quiet, verify, revised)
	.run_module("directory", years, opts, .directory_year)
}

# Parent/child flags (FLAGS) ---------------------------------------------------------

.flags_year <- function(year, opts) {
	x <- .load("FLAGS", year, opts, "flags",
			   select = function(h) grepl("^(unitid|prch_|idx_)", h))
	if (is.null(x)) return(NULL)
	df <- .one_per_unitid(x$df, x$ctx, "unitid")
	keep <- grep("^(prch_|idx_)", names(df), value = TRUE)
	if (length(keep) == 0) {
		.audit(x$ctx, "parent/child flags", NA, "not_found", "no prch_* variables")
		return(NULL)
	}
	.audit(x$ctx, "parent/child flags", keep, if (is.null(x$ctx$dict)) "unverified" else "verified")
	out <- df[, c("unitid", keep), drop = FALSE]
	# record which code of each prch_* variable denotes a child record
	child <- vapply(grep("^prch_", keep, value = TRUE), function(v) {
		vl <- .dict_values(x$ctx$dict, v)
		ok <- vapply(vl$label, .label_match, logical(1), all = "child", none = c("partial", "^parent"))
		if (any(ok)) paste(vl$code[ok], collapse = ",") else "2"
	}, character(1))
	attr(out, "child_codes") <- child
	for (v in names(child)) out[[paste0(v, "_child")]] <- .as_num(out[[v]]) %in%
		.as_num(strsplit(child[[v]], ",")[[1]])
	out
}

#' @rdname ipeds_modules
#' @export
ipeds_flags <- function(years, verify = TRUE, revised = TRUE, cache_dir = ipeds_cache_dir(),
						refresh = NULL, quiet = FALSE) {
	opts <- .module_opts(cache_dir, refresh, quiet, verify, revised)
	.run_module("flags", years, opts, .flags_year)
}

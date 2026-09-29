# A synthetic IPEDS data center used by the tests. It mimics the layout of the
# NCES complete data files (zipped CSVs with upper-case headers, imputation
# flag columns, revised "_rv" files, latin1 text, Stata program files with
# labels) for a handful of fictitious institutions, and is served through a
# file:// base URL so that the whole download -> read -> harmonize pipeline
# runs offline.

fx_env <- new.env()

# Institutions: 1 public 4-year, 2 private nonprofit 4-year, 3 public 2-year
# (a branch reported with institution 1 for enrollment), 4 private for-profit.
fx_ids <- c(100001L, 100002L, 100003L, 100004L)

fx_csv <- function(df, eol = "\n") {
	con <- textConnection("out", "w", local = TRUE)
	utils::write.csv(df, con, row.names = FALSE, na = "", eol = eol)
	close(con)
	paste0(paste(out, collapse = eol), eol)
}

fx_do <- function(file, vars, values = list()) {
	l <- c("*** Created:    9/29/2026",
		   "*** Modify the path below to point to your data file.",
		   sprintf("insheet using \"c:\\dct\\%s_data_stata.csv\", comma clear", tolower(file)),
		   sprintf("label data \"dct_%s\"", tolower(file)))
	l <- c(l, sprintf("label variable %s \"%s\"", names(vars), unname(vars)))
	for (v in names(values)) {
		codes <- names(values[[v]])
		l <- c(l, sprintf("label define label_%s %s \"%s\"%s", v, codes, unname(values[[v]]),
						  ifelse(seq_along(codes) == 1, "", ", add")))
		l <- c(l, sprintf("label values %s label_%s", v, v))
	}
	c(l, "tab sector", sprintf("save dct_%s", tolower(file)))
}

fx_zip <- function(dir, zipname, files) {
	tmp <- tempfile("fx")
	dir.create(tmp)
	for (nm in names(files)) {
		x <- files[[nm]]
		p <- file.path(tmp, nm)
		if (is.raw(x)) writeBin(x, p) else writeBin(charToRaw(paste(x, collapse = "\r\n")), p)
	}
	old <- setwd(tmp)
	on.exit(setwd(old))
	utils::zip(file.path(dir, zipname), names(files), flags = "-q")
}

fx_add <- function(dir, file, data, dict = NULL, csv_name = tolower(file), extra = list()) {
	files <- c(stats::setNames(list(data), paste0(csv_name, ".csv")), extra)
	fx_zip(dir, paste0(file, ".zip"), files)
	if (!is.null(dict)) {
		fx_zip(dir, paste0(file, "_Stata.zip"),
			   stats::setNames(list(dict), paste0(tolower(file), ".do")))
	}
}

# Race/ethnicity allocations ------------------------------------------------

fx_new_share <- c(whit = .50, bkaa = .10, hisp = .15, asia = .08, nhpi = .02, aian = .01,
				  "2mor" = .04, unkn = .05, nral = .05)
fx_old_share <- c(nonres = .05, black = .10, aian = .01, api = .10, hisp = .15, white = .54,
				  unknown = .05)

# New-scheme columns for totals `n` (40% men, 60% women).
fx_race_new <- function(n, prefix, total_stem = "TOTL") {
	out <- list()
	out[[paste0(prefix, total_stem, "T")]] <- n
	out[[paste0(prefix, total_stem, "M")]] <- n * .4
	out[[paste0(prefix, total_stem, "W")]] <- n * .6
	for (k in names(fx_new_share)) {
		out[[paste0(prefix, toupper(k), "T")]] <- n * fx_new_share[[k]]
		out[[paste0(prefix, toupper(k), "M")]] <- n * fx_new_share[[k]] * .4
		out[[paste0(prefix, toupper(k), "W")]] <- n * fx_new_share[[k]] * .6
	}
	as.data.frame(out, check.names = FALSE)
}

# Old-scheme columns <prefix>RACE01..24.
fx_race_old <- function(n, prefix) {
	out <- list()
	i <- 1
	for (k in names(fx_old_share)) {
		out[[sprintf("%sRACE%02d", prefix, i)]] <- n * fx_old_share[[k]] * .4
		out[[sprintf("%sRACE%02d", prefix, i + 1)]] <- n * fx_old_share[[k]] * .6
		i <- i + 2
	}
	out[[sprintf("%sRACE15", prefix)]] <- n * .4
	out[[sprintf("%sRACE16", prefix)]] <- n * .6
	j <- 17
	for (k in names(fx_old_share)) {
		out[[sprintf("%sRACE%02d", prefix, j)]] <- n * fx_old_share[[k]]
		j <- j + 1
	}
	out[[sprintf("%sRACE24", prefix)]] <- n
	as.data.frame(out, check.names = FALSE)
}

fx_race_new_labels <- function(prefix, total_stem = "totl") {
	names_new <- c(aian = "American Indian or Alaska Native", asia = "Asian",
				   bkaa = "Black or African American", hisp = "Hispanic",
				   nhpi = "Native Hawaiian or Other Pacific Islander", whit = "White",
				   "2mor" = "Two or more races", unkn = "Race/ethnicity unknown",
				   nral = "Nonresident alien")
	v <- c()
	v[paste0(prefix, total_stem, c("t", "m", "w"))] <- c("Grand total", "Grand total men", "Grand total women")
	for (k in names(names_new)) {
		v[paste0(prefix, k, c("t", "m", "w"))] <- paste(names_new[[k]], c("total", "men", "women"))
	}
	v
}

fx_race_old_labels <- function(prefix) {
	cats <- c("Nonresident alien", "Black non-Hispanic", "American Indian or Alaska Native",
			  "Asian or Pacific Islander", "Hispanic", "White non-Hispanic", "Race/ethnicity unknown")
	v <- c()
	for (i in seq_along(cats)) {
		v[sprintf("%srace%02d", prefix, 2 * i - 1)] <- paste(cats[i], "men")
		v[sprintf("%srace%02d", prefix, 2 * i)] <- paste(cats[i], "women")
		v[sprintf("%srace%02d", prefix, 16 + i)] <- paste(cats[i], "total")
	}
	v[sprintf("%srace%02d", prefix, 15:16)] <- c("Grand total men", "Grand total women")
	v[sprintf("%srace%02d", prefix, 24)] <- "Grand total"
	v
}

# Component builders -----------------------------------------------------------

fx_hd <- function(dir, year) {
	names <- c("Alpha State University", "Beta College", "Alpha State Community College",
			   "Gamma Technical Institute")
	df <- data.frame(UNITID = fx_ids, INSTNM = names,
					 CITY = c("Springfield", "Shelbyville", "Springfield", "Ogdenville"),
					 STABBR = c("AL", "MA", "AL", "TX"), FIPS = c(1, 25, 1, 48),
					 ZIP = c("01234-5678", "02138", "01234", "75001"),
					 OPEID = c("00123400", "00567800", "00123401", "03456700"),
					 OBEREG = c(5, 1, 5, 6), SECTOR = c(1, 2, 4, 6), CONTROL = c(1, 2, 1, 3),
					 ICLEVEL = c(1, 1, 2, 2), HBCU = c(2, 1, 2, 2), TRIBAL = c(2, 2, 2, 2),
					 INSTSIZE = c(3, 4, 2, 1), stringsAsFactors = FALSE)
	values <- list(sector = c("1" = "Public, 4-year or above",
							  "2" = "Private not-for-profit, 4-year or above",
							  "4" = "Public, 2-year", "6" = "Private for-profit, 2-year"),
				   control = c("1" = "Public", "2" = "Private not-for-profit", "3" = "Private for-profit"))
	vars <- c(unitid = "Unique identification number of the institution",
			  instnm = "Institution (entity) name", city = "City location of institution",
			  stabbr = "State abbreviation", fips = "FIPS state code", zip = "ZIP code",
			  opeid = "Office of Postsecondary Education (OPE) ID Number",
			  obereg = "Bureau of Economic Analysis (BEA) regions", sector = "Sector of institution",
			  control = "Control of institution", iclevel = "Level of institution",
			  hbcu = "Historically Black College or University", tribal = "Tribal college",
			  instsize = "Institution size category", locale = "Degree of urbanization (Urban-centric locale)")
	if (year < 2006) {
		df$LOCALE <- c(2, 3, 6, 7)
		df$CARNEGIE <- c(16, 31, 40, 59)
		vars["carnegie"] <- "Carnegie Classification 2000"
		values$carnegie <- c("16" = "Doctoral/Research Universities--Intensive", "40" = "Associate's Colleges")
	} else {
		df$LOCALE <- c(12, 21, 32, 43)
		df$CCBASIC <- c(16, 21, 1, 10)
		vars["ccbasic"] <- "Carnegie Classification 2005/2010: Basic"
		if (year >= 2021) {
			df$C21BASIC <- c(15, 21, 2, 10)
			df$C18BASIC <- c(16, 21, 2, 10)
			vars[c("c21basic", "c18basic")] <- c("Carnegie Classification 2021: Basic",
												 "Carnegie Classification 2018: Basic")
			values$c21basic <- c("15" = "Doctoral Universities: Very High Research Activity",
								 "21" = "Baccalaureate Colleges: Arts & Sciences Focus")
		}
		values$ccbasic <- c("15" = "Research Universities (very high research activity)",
							"16" = "Research Universities (high research activity)")
	}
	dict <- fx_do(paste0("HD", year), vars, values)
	file <- paste0("HD", year)
	if (year == 2009) {
		rv <- df
		rv$INSTNM[1] <- "Alpha State University (revised)"
		fx_add(dir, file, fx_csv(df), dict, extra = list("hd2009_rv.csv" = fx_csv(rv)))
	} else if (year == 2010) {
		# latin1-encoded text with Windows line endings
		df$INSTNM[4] <- "Universidad Polit@cnica Gamma"
		raw <- charToRaw(fx_csv(df, eol = "\r\n"))
		raw[raw == charToRaw("@")] <- as.raw(0xe9)
		fx_add(dir, file, raw, dict)
	} else if (year == 2023) {
		# UTF-8 with byte order mark
		fx_add(dir, file, c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw(fx_csv(df))), dict)
	} else if (year == 2007) {
		fx_add(dir, file, fx_csv(df), NULL)
	} else {
		fx_add(dir, file, fx_csv(df), dict)
	}
}

fx_efa_levels <- c("1" = 5000, "2" = 4000, "12" = 1000, "21" = 3500, "22" = 3000, "32" = 500,
				   "41" = 1500, "42" = 1000, "52" = 500, "4" = 1000, "24" = 500)
fx_efa_labels <- c("1" = "All students total", "2" = "All students, Undergraduate total",
				   "12" = "All students, Graduate", "21" = "Full-time students total",
				   "22" = "Full-time students, Undergraduate total",
				   "32" = "Full-time students, Graduate", "41" = "Part-time students total",
				   "42" = "Part-time students, Undergraduate total",
				   "52" = "Part-time students, Graduate",
				   "4" = "All students, Undergraduate, Degree/certificate-seeking, First-time",
				   "24" = "Full-time students, Undergraduate, Degree/certificate-seeking, First-time")

# scheme: "old", "new" or "mixed" (institution 1 new, institution 2 old but
# with EFTOTLT filled in).
fx_efa <- function(dir, year, scheme, relabel_24 = FALSE) {
	rows <- expand.grid(lvl = as.numeric(names(fx_efa_levels)), i = 1:2)
	n <- fx_efa_levels[as.character(rows$lvl)] * rows$i
	base <- data.frame(UNITID = fx_ids[rows$i], EFALEVEL = rows$lvl, LINE = 1, SECTION = 1)
	new <- fx_race_new(n, "EF")
	old <- fx_race_old(n, "EF")
	if (scheme == "new") df <- cbind(base, new)
	if (scheme == "old") df <- cbind(base, old)
	if (scheme == "mixed") {
		new[rows$i == 2, ] <- NA
		new$EFTOTLT[rows$i == 2] <- n[rows$i == 2]
		old[rows$i == 1, ] <- NA
		df <- cbind(base, new, old)
	}
	df$XEFTOTLT <- "R"
	labs <- fx_efa_labels
	if (relabel_24) {
		# the first-time full-time row is coded 25 in this (fictitious) year
		df$EFALEVEL[df$EFALEVEL == 24] <- 25
		labs["24"] <- "Full-time students, Undergraduate, Degree/certificate-seeking, Other degree/certificate-seeking"
		labs["25"] <- "Full-time students, Undergraduate, Degree/certificate-seeking, First-time"
		df <- rbind(df, transform(df[df$EFALEVEL == 25, ], EFALEVEL = 24, EFTOTLT = 1, EFTOTLM = 0, EFTOTLW = 1))
	}
	vars <- c(unitid = "Unique identification number of the institution",
			  efalevel = "Level of student", line = "Original line number on survey form",
			  section = "Attendance status", xeftotlt = "Imputation variable for EFTOTLT")
	if (scheme != "old") vars <- c(vars, fx_race_new_labels("ef"))
	if (scheme != "new") vars <- c(vars, fx_race_old_labels("ef"))
	fx_add(dir, sprintf("EF%dA", year), fx_csv(df),
		   fx_do(sprintf("EF%dA", year), vars, list(efalevel = labs)))
}

fx_award_labels <- c("1" = "Award of less than 1 academic year",
					 "20" = "Certificates of less than 12 weeks",
					 "21" = "Certificates of at least 12 weeks, but less than 1 year",
					 "2" = "Award of at least 1 but less than 2 academic years",
					 "3" = "Associate's degree", "4" = "Award of at least 2 but less than 4 academic years",
					 "5" = "Bachelor's degree", "6" = "Postbaccalaureate certificate",
					 "7" = "Master's degree", "8" = "Post-master's certificate",
					 "9" = "Doctor's degree", "10" = "First-professional degree",
					 "11" = "First-professional certificate (post-degree)",
					 "17" = "Doctor's degree - research/scholarship",
					 "18" = "Doctor's degree - professional practice",
					 "19" = "Doctor's degree - other")

# Completions: institution 1 awards at `levels`, 100 per level for CIP
# 27.0101 and 50 for 01.0101 (first majors), 10 second majors; grand total rows.
fx_c <- function(dir, year, scheme, levels, unknown_level = NULL) {
	mk <- function(unitid, cip, lvl, major, n) {
		data.frame(UNITID = unitid, CIPCODE = cip, MAJORNUM = major, AWLEVEL = lvl, n = n,
				   stringsAsFactors = FALSE)
	}
	rows <- do.call(rbind, lapply(c(levels, unknown_level), function(l) rbind(
		mk(fx_ids[1], "27.0101", l, 1, 100), mk(fx_ids[1], "01.0101", l, 1, 50),
		mk(fx_ids[1], "27.0101", l, 2, 10), mk(fx_ids[1], "99", l, 1, 150),
		mk(fx_ids[1], "99", l, 2, 10))))
	rows <- rbind(rows, mk(fx_ids[2], "27.0101", 5, 1, 200), mk(fx_ids[2], "99", 5, 1, 200))
	race <- if (scheme == "new") fx_race_new(rows$n, "C", "TOTAL") else fx_race_old(rows$n, "C")
	df <- cbind(rows[, c("UNITID", "CIPCODE", "MAJORNUM", "AWLEVEL")], race)
	vars <- c(unitid = "Unique identification number of the institution",
			  cipcode = "CIP Code -  2010 Classification", majornum = "First or Second Major",
			  awlevel = "Award Level code")
	vars <- c(vars, if (scheme == "new") fx_race_new_labels("c", "total") else fx_race_old_labels("c"))
	codes <- as.character(sort(unique(c(levels, unknown_level))))
	labs <- fx_award_labels[intersect(codes, names(fx_award_labels))]
	if (!is.null(unknown_level)) labs[as.character(unknown_level)] <- "Some new award level"
	fx_add(dir, sprintf("C%d_A", year), fx_csv(df),
		   fx_do(sprintf("C%d_A", year), vars, list(awlevel = labs)))
}

fx_adm <- function(dir) {
	adm <- data.frame(UNITID = fx_ids[1:2], APPLCN = c(10000, 5000), APPLCNM = c(4000, 2000),
					  APPLCNW = c(6000, 3000), ADMSSN = c(6000, 1000), ADMSSNM = c(2400, 400),
					  ADMSSNW = c(3600, 600), ENRLT = c(2000, 400), ENRLFT = c(1900, 400),
					  SATPCT = c(60, 80), ACTPCT = c(70, 30), SATVR25 = c(500, 650),
					  SATVR75 = c(600, 740), SATMT25 = c(510, 660), SATMT75 = c(620, 770),
					  ACTCM25 = c(21, 29), ACTCM75 = c(27, 33), ADMCON7 = c(1, 3))
	vars <- c(unitid = "Unique identification number of the institution",
			  applcn = "Applicants total", applcnm = "Applicants men", applcnw = "Applicants women",
			  admssn = "Admissions total", admssnm = "Admissions men", admssnw = "Admissions women",
			  enrlt = "Enrolled total", enrlft = "Full-time enrolled total",
			  satpct = "Percent of first-time degree/certificate-seeking students submitting SAT scores",
			  actpct = "Percent of first-time degree/certificate-seeking students submitting ACT scores",
			  satvr25 = "SAT Critical Reading 25th percentile score",
			  satvr75 = "SAT Critical Reading 75th percentile score",
			  satmt25 = "SAT Math 25th percentile score", satmt75 = "SAT Math 75th percentile score",
			  actcm25 = "ACT Composite 25th percentile score", actcm75 = "ACT Composite 75th percentile score",
			  admcon7 = "Admission test scores", openadmp = "Open admission policy")
	# 2013: admissions in IC, old test policy wording
	ic13 <- cbind(adm, OPENADMP = c(2, 2))
	ic13 <- rbind(ic13, data.frame(UNITID = fx_ids[3:4], lapply(ic13[1, -1], function(x) NA),
								   check.names = FALSE))
	ic13$OPENADMP[3:4] <- 1
	fx_add(dir, "IC2013", fx_csv(ic13),
		   fx_do("IC2013", vars, list(admcon7 = c("1" = "Required", "2" = "Recommended",
		   									   "3" = "Neither required nor recommended"),
		   						   openadmp = c("1" = "Yes", "2" = "No"))))
	# 2014: ADM file with new wording; open admission still in IC
	adm14 <- adm
	adm14$ADMCON7 <- c(1, 5)
	adm14$APPLCN <- NULL
	vars14 <- vars
	vars14["satvr25"] <- "SAT Evidence-Based Reading and Writing 25th percentile score"
	fx_add(dir, "ADM2014", fx_csv(adm14),
		   fx_do("ADM2014", vars14, list(admcon7 = c("1" = "Required", "2" = "Recommended",
		   										  "3" = "Not considered", "5" = "Considered but not required"))))
	fx_add(dir, "IC2014", fx_csv(data.frame(UNITID = fx_ids, OPENADMP = c(2, 2, 1, 1))),
		   fx_do("IC2014", vars["openadmp"], list(openadmp = c("1" = "Yes", "2" = "No"))))
}

fx_misc_2013 <- function(dir) {
	# Retention
	fx_add(dir, "EF2013D",
		   fx_csv(data.frame(UNITID = fx_ids, RET_PCF = c(85, 92, 60, 150), RET_PCP = c(50, NA, 40, 45),
		   				  STUFACR = c(18, 9, 22, 25))),
		   fx_do("EF2013D", c(ret_pcf = "Full-time retention rate, 2013",
		   				   ret_pcp = "Part-time retention rate, 2013",
		   				   stufacr = "Student-to-faculty ratio")))
	# Charges
	ay <- data.frame(UNITID = fx_ids, TUITION1 = c(8000, 40000, 3000, 15000),
					 FEE1 = c(1000, 1500, 500, 0), TUITION2 = c(8000, 40000, 3500, 15000),
					 FEE2 = c(1000, 1500, 500, 0), TUITION3 = c(25000, 40000, 7000, 15000),
					 FEE3 = c(1000, 1500, 500, 0), TUITION6 = c(10000, 45000, NA, NA),
					 FEE6 = c(500, 800, NA, NA), TUITION7 = c(26000, 45000, NA, NA),
					 FEE7 = c(500, 800, NA, NA), CHG4AY3 = c(1200, 1000, 1400, 900),
					 CHG5AY3 = c(11000, 14000, -2, NA), CHG7AY3 = c(9000, 9500, 9000, 9000))
	fx_add(dir, "IC2013_AY", fx_csv(ay), fx_do("IC2013_AY", c(
		tuition1 = "In-district average tuition for full-time undergraduates",
		fee1 = "In-district fees for full-time undergraduates",
		tuition2 = "In-state average tuition for full-time undergraduates",
		fee2 = "In-state fees for full-time undergraduates",
		tuition3 = "Out-of-state average tuition for full-time undergraduates",
		fee3 = "Out-of-state fees for full-time undergraduates",
		tuition6 = "In-state average tuition full-time graduates",
		fee6 = "In-state fees full-time graduates",
		tuition7 = "Out-of-state average tuition full-time graduates",
		fee7 = "Out-of-state fees full-time graduates",
		chg4ay3 = "Books and supplies 2013-14",
		chg5ay3 = "On campus, room and board 2013-14",
		chg7ay3 = "Off campus (not with family), room and board 2013-14")))
	# Aid and net price
	sfa <- data.frame(UNITID = fx_ids, UPGRNTP = c(35, 15, 45, 70), PGRNT_P = c(38, 14, 50, 75),
					  UFLOANP = c(40, 45, 10, 80), FLOAN_P = c(42, 44, 12, 82),
					  AGRNT_A = c(7000, 30000, 4000, 5000), IGRNT_A = c(4000, 28000, 1000, 2000),
					  NPIST2 = c(14000, NA, 7000, NA), NPGRN2 = c(NA, 25000, NA, 20000),
					  NPIS412 = c(9000, NA, 5000, NA), NPT412 = c(NA, 12000, NA, 18000))
	fx_add(dir, "SFA1213", fx_csv(sfa), fx_do("SFA1213", c(
		upgrntp = "Percent of undergraduate students awarded Pell grants",
		pgrnt_p = "Percent of full-time first-time undergraduates awarded Pell grants",
		ufloanp = "Percent of undergraduate students awarded federal student loans",
		floan_p = "Percent of full-time first-time undergraduates awarded federal student loans",
		agrnt_a = "Average amount of federal, state, local or institutional grant aid awarded to full-time first-time undergraduates",
		igrnt_a = "Average amount of institutional grant aid awarded to full-time first-time undergraduates",
		npist2 = "Average net price-students awarded grant or scholarship aid, 2012-13",
		npgrn2 = "Average net price-students awarded grant or scholarship aid, 2012-13",
		npis412 = "Average net price (income 0-30,000)-students awarded Title IV federal financial aid, 2012-13",
		npt412 = "Average net price (income 0-30,000)-students awarded Title IV federal financial aid, 2012-13")))
	# Flags: institution 3 reports enrollment with institution 1
	fl <- data.frame(UNITID = fx_ids, PRCH_EF = c(1, -2, 2, -2), IDX_EF = c(-2, -2, 100001, -2),
					 PRCH_C = c(-2, 2, -2, -2), IDX_C = c(-2, 100001, -2, -2))
	prch <- c("1" = "Parent record - includes data from branch campuses",
			  "2" = "Child record - data reported with parent campus",
			  "3" = "Partial child record - reports partial data and other data reported with parent campus",
			  "-2" = "Not applicable")
	fx_add(dir, "FLAGS2013", fx_csv(fl), fx_do("FLAGS2013",
		c(prch_ef = "Parent/child indicator for Enrollment", idx_ef = "ID number of parent institution - Enrollment",
		  prch_c = "Parent/child indicator for Completions", idx_c = "ID number of parent institution - Completions"),
		list(prch_ef = prch, prch_c = prch)))
	# 12-month enrollment
	effy <- cbind(data.frame(UNITID = rep(fx_ids[1:2], each = 3), EFFYLEV = rep(c(1, 2, 4), 2)),
				  fx_race_new(c(6000, 5000, 1000, 12000, 10000, 2000), "EFY"))
	fx_add(dir, "EFFY2013", fx_csv(effy), fx_do("EFFY2013",
		c(unitid = "Unique identification number of the institution", effylev = "Level of student",
		  fx_race_new_labels("efy")),
		list(effylev = c("1" = "All students total", "2" = "Undergraduate", "4" = "Graduate"))))
	efia <- data.frame(UNITID = fx_ids[1:2], FTEUG = c(4500, 9000), FTEGD = c(700, 1500),
					   FTEDPP = c(100, NA), CDACTUA = c(135000, 270000), CNACTUA = c(0, 0),
					   CDACTGA = c(12000, 24000))
	fx_add(dir, "EFIA2013", fx_csv(efia), fx_do("EFIA2013", c(
		fteug = "Reported full-time equivalent (FTE) undergraduate enrollment, 2012-13",
		ftegd = "Reported full-time equivalent (FTE) graduate enrollment, 2012-13",
		ftedpp = "Reported full-time equivalent (FTE) doctor's professional practice enrollment, 2012-13",
		cdactua = "12-month instructional activity credit hours: undergraduates",
		cnactua = "12-month instructional activity clock hours: undergraduates",
		cdactga = "12-month instructional activity credit hours: graduates")))
	# Finance: F2 lacks the expected total-expenses variable (found by label)
	fx_add(dir, "F1213_F1A",
		   fx_csv(data.frame(UNITID = fx_ids[c(1, 3)], F1D01 = c(9e8, 1e8), F1B01 = c(2e8, 3e7),
		   				  F1B11 = c(3e8, 4e7), F1D02 = c(8.5e8, 9.5e7), F1C011 = c(3e8, 4e7),
		   				  F1H02 = c(5e8, 1e6))),
		   fx_do("F1213_F1A", c(f1d01 = "Total revenues and other additions",
		   					 f1b01 = "Tuition and fees, after deducting discounts and allowances",
		   					 f1b11 = "State appropriations",
		   					 f1d02 = "Total expenses and other deductions",
		   					 f1c011 = "Instruction - Current year total",
		   					 f1h02 = "Value of endowment assets at the end of the fiscal year")))
	fx_add(dir, "F1213_F2",
		   fx_csv(data.frame(UNITID = fx_ids[2], F2D16 = 6e8, F2D01 = 3e8, F2D03 = 0, F2E999 = 5.5e8,
		   				  F2E011 = 2e8, F2H02 = 2e9)),
		   fx_do("F1213_F2", c(f2d16 = "Total revenues and investment return - Total",
		   					f2d01 = "Tuition and fees - Total", f2d03 = "State appropriations - Total",
		   					f2e999 = "Total expenses-Total amount", f2e011 = "Instruction-Total amount",
		   					f2h02 = "Value of endowment assets at the end of the fiscal year")))
	fx_add(dir, "F1213_F3",
		   fx_csv(data.frame(UNITID = fx_ids[4], F3D09 = 5e7, F3D01 = 4.5e7, F3E07 = 4e7, F3E011 = 1e7)),
		   fx_do("F1213_F3", c(f3d09 = "Total revenues and investment return",
		   					f3d01 = "Tuition and fees", f3e07 = "Total expenses",
		   					f3e011 = "Instruction-Total amount")))
}

fx_gr <- function(dir) {
	# 2013: cohort + chrtstat layout
	rows <- data.frame(
		UNITID = c(rep(fx_ids[1], 6), rep(fx_ids[3], 2)),
		GRTYPE = c(2, 3, 4, 8, 9, 12, 29, 30),
		COHORT = c(2, 2, 2, 1, 1, 1, 4, 4),
		CHRTSTAT = c(12, 13, 16, 12, 13, 16, 12, 13),
		n = c(1000, 600, 590, 1200, 700, 650, 500, 150))
	df <- cbind(rows[, 1:4], fx_race_new(rows$n, "GR"))
	vars <- c(unitid = "Unique identification number of the institution", grtype = "Cohort data",
			  cohort = "Cohort", chrtstat = "Graduation rate status in cohort", fx_race_new_labels("gr"))
	fx_add(dir, "GR2013", fx_csv(df), fx_do("GR2013", vars, list(
		cohort = c("1" = "Bachelor's/ equiv +other degree/certif-seeking subcohorts (4-yr institution)",
				   "2" = "Bachelor's or equiv degree-seeking subcohort (4-yr institution)",
				   "4" = "Degree/certif-seeking students ( 2-yr institution)"),
		chrtstat = c("12" = "Adjusted cohort (revised cohort minus exclusions)",
					 "13" = "Completers within 150% of normal time",
					 "16" = "Completers of bachelor's or equivalent degrees (150% of normal time)"))))
	# 2008: grtype-only layout with old race variables
	rows <- data.frame(UNITID = c(rep(fx_ids[1], 3), rep(fx_ids[3], 2)), GRTYPE = c(8, 9, 3, 29, 30),
					   n = c(1000, 550, 1100, 400, 100))
	df <- cbind(rows[, 1:2], fx_race_old(rows$n, "GR"))
	fx_add(dir, "GR2008", fx_csv(df), fx_do("GR2008",
		c(unitid = "Unique identification number of the institution", grtype = "Cohort data",
		  fx_race_old_labels("gr")),
		list(grtype = c(
			"3" = "Bachelor's/ equiv +other degree/certif-seeking subcohorts (4-yr institution) adjusted cohort (revised cohort minus exclusions)",
			"8" = "Bachelor's or equiv subcohort (4-yr institution) adjusted cohort (revised cohort minus exclusions)",
			"9" = "Bachelor's or equiv subcohort (4-yr institution) Completers within 150% of normal time total",
			"29" = "Degree/certif-seeking students ( 2-yr institution) Adjusted cohort (revised cohort minus exclusions)",
			"30" = "Degree/certif-seeking students ( 2-yr institution) Completers within 150% of normal time total"))))
}

fake_server <- function() {
	if (!is.null(fx_env$dir)) return(fx_env$dir)
	dir <- file.path(tempdir(), "ipeds-fake-server")
	unlink(dir, recursive = TRUE)
	dir.create(dir)
	for (y in c(2005, 2007, 2009, 2010, 2013, 2014, 2021, 2023)) fx_hd(dir, y)
	fx_efa(dir, 2007, "old")
	fx_efa(dir, 2009, "mixed")
	fx_efa(dir, 2010, "new")
	fx_efa(dir, 2013, "new", relabel_24 = TRUE)
	fx_c(dir, 2008, "old", c(3, 5, 7, 9, 10, 11))
	fx_c(dir, 2013, "new", c(1, 3, 5, 7, 8, 17, 18, 19))
	fx_c(dir, 2021, "new", c(20, 21, 2, 3, 5, 7, 17, 18), unknown_level = 12)
	fx_adm(dir)
	fx_misc_2013(dir)
	fx_gr(dir)
	fx_env$dir <- dir
	dir
}

skip_if_no_zip <- function() {
	testthat::skip_if(!nzchar(Sys.which("zip")) && !nzchar(Sys.getenv("R_ZIPCMD")),
					  "no zip program available to build test fixtures")
}

# Point the package at the fake server with a fresh cache.
local_fake_ipeds <- function(env = parent.frame()) {
	skip_if_no_zip()
	dir <- fake_server()
	cache <- tempfile("ipeds-cache")
	withr::local_options(list(
		ipeds.base_url = paste0("file://", normalizePath(dir, winslash = "/")),
		ipeds.cache_dir = cache, ipeds.pause = 0, ipeds.debug = TRUE,
		ipeds.silence_superseded = TRUE), .local_envir = env)
	invisible(list(dir = dir, cache = cache))
}

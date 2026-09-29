test_that("legacy download functions still work", {
	local_fake_ipeds()
	withr::local_options(ipeds.quiet_legacy = TRUE)
	hd <- getIPEDSSurvey("HD", 2013)
	expect_equal(hd$unitid, fx_ids)
	expect_null(attr(hd$sector, "label"))
	hdf <- getIPEDSSurvey("HD", 2013, stringsAsFactors = TRUE)
	expect_s3_class(hdf$instnm, "factor")
	expect_warning(expect_null(getIPEDSSurvey("HD", 1990)), "Could not download")
	expect_equal(downloadIPEDSSurvey("EFD", 2013)$ret_pcf[1], 85)
	d <- suppressMessages(downloadAllSurveys(2013))
	expect_true(all(c("HD", "EFA", "C_A") %in% d$component[!is.na(d$path)]))
	h <- ipedsHelp("HD", 2013)
	expect_s3_class(h, "ipeds_dictionary")
})

test_that("legacy messages point to the replacements", {
	local_fake_ipeds()
	withr::local_options(ipeds.silence_superseded = FALSE, ipeds.quiet_legacy = TRUE)
	expect_message(getIPEDSSurvey("HD", 2013), "superseded by ipeds_read")
})

test_that("recode helpers produce labelled factors", {
	expect_equal(as.character(recodeSector(c(1, 2, 99))),
				 c("Public, 4-year or above", "Private nonprofit, 4-year or above", "Sector unknown (not active)"))
	expect_equal(as.character(recodeControl(c(1, 3, -3))), c("Public", "Private for-profit", "Not available"))
	expect_equal(as.character(recodeAwardLevel(c(5, 17, 20))),
				 c("Bachelor's degree", "Doctor's degree - research/scholarship", "Certificate of less than 12 weeks"))
	expect_equal(as.character(recodeImputation("R")), "Reported")
	expect_equal(as.character(recodeInstitutionSize(c(1, -1))), c("Under 1,000", "Not reported"))
	hd <- data.frame(sector = c(1, 4), control = c(1, 1), other = 1:2)
	rd <- recodeDirectory(hd)
	expect_s3_class(rd$sector, "factor")
	expect_equal(rd$other, 1:2)
})

test_that("XML submission files are generated", {
	skip_if_not_installed("XML")
	students <- data.frame(
		cip = c("27.0101", "27.0101", "27.0101", "11.0701"),
		type = "Major", name = c("Mathematics", "Mathematics", "Mathematics", ""),
		level = c(5, 5, 7, 5), eth = c("White", "Asian", "White", "HispanicAnyRace"),
		sex = c("MALE", "FEMALE", "FEMALE", "MALE"), stringsAsFactors = FALSE)
	m <- mapIPEDSFields("cip", "type", "name", "level", "eth", "sex")
	doc <- completions(students, m, institutionId = 100001, institutionName = "Alpha")
	xml <- XML::saveXML(doc)
	expect_match(xml, "<ProgramCIPCode>27.0101</ProgramCIPCode>")
	expect_match(xml, "<EducationalProgramName>Mathematics</EducationalProgramName>")
	expect_equal(length(gregexpr("EducationalAwardsByProgram>", xml)[[1]]), 6)
	expect_error(completions(transform(students, cip = c(NA, cip[-1])), m, institutionId = 1), "CIP")
	expect_error(completions(students, m), "institutionId")
	doc12 <- twelveMonthEnrollment(students, m, institutionId = 100001, undergraduateContactHours = 10,
								   graduateCreditHours = 5)
	xml12 <- XML::saveXML(doc12)
	expect_match(xml12, "<UndergraduateContactHours>10</UndergraduateContactHours>")
	expect_match(xml12, "<GraduateEnrollment>")
	expect_s3_class(fallEnrollment(students, m, institutionId = 1), "XMLInternalDocument")
	custom <- completions(transform(students, sex = ifelse(sex == "MALE", "M", "F")), m,
						  institutionId = 1, genderValues = c(male = "M", female = "F"))
	expect_match(XML::saveXML(custom), "<CountFemale>1</CountFemale>")
})

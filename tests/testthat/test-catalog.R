test_that("file names follow the data center conventions", {
	expect_equal(ipeds_file_name("HD", c(2000, 2001, 2002, 2023)),
				 c("FA2000HD", "FA2001HD", "HD2002", "HD2023"))
	expect_equal(ipeds_file_name("EFA", 2023), "EF2023A")
	expect_equal(ipeds_file_name("EFA_DIST", 2019), "EF2019A_DIST")
	expect_equal(ipeds_file_name("C_A", c(1999, 2023)), c("C9899_A", "C2023_A"))
	expect_equal(ipeds_file_name("C_DEP", 2023), "C2023DEP")
	expect_equal(ipeds_file_name("SFA", c(2000, 2001, 2010, 2023)),
				 c("SFA9900S", "SFA0001S", "SFA0910", "SFA2223"))
	expect_equal(ipeds_file_name("F_F1A", c(2002, 2023)), c("F0102_F1A", "F2223_F1A"))
	expect_equal(ipeds_file_name("F_F2", 2000), "F9900F2")
	expect_equal(ipeds_file_name("GR200", c(2009, 2023)), c("GR200_09", "GR200_23"))
	expect_equal(ipeds_file_name("DRVEF12", 2023), "DRVEF122023")
	expect_equal(ipeds_file_name("EFANR", 1999), "EF99_ANR")
	expect_equal(ipeds_file_name("EFFY", 2001), "EF2001D1")
})

test_that("component ids are validated and aliases resolved", {
	expect_error(ipeds_file_name("NOPE", 2020), "Unknown IPEDS component")
	expect_equal(ipeds_file_name("efd1", 2005), "EFFY2005")
	expect_equal(ipeds_file_name("f1a", 2005), "F0405_F1A")
	comps <- ipeds_components()
	expect_false(anyDuplicated(comps$id) > 0)
	expect_true(all(c("HD", "EFA", "C_A", "SFA", "GR", "ADM", "F_F1A") %in% comps$id))
	expect_true(all(ipeds_components("Finance")$group == "Finance"))
})

test_that("urls and periods are built", {
	withr::local_options(ipeds.base_url = "https://example.org/data")
	expect_equal(ipeds_url("HD2023"), "https://example.org/data/HD2023.zip")
	expect_equal(ipeds_url("HD2023", "stata"), "https://example.org/data/HD2023_Stata.zip")
	expect_equal(ipeds_url("HD2023", "dictionary"), "https://example.org/data/HD2023_Dict.zip")
	expect_match(ipeds:::.period("C_A", 2023), "July 1, 2022 to June 30, 2023")
	expect_match(ipeds:::.period("SFA", 2023), "2022-23")
})

test_that("legacy surveys dataset matches the catalog", {
	expect_equal(nrow(surveys), nrow(ipeds_components()))
	expect_false(anyDuplicated(surveys$SurveyID) > 0)
	hd <- surveys[surveys$SurveyID == "SFA", ]
	expect_equal(hd$YearFormat, 22)
})

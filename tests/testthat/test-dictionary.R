test_that("Stata program files are parsed", {
	lines <- c(
		"*** Created: 6/13/2004",
		"insheet using \"c:\\dct\\hd2020_data_stata.csv\", comma clear",
		"label data \"dct_hd2020\"",
		"label variable unitid \"Unique identification number of the institution\"",
		"label variable sector \"Sector of institution\"",
		"label var hloffer \"Highest level of offering\"",
		"label define label_sector 0 \"Administrative Unit\"",
		"label define label_sector 1 \"Public, 4-year or above\", add",
		"label define label_sector 99 \"Sector unknown (not active)\", add",
		"label define label_hloffer -3 \"{Not available}\"",
		"label define label_hloffer 1 \"Award of less than one academic year\", add",
		"*label define label_stabbr \"AL\" \"Alabama\"",
		"label define lbl_yesno 1 \"Yes\"",
		"label define lbl_yesno 2 \"No\", add",
		"label values hbcu tribal lbl_yesno",
		"label values sector label_sector",
		"tab sector",
		"save dct_hd2020")
	d <- ipeds:::.parse_stata_do(lines, "HD2020")
	expect_s3_class(d, "ipeds_dictionary")
	expect_equal(d$variables$varname, c("unitid", "sector", "hloffer"))
	expect_equal(ipeds:::.dict_label(d, "hloffer"), "Highest level of offering")
	v <- ipeds:::.dict_values(d, "sector")
	expect_equal(v$code, c("0", "1", "99"))
	expect_equal(v$label[2], "Public, 4-year or above")
	expect_equal(ipeds:::.dict_values(d, "hloffer")$code, c("-3", "1"))
	expect_equal(ipeds:::.dict_values(d, "tribal")$label, c("Yes", "No"))
	expect_equal(ipeds:::.dict_values(d, "stabbr")$code, "AL")
	expect_output(print(d), "HD2020")
})

test_that("Excel dictionaries are parsed", {
	skip_if_not_installed("readxl")
	path <- test_path("fixtures", "dict.xlsx")
	skip_if_not(file.exists(path))
	d <- ipeds:::.parse_excel_dictionary(path, "HD2023")
	expect_equal(ipeds:::.dict_label(d, "sector"), "Sector of institution")
	expect_equal(ipeds:::.dict_values(d, "sector")$label[1], "Public, 4-year or above")
})

test_that("dictionaries are downloaded and cached", {
	srv <- local_fake_ipeds()
	d <- ipeds_dictionary("HD", 2013, quiet = TRUE)
	expect_equal(d$source, "stata")
	expect_true(file.exists(file.path(srv$cache, "parsed", "HD2013_auto.rds")))
	d2 <- ipeds_dictionary("HD", 2013, quiet = TRUE)
	expect_identical(d, d2)
	expect_error(ipeds_dictionary("HD", 2007, quiet = TRUE), "No dictionary could be loaded")
})

test_that("labels are normalized and matched in any locale", {
	nl <- ipeds:::.norm_label
	expect_equal(nl("Bachelor\u2019s  Degree \u2013 Total "), "bachelor's degree - total")
	expect_true(is.na(nl(NA)))
	lm <- ipeds:::.label_match
	expect_true(lm("SAT Critical Reading 25th percentile score", c("\\bsat\\b", "25"), "math"))
	expect_false(lm("SAT Math 25th percentile score", c("\\bsat\\b", "25"), "math"))
	expect_true(lm("All students, Undergraduate total", c("^all students", "undergrad"), "\\bgraduate"))
	expect_false(lm("All students, Graduate", c("^all students", "undergrad"), "\\bgraduate"))
	expect_false(lm(NA, "x"))
	expect_true(lm("anything", character(), character()))
})

test_that("row sums keep NA only when every value is missing", {
	rs <- ipeds:::.rsum
	expect_equal(rs(list(c(1, NA, NA), c(2, 3, NA)), 3), c(3, 3, NA))
	expect_equal(rs(list(NULL, NULL), 2), c(NA_real_, NA_real_))
	s <- ipeds:::.sum_by(c("a", "b", "a"), data.frame(x = c(1, NA, 2), y = c(NA, NA, NA)))
	expect_equal(s$group, c("a", "b"))
	expect_equal(s$x, c(3, NA))
	expect_equal(s$y, c(NA_real_, NA_real_))
	expect_equal(nrow(ipeds:::.sum_by(character(), data.frame(x = numeric()))), 0)
	expect_equal(ipeds:::.year_ranges(c(2001:2003, 2005, 2007:2008)), "2001-2003, 2005, 2007-2008")
})

test_that("CIP codes are formatted", {
	expect_equal(ipeds_cip_format(c(1.0101, 27.0501, 99)), c("01.0101", "27.0501", "99"))
	expect_equal(ipeds_cip_format(c("1.0101", "01.0101", "=\"11.0701\"", "5")),
				 c("01.0101", "01.0101", "11.0701", "05"))
	expect_equal(ipeds_cip_format("27.0501", digits = 4), "27.05")
	expect_equal(ipeds_cip_format(c("27.0501", "99"), digits = 2), c("27", "99"))
})

test_that("locale and Carnegie groups are harmonized", {
	expect_equal(as.character(ipeds:::.locale4(c(1, 2, 3, 4, 5, 6, 7, 8, 11, 21, 33, 42, -3, 9))),
				 c("City", "City", "Suburb", "Suburb", "Town", "Town", "Rural", "Rural",
				   "City", "Suburb", "Town", "Rural", NA, NA))
	expect_equal(as.character(ipeds:::.carnegie_group(c(15, 16, 21, 31, 40, 55, 60), "2000")),
				 c("Doctoral", "Doctoral", "Master's", "Baccalaureate", "Associate's", "Special focus", "Tribal"))
	expect_equal(as.character(ipeds:::.carnegie_group(c(1, 15, 18, 21, 24, 33, -2), "2005+")),
				 c("Associate's", "Doctoral", "Master's", "Baccalaureate", "Special focus", "Tribal",
				   "Not classified"))
	expect_true(all(is.na(ipeds:::.carnegie_group(c(1, 15), "unknown"))))
})

test_that("inflation adjustment", {
	x <- ipeds_deflate(c(100, 100), year = c(2010, 2024), base_year = 2024)
	expect_equal(x[2], 100)
	expect_equal(x[1], 100 * 313.689 / 218.056)
	expect_warning(y <- ipeds_deflate(100, 1990), "No index value")
	expect_true(is.na(y))
	idx <- data.frame(y = 2020:2021, v = c(100, 110))
	expect_equal(ipeds_deflate(100, 2020, 2021, idx), 110)
	expect_error(ipeds_deflate(1, 2020, 1900), "no value for base_year")
	expect_equal(nrow(ipeds_cpi()), 25)
})

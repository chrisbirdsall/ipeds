race_ctx <- function(dict = NULL) {
	list(audit = ipeds:::.audit_new(), dict = dict, module = "test", component = "EFA",
		 year = 2009L, file = "TEST")
}

test_that("new-scheme variables are harmonized", {
	df <- fx_race_new(c(1000, 500), "EF")
	names(df) <- tolower(names(df))
	h <- ipeds:::.harmonize_race(df, race_ctx(), ipeds:::.race_cfg$EFA, "enr")
	expect_equal(h$enr_total, c(1000, 500))
	expect_equal(h$enr_men, c(400, 200))
	expect_equal(h$enr_white, c(500, 250))
	expect_equal(h$enr_asian, c(80, 40))
	expect_equal(h$enr_nhpi, c(20, 10))
	expect_equal(h$enr_asian_pi, c(100, 50))
	expect_equal(h$enr_two_more, c(40, 20))
	expect_equal(h$enr_asian_pi_women, c(60, 30))
	expect_equal(unique(h$enr_race_scheme), "9 categories (1997 standards)")
})

test_that("old-scheme variables are harmonized", {
	df <- fx_race_old(c(1000, 500), "EF")
	names(df) <- tolower(names(df))
	h <- ipeds:::.harmonize_race(df, race_ctx(), ipeds:::.race_cfg$EFA, "enr", sex = FALSE)
	expect_equal(h$enr_total, c(1000, 500))
	expect_equal(h$enr_women, c(600, 300))
	expect_equal(h$enr_white, c(540, 270))
	expect_equal(h$enr_asian_pi, c(100, 50))
	expect_equal(h$enr_nonres, c(50, 25))
	expect_true(all(is.na(h$enr_asian)))
	expect_true(all(is.na(h$enr_two_more)))
	expect_false("enr_white_men" %in% names(h))
	expect_equal(unique(h$enr_race_scheme), "7 categories (1977 standards)")
})

test_that("old totals are computed from men and women when absent", {
	df <- fx_race_old(1000, "EF")
	names(df) <- tolower(names(df))
	df <- df[, sprintf("efrace%02d", 1:16)]
	h <- ipeds:::.harmonize_race(df, race_ctx(), ipeds:::.race_cfg$EFA, "enr")
	expect_equal(h$enr_total, 1000)
	expect_equal(h$enr_black, 100)
})

test_that("transition years are resolved row by row", {
	new <- fx_race_new(c(1000, 1000, 0), "EF")
	old <- fx_race_old(c(1000, 1000, 0), "EF")
	new[2, ] <- NA
	new$EFTOTLT[2] <- 1000
	old[1, ] <- NA
	df <- cbind(new, old)
	names(df) <- tolower(names(df))
	h <- ipeds:::.harmonize_race(df, race_ctx(), ipeds:::.race_cfg$EFA, "enr")
	expect_equal(h$enr_total, c(1000, 1000, 0))
	expect_equal(h$enr_white, c(500, 540, 0))
	expect_equal(h$enr_asian, c(80, NA, 0))
	expect_equal(h$enr_asian_pi, c(100, 100, 0))
	expect_equal(h$enr_race_scheme[1:2], c("9 categories (1997 standards)", "7 categories (1977 standards)"))
})

test_that("two or more races can be bridged back proportionally", {
	df <- fx_race_new(1000, "EF")
	names(df) <- tolower(names(df))
	h <- ipeds:::.harmonize_race(df, race_ctx(), ipeds:::.race_cfg$EFA, "enr", bridge = TRUE)
	base <- c(white = 500, black = 100, aian = 10, asian_pi = 100)
	expect_equal(h$enr_white_bridged, 500 + 40 * 500 / sum(base))
	expect_equal(h$enr_white_bridged + h$enr_black_bridged + h$enr_aian_bridged + h$enr_asian_pi_bridged,
				 sum(base) + 40)
})

test_that("variables whose dictionary label does not match are dropped", {
	df <- fx_race_new(1000, "EF")
	names(df) <- tolower(names(df))
	vars <- fx_race_new_labels("ef")
	vars["efwhitt"] <- "Asian total"
	dict <- ipeds:::.new_dictionary("TEST", "stata",
									data.frame(varname = names(vars), label = unname(vars)), NULL)
	ctx <- race_ctx(dict)
	h <- ipeds:::.harmonize_race(df, ctx, ipeds:::.race_cfg$EFA, "enr")
	expect_equal(h$enr_white, 500)  # recomputed from men + women, which verify
	a <- ipeds:::.audit_table(ctx$audit)
	expect_equal(a$status, "check_failed")
	expect_match(a$detail, "efwhitt")
	# with the correct labels everything verifies
	dict2 <- ipeds:::.new_dictionary("TEST", "stata",
									 data.frame(varname = names(fx_race_new_labels("ef")),
									 		   label = unname(fx_race_new_labels("ef"))), NULL)
	ctx2 <- race_ctx(dict2)
	ipeds:::.harmonize_race(df, ctx2, ipeds:::.race_cfg$EFA, "enr")
	expect_equal(ipeds:::.audit_table(ctx2$audit)$status, "verified")
})

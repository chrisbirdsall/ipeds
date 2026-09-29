test_that("the panel combines modules with labels, codebook and audit", {
	local_fake_ipeds()
	p <- ipeds_panel(2013:2014, modules = c(ipeds:::.all_modules), quiet = TRUE)
	expect_s3_class(p, "ipeds_panel")
	expect_equal(nrow(p), 8)
	expect_false(anyDuplicated(p[, c("unitid", "year")]) > 0)
	expect_true(all(c("instnm", "enr_total", "e12_total", "ret_ft_pct", "awards_bach", "applicants",
					  "tuitfee_ug_instate", "net_price", "gr_bach_rate_150", "rev_total",
					  "prch_ef_child") %in% names(p)))
	expect_equal(p$enr_total[p$unitid == 100001 & p$year == 2013], 5000)
	expect_equal(p$applicants[p$unitid == 100001 & p$year == 2014], 10000)
	labs <- vapply(p, function(x) attr(x, "label") %||% NA_character_, character(1))
	expect_false(anyNA(labs))
	cb <- ipeds_codebook(p)
	expect_equal(cb$variable, names(p))
	expect_false(anyNA(cb$module))
	expect_equal(cb$years[cb$variable == "enr_total"], "2013")
	expect_equal(cb$years[cb$variable == "applicants"], "2013-2014")
	a <- ipeds_audit(p)
	expect_true(all(c("verified", "file_unavailable") %in% a$status))
	expect_true(any(a$status == "file_unavailable" & a$file == "EF2014A"))
	expect_output(print(p), "ipeds_panel")
	info <- attr(p, "ipeds_info")
	expect_equal(info$years, 2013:2014)
})

test_that("child records can be blanked", {
	local_fake_ipeds()
	keep <- ipeds_panel(2013, modules = c("directory", "completions"), quiet = TRUE)
	expect_equal(keep$awards_bach[keep$unitid == 100002], 200)
	na <- ipeds_panel(2013, modules = c("directory", "completions"), child_records = "na",
					  quiet = TRUE)
	expect_true(is.na(na$awards_bach[na$unitid == 100002]))
	expect_equal(na$awards_bach[na$unitid == 100001], 150)
	expect_false("prch_c" %in% names(na))
	expect_true(any(ipeds_audit(na)$concept == "child records"))
})

test_that("panel arguments are validated", {
	local_fake_ipeds()
	expect_error(ipeds_panel(), "specify `years`")
	expect_error(ipeds_panel(2013, modules = "nope"), "should be one of")
	expect_error(suppressWarnings(ipeds_panel(1990, modules = "directory", quiet = TRUE)),
				 "No data could be loaded")
	expect_warning(ipeds_panel(c(1999, 2013), modules = "directory", quiet = TRUE), "2000 onward")
})

test_that("race bridging and grad rates by race flow through the panel", {
	local_fake_ipeds()
	p <- ipeds_panel(2013, modules = c("enrollment", "grad_rates"), race_bridge = TRUE,
					 race_by_sex = FALSE, grad_rates_by_race = TRUE, quiet = TRUE)
	expect_true("enr_white_bridged" %in% names(p))
	expect_false("enr_white_men" %in% names(p))
	expect_true("gr_bach_rate_150_white" %in% names(p))
	expect_match(attr(p$enr_white_bridged, "label"), "bridged")
})

test_that("validation summarizes the audit trail", {
	local_fake_ipeds()
	v <- ipeds_validate(2013, modules = c("enrollment", "completions", "finance"))
	expect_true(all(c("module", "concept", "verified", "relabeled", "problem") %in% names(v)))
	expect_equal(v$relabeled[v$module == "finance" & v$concept == "exp_total"], 1)
	expect_equal(v$relabeled[v$module == "enrollment" & v$concept == "student level: ug_ft_firsttime"], 1)
})

test_that("dollar columns can be converted to constant dollars", {
	local_fake_ipeds()
	p <- ipeds_panel(2013, modules = c("charges", "aid"), quiet = TRUE)
	r <- ipeds_real_dollars(p, base_year = 2024)
	expect_equal(r$tuitfee_ug_instate, p$tuitfee_ug_instate * 313.689 / 232.957, ignore_attr = TRUE)
	expect_identical(r$pell_pct_ug, p$pell_pct_ug)
	expect_match(attr(r$net_price, "label"), "constant 2024 dollars")
})

test_that("panels are written with labels", {
	local_fake_ipeds()
	p <- ipeds_panel(2013, modules = c("directory", "enrollment"), quiet = TRUE)
	d <- tempfile("out")
	dir.create(d)
	ipeds_write(p, file.path(d, "panel.csv"))
	expect_true(all(file.exists(file.path(d, c("panel.csv", "panel_codebook.csv", "panel_audit.csv")))))
	back <- utils::read.csv(file.path(d, "panel.csv"))
	expect_equal(nrow(back), nrow(p))
	expect_equal(back$locale4[1], "City")
	ipeds_write(p, file.path(d, "panel.rds"))
	expect_equal(attr(readRDS(file.path(d, "panel.rds")), "ipeds_info")$years, 2013L)
	skip_if_not_installed("haven")
	ipeds_write(p, file.path(d, "panel.dta"))
	dta <- haven::read_dta(file.path(d, "panel.dta"))
	expect_equal(attr(dta$enr_total, "label"), "Fall enrollment: all students")
	expect_error(ipeds_write(p, file.path(d, "panel.xyz")), "Unsupported")
})

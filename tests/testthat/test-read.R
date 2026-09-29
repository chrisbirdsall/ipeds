test_that("files are downloaded, cached and read with types preserved", {
	srv <- local_fake_ipeds()
	hd <- ipeds_read("HD", 2013, quiet = TRUE)
	expect_s3_class(hd, "data.frame")
	expect_identical(as.vector(hd$unitid), fx_ids)
	expect_equal(as.vector(hd$zip), c("01234-5678", "02138", "01234", "75001"))
	expect_equal(as.vector(hd$opeid), c("00123400", "00567800", "00123401", "03456700"))
	expect_true(is.numeric(hd$sector))
	expect_equal(attr(hd$sector, "label"), "Sector of institution")
	expect_equal(as.character(ipeds_as_factor(hd$sector)),
				 c("Public, 4-year or above", "Private not-for-profit, 4-year or above",
				   "Public, 2-year", "Private for-profit, 2-year"))
	info <- attr(hd, "ipeds")
	expect_equal(info$file, "HD2013")
	expect_equal(info$component, "HD")
	expect_false(info$revised)
	expect_true(file.exists(file.path(srv$cache, "data", "HD2013.zip")))
	expect_true(nrow(ipeds_cache_info(srv$cache)) >= 2)
})

test_that("revised (_rv) files are preferred", {
	local_fake_ipeds()
	hd <- ipeds_read("HD", 2009, labels = FALSE, quiet = TRUE)
	expect_equal(hd$instnm[1], "Alpha State University (revised)")
	expect_true(attr(hd, "ipeds")$revised)
	hd0 <- ipeds_read("HD", 2009, labels = FALSE, revised = FALSE, quiet = TRUE)
	expect_equal(hd0$instnm[1], "Alpha State University")
})

test_that("latin1 text and byte order marks are handled", {
	local_fake_ipeds()
	hd <- ipeds_read("HD", 2010, labels = FALSE, quiet = TRUE)
	expect_equal(attr(hd, "ipeds")$encoding, "latin1")
	expect_equal(hd$instnm[4], "Universidad Polit\u00e9cnica Gamma")
	expect_equal(Encoding(hd$instnm[4]), "UTF-8")
	hd23 <- ipeds_read("HD", 2023, labels = FALSE, quiet = TRUE)
	expect_equal(names(hd23)[1], "unitid")
	expect_identical(hd23$unitid, fx_ids)
})

test_that("the base R reader gives the same result as data.table", {
	skip_if_not_installed("data.table")
	local_fake_ipeds()
	a <- ipeds_read("HD", 2010, labels = FALSE, quiet = TRUE)
	withr::local_options(ipeds.use_data_table = FALSE)
	b <- ipeds_read("HD", 2010, labels = FALSE, quiet = TRUE)
	attr(a, "ipeds") <- attr(b, "ipeds") <- NULL
	expect_equal(a, b)
})

test_that("variables and imputation flags can be selected", {
	local_fake_ipeds()
	ef <- ipeds_read("EFA", 2010, vars = c("efalevel", "eftotlt"), labels = FALSE, quiet = TRUE)
	expect_setequal(names(ef), c("unitid", "efalevel", "eftotlt", "xeftotlt"))
	ef2 <- ipeds_read("EFA", 2010, vars = c("efalevel", "eftotlt"), labels = FALSE,
					  imputation_flags = FALSE, quiet = TRUE)
	expect_setequal(names(ef2), c("unitid", "efalevel", "eftotlt"))
	expect_warning(ipeds_read("EFA", 2010, vars = c("nosuchvar"), labels = FALSE, quiet = TRUE),
				   "does not contain")
	expect_error(ipeds_read("EFA", 2010:2011), "single year")
})

test_that("stacking binds years and fills missing columns", {
	local_fake_ipeds()
	s <- ipeds_stack("HD", c(2007, 2010, 2021), vars = c("instnm", "ccbasic", "c21basic"), quiet = TRUE)
	expect_equal(nrow(s), 12)
	expect_equal(names(s)[1:2], c("unitid", "year"))
	expect_true(all(is.na(s$c21basic[s$year < 2021])))
	expect_warning(s2 <- ipeds_stack("HD", c(2010, 1990), quiet = TRUE), "HD1990")
	expect_equal(unique(s2$year), 2010)
})

test_that("unavailable files produce informative errors", {
	local_fake_ipeds()
	expect_error(ipeds_read("HD", 1990, quiet = TRUE), "Could not download")
	d <- ipeds_download(c("HD", "ADM"), 2013:2014, quiet = TRUE)
	expect_equal(nrow(d), 4)
	expect_true(is.na(d$path[d$component == "ADM" & d$year == 2013]))
	expect_false(is.na(d$error[d$component == "ADM" & d$year == 2013]))
	expect_true(file.exists(d$path[d$component == "HD" & d$year == 2014]))
})

test_that("availability and the latest year are detected", {
	local_fake_ipeds()
	a <- ipeds_available("HD", 2022:2023)
	expect_equal(a$available, c(FALSE, TRUE))
	expect_equal(unname(ipeds_latest_year("HD", max_year = 2027)), 2023L)
	expect_equal(ipeds_latest_year(c("ADM", "EFA"), max_year = 2016),
				 c(ADM = 2014L, EFA = 2013L))
	expect_true(is.na(ipeds_latest_year("OM", max_year = 2016, lookback = 2)))
})

test_that("cached files are refreshed when NCES updates them", {
	srv <- local_fake_ipeds()
	server <- tempfile("server")
	dir.create(server)
	file.copy(file.path(srv$dir, "EF2013D.zip"), server)
	withr::local_options(ipeds.base_url = paste0("file://", normalizePath(server, winslash = "/")))
	first <- ipeds_read("EFD", 2013, labels = FALSE, quiet = TRUE)
	expect_true("ret_pcf" %in% names(first))
	# NCES publishes a new version of the file
	Sys.sleep(1.1)
	file.copy(file.path(srv$dir, "HD2013.zip"), file.path(server, "EF2013D.zip"), overwrite = TRUE)
	cached <- ipeds_read("EFD", 2013, labels = FALSE, quiet = TRUE)
	expect_true("ret_pcf" %in% names(cached))
	never <- ipeds_read("EFD", 2013, labels = FALSE, refresh = "never", quiet = TRUE)
	expect_true("ret_pcf" %in% names(never))
	checked <- ipeds_read("EFD", 2013, labels = FALSE, refresh = "check", quiet = TRUE)
	expect_true("instnm" %in% names(checked))
	# offline: the cached copy is kept
	unlink(file.path(server, "EF2013D.zip"))
	withr::local_options(ipeds.max_age = 0)
	kept <- ipeds_read("EFD", 2013, labels = FALSE, quiet = TRUE)
	expect_true("instnm" %in% names(kept))
})

test_that("the cache can be cleared", {
	srv <- local_fake_ipeds()
	ipeds_read("HD", 2013, quiet = TRUE)
	expect_gt(nrow(ipeds_cache_info(srv$cache)), 0)
	ipeds_cache_clear("dictionary", cache_dir = srv$cache)
	expect_false(dir.exists(file.path(srv$cache, "stata")))
	expect_true(dir.exists(file.path(srv$cache, "data")))
	ipeds_cache_clear(cache_dir = srv$cache)
	expect_equal(nrow(ipeds_cache_info(srv$cache)), 0)
})

test_that("the cache directory honours options and environment variables", {
	withr::local_options(ipeds.cache_dir = NULL)
	withr::local_envvar(IPEDS_CACHE_DIR = file.path(tempdir(), "env-cache"))
	expect_match(ipeds_cache_dir(), "env-cache$")
	withr::local_options(ipeds.cache_dir = file.path(tempdir(), "opt-cache"))
	expect_match(ipeds_cache_dir(), "opt-cache$")
})

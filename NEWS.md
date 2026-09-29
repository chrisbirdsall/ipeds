# ipeds 1.0.0

A rewrite focused on building clean, labelled and harmonized IPEDS panels
from the newest NCES releases.

## New features

* `ipeds_panel()` builds an institution-by-year panel from harmonized modules
  (`ipeds_directory()`, `ipeds_enrollment()`, `ipeds_enrollment_12m()`,
  `ipeds_retention()`, `ipeds_completions()`, `ipeds_admissions()`,
  `ipeds_charges()`, `ipeds_aid()`, `ipeds_grad_rates()`, `ipeds_finance()`,
  `ipeds_flags()`), with variable labels, a codebook (`ipeds_codebook()`)
  and an audit trail (`ipeds_audit()`, `ipeds_validate()`).
* Harmonization across IPEDS changes: race/ethnicity categories (2008-2010,
  including transition-year files), award levels (2010 doctoral and
  first-professional changes, short certificates), graduate enrollment
  including first-professional students, admissions moving from IC to ADM
  (2014), the SAT redesign flag, test-score policy wording, locale codes
  (2006), Carnegie editions and parent/child reporting.
* Every harmonized variable and code is checked against the IPEDS dictionary
  of the file it comes from; mismatches are resolved by label or left
  missing, never silently used.
* `ipeds_read()`, `ipeds_read_file()`, `ipeds_stack()`,
  `ipeds_dictionary()`, `ipeds_as_factor()` give labelled raw access to any
  file; `ipeds_components()`, `ipeds_file_name()` and `ipeds_url()` describe
  the catalog of IPEDS files (1995 onward, with the irregular pre-2002 names).
* `ipeds_latest_year()` and `ipeds_available()` find the newest releases;
  revised (`_rv`) files are preferred and cached files are refreshed when
  NCES updates them (`ipeds.max_age`, `refresh`).
* `ipeds_completions_programs()` returns completions by CIP code and award
  level; `ipeds_cip_format()` normalizes CIP codes.
* `ipeds_real_dollars()`, `ipeds_deflate()` and `ipeds_cpi()` convert dollar
  amounts to constant dollars.
* `ipeds_write()` writes panels to CSV (with codebook and audit), Stata,
  SPSS or RDS, keeping labels.

## Bug fixes

* Files are downloaded over HTTPS from the current NCES data center and
  stored in a user cache instead of the installed package directory.
* The `surveys` catalog no longer needs `data(surveys)` before use, and no
  longer has duplicate `FLAGS` rows that made those files unreachable.
* Cached files are found for components whose file names use two-digit years.
* `getIPEDSSurvey()` passes its arguments through on the first download.
* `completions()` now writes program names (the old check could never be
  true) and validates CIP codes in the data rather than in the mapping.
* `twelveMonthEnrollment()` writes `UndergraduateContactHours` (was
  misspelled `CndergraduateContactHours`).
* The XML functions import the XML package properly, accept the values used
  for men and women (`genderValues`), declare `xsi:schemaLocation` correctly
  and work with current versions of the XML package.
* Documentation of `recodeGeographicRegion()` no longer describes
  `recodeControl()`.
* The demo index no longer lists `SATretention` twice.

## Other changes

* Requires R >= 4.0.0. `XML`, `data.table`, `haven` and `readxl` are
  optional (Suggests).
* The Sweave vignette (which referred to R-Forge) is replaced by an R
  Markdown vignette; demos use the new functions.

# ipeds

Download, label and harmonize data from the **Integrated Postsecondary
Education Data System (IPEDS)**, the core postsecondary data collection of
the National Center for Education Statistics (NCES).

The package builds a clean institution-by-year panel from the newest IPEDS
releases. Every variable is labelled, and measures are corrected so they stay
comparable when IPEDS changes its definitions: race/ethnicity categories,
award levels, where admissions data are reported, locale and Carnegie codes,
and so on. It also keeps an audit trail showing exactly which source variable
fed each number in each year.

## Installation

```r
# install.packages("remotes")
remotes::install_github("chrisbirdsall/ipeds")

# optional but recommended:
install.packages(c("data.table", "haven", "readxl"))
```

`data.table` makes reading large files (such as completions) much faster.
`haven` is used to write Stata/SPSS files with labels, and `readxl` to read
Excel dictionaries when the Stata program file is not available.

## Quick start

```r
library(ipeds)

# Most recent year NCES has released for each component
ipeds_latest_year(c("HD", "EFA", "C_A", "ADM", "SFA", "GR"))

# A harmonized panel from 2004 to the latest directory year
p <- ipeds_panel(years = 2004:ipeds_latest_year("HD"))
p

ipeds_codebook(p)                       # labels, sources, periods, notes, coverage
ipeds_audit(p, problems_only = TRUE)    # anything that could not be verified
p <- ipeds_real_dollars(p)              # dollar columns in constant dollars (CPI-U)
ipeds_write(p, "ipeds_panel.dta")       # also .csv (+ codebook), .sav, .rds
```

Files are downloaded once to a per-user cache (`ipeds_cache_dir()`) and
re-used. Years that have not been released yet are skipped and recorded in
the audit trail.

## What is harmonized

| Change in IPEDS | How the panel handles it |
|---|---|
| **Race/ethnicity**: 7 categories (1977 standards) replaced by 9 (1997 standards). Institutions switched in 2008–2010, and transition-year files contain both sets of variables. | Each institution-year is read from whichever scheme it reported in. Asian and Native Hawaiian/Pacific Islander are combined into `*_asian_pi`, which is comparable across the break. `*_race_scheme` records the scheme. `race_bridge = TRUE` adds `*_bridged` columns that allocate "two or more races" back proportionally. |
| **Award levels**: first-professional degrees and the old doctor's degree retired after 2009–10; doctor's degrees split into research, professional practice and other; certificates under one year later split at 12 weeks. | Harmonized award groups, including `awards_doctor` (all doctoral and first-professional degrees), which is comparable across the change. First majors only, so double majors aren't double counted. |
| **First-professional students** counted separately from graduate students before 2010. | `enr_grad` is total minus undergraduate enrollment, so it's comparable in every year. |
| **Admissions** moved from IC to the ADM component in 2014. | Read from IC through 2013 and ADM from 2014. Totals are derived from men + women where the total is missing. |
| **SAT redesign** (March 2016). | `sat_version` flags pre-2016, transition and redesigned scores. |
| **Test-score policy** wording and codes changed ("Neither required nor recommended" became "Not considered"; "Considered but not required" added). | Mapped to common categories using each year's value labels. |
| **Locale** codes changed from metro-centric (1–8) to urban-centric (11–43) in 2006. | `locale4`: City / Suburb / Town / Rural. |
| **Carnegie classification** editions (2000, 2005, 2010, 2015, 2018, 2021). | `carnegie_basic` from the newest edition in each file, plus `carnegie_edition`, and `carnegie_group` (coarse groups comparable across editions). |
| **Parent/child reporting**: branch campuses reported with a parent institution. | `prch_*` flags (module `"flags"`). `child_records = "na"` blanks the affected values. |
| **Provisional vs. revised releases.** | Revised (`_rv`) files are preferred. Cached files are re-checked against NCES after `getOption("ipeds.max_age", 30)` days, so final data replace provisional data automatically. |
| **Dollar values** | `ipeds_real_dollars()` / `ipeds_deflate()` with the bundled CPI-U (or your own index). |

### Modules

`ipeds_panel(modules = ...)` combines any of the following, each also
available as its own function (see `?ipeds_modules`):

| Module | Source | Examples of variables |
|---|---|---|
| `directory` | HD | name, state, county, sector, control, level, HBCU, tribal, `locale4`, Carnegie, closure/merger fields |
| `enrollment` | EF A | fall headcount by level and attendance status, first-time students, race/ethnicity × sex |
| `enrollment_12m` | EFFY, EFIA | 12-month unduplicated headcount, FTE, credit hours |
| `retention` | EF D | full- and part-time retention rates, student-to-faculty ratio |
| `completions` | C A | awards by harmonized level; bachelor's degrees by race/ethnicity × sex |
| `admissions` | IC / ADM | applicants, admits, enrollees, admission and yield rates, SAT/ACT, test policy |
| `charges` | IC AY | tuition and fees (in-district, in-state, out-of-state, graduate), room and board |
| `aid` | SFA | Pell and federal loan shares, average grant aid, net price |
| `grad_rates` | GR | 150% graduation rates (bachelor's and 2-year cohorts), optionally by race/ethnicity |
| `finance` | F1A / F2 / F3 | *experimental*: revenues, tuition revenue, state appropriations, expenses, instruction, endowment |
| `flags` | FLAGS | parent/child reporting indicators |

`ipeds_completions_programs()` returns completions by CIP code (6-, 4- or
2-digit) and harmonized award level in long format.

### Year convention

`year` is the year in, or implied by, the IPEDS file name. The row for 2023
combines fall 2023 directory and enrollment data (`HD2023`, `EF2023A`) with
2022–23 completions (`C2023_A`), 12-month enrollment (`EFFY2023`), financial
aid (`SFA2223`) and fiscal year 2023 finance (`F2223_*`). The `period`
column of `ipeds_codebook()` gives the reference period of every variable.

## Verification and audit trail

IPEDS variable names and codes are not perfectly stable. Instead of trusting
a fixed list, each harmonized measure is looked up in the official dictionary
of the file it comes from (the Stata program file NCES publishes with every
data file):

* The expected variable or code is used only if its label matches (e.g.
  `efalevel = 24` must be labelled *full-time … first-time*).
* If it doesn't match, the dictionary is searched for a unique variable or
  code with the expected label, and the switch is logged as `relabeled`.
* If nothing suitable is found, the value is left `NA` and logged, rather
  than silently taking the wrong column.
* Consistency checks (full-time + part-time = total, men + women = total,
  completers ≤ cohort, rates within range) blank values that fail them.

`ipeds_audit()` shows every decision, and `ipeds_validate(years)`
summarizes, for each measure, how many years were verified, relabeled or
missing. Run `ipeds_validate()` after each new IPEDS release to confirm that
everything is still picked up correctly.

## Raw access

```r
ipeds_components()                      # catalog of IPEDS files known to the package
hd <- ipeds_read("HD", 2023)            # any file, with variable and value labels
table(ipeds_as_factor(hd$sector))
ipeds_dictionary("C_A", 2023)           # variable titles and value labels
ef <- ipeds_stack("EFD", 2010:2023, vars = c("ret_pcf", "ret_pcp"))
ipeds_read_file("EF99_ANR")             # files outside the catalog, by name
```

## Options

| Option | Default | Meaning |
|---|---|---|
| `ipeds.cache_dir` | `tools::R_user_dir("ipeds", "cache")` | where files are stored (also `IPEDS_CACHE_DIR`) |
| `ipeds.max_age` | `30` | days before cached files are re-checked against NCES |
| `ipeds.refresh` | `"auto"` | `"auto"`, `"check"`, `"never"` or `"always"` |
| `ipeds.timeout` / `ipeds.retries` / `ipeds.pause` | `600` / `3` / `0.5` | download behavior |
| `ipeds.base_url` | `https://nces.ed.gov/ipeds/datacenter/data/` | NCES data file location |

## Caveats

* **Mapping status.** The variable and code mappings follow NCES
  documentation, the NCES file listings and the package's test fixtures. The
  runtime label checks guard against mistakes, but run `ipeds_validate()` on
  your years and review anything `relabeled` or missing before relying on a
  measure.
* **Graduation rates** are identified from value labels, so they need the
  dictionaries (`verify = TRUE`, the default).
* **Finance** is experimental. GASB, FASB and for-profit forms define items
  differently, so compare within `fin_form`.
* **Harmonized modules** are designed for 2000 onward. Older files have
  different layouts; use `ipeds_read_file()` for them.
* **Sex.** Measures by sex use IPEDS's men/women categories. The "another
  gender" and "gender unknown" items added in recent collections are not
  included.
* **CIP codes** are reported in the edition used each year (`cip_edition`).
  Two-digit families are the most comparable level across editions.

## Earlier versions

`getIPEDSSurvey()`, `downloadIPEDSSurvey()`, `downloadAllSurveys()`,
`ipedsHelp()`, the `recode*()` helpers, the `surveys`, `cipcodes` and
`crosswalk` datasets, and the XML submission functions (`completions()`,
`twelveMonthEnrollment()`, `fallEnrollment()`) still work. Files now go to the
user cache instead of the installed package directory, and several bugs in
these functions have been fixed (see `NEWS.md`).

## Acknowledgements

Originally written by Jason Bryer. The list of IPEDS file names draws on Ben
Skinner's [downloadipeds](https://github.com/btskinner/downloadipeds).

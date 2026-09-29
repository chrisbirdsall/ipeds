# Development workflow (not part of the package build)

# Regenerate documentation and NAMESPACE
roxygen2::roxygenise()

# Rebuild the datasets in data/ (see data-raw/)
source("data-raw/datasets.R")

# Run the offline test suite (uses a synthetic IPEDS server, no network)
devtools::test()

# Full check
devtools::check()

# Against the live NCES data center: confirm that every harmonized measure is
# still identified correctly after a new IPEDS release
library(ipeds)
v <- ipeds_validate(2004:ipeds_latest_year("HD"))
subset(v, relabeled > 0 | problem > 0)

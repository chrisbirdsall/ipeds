# Regenerates the datasets in data/. Run from the package root with
#   Rscript data-raw/datasets.R

pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE)

# Legacy `surveys` catalog, derived from ipeds_components()
comp <- ipeds_components()
tmpl <- comp$template
surveys <- data.frame(
	SurveyID = comp$id,
	Survey = comp$group,
	Title = comp$title,
	DataFilePre = sub("\\{.*$", "", tmpl),
	DataFilePost = sub("^.*\\}", "", tmpl),
	YearFormat = ifelse(grepl("{PY}{YY}", tmpl, fixed = TRUE), 22L,
						ifelse(grepl("{YYYY}", tmpl, fixed = TRUE), 4L, 2L)),
	stringsAsFactors = FALSE)

# CIP codes and the CIP 2000 -> 2010 crosswalk (originally distributed as
# semicolon separated files)
read_legacy <- function(f) {
	utils::read.table(f, sep = ";", header = TRUE, quote = "\"", stringsAsFactors = FALSE,
					  encoding = "UTF-8", comment.char = "")
}
cipcodes <- read_legacy("data-raw/cipcodes.csv")
crosswalk <- read_legacy("data-raw/crosswalk.csv")

save(surveys, file = "data/surveys.rda", compress = "xz")
save(cipcodes, file = "data/cipcodes.rda", compress = "xz")
save(crosswalk, file = "data/crosswalk.rda", compress = "xz")

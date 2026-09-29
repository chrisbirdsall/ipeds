# Recoding IPEDS directory codes into labelled factors ---------------------------------

#' Recode IPEDS codes into labelled factors
#'
#' Helpers that convert common IPEDS directory (HD) codes into factors with
#' descriptive labels. `recodeDirectory()` applies all of them to a directory
#' file read with [ipeds_read()]. The harmonized [ipeds_directory()] module
#' is usually more convenient.
#'
#' @param col a vector of IPEDS codes.
#' @param hd a directory (HD) data frame with lower-case column names.
#' @return A factor (or, for `recodeDirectory()`, the recoded data frame).
#' @examples
#' recodeSector(c(1, 2, 4, 99))
#' recodeControl(c(1, 2, 3, -3))
#' @name recode
NULL

.recode <- function(col, levels, labels) factor(col, levels = levels, labels = labels)

#' @rdname recode
#' @export
recodeDirectory <- function(hd) {
	recoders <- list(control = recodeControl, obereg = recodeGeographicRegion,
					 opeflag = recodeTitleIVEligibility, sector = recodeSector,
					 iclevel = recodeLevelOfInstitution, hloffer = recodeHighestLevelOfOffering,
					 ugoffer = recodeUndergraduateOffering, groffer = recodeGraduateOffering,
					 hdegofr1 = recodeHighestDegreeOffered, openpubl = recodeOpenPublic,
					 pset4flg = recodeTitleIVIndicator, instsize = recodeInstitutionSize)
	for (v in intersect(names(recoders), names(hd))) hd[[v]] <- recoders[[v]](hd[[v]])
	hd
}

#' @rdname recode
#' @export
recodeImputation <- function(col) {
	.recode(col, c("A", "B", "C", "D", "G", "H", "J", "K", "L", "N", "P", "R", "Z"),
			c("Not applicable", "Institution left item blank", "Analyst corrected reported value",
			  "Do not know", "Data generated from other data values",
			  "Value not derived - data not usable", "Logical imputation", "Ratio adjustment",
			  "Imputed using the Group Median procedure",
			  "Imputed using Nearest Neighbor procedure",
			  "Imputed using Carry Forward procedure", "Reported", "Implied zero"))
}

#' @rdname recode
#' @export
recodeGeographicRegion <- function(col) {
	.recode(col, 0:9,
			c("US Service schools", "New England CT ME MA NH RI VT", "Mid East DE DC MD NJ NY PA",
			  "Great Lakes IL IN MI OH WI", "Plains IA KS MN MO NE ND SD",
			  "Southeast AL AR FL GA KY LA MS NC SC TN VA WV", "Southwest AZ NM OK TX",
			  "Rocky Mountains CO ID MT UT WY", "Far West AK CA HI NV OR WA",
			  "Outlying areas AS FM GU MH MP PR PW VI"))
}

#' @rdname recode
#' @export
recodeControl <- function(col) {
	.recode(col, c(1, 2, 3, -3),
			c("Public", "Private not-for-profit", "Private for-profit", "Not available"))
}

#' @rdname recode
#' @export
recodeTitleIVEligibility <- function(col) {
	.recode(col, c(1, 2, 3, 8, 5, 6, 7),
			c("Participates in Title IV federal financial aid programs",
			  "Branch campus of a main campus that participates in Title IV",
			  "Deferment only - limited participation",
			  "New participants (became eligible during spring collection)",
			  "Not currently participating in Title IV, has an OPE ID number",
			  "Not currently participating in Title IV, does not have OPE ID number",
			  "Stopped participating during the survey year"))
}

#' @rdname recode
#' @export
recodeSector <- function(col) {
	.recode(col, c(0:9, 99), unname(.sector_map))
}

#' @rdname recode
#' @export
recodeLevelOfInstitution <- function(col) {
	.recode(col, c(1, 2, 3, -3), c(unname(.iclevel_map), "{Not available}"))
}

#' @rdname recode
#' @export
recodeHighestLevelOfOffering <- function(col) {
	.recode(col, c(1:9, -3),
			c("Award of less than one academic year", "At least 1, but less than 2 academic yrs",
			  "Associate's degree", "At least 2, but less than 4 academic yrs", "Bachelor's degree",
			  "Postbaccalaureate certificate", "Master's degree", "Post-master's certificate",
			  "Doctor's degree", "{Not available}"))
}

#' @rdname recode
#' @export
recodeUndergraduateOffering <- function(col) {
	.recode(col, c(1, 2, -3), c("Undergraduate degree or certificate offering",
								"No undergraduate offering", "{Not available}"))
}

#' @rdname recode
#' @export
recodeGraduateOffering <- function(col) {
	.recode(col, c(1, 2, -3), c("Graduate degree or certificate offering",
								"No graduate offering", "{Not available}"))
}

#' @rdname recode
#' @export
recodeHighestDegreeOffered <- function(col) {
	.recode(col, c(0, 11, 12, 13, 14, 20, 30, 40, -3),
			c("Non-degree granting",
			  "Doctor's degree - research/scholarship and professional practice",
			  "Doctor's degree - research/scholarship", "Doctor's degree - professional practice",
			  "Doctor's degree - other", "Master's degree", "Bachelor's degree",
			  "Associate's degree", "{Not available}"))
}

#' @rdname recode
#' @export
recodeOpenPublic <- function(col) {
	.recode(col, c(1, 0), c("Institution is open to the public",
							"Institution is not open to the public"))
}

#' @rdname recode
#' @export
recodeTitleIVIndicator <- function(col) {
	.recode(col, c(1, 2, 3, 4, 6, 9),
			c("Title IV postsecondary institution", "Non-Title IV postsecondary institution",
			  "Title IV NOT primarily postsecondary institution",
			  "Non-Title IV NOT primarily postsecondary institution",
			  "Non-Title IV postsecondary institution that is NOT open to the public",
			  "Institution is not active in current universe"))
}

#' @rdname recode
#' @export
recodeInstitutionSize <- function(col) {
	.recode(col, c(1:5, -1, -2), c(unname(.instsize_map), "Not reported", "Not applicable"))
}

#' @rdname recode
#' @export
recodeAwardLevel <- function(col) {
	.recode(col, c(1, 20, 21, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 17, 18, 19),
			c("Award of less than 1 academic year", "Certificate of less than 12 weeks",
			  "Certificate of at least 12 weeks but less than 1 year",
			  "Award of at least 1 but less than 2 academic years", "Associate's degree",
			  "Award of at least 2 but less than 4 academic years", "Bachelor's degree",
			  "Postbaccalaureate certificate", "Master's degree", "Post-master's certificate",
			  "Doctor's degree (before 2010)", "First-professional degree (before 2010)",
			  "First-professional certificate (before 2010)",
			  "Doctor's degree - research/scholarship", "Doctor's degree - professional practice",
			  "Doctor's degree - other"))
}

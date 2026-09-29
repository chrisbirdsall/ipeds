# Generating XML files for submitting data to IPEDS -----------------------------------
#
# These functions build XML documents in the PESC formats that IPEDS accepted
# for bulk upload of the Completions, 12-month Enrollment and Fall Enrollment
# components (schemas from the 2010-2012 collections). Check the current
# IPEDS upload specifications before submitting.

.ethnicity_levels <- c("HispanicAnyRace", "AmericanIndianOrAlaskaNative", "Asian",
					   "BlackOrAfricanAmerican", "NativeHawaiianOrPacificIslander", "White",
					   "TwoOrMoreRaces", "NonresidentAlien", "RaceEthnicityUnknown")

.need_xml <- function() {
	if (!requireNamespace("XML", quietly = TRUE)) {
		stop("Install the 'XML' package to generate IPEDS XML files.", call. = FALSE)
	}
}

#' Map data frame columns to IPEDS reporting fields
#'
#' Creates the mapping between the columns of a student-level data frame and
#' the fields used by [completions()], [twelveMonthEnrollment()] and
#' [fallEnrollment()].
#'
#' @param programCIPCode column containing CIP codes.
#' @param educationalProgramType column containing program types.
#' @param educationalProgramName column containing program names.
#' @param educationalAwardLevel column containing IPEDS award levels.
#' @param ethnicityColumn column containing race/ethnicity, coded with the
#'   IPEDS XML element names (`"HispanicAnyRace"`, `"Asian"`, `"White"`, ...).
#' @param genderColumn column containing gender.
#' @return A data frame with one row per IPEDS field and a `MappedColumn`
#'   column.
#' @export
mapIPEDSFields <- function(programCIPCode, educationalProgramType, educationalProgramName,
						   educationalAwardLevel, ethnicityColumn, genderColumn) {
	fields <- c("ProgramCIPCode", "EducationalProgramType", "EducationalProgramName",
				"EducationalAwardLevel", "Ethnicity", "Gender")
	data.frame(MappedColumn = c(programCIPCode, educationalProgramType, educationalProgramName,
								educationalAwardLevel, ethnicityColumn, genderColumn),
			   row.names = fields, stringsAsFactors = FALSE)
}

.xml_header <- function(root_name, urn, schema, documentId, createdDate, transmissionType,
						documentType, noteMessage = NA) {
	prefix <- sub(":.*$", "", root_name)
	doc <- XML::newXMLDoc()
	root <- XML::newXMLNode(root_name, doc = doc,
							namespaceDefinitions = c(xsi = "http://www.w3.org/2001/XMLSchema-instance",
													 stats::setNames(urn, prefix)),
							attrs = c("xsi:schemaLocation" = paste(urn, schema)))
	td <- XML::newXMLNode("TransmissionData", parent = root)
	XML::newXMLNode("DocumentID", text = documentId, parent = td)
	XML::newXMLNode("CreatedDateTime", text = createdDate, parent = td)
	XML::newXMLNode("TransmissionType", text = transmissionType, parent = td)
	XML::newXMLNode("DocumentType", text = documentType, parent = td)
	if (!is.na(noteMessage)) XML::newXMLNode("NoteMessage", text = noteMessage, parent = td)
	list(doc = doc, root = root)
}

# Counts of men and women by race/ethnicity added under `parent`.
.xml_race_gender <- function(parent, ethnicity, gender, genderValues) {
	for (e in .ethnicity_levels) {
		node <- XML::newXMLNode(e, parent = parent)
		XML::newXMLNode("CountMale", text = sum(ethnicity == e & gender == genderValues[["male"]],
												na.rm = TRUE), parent = node)
		XML::newXMLNode("CountFemale", text = sum(ethnicity == e & gender == genderValues[["female"]],
												  na.rm = TRUE), parent = node)
	}
}

.check_reporting_args <- function(data, mappings, institutionId) {
	if (is.null(data)) stop("`data` is required.", call. = FALSE)
	if (is.null(mappings)) stop("`mappings` is required; see mapIPEDSFields().", call. = FALSE)
	if (is.na(institutionId)) stop("`institutionId` is required.", call. = FALSE)
	missing <- setdiff(stats::na.omit(mappings$MappedColumn), names(data))
	if (length(missing)) stop("Columns not found in data: ", paste(missing, collapse = ", "), call. = FALSE)
}

.col <- function(data, mappings, field) {
	m <- mappings[field, "MappedColumn"]
	if (is.na(m)) return(rep(NA, nrow(data)))
	data[[m]]
}

#' Generate IPEDS XML submission files
#'
#' Build XML documents for uploading Completions, 12-month Enrollment or Fall
#' Enrollment data to IPEDS from student-level data. These implement the
#' schemas used in the 2010-2012 collections; verify them against the current
#' IPEDS upload specifications before submitting.
#'
#' @param data a data frame with one row per student (or award).
#' @param mappings field mapping created by [mapIPEDSFields()].
#' @param institutionId the IPEDS unit ID of the institution.
#' @param institutionName the name of the institution.
#' @param documentId user specified document ID.
#' @param createdDate creation timestamp.
#' @param noteMessage optional note.
#' @param transmissionType `"Original"` for the first upload or `"Replace"`.
#' @param documentType document type.
#' @param genderValues the values of the gender column that denote men and
#'   women.
#' @param reportingPeriod for `twelveMonthEnrollment()`, the reporting period.
#' @param undergraduateContactHours,undergraduateCreditHours,graduateCreditHours
#'   for `twelveMonthEnrollment()`, instructional activity totals.
#' @return An `XMLInternalDocument` (see [XML::saveXML()] to write it).
#' @name ipeds_xml
NULL

#' @rdname ipeds_xml
#' @export
completions <- function(data, mappings = NULL, institutionId = NA, institutionName = NA,
						documentId = "a", createdDate = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
						noteMessage = NA, transmissionType = "Original", documentType = "Application",
						genderValues = c(male = "MALE", female = "FEMALE")) {
	.need_xml()
	.check_reporting_args(data, mappings, institutionId)
	cip <- .col(data, mappings, "ProgramCIPCode")
	if (anyNA(cip)) stop("All records must have a valid CIP code.", call. = FALSE)
	level <- .col(data, mappings, "EducationalAwardLevel")
	x <- .xml_header("CN:EducationalAwards", "urn:org:pesc:message:CompletionIPEDS:v1.0.0",
					 "Completions.xsd", documentId, createdDate, transmissionType, documentType)
	awards <- XML::newXMLNode("EducationalAwardsByInstitution", parent = x$root)
	inst <- XML::newXMLNode("Institution", parent = awards)
	XML::newXMLNode("IPEDSUnitID", text = institutionId, parent = inst)
	if (!is.na(institutionName)) XML::newXMLNode("OrganizationName", text = institutionName, parent = inst)
	if (!is.na(noteMessage)) XML::newXMLNode("NoteMessage", text = noteMessage, parent = inst)
	for (i in unique(cip)) {
		for (l in unique(level[cip == i])) {
			rows <- which(cip == i & level == l)
			tmp <- data[rows, , drop = FALSE]
			award <- XML::newXMLNode("EducationalAwardsByProgram", parent = awards)
			program <- XML::newXMLNode("EducationalProgram", parent = award)
			XML::newXMLNode("ProgramCIPCode", text = i, parent = program)
			XML::newXMLNode("EducationalProgramType",
							text = .col(tmp, mappings, "EducationalProgramType")[1], parent = program)
			programName <- .col(tmp, mappings, "EducationalProgramName")[1]
			if (!is.na(programName) && nzchar(programName)) {
				XML::newXMLNode("EducationalProgramName", text = substr(programName, 1, 60), parent = program)
			}
			XML::newXMLNode("EducationalAwardLevel", text = l, parent = award)
			dist <- XML::newXMLNode("RaceGenderDistribution", parent = award)
			.xml_race_gender(dist, .col(tmp, mappings, "Ethnicity"), .col(tmp, mappings, "Gender"),
							 genderValues)
		}
	}
	x$doc
}

.enrollment_split <- function(data, mappings) {
	level <- suppressWarnings(as.numeric(.col(data, mappings, "EducationalAwardLevel")))
	list(ug = data[which(level < 6), , drop = FALSE], grad = data[which(level > 5), , drop = FALSE])
}

#' @rdname ipeds_xml
#' @export
twelveMonthEnrollment <- function(data, mappings = NULL, institutionId = NA, institutionName = NA,
								  documentId = "a",
								  createdDate = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
								  noteMessage = NA, transmissionType = "Original",
								  documentType = "Application", reportingPeriod = "July-June",
								  undergraduateContactHours = NULL, undergraduateCreditHours = NULL,
								  graduateCreditHours = NULL,
								  genderValues = c(male = "MALE", female = "FEMALE")) {
	.need_xml()
	.check_reporting_args(data, mappings, institutionId)
	x <- .xml_header("EN12_4:TwelveMonthEnrollment4Year",
					 "urn:org:pesc:message:12MonthEnrollment4Year:v1.0.0",
					 "12MonthEnrollment4Year_v1.0.0.xsd", documentId, createdDate, transmissionType, documentType, noteMessage)
	s <- .enrollment_split(data, mappings)
	enrollment <- XML::newXMLNode("EnrollmentByInstitution", parent = x$root)
	inst <- XML::newXMLNode("Institution", parent = enrollment)
	XML::newXMLNode("IPEDSUnitID", text = institutionId, parent = inst)
	if (!is.na(institutionName)) XML::newXMLNode("OrganizationName", text = institutionName, parent = inst)
	counts <- XML::newXMLNode("UnduplicatedCounts", parent = enrollment)
	if (nrow(s$ug) > 0) {
		.xml_race_gender(XML::newXMLNode("UndergraduateEnrollment", parent = counts),
						 .col(s$ug, mappings, "Ethnicity"), .col(s$ug, mappings, "Gender"), genderValues)
	}
	if (nrow(s$grad) > 0) {
		.xml_race_gender(XML::newXMLNode("GraduateEnrollment", parent = counts),
						 .col(s$grad, mappings, "Ethnicity"), .col(s$grad, mappings, "Gender"), genderValues)
	}
	if (!is.null(undergraduateContactHours) || !is.null(undergraduateCreditHours) ||
		!is.null(graduateCreditHours)) {
		activity <- XML::newXMLNode("InstructionalActivity", parent = enrollment)
		XML::newXMLNode("ReportingPeriod", text = reportingPeriod, parent = activity)
		if (!is.null(undergraduateContactHours)) {
			XML::newXMLNode("UndergraduateContactHours", text = undergraduateContactHours, parent = activity)
		}
		if (!is.null(undergraduateCreditHours)) {
			XML::newXMLNode("UndergraduateCreditHours", text = undergraduateCreditHours, parent = activity)
		}
		if (!is.null(graduateCreditHours)) {
			XML::newXMLNode("GraduateCreditHours", text = graduateCreditHours, parent = activity)
		}
	}
	x$doc
}

#' @rdname ipeds_xml
#' @details `fallEnrollment()` is incomplete: it only reports part-time
#'   undergraduate transfer students and graduate students.
#' @export
fallEnrollment <- function(data, mappings = NULL, institutionId = NA, institutionName = NA,
						   documentId = "a", createdDate = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
						   noteMessage = NA, transmissionType = "Original", documentType = "IPEDS",
						   genderValues = c(male = "MALE", female = "FEMALE")) {
	.need_xml()
	.check_reporting_args(data, mappings, institutionId)
	x <- .xml_header("FE4:FallEnrollment", "urn:org:pesc:message:FallEnrollment4Year:v1.1.0",
					 "FallEnrollment4Year_v1.1.0.xsd", documentId, createdDate, transmissionType, documentType, noteMessage)
	s <- .enrollment_split(data, mappings)
	enrollment <- XML::newXMLNode("EnrollmentByInstitution", parent = x$root)
	inst <- XML::newXMLNode("Institution", parent = enrollment)
	XML::newXMLNode("IPEDSUnitID", text = institutionId, parent = inst)
	if (!is.na(institutionName)) XML::newXMLNode("OrganizationName", text = institutionName, parent = inst)
	counts <- XML::newXMLNode("EnrollmentPartTime", parent = enrollment)
	if (nrow(s$ug) > 0) {
		ug <- XML::newXMLNode("UndergraduateEnrollment", parent = counts)
		degree <- XML::newXMLNode("DegreeCertificateSeeking", parent = ug)
		.xml_race_gender(XML::newXMLNode("TransferStudent", parent = degree),
						 .col(s$ug, mappings, "Ethnicity"), .col(s$ug, mappings, "Gender"), genderValues)
	}
	if (nrow(s$grad) > 0) {
		.xml_race_gender(XML::newXMLNode("GraduateEnrollment", parent = counts),
						 .col(s$grad, mappings, "Ethnicity"), .col(s$grad, mappings, "Gender"), genderValues)
	}
	x$doc
}

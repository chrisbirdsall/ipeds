# Label verification and audit trail ---------------------------------------------
#
# Every harmonized measure is located with .pick_var() / .pick_code(). A
# candidate variable (or code) is accepted only if its label in the IPEDS
# dictionary for that very file matches a keyword specification. If it does
# not, the dictionary is searched for a unique variable (or code) whose label
# matches, and otherwise the measure is left missing. Each decision is
# recorded in an audit table so users can see exactly where every number
# came from.

.audit_new <- function() {
	e <- new.env(parent = emptyenv())
	e$rows <- list()
	e
}

.audit <- function(ctx, concept, source = NA_character_, status, detail = "") {
	a <- ctx$audit
	if (is.null(a)) return(invisible(NULL))
	a$rows[[length(a$rows) + 1L]] <- list(
		module = ctx$module %||% NA_character_,
		component = ctx$component %||% NA_character_,
		year = as.integer(ctx$year %||% NA_integer_),
		file = ctx$file %||% NA_character_,
		concept = concept,
		source = paste(stats::na.omit(source), collapse = ", "),
		status = status,
		detail = paste(detail, collapse = "; "))
	invisible(NULL)
}

.audit_table <- function(audit) {
	cols <- c("module", "component", "year", "file", "concept", "source", "status", "detail")
	if (is.null(audit) || length(audit$rows) == 0) {
		out <- as.data.frame(stats::setNames(replicate(length(cols), character(), simplify = FALSE), cols),
							 stringsAsFactors = FALSE)
		out$year <- integer()
		return(out)
	}
	out <- do.call(rbind, lapply(audit$rows, as.data.frame, stringsAsFactors = FALSE))
	out[, cols]
}

# Statuses that mean the value was not produced as intended.
.audit_problem_status <- c("not_found", "ambiguous", "check_failed", "file_unavailable")

# Find a variable in `df` for a concept. `candidates` are tried in order and
# accepted when their dictionary label satisfies `spec`. If none does and
# `search` is TRUE, the dictionary is searched for a unique match.
.pick_var <- function(df, ctx, concept, candidates, spec, search = TRUE) {
	dict <- ctx$dict
	nms <- names(df)
	present <- candidates[candidates %in% nms]
	rejected <- character()
	for (v in present) {
		lab <- .dict_label(dict, v)
		if (is.null(dict) || is.na(lab)) {
			.audit(ctx, concept, v, "unverified",
				   if (is.null(dict)) "no dictionary available" else "variable not in dictionary")
			return(v)
		}
		if (.label_match(lab, spec$all, spec$none)) {
			.audit(ctx, concept, v, "verified", lab)
			return(v)
		}
		rejected <- c(rejected, sprintf("%s ('%s')", v, lab))
	}
	if (search && !is.null(dict)) {
		cand <- dict$variables[dict$variables$varname %in% nms &
							   	!.is_flag(dict$variables$varname, nms), , drop = FALSE]
		ok <- vapply(cand$label, .label_match, logical(1), all = spec$all, none = spec$none)
		hits <- cand$varname[ok]
		if (length(hits) == 1) {
			.audit(ctx, concept, hits, "relabeled",
				   c(sprintf("found by label '%s'", cand$label[ok]),
				     if (length(rejected)) paste("rejected", paste(rejected, collapse = ", "))))
			return(hits)
		}
		if (length(hits) > 1) {
			.audit(ctx, concept, hits, "ambiguous",
				   c("several variables match the expected label", rejected))
			return(NA_character_)
		}
	}
	.audit(ctx, concept, candidates, "not_found",
		   if (length(rejected)) paste("label mismatch:", paste(rejected, collapse = ", "))
		   else "none of the expected variables is in the file")
	NA_character_
}

# Convenience wrapper returning the (numeric) column or NULL.
.get_var <- function(df, ctx, concept, candidates, spec, search = TRUE, numeric = TRUE) {
	v <- .pick_var(df, ctx, concept, candidates, spec, search)
	if (is.na(v)) return(NULL)
	if (numeric) .as_num(df[[v]]) else df[[v]]
}

# Find the code of coded variable `var` for a concept. Codes in `codes` are
# tried in order; if none has a matching value label the dictionary's value
# labels are searched for a unique match among codes present in the data.
.pick_code <- function(df, var, ctx, concept, codes, spec) {
	dict <- ctx$dict
	x <- .as_num(df[[var]])
	present <- unique(x[!is.na(x)])
	vl <- .dict_values(dict, var)
	vl_code <- .as_num(vl$code)
	rejected <- character()
	for (code in codes) {
		if (!(code %in% present)) next
		lab <- vl$label[match(code, vl_code)]
		if (is.null(dict) || nrow(vl) == 0 || is.na(lab)) {
			.audit(ctx, concept, sprintf("%s = %s", var, code), "unverified",
				   if (is.null(dict)) "no dictionary available" else "code has no value label")
			return(code)
		}
		if (.label_match(lab, spec$all, spec$none)) {
			.audit(ctx, concept, sprintf("%s = %s", var, code), "verified", lab)
			return(code)
		}
		rejected <- c(rejected, sprintf("%s ('%s')", code, lab))
	}
	if (nrow(vl) > 0) {
		ok <- vapply(vl$label, .label_match, logical(1), all = spec$all, none = spec$none) &
			vl_code %in% present
		hits <- unique(vl_code[ok])
		if (length(hits) == 1) {
			if (length(codes) == 0) {
				.audit(ctx, concept, sprintf("%s = %s", var, hits), "verified",
					   sprintf("selected by label '%s'", vl$label[ok][1]))
			} else {
				.audit(ctx, concept, sprintf("%s = %s", var, hits), "relabeled",
					   c(sprintf("found by label '%s'", vl$label[ok][1]),
					     if (length(rejected)) paste("rejected", paste(rejected, collapse = ", "))))
			}
			return(hits)
		}
		if (length(hits) > 1) {
			.audit(ctx, concept, paste(var, "=", hits), "ambiguous",
				   c("several codes match the expected label", rejected))
			return(NA_real_)
		}
	}
	.audit(ctx, concept, var, "not_found",
		   if (length(rejected)) paste("value label mismatch:", paste(rejected, collapse = ", "))
		   else if (is.null(dict) && length(codes) == 0) "a dictionary is required to identify this code"
		   else "expected code not present")
	NA_real_
}

# Does code `code` of `var` carry a label matching `spec`? Returns TRUE when
# it matches, FALSE when it does not, and NA when there is no label to check.
.code_label_ok <- function(dict, var, code, spec) {
	vl <- .dict_values(dict, var)
	if (nrow(vl) == 0) return(NA)
	lab <- vl$label[match(code, .as_num(vl$code))]
	if (is.na(lab)) return(NA)
	.label_match(lab, spec$all, spec$none)
}

#' Audit trail of a harmonized IPEDS table
#'
#' Every panel or module result records, for each file and harmonized
#' concept, which source variable or code was used and whether its label was
#' verified against the IPEDS dictionary for that file. Statuses are:
#' \describe{
#'   \item{verified}{the expected variable/code was found and its dictionary
#'     label matched.}
#'   \item{unverified}{the expected variable/code was used but no dictionary
#'     label was available to check it.}
#'   \item{relabeled}{the expected variable/code was absent or its label did
#'     not match, and a different one with the expected label was used.}
#'   \item{not_found, ambiguous}{nothing suitable was found; the measure is
#'     `NA` for that year.}
#'   \item{check_failed}{a consistency check failed (for example full-time
#'     plus part-time enrollment not adding up to the total) and the affected
#'     values were set to `NA`.}
#'   \item{derived, note, loaded}{informational.}
#'   \item{file_unavailable}{the file could not be downloaded (usually because
#'     it has not been released yet).}
#' }
#'
#' @param x a result of [ipeds_panel()] or one of the module functions.
#' @param problems_only only return rows describing problems.
#' @return A data frame.
#' @export
ipeds_audit <- function(x, problems_only = FALSE) {
	a <- attr(x, "ipeds_audit")
	if (is.null(a)) a <- .audit_table(NULL)
	if (problems_only) a <- a[a$status %in% .audit_problem_status, , drop = FALSE]
	rownames(a) <- NULL
	a
}

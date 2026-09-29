# Build a harmonized panel and look at the composition of fall enrollment
library(ipeds)

latest <- ipeds_latest_year("EFA")
p <- ipeds_panel(2004:latest, modules = c("directory", "enrollment"), race_by_sex = FALSE)
print(p)

# Totals by year: asian_pi is comparable across the 2008-2010 change in
# race/ethnicity categories; two_more exists only after the change.
vars <- c("enr_total", "enr_white", "enr_black", "enr_hispanic", "enr_asian_pi",
		  "enr_two_more", "enr_nonres")
tot <- aggregate(p[vars], by = list(year = p$year), FUN = sum, na.rm = TRUE)
shares <- cbind(year = tot$year, round(100 * tot[vars[-1]] / tot$enr_total, 1))
print(shares)

if (requireNamespace("ggplot2", quietly = TRUE)) {
	long <- stats::reshape(shares, direction = "long", varying = names(shares)[-1],
						   v.names = "percent", timevar = "group",
						   times = sub("^enr_", "", names(shares)[-1]), idvar = "year")
	print(ggplot2::ggplot(long, ggplot2::aes(year, percent, colour = group)) +
		  	ggplot2::geom_line() +
		  	ggplot2::labs(x = "Fall", y = "Percent of fall enrollment",
		  				  caption = "Source: IPEDS fall enrollment (EF A), harmonized with the ipeds package"))
}

# Where did each number come from?
print(head(ipeds_audit(p, problems_only = TRUE)))

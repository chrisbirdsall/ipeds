# The relationship between SAT scores and first-year retention
library(ipeds)

year <- ipeds_latest_year("ADM")
p <- ipeds_panel(year, modules = c("directory", "admissions", "retention"))
p <- p[!is.na(p$sat_math_25) & !is.na(p$ret_ft_pct) & p$ret_ft_pct >= 20, ]

# IPEDS reports the 25th and 75th percentiles; use their means as a proxy
p$sat_math <- (p$sat_math_25 + p$sat_math_75) / 2
p$sat_rw <- (p$sat_rw_25 + p$sat_rw_75) / 2
p$sat_total <- p$sat_math + p$sat_rw

fit <- lm(ret_ft_pct ~ sat_total + admit_rate + control, data = p, weights = enrolled)
print(summary(fit))

if (requireNamespace("ggplot2", quietly = TRUE)) {
	print(ggplot2::ggplot(p, ggplot2::aes(sat_total, ret_ft_pct, colour = test_policy)) +
		  	ggplot2::geom_point(alpha = .5) +
		  	ggplot2::geom_smooth(method = "loess", se = FALSE) +
		  	ggplot2::labs(x = "SAT (midpoint of 25th and 75th percentiles)",
		  				  y = "Full-time retention rate (%)"))
}

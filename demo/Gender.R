# Share of bachelor's degrees awarded to women in mathematics, statistics and
# computer and information sciences
library(ipeds)

years <- 2000:ipeds_latest_year("C_A")
prog <- ipeds_completions_programs(years, cip_digits = 6)
prog <- prog[prog$award_level == "bach", ]

prog$subject <- NA_character_
prog$subject[substr(prog$cip, 1, 5) %in% c("27.01", "27.03", "27.99")] <- "Math"
prog$subject[substr(prog$cip, 1, 5) == "27.05"] <- "Stats"
prog$subject[substr(prog$cip, 1, 2) == "11"] <- "CIS"
prog <- prog[!is.na(prog$subject), ]

sums <- aggregate(cbind(awards, awards_women) ~ subject + year, data = prog, FUN = sum)
sums$pct_women <- 100 * sums$awards_women / sums$awards
print(sums)

if (requireNamespace("ggplot2", quietly = TRUE)) {
	print(ggplot2::ggplot(sums, ggplot2::aes(year, pct_women, colour = subject)) +
		  	ggplot2::geom_line() +
		  	ggplot2::labs(x = "Academic year ending", y = "Percent of bachelor's degrees awarded to women",
		  				  caption = "Source: IPEDS completions (C A), first majors"))
}

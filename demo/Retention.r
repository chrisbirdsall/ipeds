# Admissions selectivity and first-year retention
library(ipeds)

year <- ipeds_latest_year("ADM")
p <- ipeds_panel(year, modules = c("directory", "admissions", "retention"))
p <- p[!is.na(p$admit_rate) & !is.na(p$ret_ft_pct), ]

summary(p$admit_rate)
tapply(p$ret_ft_pct, cut(p$admit_rate, c(0, .25, .5, .75, 1)), mean)
tapply(p$ret_ft_pct, p$control, mean)

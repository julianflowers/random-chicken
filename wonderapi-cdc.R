## accessing cdc wonderapi

devtools::install_github("socdataR/wonderapi", build_vignettes = TRUE)

library(wonderapi)
wonderapi::show_databases()
dmdata <- getData(TRUE, "Detailed Mortality")
dmdata %>% head(20)

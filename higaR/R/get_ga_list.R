## gets list of IDs for google analytics

get_ga_list <- function(){
  
  cat("Getting list of IDs for HI sites")    
  require(RGA)  
  
  lookup <- list_profiles()
  fingertips_sites <- c(78054916, 93017633, 65073818)
  khub_sites <- as.numeric(lookup[grepl("khub", lookup$websiteUrl), ]$id)
  kandi_sites <- as.numeric(c("102677216", "82317641", "126950135", "126951434", "126962203"))
  local_health <- 47898340
  
  ga_list <- as.numeric(c(fingertips_sites, khub_sites, kandi_sites, local_health))
  
}
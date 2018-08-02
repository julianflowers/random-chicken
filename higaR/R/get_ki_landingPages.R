##  gets ki landing pages


get_ki_landingPages <- function(first = "yesterday", last = "today", ids = ga_list){
  
  require(RGA)
  require(tidyverse)
  
  df <- data.frame()
  
  
  for(i in seq_along(ids)) {
    id<- ids[i]
    first <- first
    last <- last
    ga <- get_ga(id, start.date = first, end.date = last,
                 metrics = "ga:users,
               ga:pageviews", 
                 dimension = "ga:date, ga:landingPagePath", 
                 fetch.by = "month")
    ga <- cbind(id, ga)
    df <- rbind(df, ga)
    
  }
  as.tibble(df)
}  




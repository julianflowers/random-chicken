
## gets k and i analytics

get_ki_analytics <- function(first = "yesterday", last = "today", ids = ga_list){
  
  require(RGA)
  require(tidyverse)  
  df <- data.frame()
  
  for(i in seq_along(ids)) {
    id<- ids[i]
    first <- first
    last <- last
    ga <- get_ga(id, start.date = first, end.date = last,
                 metrics = "ga:users,
               ga:newUsers,   
               ga:sessions,
               ga:avgTimeonPage,
               ga:pageviews,
               ga:bounceRate", 
                 dimension = "ga:date", 
                 fetch.by = "month")
    ga <- cbind(id, ga)
    df <- rbind(df, ga)
    
  }
  as.tibble(df)
}  
## gets k and i analytics

get_ki_analytics <- function(first = "yesterday", last = "today", ids = ga_list){
  
  require(RGA)
  require(tidyverse)  
  df <- data.frame()
  
  for(i in seq_along(ids)) {
    id<- ids[i]
    first <- first
    last <- last
    ga <- get_ga(id, start.date = first, end.date = last,
                 metrics = "ga:users,
               ga:newUsers,   
               ga:sessions,
               ga:avgTimeonPage,
               ga:pageviews,
               ga:bounceRate", 
                 dimension = "ga:date", 
                 fetch.by = "month")
    ga <- cbind(id, ga)
    df <- rbind(df, ga)
    
  }
  as.tibble(df)
}  
## get ki events

get_ki_events <- function(first = "yesterday", last = "today", ids = ga_list){
  
  df <- data.frame()
  
  
  for(i in seq_along(ids)) {
    id<- ids[i]
    first <- first
    last <- last
    ga <- get_ga(id, start.date = first, end.date = last,
                 metrics = "ga:totalEvents,
               ga:sessionsWithEvent", 
                 dimension = "ga:date, ga:eventCategory", 
                 fetch.by = "month")
    ga <- cbind(id, ga)
    df <- rbind(df, ga)
    
  }
  as.tibble(df)
}  
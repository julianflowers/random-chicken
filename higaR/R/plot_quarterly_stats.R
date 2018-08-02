## plots quarterly stats

plot_quarterly_stats <- function(ds, enddate = Sys.Date(), title = "Title", subtitle = "Subtitle", caption = NULL){
  
  require(dplyr)
  require(ggplot2)  
  
  ds %>%
    filter(date < enddate) %>%
    group_by(qtr = zoo::as.yearqtr(date)) %>%
    summarise(totUsers = sum(users), 
              totnewUsers = sum(newUsers), 
              totSessions = sum(sessions), 
              meanbounce = mean(bounceRate), 
              meandur = mean(avgTimeonPage)) %>%
    gather(metric, value, totUsers:meandur) %>%
    ggplot(aes(qtr, value, fill = metric)) +
    geom_col(aes(group = metric)) +
    geom_point() +
    #geom_smooth(se = FALSE) +
    facet_wrap(~metric, scales = "free") +
    zoo::scale_x_yearqtr() +
    govstyle::theme_gov() +
    labs(title = title,
         subtitle = subtitle, 
         caption = caption)
  
}

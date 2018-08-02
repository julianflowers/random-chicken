## search heatmap

search_heatmap <- function(ds, start_date = "2017-01-01", title = "Title", topn = 20){
  require(dplyr)
  require(ggplot2)

  ds %>%
    filter(date >= start_date) %>%
    mutate(landingPagePath = str_replace_all(landingPagePath, "/fingertips.phe.org.uk/", "" )) %>%
    mutate(page = str_replace_all(landingPagePath, "profile/|profile-group/", ""),
           page  = str_replace_all(page, "/data|mental-health/|mentalhealth", "")) %>%
    filter(!str_detect(landingPagePath, "area-search")) %>%
    filter(str_detect(landingPagePath, "search")) %>%
    mutate(page = str_replace_all(page, "search/", "")) %>%
    group_by(quarter = zoo::as.yearqtr(date), page) %>%
    summarise(totUsers = sum(users)) %>%
    arrange(quarter, -totUsers) %>%
    filter(!str_detect(page, "public-health")) %>%
    top_n(topn) %>%
    ggplot(aes(quarter, reorder(page,totUsers),  fill = totUsers)) +
    geom_tile() +
    zoo::scale_x_yearqtr() +
    #coord_equal() +
    scale_fill_distiller(palette = "Spectral") +
    labs(y = "Search term",
         title = title) +
    govstyle::theme_gov()+
    theme(legend.position = "bottom")

}

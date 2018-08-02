## draws heatmap of popular profiles

profiles_heatmap <- function(ds, title = "Title"){

  require(dplyr)
  require(ggplot2)

  ds %>%
    mutate(landingPagePath = str_replace_all(landingPagePath, "/fingertips.phe.org.uk/", "" )) %>%
    mutate(page = str_replace_all(landingPagePath, "profile/|profile-group/", ""),
           page  = str_replace_all(page, "/data|mental-health/|mentalhealth", "")) %>%
    filter(!str_detect(landingPagePath, "area-search")) %>%
    filter(str_detect(landingPagePath, "data$")) %>%
    group_by(month = zoo::as.yearmon(date), page) %>%
    summarise(totUsers = sum(users)) %>%
    arrange(month, -totUsers) %>%
    top_n(25) %>%
    ggplot(aes(month, reorder(page,totUsers),  fill = totUsers)) +
    geom_tile() +
    zoo::scale_x_yearmon() +
    scale_fill_distiller(palette = "Spectral") +
    labs(y = "Profile",
         title = title) +
    govstyle::theme_gov() +
    theme(legend.position = "bottom")
}

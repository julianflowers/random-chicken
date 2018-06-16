indicatorList<- function(){
  require(tidyverse)
  require(fingertipsR)
  
  fingertipsR::areatypes_by_indicators %>%
    left_join(area_types()) %>%
    group_by(IndicatorID) %>%
    select(IndicatorID, AreaTypeName, AreaTypeID) %>%
    arrange(IndicatorID) %>%
    left_join(indicators()) %>%
    select(IndicatorID, IndicatorName, ProfileName, AreaTypeName, AreaTypeID) %>%
    distinct() 
    
    
}
library(tidyverse)

data <- fingertipsR::fingertips_data(IndicatorID = 90409, AreaTypeID = c(120, 153))

data %>%
  filter(AreaName == "England", Age == "18+ yrs", !is.na(ParentName)) %>%
  distinct() %>%
  ggplot(aes(Timeperiod, Value)) +
  geom_point() +
  geom_smooth(aes(group = 1), lty = "dotted") +
  geom_linerange(aes(ymin = LowerCIlimit, 
                ymax = UpperCIlimit)) +
  labs(title = data$IndicatorName) +
    theme(plot.title = element_text(size = 9))

indicatorList()
       
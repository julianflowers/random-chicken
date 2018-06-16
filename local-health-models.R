## Estimate adult obesity rates at ward level using local health data

library(pacman)

p_load(tidyverse, readxl,  caret, caretEnsemble, data.table, ggridges, leaps, rpart, partykit, DALEX)


# read in data

s1 <- read_excel("~/Downloads/Theme1_2017.xlsx")
s2 <- read_excel("~/Downloads/Theme2_2017_Nov.xlsx")
s3 <- read_excel("~/Downloads/Theme3_2017.xlsx")
s4 <- read_excel("~/Downloads/Theme4_2017.xlsx")

# combine
ds <- data.table(rbind(s1, s2, s3, s4))

##write_rds(ds, "local_health.rds", compress = "gz")

# explore

summary(ds)

theme_lookup <- data.frame(theme = c("our community", 'behavioural risk and child health', 'disease and poor health', 'life expectancy and mortality'),
                           ThemeID = c(1, 2, 3, 4))

ds <- left_join(ds, theme_lookup) %>% data.table()

## review geography types
ds[, .N, by = .(Name)]


## select ward data

ward_ds <- ds[GeographyType == "Ward", ]

ward_ds[, .N, by = Period]

## counts

ward_ds %>%
  group_by(Name,Period) %>%
  tally()

## Check distributions

library(ggridges)

ward_ds[, quantile := quantile(Value, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1), na.rm = TRUE), by = Name]


print(ward_ds %>%
        filter(!Name == "Total population", Value < 100) %>%
        #mutate(logval = log10(Value)) %>%
        ggplot(aes(Value, Name), alpha = .4) +
        geom_density_ridges(scale = 4)
)

## Check missing data

ward_ds %>%
  group_by(Name) %>%
  summarise(meanna = mean(is.na(Value))) %>%
  arrange(-meanna)

## convert to wide format

ward_ds_w <- ward_ds %>%
  filter(!stringr::str_detect(Name, "^Modelled"), Name != "Percentage of deliveries where the mother is aged under 18 years" ) %>%
  select(Name, AreaCode, AreaName, Value) %>%
  spread(Name, Value)

## exclude variables with > 10% missing data, else impute to mean

## check mean values before imputation

means <- ward_ds_w %>%
  summarise_at(vars(3:69), funs(mean, sd), na.rm = TRUE)

head(means)




ward_ds_w_clean <- map_df(ward_ds_w[, -c(1:2)], function(x) ifelse(is.na(x), median(x, na.rm = TRUE ), x) )

ward_ds_w_clean <- cbind(ward_ds_w[, 1:2], ward_ds_w_clean)  

which(is.na(ward_ds_w_clean))

## and after

means1 <-  ward_ds_w_clean %>%
  summarise_at(vars(3:69), funs(mean, sd), na.rm = TRUE)

check_means <- means %>%
  bind_rows(means1) %>%
  t()



## scale dataset

ward_ds_scale <- map_df(ward_ds_w_clean[, -c(1:2)], scale) %>%
  cbind(ward_ds_w[, 1:2])  %>%
  select(AreaCode, AreaName, everything())

mean(is.na(ward_ds_scale))

print(ward_ds_scale %>%
        select(-starts_with("Area")) %>%
        do(broom::glance(lm(`Obese adults` ~ ., data = .))) )
      
      
      %>%
        filter(p.value < 0.01) %>%
        select(term, estimate, p.value) %>%
        arrange(estimate))

library (caret)
library(caretEnsemble)

ward_ds_w_clean <- ward_ds_w_clean %>% janitor::clean_names()

# glmnet

train <- createDataPartition(ward_ds_w_clean$obese_adults, p = 0.7, list = FALSE)

warddsTrain <- ward_ds_w_clean[train, ]
warddsTest <- ward_ds_w_clean[-train, ]

seed <- 123
summary(warddsTrain)

## Set cross validation
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions = "final", predictionBounds = c(TRUE, TRUE))


### SVM

svm <- train(obese_adults ~., data=warddsTrain[,-c(1:2)], trControl=control, method = "svmRadial")

varImp(svm)

pred_svm <- predict(svm, newdata = warddsTest, interval = "prediction")

head(pred_svm)

preds_svm <- data.frame(pred_svm, obs = warddsTest$obese_adults) %>% janitor::clean_names()

pairs(preds_svm)

RMSE(preds_svm$pred_svm, preds_svm$obs)

broom::glance(lm(pred_svm ~ obs, data = preds_svm)) %>% select(r.squared)


### glmnet

glmnet <- train(obese_adults ~., data=warddsTrain[,-c(1:2)], trControl=control, method = "glmnet")

varImp(glmnet)

pred_glmnet <- predict(glmnet,  newdata = warddsTest, interval = "prediction")

head(pred_glmnet)

preds_glmnet <- data.frame(pred_glmnet, obs = warddsTest$obese_adults) %>% janitor::clean_names()

pairs(preds_glmnet)

RMSE(preds_glmnet$pred_glmnet, preds_glmnet$obs)

broom::glance(lm(pred_glmnet ~ obs, data = preds_glmnet)) %>% select(r.squared)


### neural network

brnn <- train(obese_adults ~., data=warddsTrain[,-c(1:2)], trControl=control, method = "brnn")

varImp(brnn)

pred_brnn <- predict(brnn, newdata = warddsTest, interval = "prediction")

head(pred_brnn)

preds_brnn <- data.frame(pred_brnn, obs = warddsTest$obese_adults) %>% janitor::clean_names()

pairs(preds_brnn)

RMSE(preds_brnn$pred_brnn, preds_brnn$obs)

broom::glance(lm(pred_brnn ~ obs, data = preds_brnn)) %>% select(r.squared)


              
### xgblinesr
              
xgb <- train(obese_adults ~., data=warddsTrain[,-c(1:2)], trControl=control, method = "xgbLinear")
              
varImp(xgb)
              
pred_xgb <- predict(xgb, newdata = warddsTest, interval = "prediction")
              

preds_xgb <- data.frame(pred_xgb, obs = warddsTest$obese_adults) %>% janitor::clean_names()
              
pairs(preds_xgb)
              
RMSE(preds_xgb$pred_xgb, preds_xgb$obs)
              
broom::glance(lm(pred_xgb ~ obs, data = preds_xgb)) %>% select(r.squared)
              

### rf - ranger

rf <- train(obese_adults ~., data=warddsTrain[,-c(1:2)], trControl=control, method = "ranger")

varImp(rf)

pred_rf <- predict(rf, newdata = warddsTest, interval = "prediction")


preds_rf <- data.frame(pred_rf, obs = warddsTest$obese_adults) %>% janitor::clean_names()

pairs(preds_rf)

RMSE(preds_xgb$pred_rf, preds_rf$obs)

broom::glance(lm(pred_rf~ obs, data = preds_rf)) %>% select(r.squared)

## Set model list
algorithmList <- c('glm', 'rpart', 'knn', 'svmRadial', 'lasso', 'ranger', 'brnn')
                   
                   
                   'glm', 'knn', 'svmRadial', 'lasso', 'ranger', 'brnn')

set.seed(seed)

models <- caretList(obese_adults ~., data=warddsTrain[,-c(1:2)], trControl=control, methodList=algorithmList)

varImp(models$glm$finalModel) %>% arrange(-Overall)

plot(models$glm$pred$pred, models$knn$pred$obs)

results <- resamples(models)
summary(results)
## results
dotplot(results)

pred <- as.data.frame(predict(models, newdata = warddsTest)))

preds <- data.frame(pred, obs = warddsTest$obese_adults) %>% janitor::clean_names()

sqrt(mean((preds$glm - preds$obs)^2))

hist(preds[, 6])













caretList()

mean(ward_ds_w_clean$`Obese adults`)
mod1 <- train(`Obese adults` ~., method = "brnn", data =warddsTrain[, -c(1:2)], metric = "RMSE")

mod1$results %>%
  summarise(rmse = mean(RMSE), 
            rsq = mean(Rsquared))

varimp <- varImp(mod1)

plot(varimp)

predictions1 <- predict(mod1, warddsTest)

#head(predictions)
rmse1 <- RMSE(predictions1, warddsTest$`Obese adults`)
r21 <- R2(predictions1, warddsTest$`Obese adults`)

## gbm - gradient boosted machine
mod2 <- train(`Obese adults` ~., method = "gbm", data =warddsTrain[, -c(1:2)])

mod2$results %>%
  summarise(rmse = mean(RMSE), 
            rsq = mean(Rsquared))

varimp1 <- rel(mod2)

plot(varimp1)

predictions <- predict(mod2, warddsTest)

head(predictions)
rmse <- RMSE(predictions, warddsTest$`Obese adults`)
r2 <- R2(predictions, warddsTest$`Obese adults`)


## GLMNET
set.seed(1234)

mod3 <- train(`Obese adults` ~., method = "glmnet", data =warddsTrain[, -c(1:2)])

mod3$results %>%
  summarise(rmse = mean(RMSE), 
            rsq = mean(Rsquared))

varimp1 <- varImp(mod3)

plot(varimp1)

predictions3 <- predict(mod3, warddsTest)

head(predictions)
rmse3 <- RMSE(predictions3, warddsTest$`Obese adults`)
r23 <- R2(predictions3, warddsTest$`Obese adults`)

# Cluster analysis

## kmeans
set.seed(1234)
k <- 6

ward_k <- factoextra::eclust(ward_ds_scale[, -c(1:2)], "kmeans", k)

ward_ds_scale1 <- broom::augment(ward_k, ward_ds_scale) %>% janitor::clean_names()

## convert back to long format
ward_ds_scale_long <- ward_ds_scale1 %>%
  select(areacode, areaname, cluster = `_cluster`, everything()) %>%
  gather(metric, value, 4:ncol(.))

str(ward_ds_scale_long)
## cluster characteristics

ward_ds_scale_long %>%
  mutate(metric_name = stringr::str_trunc(metric, width = 40, side = "center")) %>%
  group_by(cluster, metric_name) %>%
  summarise(meanval = mean(value)) %>%
  ggplot(aes(metric_name, meanval, fill = cluster)) +
  geom_col() +
  coord_flip() +
  theme_bw()+
  facet_wrap(~cluster, nrow = 1) +
  theme(axis.text.y = element_text(size = 6)) +
  geom_hline(yintercept = c(-1,1), colour = "red", lty = "dotted") +
  scale_fill_viridis(discrete = TRUE)

## map clusters/ local health data

library(tmap)
library(geojsonio)
library(viridis)

map_data <- ward_ds_scale1 %>%
  select(areacode, areaname, `_cluster`)

dim(map_data)

## get shape files

shp <- geojson_read("https://opendata.arcgis.com/datasets/afcc88affe5f450e9c03970b237a7999_3.geojson", what = "sp")

shp1 <- geojson_read("https://opendata.arcgis.com/datasets/687f346f5023410ba86615655ff33ca9_4.geojson", what = "sp")

shp2 <- geojson_read("https://opendata.arcgis.com/datasets/686603e943f948acaa13fb5d2b0f1275_3.geojson", what = "sp")


head(shp)

shp <- subset(shp, substring(wd16cd, 1, 1) == "E" )
shp1 <- subset(shp1, substring(ctyua16cd, 1, 1) == "E" )
shp2 <- subset(shp2, substring(lad16cd, 1, 1) == "E" )


shp@data <- shp@data %>%
  left_join(map_data, by = c("wd16cd" = "areacode")) 

#shp@data <- shp@data %>% filter(`_cluster.y` == )

head(shp@data)

palette <-viridis::viridis(k)
credits <- "Contains ordnance survey data (c) \nCrown copyright and database right 2016"


eng <- tmap::tm_shape(shp) +
  tm_fill("_cluster.y", n = k, palette = palette, title = "Cluster") +
  tm_credits( credits, size = 0.5, align = "right") +
  tm_shape(shp2) +
  tm_borders("grey", lwd  =0.5) +
  tm_layout(
    legend.outside = TRUE,
    frame = FALSE) 
eng
tmap::tmap_mode("view")

## london

shpLdn <- subset(shp, substring(lad16cd, 1, 3) == "E09" )
shp1Ldn <- subset(shp2, substring(lad16cd, 1, 3) == "E09" )

shpLdn@data <- shpLdn@data %>%
  left_join(ward_ds_w_clean, by = c("wd16cd" = "AreaCode"))


lond <- tmap::tm_shape(shpLdn) +
  tm_fill("_cluster", style = "kmeans", n = k, palette = palette, title = "Cluster") +
  tm_shape(shp1Ldn) +
  tm_borders(lty = "dotted", col = "grey") +
  #tm_text(str_wrap("lad16nm", 30), size = .6, col = "white", fontface = "italic") +
  tm_credits( credits, size = 0.4, align = "right") +
  tm_layout(
    legend.outside = TRUE,
    frame = FALSE)

library(grid)
print(eng, vp=viewport(x= 0.15, y= 0.15, width= 0.3, height= 0.3))
print(lond, vp=viewport(x= 0.4, y= 0.1, width= 0.2, height= 0.1))

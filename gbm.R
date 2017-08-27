# use with "data_cleaning_simple.R"

# setwd("C:/Projects/NYCDS/Projects/030 Machine Learning/data")
# load(file="clean.dat")
# load(file="imp.dat")
# load(file="clean_prop.dat")
# load(file="imp_prop.dat")

brary(caret)
library(data.table)
library(dplyr)
library(gbm)

all_data = clean
all_data$logerror_q3 = NULL


# convert logical to factor
log_cols = colnames(all_data)[sapply(all_data, class) == "logical"]
for (log_col in log_cols) {
  all_data[[log_col]] = as.factor(all_data[[log_col]])
}

col_names = colnames(all_data)
rm_names = c("id_parcel", "fips_blockid", "date", "censustractandblock", "region_zip",
             "zoning_landuse_county", "region_city", "zoning_property", "region_neighbor",
             "month_factor", "num_unit_fac_1_30",
             "tax_building", "tax_land", "tax_property", "tax_total",
             "missing_values_pattern",
             "num_bathroom_fac", "num_bedroom_fac", "num_room_fac", "num_75_bath_fac", "num_bath_fac", 
             "num_bathroom", "num_bath",
             "num_garage_fac")
rm_names = c(rm_names, col_names[grep("region_zip_fac.*", col_names)])
rm_names = c(rm_names, col_names[grep("region_city_fac.*", col_names)])
rm_names = c(rm_names, col_names[grep("region_neighbor_fac.*", col_names)])
rm_names = c(rm_names, col_names[grep("zoning_property_fac.*", col_names)])
rm_names = c(rm_names, col_names[grep("zoning_landuse_county_fac.*", col_names)])
rm_names = c(rm_names, col_names[grep("missing_values_pattern_fac.*", col_names)])

cn = col_names[(!(col_names %in% rm_names))]

# train / test data split
set.seed(2344)
train_inx = sample(1:nrow(all_data), nrow(all_data)*0.8)
train_data = all_data[train_inx, cn, with=F]
test_data = all_data[-train_inx, cn, with=F]



# Machine Learning
## Data splitting based on the outcome
set.seed(123)
train_inx <- createDataPartition(all_data$logerror, 
                                  p = .8, 
                                  list = FALSE, 
                                  times = 1)
## training set
train_data <- all_data[ train_inx, cn, with=F]
## testing set
test_data  <- all_data[-train_inx, cn, with=F]

## define metric - MAE
maeSummary <- function(data, lev = NULL, model = NULL) {
  mae_score = mean(abs(data$obs - data$pred))
  names(mae_score) <- "DENFIS"
  if(is.na(mae_score))
     cat("mae_score:",  sum(is.na(data$pred)), "\n")
  mae_score
}

## grid search
gridSearch <- trainControl(method = "cv",
                           number = 3,
                           summaryFunction = maeSummary,
                           search="grid")

gbm.grid <-  expand.grid(
                          n.trees = c(100, 200, 300, 500),
                          shrinkage = c(0.03, 0.01, 0.005),
                          n.minobsinnode = c(100, 250, 500),
                          interaction.depth = c(8, 12, 15)
                        )

gbm.fit = train(logerror ~ .,
             data = train_data, 
             method = "gbm", 
             metric = "MAE",
             maximize = FALSE,
             tuneGrid = gbm.grid,
             trControl = gridSearch,
             distribution = "laplace",
             verbose = TRUE
            )

gbm.fit

best_n.trees = gbm.fit$bestTune$n.trees
best_interaction.depth = gbm.fit$bestTune$interaction.depth
best_shrinkage = gbm.fit$bestTune$shrinkage
best_minobsinnode = gbm.fit$bestTune$n.minobsinnode

## parameters
# plot(gbmFit2)
rf.imp = varImp(rf.fit, scale = FALSE)
plot(rf.imp, top = 40)

#mse-plot of finalModel
plot((1:length(gbm.fit$finalModel$mse)), gbm.fit$finalModel$mse)


gbm.pred = predict(gbm.fit, test_data)

err = test_data$logerror - gbm.pred
cat("mean abs err: ", mean(abs(err)), "\n")
cat("mean square err: ", mean(err^2), "\n")
cat("baseline mean abs err", mean(abs(mean(all_data$logerror)-test_data$logerror)), "\n")
cat("baseline mean square err: ", mean((mean(all_data$logerror)-test_data$logerror)^2), "\n")

cor(gbm.pred, test_data$logerror)

#######################################
# train best model forest on all data.

bestGbmFit = gbm(logerror ~ ., 
                 data=all_data[, cn, with=F], 
                 distribution = "laplace", 
                 n.trees = best_n.trees, 
                 shrinkage = best_shrinkage,
                 interaction.depth = best_interaction.depth, 
                 n.minobsinnode = best_minobsinnode,
                 n.cores = 7,
                 keep.data = T,
                 verbose=T)


############################################
# make submission

newdata = clean_prop
newdata$logerror_q3 = NULL

newdata$parcelid = newdata$id_parcel

months = c(10, 11, 12, 10, 11, 12)
labels = c("201610", "201611", "201612", "201710", "201711", "201712")

predictions <- newdata[, "parcelid", drop=FALSE]
for(i in 1:3) {
  cat("month: ", months[i], "\n")
  newdata$month <- months[i]
  newdata$month_factor = factor(newdata$month, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
  #newdata$month_factor = as.factor(newdata$month)
  predictions[, labels[i]] <- predict(bestGbmFit, best_n.trees, newdata = newdata)
}

predictions[["201710"]] = 0
predictions[["201711"]] = 0
predictions[["201712"]] = 0


write.csv(x = predictions, file = "submission_gbm-V4.csv", 
          quote = FALSE, row.names = FALSE)

# use with "data_cleaning_simple.R"

# setwd("C:/Projects/NYCDS/Projects/030 Machine Learning/data")
# load(file="clean.dat")
# load(file="imp.dat")
# load(file="clean_prop.dat")
# load(file="imp_prop.dat")

library(caret)
library(data.table)
library(dplyr)
library(rpart)

all_data = cbind(clean, imp[, -1])
all_data$logerror_q3 = NULL

col_names = colnames(all_data)
rm_names = c("id_parcel", "fips_blockid", "date", "censustractandblock", "region_zip",
             "zoning_landuse_county", "region_city", "zoning_property", "region_neighbor",
             "tax_building", "tax_land", "tax_property", "tax_total",
             "num_bathroom_fac", "num_bedroom_fac", "num_room_fac", "num_75_bath_fac", "num_bath_fac", 
             "num_bathroom", "num_bath",
             "num_garage_fac", "missing_values_pattern")
# rm_names = c(rm_names, col_names[grep("region_zip_fac.*", col_names)])
# rm_names = c(rm_names, col_names[grep("region_city_fac.*", col_names)])
# rm_names = c(rm_names, col_names[grep("region_neighbor_fac.*", col_names)])
# rm_names = c(rm_names, col_names[grep("zoning_property_fac.*", col_names)])
# rm_names = c(rm_names, col_names[grep("zoning_landuse_county_fac.*", col_names)])
#rm_names = c(rm_names, col_names[grep("missing_values_pattern_fac.*", col_names)])

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
  mae_score <- sum(abs(data$obs - data$pred)) / nrow(data)
  if(is.na(mae_score))
    mae_score = 10
  names(mae_score) <- "DENFIS"
  mae_score
}

## grid search
gridSearch <- trainControl(method = "cv",
                           number = 3,
                           summaryFunction = maeSummary,
                           search="grid")

rpart.grid <-  expand.grid(cp = c(0.012, 0.011, 0.010, 0.009, 0.007, 0.001, 0.0005))

rpart.fit <- train(logerror ~ .,
                data = train_data, 
                method = "rpart", 
                metric = "MAE",
                maximize = FALSE,
                tuneGrid = rpart.grid,
                trControl = gridSearch
                )

# investigating the result gives the following findings:
# the best model has a cv = 0.012
# when the final model is printed it states:
#
# > rpart.fit$finalModel
# n= 72222 
# 
# node), split, n, deviance, yval
# * denotes terminal node
# 
# 1) root 72222 1898.112 0.01145013 *
#
# The best model contains only one node returning the logerror mean. I.e. rpart failed to find
# any structure in the data with some predictive power (lower cv which produce deeper trees weren't 
# successfully).
# 


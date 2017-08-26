# library(dplyr)
# library(xgboost)

######################
###Data preparation###
######################


xgb_clean <- sales
xgb_clean_prop <- prop_imp
setdiff(names(xgb_clean), names(xgb_clean_prop))

p = 32 # sigmoid function: p / 1 + exp-x

nround = 800

# col_tofac_list <- c('pooltypeid2',
#                     'fips_blockid',
#                     'flag_tub',
#                     'zoning_property',
#                     'zoning_landuse_county',
#                     'region_zip',
#                     'pooltypeid7',
#                     'region_neighbor',
#                     'pooltypeid10',
#                     'region_city',
#                     'heating',
#                     'aircon',
#                     'quality',
#                     'zoing_landuse',
#                     'region_county')


tofac_idx <- which(colnames(xgb_clean) %in% col_tofac_list)
for(i in tofac_idx){
  xgb_clean[[i]] <- as.factor(as.character(xgb_clean[[i]]))
}

tofac_idx_prop <- which(colnames(xgb_clean_prop) %in% col_tofac_list)
for(i in tofac_idx_prop){
  xgb_clean_prop[[i]] <- as.factor(as.character(xgb_clean_prop[[i]]))
}


ctypes <- sapply(xgb_clean, class)
cidx <- which(ctypes %in% c("character", "factor"))

for(i in cidx){
  xgb_clean[[i]] <- as.integer(factor(xgb_clean[[i]]))
}

ctypes_prop <- sapply(xgb_clean_prop, class)
cidx_prop <- which(ctypes_prop %in% c("character", "factor"))

for(i in cidx_prop){
  xgb_clean_prop[[i]] <- as.integer(factor(xgb_clean_prop[[i]]))
}

xgb_clean$sig_logerror <- p / (1 + exp(-xgb_clean$logerror))

for_xgboost_train <- xgb_clean %>% select(-id_parcel, -logerror)

for_xgboost_train <- for_xgboost_train

for_xgboost_test <- xgb_clean_prop %>% mutate(month = 10) %>% select(-id_parcel)

for_xgboost_test <- for_xgboost_test

dtrain <- xgb.DMatrix(data.matrix(for_xgboost_train %>% select(-sig_logerror)), 
                      label = for_xgboost_train$sig_logerror)

dval <- xgb.DMatrix(data.matrix(for_xgboost_test))
  
bstSparse <- xgb.train(  param                 = best_param,
                         data                  = dtrain, 
                         nrounds               = nround, 
                         verbose               = 1,
                         print_every_n         = 10L
                      )
  
pred <- predict(bstSparse, dval)
pred <- -log((p / pred) - 1)

for_xgboost_test <- xgb_clean_prop %>% mutate(month = 11) %>% select(-id_parcel)
for_xgboost_test <- for_xgboost_test
dval <- xgb.DMatrix(data.matrix(for_xgboost_test))
pred2 <- predict(bstSparse, dval)
pred2 <- -log((p / pred2) - 1)


for_xgboost_test <- xgb_clean_prop %>% mutate(month = 12) %>% select(-id_parcel)
for_xgboost_test <- for_xgboost_test
dval <- xgb.DMatrix(data.matrix(for_xgboost_test))
pred3 <- predict(bstSparse, dval)
pred3 <- -log((p / pred3) - 1)


for_xgboost_test <- xgb_clean_prop %>% mutate(month = 22) %>% select(-id_parcel)
for_xgboost_test <- for_xgboost_test
dval <- xgb.DMatrix(data.matrix(for_xgboost_test))
pred4 <- predict(bstSparse, dval)
pred4 <- -log((p / pred4) - 1)


for_xgboost_test <- xgb_clean_prop %>% mutate(month = 23) %>% select(-id_parcel)
for_xgboost_test <- for_xgboost_test
dval <- xgb.DMatrix(data.matrix(for_xgboost_test))
pred5 <- predict(bstSparse, dval)
pred5 <- -log((p / pred5) - 1)

for_xgboost_test <- xgb_clean_prop %>% mutate(month = 24) %>% select(-id_parcel)
for_xgboost_test <- for_xgboost_test
dval <- xgb.DMatrix(data.matrix(for_xgboost_test))
pred6 <- predict(bstSparse, dval)
pred6 <- -log((p / pred6) - 1)

prop_result <- as.data.frame(prop$id_parcel)
prop_result$X201610 <- pred
prop_result$X201611 <- pred2
prop_result$X201612 <- pred3
prop_result$X201710 <- pred4
prop_result$X201711 <- pred5
prop_result$X201712 <- pred6

colnames(prop_result)[1] <- 'ParcelId'


rm(submit)
submit <- read.csv('sample_submission.csv')

submit <- as.data.frame(submit$ParcelId)

colnames(submit)[1] <- 'ParcelId'

submit <- left_join(submit, prop_result, by = 'ParcelId')

submit[is.na(submit)] <- mean(transactions$logerror)

head(submit)

write.csv(submit, 'submit.csv', row.names = FALSE)
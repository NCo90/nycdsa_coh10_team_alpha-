library(dplyr)
library(xgboost)

spc = 9028
range1 = 1:spc
range2 = spc : (2*spc)
range3 = (2*spc) : (3*spc)
range4 = (3*spc) : (4*spc)
range5 = (4*spc) : (5*spc)
range6 = (5*spc) : (6*spc)
range7 = (6*spc) : (7*spc)
range8 = (7*spc) : (8*spc)
range9 = (8*spc) : (9*spc)
range10 = (9*spc) : 90275

a = list(range1, range2, range3, range4, range5, range6, range7, range8, range9, range10)

p = 32 # sigmoid function: p / 1 + exp-x

nround = 800

######################
###Data preparation###
######################

sales <- inner_join(transactions, prop_imp, by = 'id_parcel')

sales$date <- as.Date(sales$date)
sales$month <- month(sales$date)
sales$date <- NULL

xgb_clean <- sales
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
#                     'region_county'
#                     )
# 
tofac_idx <- which(colnames(xgb_clean) %in% col_tofac_list)
 
for(i in tofac_idx){
  xgb_clean[[i]] <- as.factor(as.character(xgb_clean[[i]]))
}

xgb_clean_prop <- prop_imp
setdiff(names(xgb_clean), names(xgb_clean_prop))

xgb_clean$sig_logerror <- p / (1 + exp(-xgb_clean$logerror))
ctypes <- sapply(xgb_clean, class)
cidx <- which(ctypes %in% c("character", "factor"))
 
for(i in cidx){
  xgb_clean[[i]] <- as.integer(factor(xgb_clean[[i]]))
}


best_param <- list()
mae_best = 10
mae_ori_best = 1

for(i in 1:10) {

  for_xgboost_train <- xgb_clean %>% select(-id_parcel, -logerror)
  
  for_xgboost_train <- for_xgboost_train[-a[[i]], ]
  
  for_xgboost_test <- xgb_clean %>% select(-id_parcel, -logerror, -sig_logerror)
  
  for_xgboost_test <- for_xgboost_test[a[[i]], ]
  
  dtrain <- xgb.DMatrix(data.matrix(for_xgboost_train %>% select(-sig_logerror)), 
                        label = for_xgboost_train$sig_logerror)
  
  dval <- xgb.DMatrix(data.matrix(for_xgboost_test))
  
  ori <- xgb_clean$sig_logerror[a[[i]]]
  
  for (iter in 1 : 100){
    param <- list(  objective           = "reg:linear",
                    booster             = "gbtree",
                    eval_metric         = "mae",
                    eta                 = runif(1, .01, .3), 
                    max_depth           = sample(1 : 10, 1), 
                    subsample           = runif(1, .2, .8),
                    gamma               = runif(1, 0.0, 0.2),
                    min_child_weight    = sample(1 : 40, 1),
                    colsample_bytree    = sample(0.2 : 0.8, 1),
                    max_delta_step      = sample(1 : 10, 1),
                    maximize            = FALSE
    )
    
    
    bstSparse <- xgb.train(param,
                           data                  = dtrain, 
                           nrounds               = nround, 
                           verbose               = 1,
                           print_every_n         = 10L
    )
  
    pred <- predict(bstSparse, dval)
    
    mae <- mean(abs(pred - ori))
    
    mae_ori <- mean(abs( -log((p / pred) - 1)  + log((p / ori) - 1) ))
    
    if (mae < mae_best){
      mae_best <- mae
      best_param <- param
      mae_ori_best <- mae_ori
    }
    print('finished one loop')
    # print(mae_best)
    # print(mae)
    print(mae_ori)
    print(mae_ori_best)
  }
}
mae_best
best_param
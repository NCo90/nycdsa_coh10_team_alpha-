# setwd("C:/Projects/NYCDS/Projects/030 Machine Learning/data")
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)
library(mice)
# library(stats)
# library(flexclust)
# library(ade4)

############################################################################################
###################### Stephan, creates clean_proped, imputation free data ######################
############################################################################################
#
#   data loading and renaming
#


properties <- fread('properties_2016.csv')
transactions <- fread('train_2016_v2.csv')
#sample_submission <- fread('./data/sample_submission.csv')

properties <- properties %>% dplyr::rename(
  id_parcel = parcelid,
  build_year = yearbuilt,
  area_basement = basementsqft,
  area_patio = yardbuildingsqft17,
  area_shed = yardbuildingsqft26, 
  area_pool = poolsizesum,  
  area_lot = lotsizesquarefeet, 
  area_garage = garagetotalsqft,
  area_firstfloor_finished = finishedfloor1squarefeet,
  area_total_calc = calculatedfinishedsquarefeet,
  area_base = finishedsquarefeet6,
  area_live_finished = finishedsquarefeet12,
  area_liveperi_finished = finishedsquarefeet13,
  area_total_finished = finishedsquarefeet15,  
  area_unknown = finishedsquarefeet50,
  num_unit = unitcnt, 
  num_story = numberofstories,  
  num_room = roomcnt,
  num_bathroom = bathroomcnt,
  num_bedroom = bedroomcnt,
  num_bathroom_calc = calculatedbathnbr,
  num_bath = fullbathcnt,  
  num_75_bath = threequarterbathnbr, 
  num_fireplace = fireplacecnt,
  num_pool = poolcnt,  
  num_garage = garagecarcnt,  
  region_county = regionidcounty,
  region_city = regionidcity,
  region_zip = regionidzip,
  region_neighbor = regionidneighborhood,  
  tax_total = taxvaluedollarcnt,
  tax_building = structuretaxvaluedollarcnt,
  tax_land = landtaxvaluedollarcnt,
  tax_property = taxamount,
  tax_year = assessmentyear,
  tax_delinquency = taxdelinquencyflag,
  tax_delinquency_year = taxdelinquencyyear,
  zoning_property = propertyzoningdesc,
  zoning_landuse = propertylandusetypeid,
  zoning_landuse_county = propertycountylandusecode,
  flag_fireplace = fireplaceflag, 
  flag_tub = hashottuborspa,
  quality = buildingqualitytypeid,
  framing = buildingclasstypeid,
  material = typeconstructiontypeid,
  deck = decktypeid,
  story = storytypeid,
  heating = heatingorsystemtypeid,
  aircon = airconditioningtypeid,
  architectural_style= architecturalstyletypeid
)

transactions <- transactions %>% dplyr::rename(
  id_parcel = parcelid,
  date = transactiondate
)

#properties <- properties %>% 
#  mutate(tax_delinquency = ifelse(tax_delinquency=="Y",1,0),
#         flag_fireplace = ifelse(flag_fireplace=="Y",1,0),
#         flag_tub = ifelse(flag_tub=="Y",1,0))

prop = properties

sales = as.data.table(
  transactions %>%
    inner_join(properties, by="id_parcel")
)


clean_prop = data.table(id_parcel = prop$id_parcel)#, date=as.Date(prop$date), logerror=prop$logerror)

imp_prop = data.table(id_parcel = prop$id_parcel)


################################################
##  creat missingness features

# calc number of missing values per row
clean_prop$missing_values_count= apply(prop, 1, function(r) sum(is.na(r)))

####----------------------------------
# detect missingness pattern in sales which occurre more then 50 times and add pattern to prop

miss.pattern <- function(x, topn) {
  # find missingness patterns, 1 represents missing
  r <- 1 * data.frame(is.na(x))
  pattern <- data.frame(plyr::count(r))
  pattern <- pattern[order(-pattern$freq),]
  return(pattern[1:topn,])
}

cn = base::intersect(colnames(prop), colnames(sales))

# get the top 130 missingness pattern in sales (hightest freq is 50)
miss_pattern = miss.pattern(sales[,cn, with=F], 130)
miss_pattern$freq = NULL

miss_pattern_str = apply(miss_pattern, 1, function(r) paste(r, collapse=""))
prop_miss_str = apply(prop[, cn, with=F], 1, function(r) paste(is.na(r)*1, collapse=""))
prop$missing_values_pattern = ifelse(prop_miss_str %in% miss_pattern_str, prop_miss_str, "")
clean_prop$missing_values_pattern = as.factor(prop$missing_values_pattern)


clean_tmp = as.data.table(
  transactions %>%
    inner_join(clean_prop, by="id_parcel")
)

t = clean_tmp[, .(count=.N, abs_logerror=mean(abs(logerror))), by=missing_values_pattern]
t=t[order(-abs_logerror)]
t$missing_values_pattern_order = 1:nrow(t)

missing_values_pattern_order = as.data.table(
  clean_prop %>% left_join(t, by = "missing_values_pattern") %>% select(missing_values_pattern_order)
  )

clean_prop$missing_values_pattern_order = missing_values_pattern_order

rm(clean_tmp)
rm(miss_pattern_str)
rm(miss_pattern)
rm(prop_miss_str)
rm(cn)


################################################
##  build_year

imp_prop$build_year_isna = is.na(prop$build_year)
clean_prop$build_year = ifelse(imp_prop$build_year_isna, 0, prop$build_year)


################################################
##  garagetotalsqft (area_garage)
##  num_garage
##
##  Total number of square feet of all garages on lot including an attached garage.

imp_prop$num_garage_isna = is.na(prop$num_garage)
clean_prop$num_garage = ifelse(is.na(prop$num_garage), 0, prop$num_garage)

imp_prop$area_garage_isna = is.na(prop$area_garage)
clean_prop$area_garage = ifelse(is.na(prop$area_garage), 0, prop$area_garage)

# calc avg. garage size
avg_garage_area = (properties %>% 
                     filter(num_garage>0 & area_garage>0) %>% 
                     summarize(sum(area_garage)/sum(num_garage)))[1,1]

print(paste0("avg. garage area: ", avg_garage_area))

imp_prop$num_garage_1_new = clean_prop$area_garage>0 & clean_prop$num_garage==0

clean_prop$num_garage_1 = ifelse(
  imp_prop$num_garage_1_new, 
  1, 
  clean_prop$num_garage
)

imp_prop$area_garage_1_new = clean_prop$num_garage>0 & clean_prop$area_garage==0

clean_prop$area_garage_1 =  ifelse(
  imp_prop$area_garage_1_new,
  clean_prop$num_garage * avg_garage_area,
  clean_prop$area_garage
)     


num_garage = clean_prop$num_garage + clean_prop$num_garage_1
area_garage = clean_prop$area_garage + clean_prop$area_garage_1

clean_prop$area_per_garage = ifelse(num_garage==0, 0, area_garage / num_garage)

################################################
## area_basement
## area_patio
## area_shed
## area_lot
## area_firstfloor_finished
## area_total_calc
## area_base
## area_live_finished
## area_liveperi_finished
## area_total_finished
## area_unknown

imp_prop$area_basement_isna = is.na(prop$area_basement)
clean_prop$area_basement = ifelse(is.na(prop$area_basement), 0, prop$area_basement)

imp_prop$area_patio_isna = is.na(prop$area_patio)
clean_prop$area_patio = ifelse(is.na(prop$area_patio), 0, prop$area_patio)

imp_prop$area_shed_isna = is.na(prop$area_shed)
clean_prop$area_shed = ifelse(is.na(prop$area_shed), 0, prop$area_shed)

imp_prop$area_lot_isna = is.na(prop$area_lot)
clean_prop$area_lot = ifelse(is.na(prop$area_lot), 0, prop$area_lot)

imp_prop$area_firstfloor_finished_isna = is.na(prop$area_firstfloor_finished)
clean_prop$area_firstfloor_finished = ifelse(is.na(prop$area_firstfloor_finished), 0, prop$area_firstfloor_finished)

imp_prop$area_total_calc_isna = is.na(prop$area_total_calc)
clean_prop$area_total_calc = ifelse(is.na(prop$area_total_calc), 0, prop$area_total_calc)

imp_prop$area_base_isna = is.na(prop$area_base)
clean_prop$area_base = ifelse(is.na(prop$area_base), 0, prop$area_base)

imp_prop$area_live_finished_isna = is.na(prop$area_live_finished)
clean_prop$area_live_finished = ifelse(is.na(prop$area_live_finished), 0, prop$area_live_finished)

imp_prop$area_liveperi_finished_isna = is.na(prop$area_liveperi_finished)
clean_prop$area_liveperi_finished = ifelse(is.na(prop$area_liveperi_finished), 0, prop$area_liveperi_finished)

imp_prop$area_total_finished_isna = is.na(prop$area_total_finished)
clean_prop$area_total_finished = ifelse(is.na(prop$area_total_finished), 0, prop$area_total_finished)

imp_prop$area_unknown_isna = is.na(prop$area_unknown)
clean_prop$area_unknown = ifelse(is.na(prop$area_unknown), 0, prop$area_unknown)

imp_prop$area_no_lot_and_no_total = imp_prop$area_lot_isna & imp_prop$area_total_calc_isna

# calculate ratios
clean_prop$area_live_finished_rel =ifelse(clean_prop$area_total_calc==0, 0, clean_prop$area_live_finished/clean_prop$area_total_calc)
clean_prop$area_base_rel = ifelse(clean_prop$area_total_calc==0, 0, clean_prop$area_base/clean_prop$area_total_calc)
clean_prop$area_firstfloor_finished_rel = ifelse(clean_prop$area_total_calc==0, 0, clean_prop$area_firstfloor_finished/clean_prop$area_total_calc)
clean_prop$area_shed_rel = ifelse(clean_prop$area_total_calc==0, 0, clean_prop$area_shed/clean_prop$area_total_calc)
clean_prop$area_patio_rel = ifelse(clean_prop$area_total_calc==0, 0, clean_prop$area_patio/clean_prop$area_total_calc)
clean_prop$area_base_rel = ifelse(clean_prop$area_total_calc==0, 0, clean_prop$area_base/clean_prop$area_total_calc)
clean_prop$area_lot_rel = ifelse(clean_prop$area_total_calc==0, 0, clean_prop$area_lot/clean_prop$area_total_calc)

################################################
##  poolsizesum (area_pool)
##  num_pool
##  pooltypeid2
##  pooltypeid7
##  pooltypeid10
##
##  Total square footage of all pools on property.

imp_prop$num_pool_isna = is.na(prop$num_pool)
clean_prop$num_pool = ifelse(is.na(prop$num_pool), 0, prop$num_pool)

imp_prop$area_pool_isna = is.na(prop$area_pool)
clean_prop$area_pool = ifelse(is.na(prop$area_pool), 0, prop$area_pool)

# calculate avg pool area
avg_pool_area = (properties %>% 
                   filter(num_pool>0 & area_pool>0) %>% 
                   summarize(sum(area_pool)/sum(num_pool)))[1,1]

print(paste0("avg. pool area: ", avg_pool_area))


imp_prop$num_pool_1_new = clean_prop$area_pool>0 & clean_prop$num_pool==0

clean_prop$num_pool_1 = ifelse(
  imp_prop$num_pool_1_new, 
  1, 
  clean_prop$num_pool
)

imp_prop$area_pool_1_new = clean_prop$num_pool>0 & clean_prop$area_pool==0

clean_prop$area_pool_1 =  ifelse(
  imp_prop$area_pool_1_new,
  clean_prop$num_pool * avg_pool_area,
  clean_prop$area_pool
)     

imp_prop$pooltypeid2_isna = is.na(prop$pooltypeid2)
clean_prop$pooltypeid2 = ifelse(imp_prop$pooltypeid2_isna, 0, prop$pooltypeid2)

imp_prop$pooltypeid7_isna = is.na(prop$pooltypeid7)
clean_prop$pooltypeid7 = ifelse(imp_prop$pooltypeid7_isna, 0, prop$pooltypeid7)

imp_prop$pooltypeid10_isna = is.na(prop$pooltypeid10)
clean_prop$pooltypeid10 = ifelse(imp_prop$pooltypeid10_isna, 0, prop$pooltypeid10)


################################################
##  region_city
##  region_neighbor
##  region_county
##  region_zip
##

imp_prop$region_city_isna = is.na(prop$region_city)
clean_prop$region_city = as.integer(as.factor(ifelse(imp_prop$region_city_isna, 0, prop$region_city)))

imp_prop$region_neighbor_isna = is.na(prop$region_neighbor)
clean_prop$region_neighbor = as.integer(as.factor(ifelse(imp_prop$region_neighbor_isna, 0, prop$region_neighbor)))

imp_prop$region_county_isna = is.na(prop$region_county)
clean_prop$region_county = as.factor(ifelse(imp_prop$region_county_isna, 0, prop$region_county))

imp_prop$region_zip_isna = is.na(prop$region_zip)
clean_prop$region_zip = as.integer(as.factor(ifelse(imp_prop$region_zip_isna, 0, prop$region_zip)))

#############################
# for region_zip the mean(logerror) as a 30% correlation on the number of the observations for the region_zip.
t = sales[, .(region_zip_obs_count = .N), by=region_zip]
t1 = prop %>% left_join(t, by = "region_zip") %>% select(region_zip_obs_count)
clean_prop$region_zip_obs_count = ifelse(is.na(t1$region_zip_obs_count), 0, t1$region_zip_obs_count)

##########
# region_city
t = sales[, .(region_city_obs_count = .N), by=region_city]
t1 = prop %>% left_join(t, by = "region_city") %>% select(region_city_obs_count)
clean_prop$region_city_obs_count = ifelse(is.na(t1$region_city_obs_count), 0, t1$region_city_obs_count)

##########
# zoning_property
t = sales[, .(zoning_property_obs_count = .N), by=zoning_property]
t1 = prop %>% left_join(t, by = "zoning_property") %>% select(zoning_property_obs_count)
clean_prop$zoning_property_obs_count = ifelse(is.na(t1$zoning_property_obs_count), 0, t1$zoning_property_obs_count)

##########
# region_neighbor
t = sales[, .(region_neighbor_obs_count = .N), by=region_neighbor]
t1 = prop %>% left_join(t, by = "region_neighbor") %>% select(region_neighbor_obs_count)
clean_prop$region_neighbor_obs_count = ifelse(is.na(t1$region_neighbor_obs_count), 0, t1$region_neighbor_obs_count)


rm(t)
rm(t1)



################################################
##  zoning_landuse_county
##  zoning_landuse
##  zoning_property

# no missing values
clean_prop$zoning_landuse_county = as.integer(as.factor(ifelse(is.na(prop$zoning_landuse_county), 0, prop$zoning_landuse_county)))

# no missing values
clean_prop$zoning_landuse = as.factor(ifelse(is.na(prop$zoning_landuse), 0, prop$zoning_landuse))

# no missing values
clean_prop$zoning_property = as.integer(as.factor(prop$zoning_property))

################################################
##  num_room
##  num_unit
##  num_story
##  num_bathroom
##  num_bedroom
##  num_bathroom_calc
##  num_bath
##  num_75_bath
##  num_fireplace

#num_room is always set
clean_prop$num_room = ifelse(is.na(prop$num_room), 0, prop$num_room)

imp_prop$num_story_isna = is.na(prop$num_story)
clean_prop$num_story = as.factor(ifelse(imp_prop$num_story_isna, 0, prop$num_story))

imp_prop$num_unit_isna = is.na(prop$num_unit)
clean_prop$num_unit = ifelse(imp_prop$num_unit_isna, 0, prop$num_unit)

imp_prop$num_bathroom_isna = is.na(prop$num_bathroom)
clean_prop$num_bathroom = ifelse(imp_prop$num_bathroom_isna, 0, prop$num_bathroom)

imp_prop$num_bedroom_isna = is.na(prop$num_bedroom)
clean_prop$num_bedroom = ifelse(imp_prop$num_bedroom_isna, 0, prop$num_bedroom)

imp_prop$num_bathroom_calc_isna = is.na(prop$num_bathroom_calc)
clean_prop$num_bathroom_calc = ifelse(imp_prop$num_bathroom_calc_isna, 0, prop$num_bathroom_calc)

imp_prop$num_bath_isna = is.na(prop$num_bath)
clean_prop$num_bath = ifelse(imp_prop$num_bath_isna, 0, prop$num_bath)

imp_prop$num_75_bath_isna = is.na(prop$num_75_bath)
clean_prop$num_75_bath = ifelse(imp_prop$num_75_bath_isna, 0, prop$num_75_bath)

imp_prop$num_fireplace_isna = is.na(prop$num_fireplace)
clean_prop$num_fireplace = ifelse(imp_prop$num_fireplace_isna, 0, prop$num_fireplace)

imp_prop$num_fireplace_imp = is.na(prop$num_fireplace) & prop$flag_fireplace=="true"
clean_prop$num_fireplace = ifelse(imp_prop$num_fireplace_imp, 1, clean_prop$num_fireplace)

################################################
##  tax_total
##  tax_building
##  tax_land
##  tax_property
##  tax_year
##  tax_delinquency
##  tax_delinquency_year
##

imp_prop$tax_total_isna = is.na(prop$tax_total)
clean_prop$tax_total = ifelse(imp_prop$tax_total_isna, 0, prop$tax_total)

imp_prop$tax_building_isna = is.na(prop$tax_building)
clean_prop$tax_building = ifelse(imp_prop$tax_building_isna, 0, prop$tax_building)

imp_prop$tax_land_isna = is.na(prop$tax_land)
clean_prop$tax_land = ifelse(imp_prop$tax_land_isna, 0, prop$tax_land)

imp_prop$tax_property_isna = is.na(prop$tax_property)
clean_prop$tax_property = ifelse(imp_prop$tax_property_isna, 0, prop$tax_property)

# tax_year has always the same value; drop it;

clean_prop$tax_delinquency = ifelse(prop$tax_delinquency=="Y", 1, 0)

imp_prop$tax_delinquency_year_isna = is.na(prop$tax_delinquency_year)
clean_prop$tax_delinquency_year = ifelse(imp_prop$tax_delinquency_year_isna, 0, prop$tax_delinquency_year)

pc_tax_df = clean_prop[, c("tax_building", "tax_land", "tax_property"), with=F]
pc_tax = principal(pc_tax_df, nfactors = 1, rotate = "none")
clean_prop$pc_tax = pc_tax$scores


clean_prop$tax_building_rel =ifelse(clean_prop$tax_total==0, 0, clean_prop$tax_building/clean_prop$tax_total)
clean_prop$tax_land_rel =ifelse(clean_prop$tax_total==0, 0, clean_prop$tax_land/clean_prop$tax_total)
clean_prop$tax_property_rel =ifelse(clean_prop$tax_total==0, 0, clean_prop$tax_property/clean_prop$tax_total)


################################################
##  material
##  deck
##  quality
##  framing
##  story
##  heating
##  aircon
##  architectural_style
##  flag_tub

clean_prop$material = ifelse(is.na(prop$material), 0 , prop$material)

clean_prop$material = factor(clean_prop$material, 0:18, labels=c('NA', 'Adobe', 'Brick', 'Concrete Block', 'Concrete', 'Dome', 'Frame', 
                                                       'Heavy', 'Log', 'Light', 'Metal', 'Manufactured', 'Mixed', 'Masonry', 
                                                       'Other', 'Steel', 'Stone', 'Tilt-Up', 'Wood'))

clean_prop$deck = as.factor(ifelse(is.na(prop$deck), 0, prop$deck))

clean_prop$quality = as.factor(ifelse(is.na(prop$quality), 0, prop$quality))

clean_prop$framing = as.factor(ifelse(is.na(prop$framing), 0, prop$framing))

clean_prop$story = as.factor(ifelse(is.na(prop$story), 0, prop$story))

clean_prop$heating = as.factor(ifelse(is.na(prop$heating), 0, prop$heating))

clean_prop$aircon = as.factor(ifelse(is.na(prop$aircon), 0, prop$aircon))

clean_prop$architectural_style = as.factor(ifelse(is.na(prop$architectural_style), 0, prop$architectural_style))

clean_prop$flag_tub = ifelse(prop$flag_tub=="true", 1, 0)


################################################
##  longitude
##  latitude

clean_prop$longitude = ifelse(is.na(prop$longitude), 0, prop$longitude)
clean_prop$latitude = ifelse(is.na(prop$latitude), 0, prop$latitude)

pc_longlat = principal(clean_prop[, c("longitude", "latitude"), with=F], nfactors = 1, rotate = "none") 
clean_prop$pc_long_lat = pc_longlat$scores

################################################
##  censustractandblock
##  

clean_prop$censustractandblock = as.integer(as.factor(prop$censustractandblock))


############################################################################################
############################################################################################
## add latitude / longitude cluster
##

# Sys.time()
# set.seed(123)
# clust_df = data.frame(latitude = clean_prop$latitude, longitude=clean_prop$longitude)#, logerror=clean_prop$logerror)
# km_model = kcca(clust_df, k=30, kccaFamily("kmeans"))
# Sys.time()
# clean_prop$long_lat_cluster = as.factor(predict(km_model, clust_df))
# Sys.time()

################################################
##  add numeric columns additional as feature
##

#clean_prop$month_factor = as.factor(month(clean_prop$date))
#clean_prop$month = month(clean_prop$date)
clean_prop$num_garage_fac=as.factor(clean_prop$num_garage)
clean_prop$num_garage_1_fac=as.factor(clean_prop$num_garage_1)
clean_prop$num_pool_fac=as.factor(clean_prop$num_pool)
clean_prop$num_pool_1_fac=as.factor(clean_prop$num_pool_1)
clean_prop$num_room_fac=as.factor(clean_prop$num_room)
clean_prop$num_story_fac=as.factor(clean_prop$num_story)
#clean_prop$num_unit_fac=as.factor(clean_prop$num_unit)
clean_prop$num_bathroom_fac=as.factor(clean_prop$num_bathroom)
clean_prop$num_bedroom_fac=as.factor(clean_prop$num_bedroom)
clean_prop$num_bath_fac=as.factor(clean_prop$num_bath)
clean_prop$num_75_bath_fac=as.factor(clean_prop$num_75_bath)
clean_prop$num_fireplace_fac=as.factor(clean_prop$num_fireplace)


##############################################################################
##############################################################################
#
#   unfolding to dummy columns 
#

# convertFactorToDummyTable = function(factor_vec, feature_name, min_count=100, verbose=F)
# {
#   ft = data.table(id = 1:length(factor_vec), fac = factor_vec)
#   t = ft[, .(count=length(id)), by=fac]
#   t = t[count>=min_count]
#   t = t[order(fac)]
#   
#   fac_mat = data.table(id = 1:length(factor_vec))
#   
#   i=1
#   for(fac_val in t$fac) {
#     if (verbose == T) {
#       cat("convertFactorToDummyTable for", feature_name, ": create bool vec for level ", as.character(fac_val), "(", i, "/", nrow(t), ")\n")
#     }
# 
#     tmp_mat = data.table(v1=(factor_vec == fac_val))
#     colnames(tmp_mat) = paste(feature_name, as.character(fac_val), sep = "_")
#     
#     fac_mat = cbind(
#       fac_mat,
#       tmp_mat
#     )
#     i=i+1
#   }
#   
#   fac_mat$id = NULL
#   return (fac_mat)
# }

# dummy_prop = data.table(id_parcel = clean_prop$id_parcel)
# dummy_prop = cbind(dummy_prop, convertFactorToDummyTable(clean_prop$region_city, "region_city", verbose=T))
# dummy_prop = cbind(dummy_prop, convertFactorToDummyTable(clean_prop$region_neighbor, "region_neighbor", verbose=T))
# dummy_prop = cbind(dummy_prop, convertFactorToDummyTable(clean_prop$region_zip, "region_zip", verbose=T))
# dummy_prop = cbind(dummy_prop, convertFactorToDummyTable(clean_prop$zoning_landuse_county, "zoning_landuse_county", verbose=T))


##############################################################################
#
#   expanding factors with many levels to multiple columns 
#

big_factor_names =c("num_unit", "region_neighbor", "region_zip", "region_city", "zoning_landuse_county", 
                    "zoning_property", "missing_values_pattern")

clean_tmp = as.data.table(
  transactions %>%
    inner_join(clean_prop, by="id_parcel")
)

for (big_factor_name in big_factor_names){
  cat("expanding factors to multiple features for", big_factor_name, "\n")
  t = clean_tmp[, .(count=.N), by=mget(big_factor_name)]
  t = t[count>50]
  
  t = t[order(-t$count)]
  i=0
  while (i<nrow(t)) {
    start=i+1
    end = i+30
    lev = t[[big_factor_name]][start:end]
    clean_prop[[paste(big_factor_name, "fac", start, end, sep="_")]] = as.factor(ifelse(clean_prop[[big_factor_name]] %in% lev, clean_prop[[big_factor_name]], 0))
    i = i + 30
  }
}

rm(clean_tmp)

################################################
##  clean_prop-up
##  
rm(avg_garage_area)
rm(avg_pool_area)
rm(qt)
rm(t)
rm(start)
rm(end)
rm(lev)
rm(big_factor_name)
rm(big_factor_names)
rm(i)

############################################################################################
############################################################################################
############################################################################################


# missing_values <- clean_prop %>% summarize_each(funs(sum(is.na(.))/n()))
# 
# missing_values <- gather(missing_values, key="feature", value="missing_pct")
# missing_values %>% 
#   ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
#   geom_bar(stat="identity",fill="red")+
#   coord_flip()+theme_bw()
# 
############################################################################################
################### Yiming, updates, adds fips_blockid column in clean_prop data ################
############################################################################################

##### I don't find rawcensustractandblock in clean_prop, which is a very informative column.

##### Introduce and extract the blockid from rawcensustractandblock column in prop
##### For example: 60371066.461001: '6037'(County id or so called 'fips')
#####                               '1066.46' (block id, 4 digits before and 2 digits after dot)
#####                               omit the rest digits

clean_prop$rawcensustractandblock <- prop$rawcensustractandblock
clean_prop$fips_blockid <- sapply(clean_prop$rawcensustractandblock, function(x) substr(x, 1, 11))
clean_prop$rawcensustractandblock <- NULL

##### add NA tag column of censustractandblock in imp_prop, set NAs in clean_prop$censustractandblock to 0
imp_prop$censustractandblock_isna = is.na(prop$censustractandblock)
clean_prop$censustractandblock = prop$censustractandblock
clean_prop$censustractandblock[imp_prop$censustractandblock_isna] <- 0

# #### before mice impute
# data_sale_mice <- clean_prop %>% select(aircon, heating, logerror, latitude, longitude, fips_blockid)
# data_sale_mice$region_zip <- prop$region_zip
# data_sale_mice$aircon_isna <- is.na(data_sale_mice$aircon)
# data_sale_mice$heating_isna <- is.na(data_sale_mice$heating)
# data_sale_mice$region_zim_isna <- is.na(data_sale_mice$region_zip)
# data_sale_mice$region_zip[data_sale$region_zip == 399675] <- 99675
# 
# ##### imputation train
# mice_mod <- mice(data_sale_mice, method='rf') 
# mice_output <- complete(mice_mod)
# 
# ##### imputation
# data_sale_mice$airconimp <- mice_output$aircon
# data_sale_mice$heatingimp <- mice_output$heating
# data_sale_mice$region_zip <- mice_output$region_zip
# ##### The mice imputation seems perfect! histograms matches perfectly!
# ##### Much better than the result (aircon, heating) from missForest



# sales = as.data.table(
#   transactions %>%
#     inner_join(properties, by="id_parcel")
# )

clean = as.data.table(
  transactions %>%
    inner_join(clean_prop, by="id_parcel")
)

imp = as.data.table(
  transactions %>%
    inner_join(imp_prop, by="id_parcel")
)

imp$date = NULL
imp$logerror = NULL

clean$date = as.Date(clean$date)
clean$month_factor = as.factor(month(clean$date))
clean$month = month(clean$date)

rm(prop)

################################################
##  add logerror quantile
##  

qt = quantile(clean$logerror, c(1/3, 2/3))
clean$logerror_q3 = ifelse(clean$logerror<=qt[1], 1, 
                                ifelse(clean$logerror<=qt[2], 2, 3))

clean$logerror_q3 = as.factor(clean$logerror_q3)
rm(qt)

################################################
################################################
#  SAVE

# save(clean, file="clean.dat")
# save(imp, file="imp.dat")
# save(clean_prop, file="clean_prop.dat")
# save(imp_prop, file="imp_prop.dat")



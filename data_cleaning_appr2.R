library(dplyr)
library(ggplot2)
library(data.table)


properties <- fread('../properties_2016.csv')
transactions <- fread('../train_2016_v2.csv')

properties <- properties %>% rename(
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

transactions <- transactions %>% rename(
  id_parcel = parcelid,
  date = transactiondate
)

try_properties <- properties

del <- sapply(try_properties, function(x) sum(is.na(x)) / nrow(try_properties) > 0.75)
del <- c(1:ncol(try_properties))[del]
try_properties <- try_properties %>% select(-del)

try_properties$rawcensustractandblock <- as.character(try_properties$rawcensustractandblock)
try_properties$fips_blockid <- sapply(try_properties$rawcensustractandblock, function(x) substr(x, 1, 11))
try_properties$rawcensustractandblock <- NULL



#########remove all NA properties########################
# I don't plan to train them (11437 obs.)
# I plan to add mean logerror to those obs. in prediction
#########################################################
head(try_properties %>% filter(is.na(latitude)), 20)
prop <- try_properties %>% filter(!is.na(latitude))
all_na_prop <- try_properties %>% filter(is.na(latitude))

prop_ori_backup <- try_properties %>% filter(!is.na(latitude))



######### do more feature engineering ###################
num_na <- sapply(prop, function(x) sum(is.na(x)) / nrow(prop))
na_df <- data.frame(cols = colnames(prop), perc_na = num_na, stringsAsFactors = F, row.names = NULL)
ggplot(data = na_df, aes(x = reorder(cols, perc_na), y = perc_na)) + 
  geom_bar(stat = 'identity') + ylim(c(0, 1)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


prop$aircon <- ifelse(prop$aircon %in% c(3, 9, 12), 13, prop$aircon)


prop$region_neighbor <- NULL

prop$heating <- ifelse(prop$heating %in% c(1, 10, 11, 12, 24, 19), 14, prop$heating)
prop$heating <- ifelse(prop$heating %in% c(18, 21), 7, prop$heating)
prop$heating <- ifelse(prop$heating == 6, 2, prop$heating)

prop$area_live_finished <- NULL

prop$num_bathroom_calc <- NULL
prop$num_bath <- NULL
prop$censustractandblock <- NULL

prop$tax_land <- NULL

prop$tax_building <- NULL

prop$tax_total <- NULL

prop$region_zip <- ifelse(prop$region_zip == 399675, 99675, prop$region_zip)

prop$zoning_property <- NULL

prop$tax_delinquency <- NULL

prop$lon <- (prop$longitude - min(prop$longitude)) / (max(prop$longitude) - min(prop$longitude))
prop$lat <- (prop$latitude - min(prop$latitude)) / (max(prop$latitude) - min(prop$latitude))

prop$latitude <- NULL
prop$longitude <- NULL


########flag related
prop$flag_tub <- ifelse(prop$flag_tub == '', 'false', prop$flag_tub)
prop$flag_fireplace <- ifelse(prop$flag_fireplace == '', 'false', prop$flag_fireplace)

# same with region_county, remove
prop$fips <- NULL

prop$num_bedroom <- as.integer(prop$num_bedroom)
prop$heating <- as.integer(prop$heating)
prop$build_year <- as.integer(prop$build_year)
prop$num_room <- as.integer(prop$num_room)
prop$region_zip <- as.integer(prop$region_zip)
prop$zoning_landuse <- as.factor(prop$zoning_landuse)
prop$zoning_landuse_county <- as.factor(prop$zoning_landuse_county)
prop$flag_tub <- as.factor(prop$flag_tub)
prop$region_county <- as.factor(prop$region_county)
prop$flag_fireplace <- as.factor(prop$flag_fireplace)
prop$fips_blockid <- as.factor(prop$fips_blockid)
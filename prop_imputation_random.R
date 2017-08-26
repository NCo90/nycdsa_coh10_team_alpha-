library(mice)


mice_prop <- prop %>% select(-id_parcel)

mice_prop$fips_blockid <- as.integer(mice_prop$fips_blockid)
mice_prop$zoning_landuse_county <- as.integer(mice_prop$zoning_landuse_county)
mice_prop$zoning_landuse <- as.integer(mice_prop$zoning_landuse)


mice_mod <- mice(mice_prop, method='sample')
prop_imp <- complete(mice_mod)


prop_imp$fips_blockid <- prop$fips_blockid
prop_imp$zoning_landuse <- prop$zoning_landuse
prop_imp$zoning_landuse_county <- prop$zoning_landuse_county
prop_imp$region_city <- as.factor(prop_imp$region_city)
prop_imp$fips_blockid <- as.factor(prop_imp$fips_blockid)
prop_imp$zoning_landuse <- as.factor(prop_imp$zoning_landuse)
prop_imp$zoning_landuse_county <- as.factor(prop_imp$zoning_landuse_county)
prop_imp$aircon <- as.factor(prop_imp$aircon)
prop_imp$heating <- as.factor(prop_imp$heating)
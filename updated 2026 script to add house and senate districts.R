#updated house senate districts

library(sf)
library(dplyr)
library(readxl)
library(lwgeom)
library(tidygeocoder)


lara_df <- read_xlsx("Michigan MHCs - LARA 012026.xlsx")
lara_df$County <- str_to_title(lara_df$County)
lara_df$Zip_Code <- as.character(lara_df$Zip_Code)
  
lara_df

house_districts <- st_read("Michigan_State_House_Districts_2021.json")  # Map Layer (Geographic Boundaries) - it changes every 10 years so don't have to change
senate_districts <- st_read("Michigan_State_Senate_Districts_2021.json")

# half <- ceiling(nrow(lara_df) / 2)

# lara1 <- lara_df[1:half, ]
# lara2 <- lara_df[(half + 1):nrow(lara_df), ]
# 
# geolara1 <- lara1 %>%
#   geocode(
#     address = Location_Address,
#     method = "osm",
#     lat = latitude,
#     long = longitude
#   )
# 
# geolara <- lara_df %>%
#   geocode(
#     address = Location_Address,
#     method = "census",
#     lat = latitude,
#     long = longitude
#   )


geolara_arcgis <- lara_df %>%
  geocode(
    address = Location_Address,
    method = "arcgis",
    lat = latitude,
    long = longitude
  )

write.csv(geolara_arcgis, "geolara_df.csv")

# geolara2 <- lara2 %>%
#   geocode(
#     address = Location_Address,
#     method = "osm",
#     lat = latitude,
#     long = longitude
#   )

# write.csv(geolara1, "geolara1.csv")
# write.csv(geolara2, "geolara2.csv")

lara_sf <- st_as_sf(geolara_arcgis,
                    coords = c("longitude", "latitude"),
                    crs = 4326)

house_districts <- st_transform(house_districts, st_crs(lara_sf))
senate_districts <- st_transform(senate_districts, st_crs(lara_sf))
house_districts <- st_make_valid(house_districts)
senate_districts <- st_make_valid(senate_districts)

sf_use_s2(FALSE) # RUN THIS TO MAKE GEO NON SPHERICAL AND PLANAR

lara_with_districts <- st_join(lara_sf, house_districts, join = st_intersects)

lara_with_house <- lara_with_districts %>%
  mutate(`House district` = NAME)

lara_with_house <- lara_with_house %>%
  select(-geometry,-ShapeSTLength,-ShapeSTArea,-PENINSULA,-LAYOUT,-VER,-ACRES,
         -SQMILES,-SQKM,-URL,-PARTY,-LEGISLATOR,-TYPE,-LABEL,-NAME,-OBJECTID)

lara_with_house_senate <- st_join(lara_with_house, senate_districts, join = st_intersects)

lara_with_house_senate <- lara_with_house_senate %>%
  mutate(`Senate district` = NAME)

lara_with_house_senate <- lara_with_house_senate %>%
  select(-geometry,-ShapeSTLength,-ShapeSTArea,-PENINSULA,-LAYOUT,-VER,-ACRES,
         -SQMILES,-SQKM,-URL,-PARTY,-LEGISLATOR,-TYPE,-LABEL,-NAME,-OBJECTID)
lara_with_house_senate <- lara_with_house_senate %>%
  st_drop_geometry()

lara_final <- bind_cols(lara_with_house_senate, geolara_arcgis %>% select(latitude, longitude))

write.csv(lara_final, "lara_house-senate_districts_coords_2026.csv")

# adding district numbers to the raw files

library(sf)
library(stringr)
library(readr)
library(dplyr)
library(readxl)
library(tidygeocoder)

missing_lara <- read.csv("geocoded_missing.csv")

lara_old <- read.csv("LARA_with_coord_and_legislativedistrict1.csv")
lara_old$County <- str_to_title(lara_old$County)

lara_df <- read.csv("Michigan_MHCs_2025_with_lara_coords.csv") # Updated Data: I can get this on CTAC Drive -> reformat as Vicky's / (Lat,Long) Location Address, use R package to find the corresponding lat/long
lara_df$County <- str_to_title(lara_df$County)

house_districts <- read_sf("Michigan_State_House_Districts_2021.json")  # Map Layer (Geographic Boundaries) - it changes every 10 years so don't have to change
senate_districts <- read_sf("Michigan_State_Senate_Districts_2021.json")

lara_old_subset <- lara_old %>%
  select(Record_No, "House District" = House.district, "Senate District" = Senate.district)

lara_df <- lara_df %>%
  left_join(lara_old_subset, by = "Record_No")

write.csv(lara_df, "lara_withcoord_with_districts2025.csv")

missing_lara_df <- lara_df %>%
  filter(is.na(`House District`) | is.na(`Senate District`)) %>%
  filter(!is.na(latitude) & !is.na(longitude))

lara_missing_sf <- st_as_sf(missing_lara_df, coords = c("longitude", "latitude"), crs = 4326)

lara_with_house <- st_join(lara_missing_sf, house_districts, join = st_intersects)

st_join(senate_districts, join = st_within, suffix = "_senate")
  



# MHVillage
mhvillage_old <- read.csv("MHVillageDec7_Legislative1.csv")
mhvillage_old$County <- str_to_title(mhvillage_old$County)

mhvillage_df <- read_xlsx("mi_parks from MHVillage 2-5-25.xlsx") # Updated Data: I can get this on CTAC Drive -> reformat as Vicky's / (Lat,Long) Location Address, use R package to find the corresponding lat/long

house_districts <- read_sf("Michigan_State_House_Districts_2021.json")  # Map Layer (Geographic Boundaries) - it changes every 10 years so don't have to change
senate_districts <- read_sf("Michigan_State_Senate_Districts_2021.json")

mhv_old_subset <- mhvillage_old %>%
  select(Url, House.district, Senate.district)

mhvillage_df_district <- mhvillage_df %>%
  left_join(mhv_old_subset, by = "Url")

write.csv(mhvillage_df_district, "mhvillage_withcoord_withdistricts2025.csv")

mhvillage_new <- read.csv("mhvillage_withcoord_withdistricts2025.csv")

mhvillage_na <- mhvillage_new %>%
  filter(is.na(House.district) | is.na(Senate.district)) %>%
  mutate(full_address = paste(Address,City.State,ZIP))

geocoded_df <- mhvillage_na %>%
  geocode(address = full_address, method = "osm")

failed <- geocoded_df %>% filter(is.na(lat) | is.na(long))

results_retry <- failed %>%
  geocode(address = full_address, method = "arcgis")  # or "census"

# Combine results
combined <- bind_rows(
  geocoded_df %>% filter(!is.na(lat)),
  results_retry
)

df <- combined %>%
  mutate(
    lat = coalesce(lat, `lat...13`),
    long = coalesce(long, `long...14`)
  ) %>%
  select(-lat...11,-long...12,-lat...13,-long...14)

house_districts <- st_make_valid(house_districts)
senate_districts <- st_make_valid(senate_districts)

house_districts <- house_districts %>%
  mutate(geometry = lwgeom::lwgeom_make_valid(geometry))

geocoded_sf <- df %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)  # WGS 84

library(lwgeom)

geocoded_with_districts <- geocoded_sf %>%
  st_join(house_districts %>% select(House.district = NAME))

senate_districts <- senate_districts %>%
  mutate(geometry = lwgeom::lwgeom_make_valid(geometry))



geocoded_with_districts_senate <- geocoded_sf %>%
  st_join(senate_districts %>% select(Senate.district = NAME))


final <- final %>%
  select(Name...2, Address...3, City.State...4, 
         ZIP...5, Number.of.Sites...6, Url...7,
         House.district.y, Senate.district.y)

final <- final %>%
  rename(Name = Name...2,
         Address = Address...3,
         City.State = City.State...4,
         Zip = ZIP...5,
         Number.of.Sites = Number.of.Sites...6,
         Url = Url...7,
         House.district = House.district.y,
         Senate.district = Senate.district.y)

final_subset <- final %>%
  select(Url, House.district, Senate.district)

to_csv <- mhvillage_new %>%
  left_join(final_subset, by = "Url") %>%
  mutate(House.district.y = as.integer(House.district.y),
         Senate.district.y = as.integer(Senate.district.y)) %>%
  mutate(
    House.district = coalesce(House.district.x,House.district.y),
    Senate.district = coalesce(Senate.district.x,Senate.district.y)
    ) %>%
  select(-Senate.district.x,-Senate.district.y,-House.district.x,-House.district.y)

write.csv(to_csv, "mhvillage_2025.csv")

to_csv <- to_csv %>%
  mutate(full_address = paste(Address,City.State,ZIP))

to_csv_coords <- to_csv %>%
  left_join(df, by = "Url")

to_csv_coords <- to_csv_coords %>%
  select(Name.x,Address.x,City.State.x,ZIP.x,Number.of.Sites.x,Url,House.district.x,
        Senate.district.x,full_address.x,lat,long) 

to_csv_coords <- to_csv_coords %>%
  rename(Name = Name.x,
         Address = Address.x,
         City.State = City.State.x,
         ZIP = ZIP.x,
         Number.of.Sites = Number.of.Sites.x,
         Url = Url,
         House.district = House.district.x,
         Senate.district = Senate.district.x,
         full_address = full_address.x,
         lat = lat,
         long = long)

remaining_coords <- to_csv_coords %>%
  geocode(address = full_address, method = "arcgis")  # or "census"

remaining_coords <- remaining_coords %>%
  rename(lat = lat...12, long = long...13) %>%
  select(-lat...10,-long...11)

locations_with_county <- reverse_geocode(
  .tbl = remaining_coords,
  lat = lat,
  long = long,
  method = "osm",
  full_results = TRUE
)

csv_file <- locations_with_county %>%
  select(Name,Address,City.State,ZIP,Number.of.Sites,
         Url,House.district,Senate.district,full_address,lat,
         long,county,state)
csv_file <- csv_file %>%
  rename(County = county)
write.csv(csv_file, "mhvillage_2025.csv")

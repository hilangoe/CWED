# prep file for shiny CWED map app
library(tidyverse)
library(sf)
library(cshapes)
#library(ggplot2)
library(countrycode)
library(lubridate)

# pulling in whole shape file from cshapes, then fixing invalid geometries
cshape_df <- cshp(useGW = FALSE, dependencies = FALSE)
valid_check <- st_is_valid(cshape_df)
if (!all(valid_check)) {
  cshape_df <- st_make_valid(cshape_df)
}

# getting cap coordinates
cap_df <- data.frame(cowcode = cshape_df$cowcode,
                       start = cshape_df$start,
                       end = cshape_df$end,
                       caplong = cshape_df$caplong,
                       caplat = cshape_df$caplat)


# estimating centroids for each multipolygon (country) and extracting coordinates
centroids <- st_centroid(cshape_df)
centroid_coords <- st_coordinates(centroids) %>%
  data.frame() %>%
  rename(cent_long = X, cent_lat = Y)
head(centroid_coords)

# creating dataframe with country code, start and end dates, and centroid coordinates
centroid_df <- data.frame(cowcode = cshape_df$cowcode, 
                          start = cshape_df$start, 
                          end = cshape_df$end, 
                          centroid_coords)
# new observation everytime a country's territory changes
head(centroid_df)

# time to pull in CWED data
df <- read_csv("_data/_raw/cwedfull2clean.csv")

# clean up, add country names, and aggregate to conflict-intervener level
df <- df %>%
  rename(ccode1 = locationid1, ccode2 = external_nameid) %>%
  mutate(ccode1 = ifelse(ccode1==340, 345, ccode1)) %>% # fixing Yugoslavia
  mutate(ccode2 = ifelse(ccode2==340, 345, ccode2)) %>% # ditto
  mutate(ccode1 = ifelse(ccode1==678, 679, ccode1)) %>% # fixing Yemen
  mutate(ccode2 = ifelse(ccode2==678, 679, ccode2)) %>% # ditto
  mutate(ccode2 = ifelse(ccode2==260, 255, ccode2)) %>% # fixing Germany
  mutate(countryname1 = countrycode(ccode1, 'cown', 'country.name'),
         countryname2 = countrycode(ccode2, 'cown', 'country.name')) %>%
  select(conflictID, ccode1, countryname1, ccode2, countryname2, starty, intyear, direct) %>% 
  group_by(conflictID, ccode1, countryname1, ccode2, countryname2) %>%
  summarise(direct = max(direct), startyear = min(starty), startyear_int = min(intyear)) %>%
  ungroup() 
# 2 warnings on countrycode
# going to manually fix Yugoslavia (it's supposed to be 345 in the 1990s, not 340, which is Serbia, but only from 2006)
# also Yemen and Germany

# need conflictID list for drop-down menu
conflict_list <- df %>%
  rename(cw_country = countryname1) %>%
  select(conflictID, cw_country, startyear) %>%
  unique()

write.csv(conflict_list, "_shiny/cwed_map/conflict_list.csv", row.names = FALSE)


# join in capital coordinates (D) from cshape_df and centroid coordinates (T) from centroid_df
df <- df %>%
  left_join(cap_df, by = c("ccode1" = "cowcode")) %>%
  filter(startyear >= year(start) & startyear <= year(end)) %>%
  group_by(conflictID, ccode2) %>%
  slice(1) %>%
  ungroup() %>%
  select(-c("start", "end")) %>%
  rename(caplong_d = caplong, caplat_d = caplat) %>%
  left_join(cap_df, by = c("ccode2" = "cowcode")) %>%
  filter(startyear_int >= year(start) & startyear_int <= year(end)) %>%
  group_by(conflictID, ccode2) %>%
  slice(1) %>%
  ungroup() %>%
  select(-c("start", "end")) %>%
  rename(caplong_t = caplong, caplat_t = caplat) %>%
  left_join(centroid_df, by = c("ccode2" = "cowcode")) %>%
  filter(startyear_int >= year(start) & startyear_int <= year(end)) %>%
  group_by(conflictID, ccode2) %>%
  slice(1) %>%  # taking the first observation by group
  ungroup() %>%
  select(-c("start", "end")) %>%
  rename(centlong_t = cent_long, centlat_t = cent_lat)

# save as csv for shiny app
write.csv(df, "_shiny/cwed_map/cwed.csv", row.names = FALSE)


# scraps
# %>%
#   left_join(., capcoord, by = c("ccode1" = "cowcode")) %>%
#   rename(caplong_d = caplong, caplat_d = caplat) %>%
#   left_join(., capcoord, by = c("ccode2" = "cowcode")) %>%
#   rename(caplong_t = caplong, caplat_t = caplat)

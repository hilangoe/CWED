# script for shiny app generating map of CWED data per conflict

library(ggrepel)

conf <- read.csv("_shiny/cwed_map/conflict_list.csv")
df <- read.csv("_shiny/cwed_map/cwed.csv")


row <- 8

selectedID <- unique(paste(conf$conflictID[row], conf$cw_country[row], conf$startyear[row]))
selected_info <- strsplit(selectedID, " ")[[1]]

filtered_df <- df %>% 
  filter(., conflictID == as.numeric(selected_info[1]))

selected_year <- as.Date(selected_info[3], format = "%Y")

# creating the shape file for the year the civil war started
shape <- cshp(date = selected_year, useGW = FALSE, dependencies = FALSE) %>%
  mutate(cw = ifelse(country_name==selected_info[2], 1, 0)) # generating dummy for civil war country
# filter(shape, cw==1) # making sure it went right

# create intervener df
df_int <- df %>%
  filter(conflictID==selected_info[1]) %>%
  select(ccode2, direct) %>%
  rename(cowcode = ccode2) %>%
  mutate(int = 1)

# create coordinate df for intervention
df_coord_int <- df %>%
  filter(conflictID==selected_info[1]) %>%
  select(caplong_d, caplat_d, caplong_t, caplat_t)

# create coordinate df for retaliation
df_coord_ret <- df %>%
  filter(conflictID==selected_info[1] & direct==1) %>%
  select(caplong_d, caplat_d, centlong_t, centlat_t)

# join shape and df_int, create cat variable for roles
shape <- shape %>%
  left_join(., df_int, by = 'cowcode') %>% # joining intervention
  mutate(direct = ifelse(is.na(direct), 0, direct), int = ifelse(is.na(int), 0, int)) %>%
  mutate(role = ifelse(direct==1, 4, 
                       ifelse(direct==0 & int==1, 3, 
                              ifelse(cw==1, 2, 1))))

# set levels
labs <- c('Peace', 'Civil war', 'Not targeted INT', 'Targeted INT')
shape$role <- factor(shape$role, levels = seq_along(labs), labels = labs)


# define color scheme
cols_map <- c('white', 'pink2', 'lightskyblue2', 'lightskyblue3')

# generate map
map <- ggplot(data = shape) +
  geom_sf(aes(fill = as.factor(role))) +
  scale_fill_manual(values = cols_map, name = "Civil war expansion", labels = levels(shape$role), drop = FALSE) +
  geom_curve(data = df_coord_int, 
             aes(x = caplong_t, y = caplat_t, xend = caplong_d, yend = caplat_d),
             curvature = -0.4,
             arrow = arrow(length = unit(0.1, "cm"), ends = "last", type = "closed"),
             size = 1,
             colour = "gray30",
             alpha = 0.7) +
  geom_curve(data = df_coord_ret, 
             aes(x = caplong_d, y = caplat_d, xend = centlong_t, yend = centlat_t), # update with different coordinates
             curvature = -0.4,
             arrow = arrow(length = unit(0.3, "cm"), ends = "last", type = "closed"),
             size = 1,
             colour = "firebrick4",
             alpha = 0.8)

ggsave(filename = "1.png", plot = map, width = 24, height = 9, dpi = 300)



map <- ggplot(data = shape) +
  geom_sf(aes(fill = as.factor(role))) +
  scale_fill_manual(values = cols_map, name = "Civil war expansion", labels = levels(shape$role), drop = FALSE) +
  geom_label_repel(data = filter(shape, role != "Peace"), 
                   aes(x = caplong, y = caplat, label = country_name), 
                   size = 3, color = "black", box.padding = 0.5, point.padding = 0.5)

ggsave(filename = "2.png", plot = map, width = 24, height = 9, dpi = 300)

map <- ggplot(data = shape) +
  geom_sf(aes(fill = as.factor(role))) +
  scale_fill_manual(values = cols_map, name = "Civil war expansion", labels = levels(shape$role), drop = FALSE) +
  geom_curve(data = df_coord_int, 
             aes(x = caplong_t, y = caplat_t, xend = caplong_d, yend = caplat_d),
             curvature = -0.4,
             arrow = arrow(length = unit(0.1, "cm"), ends = "last", type = "closed"),
             size = 1,
             colour = "gray30",
             alpha = 0.7) +
  geom_curve(data = df_coord_ret, 
             aes(x = caplong_d, y = caplat_d, xend = centlong_t, yend = centlat_t), # update with different coordinates
             curvature = -0.4,
             arrow = arrow(length = unit(0.3, "cm"), ends = "last", type = "closed"),
             size = 1,
             colour = "firebrick4",
             alpha = 0.8) +
  geom_label_repel(data = filter(shape, role != "Peace"), 
                  aes(x = caplong, y = caplat, label = country_name), 
                  size = 3, color = "black", box.padding = 0.5, point.padding = 0.5)
ggsave(filename = "3.png", plot = map, width = 24, height = 9, dpi = 300)



# Old stuff ---------------------------------------------------------------



# need df for conflictID with year and ccode

# now let's create a map with lines for a specific conflict
# let's do South Africa (560), conflictID == 150, year ==1981

# need input for year from conflictID list


shape_1981 <- cshp(date= as.Date('1981-01-01', useGW = FALSE, dependencies = FALSE)) %>%
  rename(cowcode = gwcode)

# getting coordinates for geom_curve
capcoord <- data.frame(cowcode = shape_1981$cowcode,
                       caplong = shape_1981$caplong,
                       caplat = shape_1981$caplat)
capcoord <- unique(capcoord)

# need coordinates for middle of country



# need input for country from conflictID list


## this can probably be done separately and pulled as ready df
# adding in name of civil war country
df <- df.cwed %>%
  rename(ccode1 = locationid1, ccode2 = external_nameid) %>%
  mutate(countryname1 = countrycode(ccode1, 'cown', 'country.name'),
         countryname2 = countrycode(ccode2, 'cown', 'country.name')) %>%
  select(intyear, conflictID, ccode1, countryname1, ccode2, countryname2, direct) %>%
  group_by(intyear, conflictID, ccode1, countryname1, ccode2, countryname2) %>%
  summarise(direct = max(direct)) %>%
  ungroup() %>%
  left_join(., capcoord, by = c("ccode1" = "cowcode")) %>%
  rename(caplong_d = caplong, caplat_d = caplat) %>%
  left_join(., capcoord, by = c("ccode2" = "cowcode")) %>%
  rename(caplong_t = caplong, caplat_t = caplat)

df_sa_int <- df %>%
  filter(conflictID==150) %>%
  select(ccode2, direct) %>%
  rename(cowcode = ccode2) %>%
  mutate(int = 1)

# df coordinates for intervention
df_dyad_int <- df %>%
  filter(conflictID==150) %>%
  select(caplong_d, caplat_d, caplong_t, caplat_t, direct)

# df coordinates for retaliation
df_dyad_ret <- df %>%
  filter(conflictID==150 & direct==1) %>%
  select(caplong_d, caplat_d, caplong_t, caplat_t, direct)


shape_1981 <- shape_1981 %>%
  mutate(cw = ifelse(cowcode==560, 1, 0)) %>% # creating a dummy for South Africa
  left_join(., df_sa_int, by = 'cowcode') %>% # joining intervention
  mutate(direct = ifelse(is.na(direct), 0, direct), int = ifelse(is.na(int), 0, int)) %>%
  mutate(role = ifelse(direct==1, 3, 
                       ifelse(direct==0 & int==1, 2, 
                              ifelse(cw==1, 1, 0))))
levels(shape_1981$role) <- c('Peace', 'Civil war', 'Not targeted INT', 'Targeted INT')
table(shape_1981$role)

cols_map <- c('white', 'pink2', 'lightskyblue2', 'lightskyblue3')

sa_map <- ggplot(data = shape_1981) +
  geom_sf(aes(fill = as.factor(role))) +
  scale_fill_manual(values = cols_map, name = "Civil war expansion", labels = levels(shape_1981$role)) +
  geom_curve(data = df_dyad_int, 
             aes(x = caplong_t, y = caplat_t, xend = caplong_d, yend = caplat_d),
             curvature = -0.4,
             arrow = arrow(length = unit(0.1, "cm"), ends = "last"),
             size = 1,
             colour = "gray30",
             alpha = 0.7) +
  geom_curve(data = df_dyad_ret, 
             aes(x = caplong_d, y = caplat_d, xend = caplong_t, yend = caplat_t), # update with different coordinates
             curvature = 0,
             arrow = arrow(length = unit(0.3, "cm"), ends = "last", type = "closed"),
             size = 1,
             colour = "firebrick4",
             alpha = 0.8)

# saving
ggsave(filename = "_output/_figures/sa_map.png", plot = sa_map, width = 24, height = 18, dpi = 300)


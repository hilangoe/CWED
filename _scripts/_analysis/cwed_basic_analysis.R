## Basic descriptive statistics and graphs for CWED

library(tidyverse)
library(sf)
library(cshapes)
library(ggplot2)
library(countrycode)

# to-do:
# load dataset
df.cwed <- read_csv("_data/_raw/cwedfull2clean.csv")


# Descriptive stats and graphs --------------------------------------------
head(df.cwed)

# creating a long dataset to count ret types
df_long <- df.cwed %>%
  select(confintID, direct, proxy, indirect, covert, threat) %>%
  gather(ret_type, value, direct:threat) %>%
  filter(value==1) %>%
  mutate(ret_type = factor(ret_type, levels = custom_order))

# create a list for custom order of categories
custom_order <- c('direct', 'proxy', 'indirect', 'covert', 'threat')

# bar graph of retaliation types: disaggregated
ret_type_dis <- ggplot(df_long, aes(x = factor(ret_type, levels = custom_order), fill = ret_type)) +
  geom_bar() +
  scale_fill_brewer(palette="Spectral") +
#  scale_fill_manual(values = c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF")) +
  labs(title = "Count of retaliation types (disaggregated)",
       x = "Category",
       y = "Count",
       fill = "Retaliation")

# saving
ggsave(filename = "_output/_figures/ret_type_dis.png", plot = ret_type_dis, width = 8, height = 6, dpi = 300)

# bar graph of retaliation types: conflict-level
df_long <- df.cwed %>%
  select(conflictID, direct, proxy, indirect, covert, threat) %>%
  group_by(conflictID) %>%
  summarise_all(max) %>%
  ungroup() %>%
  pivot_longer(cols = c(direct, proxy, indirect, covert, threat),
               names_to = "ret_type",
               values_to = "value") %>%
  filter(value==1) %>%
  select(-value) %>%
  mutate(ret_type = factor(ret_type, levels = custom_order))

ret_type_conf <- ggplot(df_long, aes(x = factor(ret_type, levels = custom_order), fill = ret_type)) +
  geom_bar() +
  scale_fill_brewer(palette="Spectral") +
  #  scale_fill_manual(values = c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF")) +
  labs(title = "Count of retaliation types (conflict-level)",
       x = "Category",
       y = "Count",
       fill = "Retaliation")

# saving
ggsave(filename = "_output/_figures/ret_type_conf.png", plot = ret_type_conf, width = 8, height = 6, dpi = 300)

# histogram of retaliation type count: disaggregated
df_hist <- df.cwed %>%
  mutate(type_count = direct + proxy + indirect + covert + threat) %>%
  select(type_count)

type_count_dis <- ggplot(df_hist, aes(x = type_count)) +
  geom_histogram(bins = 6, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  labs(title = "Number of retaliation types by observation",
       x = "Number of retaliation types",
       y = "Count") +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5))

# saving
ggsave(filename = "_output/_figures/type_count_dis.png", plot = type_count_dis, width = 8, height = 6, dpi = 300)

# histogram of retaliation type count: conflict-level

df_hist <- df.cwed %>%
  select(conflictID, direct, proxy, indirect, covert, threat) %>%
  group_by(conflictID) %>%
  summarise_all(max) %>%
  ungroup() %>%
  mutate(type_count = direct + proxy + indirect + covert + threat) %>%
  select(conflictID, type_count)
table(df_hist$type_count)

type_count_conf <- ggplot(df_hist, aes(x = type_count)) +
  geom_histogram(bins = 6, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  labs(title = "Number of retaliation types by conflict",
       x = "Number of retaliation types",
       y = "Count") +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5))

# saving
ggsave(filename = "_output/_figures/type_count_conf.png", plot = type_count_conf, width = 8, height = 6, dpi = 300)


# Maps --------------------------------------------------------------------

# maps

# going to start by building a map of all countries with civil wars with rebel-sided intervention, targets for retaliation for all sample
# first pull shape file from cshapes, setting it to december 31 of 2009, which is when sample ends
shape <- cshp(date= as.Date('2009-12-31', useGW = FALSE, dependencies = FALSE))
str(shape)

# now create df for civil war and retaliation vars
df_cw <- df.cwed %>%
  rename(ccode = locationid1) %>%
  group_by(ccode) %>%
  summarise(direct = max(direct)) %>%
  ungroup() %>%
  mutate(cw=1)

df_int <- df.cwed %>%
  rename(ccode = external_nameid) %>%
  group_by(ccode) %>%
  summarise(target = max(direct)) %>%
  ungroup() %>%
  mutate(rs = 1)

# time to join
shape <- shape %>%
  rename(ccode = gwcode) %>%
  left_join(., df_cw, by = c('ccode')) %>%
  left_join(., df_int, by = c('ccode'))

# let's see how many categories we need
head(shape)
filter(shape, cw==1 & rs==1)
table(shape$cw, shape$rs)

# need to fix NAs
shape[is.na(shape)] <- 0

# now need to create a factor variable for outcome with 8 (!) categories
shape <- shape %>%
  mutate(outcome = ifelse(cw==1 & direct==1 & target==1 & rs==1, 8, 
                          ifelse(cw==1 & direct==1 & target!=1 & rs==1, 7, 
                                 ifelse(cw==1 & direct!=1 & rs==1 & target==1, 6, 
                                        ifelse(cw==1 & direct!=1 & rs==1 & target!=1, 5,
                                               ifelse(cw!=1 & rs==1 & target==1, 4,
                                                      ifelse(cw!=1 & rs==1 & target!=1, 3,
                                                             ifelse(cw==1 & direct==1 & rs!=1, 2, 
                                                                    ifelse(cw==1 & direct!=1 & rs!=1, 1, 0)))))))))
table(shape$outcome)
# need to validate to make sure everything went well
filter(shape, outcome>5 & cw==0)
filter(shape, outcome>5 & rs==0)
filter(shape, outcome>2 & outcome<5 & cw==1)
filter(shape, outcome>0 & outcome<3 & rs==1)
# looks good

# setting levels
levels(shape$outcome) <- c('Peace', 'Local CW', 
                           'Expanded CW', 'Not targeted INT',
                           'Targeted INT', 'Local CW, Not targeted INT',
                           'Local CW, Targeted INT', 'Expanded CW, Not targeted INT',
                           'Expanded CW, Targeted INT')

cols_map <- c('white', 'pink2', 'pink3','lightskyblue2', 'lightskyblue3',  'gold2', 'gold3', 'maroon2', 'maroon3')

cwed_map <- ggplot(data = shape) +
  geom_sf(aes(fill = as.factor(outcome))) +
  scale_fill_manual(values = cols_map, name = "Outcomes", 
                    labels = c('Peace', 'Local CW', 
                              'Expanded CW', 'Not targeted INT',
                              'Targeted INT', 'Local CW, Not targeted INT',
                              'Local CW, Targeted INT', 'Expanded CW, Not targeted INT',
                              'Expanded CW, Targeted INT'))

# saving
ggsave(filename = "_output/_figures/cwed_map.png", plot = cwed_map, width = 24, height = 9, dpi = 300)

# now let's create a map with lines for a specific conflict
# let's do South Africa (560), conflictID == 150, year ==1981
shape_1981 <- cshp(date= as.Date('1981-01-01', useGW = FALSE, dependencies = FALSE)) %>%
  rename(cowcode = gwcode)

# getting coordinates for geom_curve
capcoord <- data.frame(cowcode = shape_1981$cowcode,
                       caplong = shape_1981$caplong,
                       caplat = shape_1981$caplat)

capcoord <- unique(capcoord)


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
             aes(x = caplong_d, y = caplat_d, xend = caplong_t, yend = caplat_t),
             curvature = 0,
             arrow = arrow(length = unit(0.3, "cm"), ends = "last", type = "closed"),
             size = 1,
             colour = "firebrick4",
             alpha = 0.8)

# saving
ggsave(filename = "_output/_figures/sa_map.png", plot = sa_map, width = 24, height = 18, dpi = 300)



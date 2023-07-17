## Basic descriptive statistics and graphs for CWED

library(tidyverse)
library(sf)
library(cshapes)
library(ggplot2)

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

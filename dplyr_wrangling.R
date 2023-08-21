library(tidyverse)
library(palmerpenguins)
library(lterdatasampler)

# look for an exact match: ==

penguins_biscoe <- penguins |> filter(penguins$island == "Biscoe")

penguins_2007 <- penguins %>% filter(year == 2007)

adelie_torgersen <- penguins %>%  filter(species == "Adelie" & island == "Torgersen")

# Create a subset from penguins that only contains Gentoo penguins observed in 2008

penguins_gentoo <- penguins %>%  filter(species == "Gentoo", year == 2008)

gentoo_adelie <- penguins %>% filter(species == "Gentoo" | species == "Adelie")

#create a subset that contains obbservations where the island is Dream OR the year is 2009

dream_or_2009 <- penguins %>% filter(year == 2009 | island == "Dream")


# make a ggplot chart of water temperature versus crab size
ggplot(data = pie_crab , aes(x=water_temp, y=size))+geom_point()+labs(title = "crab size vs. water temperature")+ylab("crab size") + xlab("water temperature")

#pie_crab %>% filter(site == "NIB" | site == "ZI" | site == "DB" | site == "JC")
#we can use the %in% operator to ask: does the value in our column match ANY of the values IN this vector?

pie_sites <- pie_crab %>%  filter(site %in% c("NIB", "ZI", "DB", "JC"))

sites <- c("CC", "BB", "PIE")
pie_sites_2 <- pie_crab %>%  filter(site %in% sites)


#create a subset using the %in% operator that includes sites PIE, ZI, NIB, BB, and CC


sites_2 <- c("PIE", "ZI", "NIB", "BB", "CC")
pie_sites_3 <- pie_crab %>% filter(site %in% sites_2)

# Excluding filter statements

# != (asks is this NOT equal to that value)?

exclude_zi <- pie_crab %>% filter(site != "ZI")
exclude_bbccpi <- pie_crab %>% filter(!site %in% c("BB", "CC", "PI"))


# Create a subset from pie_crab that only contains observations from NIC, CC, PI, from size exceeding 13

pie_crab3 <- pie_crab %>% filter(site %in% c("NIC", "CC", "PI") & size > 13)


# -------- selecting columns -------- #

# Select individual columns by name, separate them by a comma
crabs_subset <- pie_crab %>% select(latitude, size, water_temp)

# select range of columns
crabs_subset2 <- pie_crab %>% select(site:air_temp)

#select a range and an individual column
crabs_subset <- pie_crab %>% select(date:water_temp, name)

pie_crab %>% select(name, water_temp, size)

#-----mutate!----#

crabs_cm <- pie_crab %>% mutate(size_cm = size / 10)

# what happens if I use mutate to add a new column containing the mean of the size column?

crabs_new_col <- pie_crab %>% mutate(mean_value = mean(pie_crab$size, na.rm= TRUE))

crabs_awesome <- pie_crab %>% mutate(name = "Teddy is awesome")

mean_size_by_site <- pie_crab %>%  group_by(site) %>% summarize(mean_size = mean(size, na.rm = TRUE))

group_mutate <- pie_crab %>%
  group_by(site) %>%  #recognize that there are discrete levels within each column#
  mutate(mean_size = mean(size, na.rm = TRUE))


#what if i want to create a new column in pie crab that contains giant if the size is greater than 35, or "not giant" if the size is less than or equal to 35?

# use dplyr::case_when() to write if-else statements more easily
crabs_bin <- pie_crab %>% mutate(size_binned = case_when(
  size > 35 ~ "giant", #this is my condition and ~ which is what you want to say when it is true
  size <= 35 ~ "not giant"
))


sites_binned <- pie_crab %>%
  mutate(region = case_when(
    site %in% c("ZI", "CC", "PIE") ~ "Low",
    site %in% c("BB", "NIB") ~ "Middle",
    TRUE ~ "HIGH" #in all other situations, all columns should contain this#
  ))







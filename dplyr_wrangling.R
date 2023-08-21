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













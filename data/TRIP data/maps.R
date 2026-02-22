# Figures 3 and 4

library(dplyr)
library(countrycode)
library(maps)
library(ggplot2)
library(haven)


# load the master data
data <- read_dta("replication_data.dta")


# load the map data and attach the iso codes
world_data <- map_data("world") %>%
  fortify() %>%
  mutate(country_text_id = countryname(region, destination = "iso3c"))
    

# plot gmc for 2000
world_data %>% 
    # generate year variable
  mutate(year=2000) %>%
  full_join(subset(data, year==2000), by=c("country_text_id", "year")) %>%
    # fixes sudan/south sudan
  mutate(country_text_id = ifelse(country_text_id=="SSD", "SDN", country_text_id)) %>%
  group_by(country_text_id) %>%
  mutate(gmc = ifelse(country_text_id=="SDN", max(gmc, na.rm = T), gmc)) %>%
    #plots the gmc
  ggplot(aes(x=long, y=lat, group=group, fill = as.factor(gmc))) +
  geom_polygon(color="white", size=0.1) +
  theme_void() +
  scale_fill_viridis_d(end=0.8, direction = 1,  na.value = "grey50", name="",
                       labels = c("Not possible/specified", "Possible, de jure", "No data")) 

ggsave("gmc2000.pdf", width=7, height=3)


world_data %>% 
  # generate year variable
  mutate(year=2021) %>%
  full_join(subset(data, year==2021), by=c("country_text_id", "year")) %>%
  #plots the gmc
  ggplot(aes(x=long, y=lat, group=group, fill = as.factor(gmc))) +
  geom_polygon(color="white", size=0.1) +
  theme_void() +
  scale_fill_viridis_d(end=0.8, direction = 1,  na.value = "grey50", name="",
                       labels = c("Not possible/specified", "Possible, de jure", "No data")) 

ggsave("gmc2021.pdf", width=7, height=3)


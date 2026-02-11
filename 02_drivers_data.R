## Exploring driver data
library(tidyverse)


drivers <- readRDS("./Data/beewalk_environmental_drivers_centroids.rds") 
#10173

# explore spatial variation in land cover

drivers_long <- drivers %>%
  select(site_id, BNG_east, BNG_north,
         lcm_2024_Deciduous_woodland_pct:lcm_2024_Saltmarsh_pct) %>%
  pivot_longer(starts_with("lcm_2024"), 
               names_to = "landcover_type",
               values_to = "value") %>%
  filter(landcover_type %in% c("lcm_2024_Arable_pct",
                               "lcm_2024_Urban_pct",
                               "lcm_2024_Deciduous_woodland_pct",
                               "lcm_2024_Coniferous_woodland_pct" ))



ggplot(drivers_long, aes(BNG_east, BNG_north, colour = value)) +
  geom_point(size = 3) +
  scale_colour_viridis_c() +
  facet_grid(~landcover_type) +
  coord_equal() +
  theme_bw()

# what about pesticide load?
# no temporal signal

ggplot(drivers, aes(Year, pesticide_total_load, group = site_id, colour = site_id)) +
  geom_line(alpha = 0.4) +
  theme_bw() +
  theme(legend.position = "none")

ggplot(drivers, aes(BNG_east, BNG_north, colour = pesticide_total_load)) +
  geom_point(size = 3) +
  scale_colour_viridis_c() +
  facet_grid(~Year) +
  coord_equal() +
  theme_bw()



# how about temperature
# this is the most promising
# maybe we can derive some temperature anomaly metrics

ggplot(drivers, aes(Year, temp_mean, group = site_id, colour = site_id)) +
  geom_line(alpha = 0.4) +
  theme_bw() +
  theme(legend.position = "none") +
  xlim(2010,2024)

ggplot(drivers %>%
         filter(Year > 2009), 
       aes(BNG_east, BNG_north, colour = temp_mean)) +
  geom_point(size = 3) +
  scale_colour_viridis_c() +
  facet_grid(~Year) +
  coord_equal() +
  theme_bw()


# calculate landcover change


# Step 1: Keep one row per site (land cover is same across years)
sites <- drivers %>%
  distinct(site_id, .keep_all = TRUE)

# Step 2: Reshape land-cover columns into long format
lcm_long <- sites %>%
  select(site_id, BNG_east, BNG_north,
         lcm_2024_Deciduous_woodland_pct, lcm_1990_Deciduous_woodland_pct,
         lcm_2024_Coniferous_woodland_pct, lcm_1990_Coniferous_woodland_pct,
         lcm_2024_Arable_pct, lcm_1990_Arable_pct,
         lcm_2024_Urban_pct, lcm_1990_Urban_pct,
         lcm_2024_Suburban_pct, lcm_1990_Suburban_pct) %>%
  mutate(lcm_2024_forest = lcm_2024_Deciduous_woodland_pct + lcm_2024_Coniferous_woodland_pct,
         lcm_1990_forest = lcm_1990_Deciduous_woodland_pct + lcm_1990_Coniferous_woodland_pct,
         lcm_2024_urban = lcm_2024_Urban_pct + lcm_2024_Suburban_pct,
         lcm_1990_urban = lcm_1990_Urban_pct, lcm_1990_Suburban_pct) %>%
  select(site_id, BNG_east, BNG_north, lcm_2024_forest, lcm_1990_forest,
         lcm_2024_urban, lcm_1990_urban, lcm_2024_Arable_pct, lcm_1990_Arable_pct) %>%
  pivot_longer(
    cols = matches("^lcm_"),
    names_to = c("prefix", "year", "landcover_type"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  select(site_id, BNG_east, BNG_north, year, landcover_type, value)




# Step 3: Reshape to wide again to get year_1990 and year_2024 columns
lcm_change <- lcm_long %>%
  pivot_wider(
    names_from = year,
    values_from = value,
    names_prefix = "year_"
  ) %>%
  mutate(change = year_2024 - year_1990)


# visualise

ggplot(lcm_change, aes(BNG_east, BNG_north, colour = change)) +
  geom_point(size = 3) +
  scale_colour_viridis_c() +
  facet_grid(~landcover_type) +
  coord_equal() +
  theme_bw()

# urban is the only landcover type to show any significant changes between 1990 and 2024




# merge with BeeWalk data


beewalk <- readRDS("./Data/beewalk_filtered.rds")

# there are duplicates within the drivers dataset that need to be removed
duplicates <- drivers[duplicated(drivers), ]
#277
drivers_unique <- drivers %>% distinct()

# still lots of duplicated site, year combinations
# something weird is going on with the dominant class names
# row 8249 and 8551 have the exact same values in all colums but
# lcm_2024_dominant_class_name  and lcm_1990_dominant_class_name
# row 8249 has Suburban and 8551 has Improved_grassland in both
# I am going to exclude these two columns as I am not using them anyway 
# but worth asking Mariana how this happened

drivers <- drivers %>%
  select(-c(lcm_2024_dominant_class_name, lcm_1990_dominant_class_name))

drivers_unique <- drivers %>% distinct()


all_data <- beewalk %>%
  left_join(drivers_unique, by = c("GridReference" = "site_id", "Year", "Precision"))

# calculate temperature anomaly variable
all_data <- all_data %>%
  group_by(GridReference) %>%
  mutate(site_mean_temp = mean(temp_mean, na.rm = TRUE),
         temp_anomaly = temp_mean - site_mean_temp)



saveRDS(all_data, "./Data/beewalk_drivers_merged.rds")

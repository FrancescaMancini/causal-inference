#### BeeWalk site coordinates
library(tidyverse)

beewalk <- read.csv("./Data/BeeWalk data 2008-24 09012025.csv", header = TRUE, stringsAsFactors = FALSE)

beewalk_sites <- unique(beewalk[c("GridReference", "Year", "Transect.lat", "Transect.long", "Precision")])

write.csv(beewalk_sites, "./data/BeeWalk_sites.csv", row.names = FALSE)

summary(beewalk$Precision)
table(beewalk$Precision)


# filters and checks

beewalk <- beewalk %>%
  filter(Year > 2013) %>%
  filter(Precision <= 1000)

nyear_by_site <- beewalk %>%
  group_by(GridReference) %>%
  summarise(n_years = n_distinct(Year))


hist(nyear_by_site$n_years)
# exclude all sites that have only been visited in < 3 years

beewalk_filt <- beewalk %>%
  group_by(GridReference) %>%
  filter(n_distinct(Year) >= 3) %>%
  ungroup()

# check if any site name maps to more than one grid reference

duplicated_sites <- beewalk_filt %>%
  distinct(SiteName, GridReference) %>%
  add_count(SiteName, name = "n_gridrefs_for_site") %>%
  filter(n_gridrefs_for_site > 1)

# 12 site names map to two different grid references
# grid reference are very similar, so maybe only due to small changes in the start of the transect

# Check if any grid reference maps to more than one site name

duplicated_grefs <- beewalk_filt %>%
  distinct(GridReference, SiteName) %>%
  add_count(GridReference, name = "n_sites_for_gridref") %>%
  filter(n_sites_for_gridref > 1)

# and 70 grid references map to 2 site names
# most of these are typos or sites being renamed something slightly different

# not sure how to fix these...
# possibly best to use grid references to identify transects as they seem to be more reliable
# for now remove the 12 sites that are associated with more than 1 grid ref

beewalk_filt <- beewalk_filt %>%
  filter(!(GridReference %in% duplicated_sites$GridReference))


# spatial distribution of transects through time

ggplot(beewalk_filt,
       aes(Transect.long, Transect.lat)) +
  geom_point() +
  facet_wrap(~Year) +
  coord_equal() +
  theme_bw()

saveRDS(beewalk_filt, "./Data/beewalk_filtered.rds")

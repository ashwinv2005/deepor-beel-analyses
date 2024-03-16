library(tidyverse)
load("complete_data.RData")
SoIB_mapping = read.csv("SoIB_2023_metadata.csv")


data_final =  data_final %>%
  mutate(species.category = case_when(eBird.English.Name.2023 %in% 
                                        SoIB_mapping$eBird.English.Name.2023 ~ "species",
         TRUE ~ "spuh"),
         day = case_when(day == 4 & year == 2023 ~ 5,
                         TRUE ~ day))

data_final = data_final %>%
  mutate(Family = zoo::na.locf(Family, na.rm = FALSE))

data_final$Order = zoo::na.locf(data_final$Order)
data_final$Family = zoo::na.locf(data_final$Family)
data_final$Diet.Guild = zoo::na.locf(data_final$Diet.Guild)
data_final$Habitat.Specialization = zoo::na.locf(data_final$Habitat.Specialization)

# Compare diversity and abundance across locations and years. 
# Also check whether any 1% thresholds are crossed.

summary_location_day_abundance = data_final %>%
  filter(high.count > 0) %>%
  group_by(year,location,day) %>%
  reframe(abundance = sum(high.count))

summary_location_day_richess = data_final %>%
  filter(high.count > 0, species.category == "species") %>%
  group_by(year,location,day) %>%
  reframe(richness = n_distinct(eBird.English.Name.2023))

summary_location_day = summary_location_day_abundance  %>%
  left_join(summary_location_day_richess)

summary_day_abundance = data_final %>%
  filter(high.count > 0) %>%
  group_by(year,day) %>%
  reframe(abundance = sum(high.count))

summary_day_richness = data_final %>%
  filter(high.count > 0, species.category == "species") %>%
  group_by(year,day) %>%
  reframe(richness = n_distinct(eBird.English.Name.2023))

summary_day = summary_day_abundance %>%
  left_join(summary_day_richness)




## just waterfowl

summary_location_day_abundance = data_final %>%
  filter(Order %in% c("Anseriformes")) %>%
  filter(high.count > 0) %>%
  group_by(year,location,day) %>%
  reframe(abundance = sum(high.count))

summary_location_day_richess = data_final %>%
  filter(Order %in% c("Anseriformes")) %>%
  filter(high.count > 0, species.category == "species") %>%
  group_by(year,location,day) %>%
  reframe(richness = n_distinct(eBird.English.Name.2023))

waterfowl_summary_location_day = summary_location_day_abundance  %>%
  left_join(summary_location_day_richess)

summary_day_abundance = data_final %>%
  filter(Order %in% c("Anseriformes")) %>%
  filter(high.count > 0) %>%
  group_by(year,day) %>%
  reframe(abundance = sum(high.count))

summary_day_richness = data_final %>%
  filter(Order %in% c("Anseriformes")) %>%
  filter(high.count > 0, species.category == "species") %>%
  group_by(year,day) %>%
  reframe(richness = n_distinct(eBird.English.Name.2023))

waterfowl_summary_day = summary_day_abundance %>%
  left_join(summary_day_richness)




## only diving

summary_location_day_abundance = data_final %>%
  filter(eBird.English.Name.2023 %in% c("Red-crested Pochard","Common Pochard",
                                        "Ferruginous Duck","Tufted Duck")) %>%
  filter(high.count > 0) %>%
  group_by(year,location,day) %>%
  reframe(abundance = sum(high.count))

summary_location_day_richess = data_final %>%
  filter(eBird.English.Name.2023 %in% c("Red-crested Pochard","Common Pochard",
                                        "Ferruginous Duck","Tufted Duck")) %>%
  filter(high.count > 0, species.category == "species") %>%
  group_by(year,location,day) %>%
  reframe(richness = n_distinct(eBird.English.Name.2023))

divingduck_summary_location_day = summary_location_day_abundance  %>%
  left_join(summary_location_day_richess)

summary_day_abundance = data_final %>%
  filter(eBird.English.Name.2023 %in% c("Red-crested Pochard","Common Pochard",
                                        "Ferruginous Duck","Tufted Duck")) %>%
  filter(high.count > 0) %>%
  group_by(year,day) %>%
  reframe(abundance = sum(high.count))

summary_day_richness = data_final %>%
  filter(eBird.English.Name.2023 %in% c("Red-crested Pochard","Common Pochard",
                                        "Ferruginous Duck","Tufted Duck")) %>%
  filter(high.count > 0, species.category == "species") %>%
  group_by(year,day) %>%
  reframe(richness = n_distinct(eBird.English.Name.2023))

divingduck_summary_day = summary_day_abundance %>%
  left_join(summary_day_richness)




## just shorebirds

summary_location_day_abundance = data_final %>%
  filter(Order %in% c("Charadriiformes")) %>%
  filter(high.count > 0) %>%
  group_by(year,location,day) %>%
  reframe(abundance = sum(high.count))

summary_location_day_richess = data_final %>%
  filter(Order %in% c("Charadriiformes")) %>%
  filter(high.count > 0, species.category == "species") %>%
  group_by(year,location,day) %>%
  reframe(richness = n_distinct(eBird.English.Name.2023))

shorebirds_summary_location_day = summary_location_day_abundance  %>%
  left_join(summary_location_day_richess)

summary_day_abundance = data_final %>%
  filter(Order %in% c("Charadriiformes")) %>%
  filter(high.count > 0) %>%
  group_by(year,day) %>%
  reframe(abundance = sum(high.count))

summary_day_richness = data_final %>%
  filter(Order %in% c("Charadriiformes")) %>%
  filter(high.count > 0, species.category == "species") %>%
  group_by(year,day) %>%
  reframe(richness = n_distinct(eBird.English.Name.2023))

shorebirds_summary_day = summary_day_abundance %>%
  left_join(summary_day_richness)




## just swallows

summary_location_day_abundance = data_final %>%
  filter(Family %in% c("Hirundinidae (Swallows)")) %>%
  filter(high.count > 0) %>%
  group_by(year,location,day) %>%
  reframe(abundance = sum(high.count))

summary_location_day_richess = data_final %>%
  filter(Family %in% c("Hirundinidae (Swallows)")) %>%
  filter(high.count > 0, species.category == "species") %>%
  group_by(year,location,day) %>%
  reframe(richness = n_distinct(eBird.English.Name.2023))

swallows_summary_location_day = summary_location_day_abundance  %>%
  left_join(summary_location_day_richess)

summary_day_abundance = data_final %>%
  filter(Family %in% c("Hirundinidae (Swallows)")) %>%
  filter(high.count > 0) %>%
  group_by(year,day) %>%
  reframe(abundance = sum(high.count))

summary_day_richness = data_final %>%
  filter(Family %in% c("Hirundinidae (Swallows)")) %>%
  filter(high.count > 0, species.category == "species") %>%
  group_by(year,day) %>%
  reframe(richness = n_distinct(eBird.English.Name.2023))

swallows_summary_day = summary_day_abundance %>%
  left_join(summary_day_richness)




# graph to produce simple line graphs

ggp = ggplot(data = summary_day, aes(x = day, y = abundance, col = as.factor(year))) +
  geom_point() +
  geom_line(aes(x = day, y = abundance, col = as.factor(year))) +
  scale_y_continuous(lim = c(0,30000)) +
  theme_bw() +
  xlab("Day") +
  ylab("Count")

ggp +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        legend.title = element_blank(),
        #axis.line.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        #plot.background = element_rect(fill = "transparent",colour = NA),
        #panel.background = element_rect(fill = "transparent",colour = NA),
        #strip.background = element_blank(),
        legend.position = "bottom")

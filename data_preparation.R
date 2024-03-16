library(tidyverse)

data_wbf = read.csv("data_wbf.csv")
eBird_mapping = read.csv("India_species_spuhs_slashes_2023.csv")
SoIB_mapping = read.csv("SoIB_2023_metadata.csv")


spec_list = unique(data_wbf$species)
deepor_beel_list = eBird_mapping %>%
  filter(eBird.English.Name.2023 %in% spec_list)

census_metadata = data_wbf %>%
  distinct(year,day,location,festival)
data_final = census_metadata %>%
  uncount(length(deepor_beel_list$eBird.English.Name.2023)) %>%
  bind_cols(data.frame(species = 
                         rep(deepor_beel_list$eBird.English.Name.2023,
                                     length(census_metadata$year)))) %>%
  left_join(data_wbf) %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  rowwise() %>%
  mutate(high.count = max(c_across(c1:c3), na.rm = TRUE)) %>%
  left_join(SoIB_mapping, by = c("species" = "eBird.English.Name.2023")) %>%
  rename(eBird.English.Name.2023 = species)

save(data_final, file = "complete_data.RData")




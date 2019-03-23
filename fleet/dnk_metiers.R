

#Finding metiers used by the Danish fleet

library(haven)
library(dplyr)
library(lubridate)

years <- c(1987:2019)

metiers <- c()

for (i in years) {
  
  dfad_0 <- readRDS(paste("Q:/dfad/data/Data/udvidet_data/dfad_udvidet", i, ".rds", sep = ""))
  dfad_0 <- mutate(dfad_0, year = year(ldato))
  
  metiers_0 <- summarise(group_by(dfad_0, year, fao_area, level_6, merged_level_6, metier_level6_ret), kg = sum(hel, na.rm = T), trips = length(unique(match_alle)))
  
  metiers <- bind_rows(metiers, metiers_0)
}
  
saveRDS(metiers, "Q:/mynd/kibi/reference_tables/fleet/dnk_metiers.rds")
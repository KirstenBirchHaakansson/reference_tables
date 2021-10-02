

# Align names and coding in Lykkehjulet


library(dplyr)
library(lubridate)
library(readxl)

years <- 2020

lh_name <- "observer_at_sea_sampling_frame_2019-12-16.xlsx"

lh_path <- "Q:/dfad/data/Data/Lykkehjul/Listerne/"

output_path <- "Q:/mynd/kibi/reference_tables/sampling_frames/"

lh <- read_xlsx(paste0(lh_path, lh_name), sheet = 1)

lh$year <- lh$samp_year

distinct(lh, strata_location, strata_fleet, strata_area)

lh$stratumName <- gsub(" NA", "", paste(lh$strata_location, gsub(" ", "", tolower(lh$strata_fleet)), lh$strata_area))

distinct(lh, stratumName)

lh$stratumName[lh$stratumName == "Lyngby hesterejer"] <- "Crangon"
lh$stratumName[lh$stratumName == "Hirtshals otb_cru_32-69_0_0"] <- "Pandalus"

distinct(lh, stratumName)

lh_ok <- distinct(lh, year, fid, stratumName)

write.table(lh_ok, paste0(output_path, "lykkehjul_", years, ".csv"), sep = ";", row.names = F)
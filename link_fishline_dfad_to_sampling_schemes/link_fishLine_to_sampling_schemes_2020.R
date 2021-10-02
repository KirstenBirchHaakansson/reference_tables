

# title: "Link between FishLine and sampling schemes, 2019"
# author: "Kirsten Birch Håkansson, DTU Aqua"

# Setup ----

library(RODBC)
library(sqldf)
library(dplyr)
library(knitr)
library(lubridate)
library(RColorBrewer)
library(viridis)
library(ggplot2)
library(readxl)

####################################################################

years <- 2019 # only a single

cruises <- c("MON", "SEAS", "IN-LYNG", "IN-HIRT")

lh_name <- "observer_at_sea_sampling_frame_2019-01-04.xlsx"

####################################################################

ref_path <- "Q:/mynd/kibi/reference_tables/sampling_scheme/"

output_path <- "Q:/mynd/kibi/reference_tables/link_fishline_dfad_to_sampling_schemes/"

lh_path <- "Q:/dfad/data/Data/Lykkehjul/Listerne/"

# Get Sampling schemes ----

ss <- read.csv(paste0(ref_path, "sampling_scheme_ref_", years, ".csv"), sep = ";")
ss <- subset(ss, year == years)

# Get FishLine ----
channel <- odbcConnect("FishLineDW")
samp <- sqlQuery(
  channel,
  paste(
    "SELECT Trip.tripId, Trip.cruiseId, Trip.year, Trip.cruise, Trip.trip, Trip.tripType, Trip.logBldNr, Trip.timeZone, Trip.dateStart,                        Trip.dateEnd, Trip.samplingType, Trip.samplingMethod, Trip.fisheryType, Trip.platform1, Trip.nationalityPlatform1, 
                  Trip.fDFVesselPlatform1, Trip.nationalityPlatform2, Trip.platform2, Trip.dateSample, Trip.harbourSample,                                         Trip.nationalityHarbourSample, Trip.harbourLanding, Trip.nationalityHarbourLanding, Sample.sampleId, Sample.station, 
                  Sample.gearQuality, Sample.dfuArea, Sample.targetSpecies1, Sample.catchRegistration, Sample.speciesRegistration,                                 SpeciesListRaised.speciesCode, SpeciesListRaised.landingCategory, SpeciesListRaised.weightSubSample, 
                  SpeciesListRaised.weightTotal
         FROM     SpeciesListRaised LEFT OUTER JOIN
                  Sample ON SpeciesListRaised.sampleId = Sample.sampleId RIGHT OUTER JOIN
                  Trip ON Sample.tripId = Trip.tripId
         WHERE (Trip.year between ", min(years), " and ", max(years) , ")
                and Trip.cruise in ('", paste(cruises, collapse = "','"),
    "')",
    sep = ""
  )
)
close(channel)

# Get lykkehjul ----

lh <- read_xlsx(paste0(lh_path, lh_name), sheet = 1)

distinct(subset(ss, samplingScheme == "DNK_AtSea_Observer_Active"), stratumName)

distinct(lh, strata_location, strata_fleet, strata_area)

lh$stratumName <- gsub(" NA", "", paste(lh$strata_location, gsub(" ", "", tolower(lh$strata_fleet)), lh$strata_area))

distinct(lh, stratumName)

lh$stratumName[lh$stratumName == "Lyngby hesterejer"] <- "Crangon"
lh$stratumName[lh$stratumName == "Hirtshals otb_cru_32-69_0_0"] <- "Pandalus"

test <- inner_join(lh, distinct(ss, samplingScheme, stratumName))

lh_ok <- distinct(lh, fid, stratumName)

# Make DNK_Market_Sampling ----

ss_MS <-
  subset(ss, samplingScheme == "DNK_Market_Sampling")

samp_MS <- subset(samp, cruise %in% c("IN-HIRT", "IN-LYNG"))

unique(samp_MS$targetSpecies1)

out <-
  subset(
    samp_MS,
    targetSpecies1 %in% c("BLH", "BRS", "TBM", "SPE", "HMK", "LOD", "MAK", "SIL", "PIL") |
      landingCategory %in% c("IND", "DIS") |
      speciesCode %in% c("MAK", "SIL") &
      landingCategory == "KON" | speciesCode == "GLL"
  )

out_sum <-
  summarise(
    group_by(out, speciesCode, targetSpecies1, landingCategory),
    kg = sum(weightTotal, na.rm = T)
  )

ok <- subset(samp_MS,!(sampleId %in% out$sampleId))

ok_sum <-
  summarise(
    group_by(ok, speciesCode, targetSpecies1, landingCategory),
    kg = sum(weightTotal, na.rm = T)
  )

samp_MS_ok <- distinct(ok, year, tripId, cruise, trip, harbourSample, dateSample)

ok$samplingScheme <- "DNK_Market_Sampling"
ok$quarter <- quarter(ok$dateSample)
ok$stratumName <- "High activity"

samp_MS_ok_1 <- distinct(ok, year, samplingScheme, quarter, stratumName, tripId, cruise, trip)

ss_MS_ok <- full_join(ss_MS, samp_MS_ok_1)

rm(ss_MS, ok, ok_sum, out, out_sum, samp_MS, samp_MS_ok, samp_MS_ok_1)

# Make DNK_AtSea_Observer_Active ----

ss_AOA <-
  subset(ss, samplingScheme == "DNK_AtSea_Observer_Active")

samp_AOA <- subset(samp, cruise %in% c("MON", "SEAS"))
samp_AOA$quarter <- quarter(samp_AOA$dateStart)

samp_lh <- left_join(samp_AOA, lh_ok, by = c("platform1" = "fid"))

samp_lh_uniq <- distinct(samp_lh, tripId, year, cruise, trip, platform1, stratumName, quarter)

unique(samp_lh$stratumName)

test_no_stratum <- subset(samp_lh_uniq, is.na(stratumName))

ss_AOA_ok <- full_join(ss_AOA, samp_lh_uniq)

test <- summarise(group_by(ss_AOA_ok, tripId), no = length(tripId))

ss_AOA_ok$sampled[!(is.na(ss_AOA_ok$samplingScheme)) & is.na(ss_AOA_ok$tripId)] <- "N"
ss_AOA_ok$reasonNotSampled[!(is.na(ss_AOA_ok$samplingScheme)) & is.na(ss_AOA_ok$tripId)] <- "Other"

ss_AOA_ok$samplingScheme[is.na(ss_AOA_ok$stratumName)] <- "DNK_AtSea_Observer_Active"
ss_AOA_ok$hierarchy[is.na(ss_AOA_ok$stratumName)] <- 1
ss_AOA_ok$stratumSamplingFrame[is.na(ss_AOA_ok$stratumName)] <- paste0("Q", ss_AOA_ok$quarter[is.na(ss_AOA_ok$stratumName)])
ss_AOA_ok$PSU[is.na(ss_AOA_ok$stratumName)] <- "VS"
ss_AOA_ok$stratumName[is.na(ss_AOA_ok$stratumName)] <- "Sampling outside frame"
ss_AOA_ok$selectionMethod[is.na(ss_AOA_ok$stratumName)] <- "NPCS"
ss_AOA_ok$sampled[is.na(ss_AOA_ok$stratumName)] <- "Y"
ss_AOA_ok$reasonNotSampled[is.na(ss_AOA_ok$stratumName)] <- ""

rm(lh, lh_ok, samp_AOA, samp_lh, samp_lh_uniq, ss_AOA, test, test_no_stratum)

# Merge linkage and out put ----

link <- bind_rows(ss_AOA_ok, ss_MS_ok)

write.table(link, paste0(output_path, "link_fishLine_to_sampling_schemes_", years, ".csv"), sep = ";", row.names = F)

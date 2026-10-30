## Preprocess data, write TAF data tables

## Before: boot/data/ (set up by taf.boot() from boot/DATA.bib): the official landings (ICES_1950-2010.csv,
##         ICESCatchDataset2006-2023.csv, Preliminary_landings_allSpecies_<year>.csv), TAC*.csv, LandingOnly.txt
##         (InterCatch), the survey indices (survey_index_*.csv, from 2026_bll.27.3a47de_survey), the SCF allocation
##         tables rdbes_SCF_<CAR|CAS|DAR|DAS>_<year>.csv.gz (from 2026_bll.27.3a47de_RDBES_combined) and
##         RDBES_SCF/export_*.zip (downloaded by boot/RDBES_SCF.R)
## After:  data/bll.27.3a47de.*.csv (landings, catches, RDBES SCF, survey indices); the tables and figures of the
##         reports are made from them by the output scripts

library(icesTAF)
library(dplyr)
library(tidyr)
library(readr)

mkdir("data")

cp("boot/data/utilities.R", ".")      # the functions and stock settings, set up by taf.boot() from boot/DATA.bib
source("utilities.R")


## ###########################################################################
## Settings
## ###########################################################################

st <- stock_settings
prefix <- st$code                     # of the files written to data/

check_boot(file.path("boot/data", c("ICES_1950-2010.csv", "ICESCatchDataset2006-2023.csv", "TAC.csv", "TAC_BLL_2a3a47de.csv",
                                     "LandingOnly.txt", "survey_index_sem1.csv", "survey_index_sem2.csv",
                                     "survey_index_historical_1983_1998.csv", "RDBES_SCF")))

prelim_files <- list.files("boot/data", "^Preliminary_landings_allSpecies_[0-9]{4}[.]csv$",
                           full.names = TRUE)

first_commercial_year <- 2014         # the first year of the commercial catch (landings and discards) in the catch series

## The source of the commercial catch for the years the RDBES has (the SCF): "RDBES" or "IC" (InterCatch). The other
## years always come from InterCatch.
commercial_source <- "RDBES"

## The TACs the uptake is calculated for
tac_species_2a4 <- data.frame(label = c("Brill", "Turbot"),
                              name  = c("Brill", "Turbot"),
                              code  = c("BLL", "TUR"),
                              latin = c("Scophthalmus rhombus", "Scophthalmus maximus"))
tac_2a4_hist_divisions <- c("II a (not specified)", "II a2", "IV a", "IV a+b (not specified)", "IV b", "IV c")
tac_2a4_dataset_areas  <- c("27.2.a", "27.4")
tac_2a4_prelim_areas   <- c("27.2.a.2", "27.4.a", "27.4.b", "27.4.c")
tac_2a3a47de_areas     <- c("27.2.a.2", "27.2.a.1", "27.2.a", "27.4.a", "27.4.b", "27.4.c",
                            "27.3.a", "27.3.a.20", "27.3.a.21", "27.7.d", "27.7.e")

write_data <- function(x, name) write.taf(x, file = paste0(prefix, ".", name, ".csv"), dir = "data")


###################################
msg("Data: Loading Official Landings data")
###################################

hist_landings <- read_hist_landings("boot/data/ICES_1950-2010.csv")
ol   <- read_catch_dataset("boot/data/ICESCatchDataset2006-2023.csv")

olhist <- hist_landings %>%
  filter(Species == st$species_name, Year >= 1950) %>%
  mutate(Country = recode_country(Country),
         Area = group_areas(Division, st$hist_divisions)) %>%
  filter(!is.na(Area)) %>%
  group_by(Species, Year, Country, Area) %>%
  summarize(Landings = sum(value, na.rm = TRUE)) %>%
  mutate(Species = st$species_code, Units = "TLW") %>%
  filter(Year < 2006, Landings > 0)

## The preliminary landings by area group (as the other years), all of them and without the confidential ones
prelim <- function(exclude_confidential) {
  read_prelim_landings(prelim_files, exclude_confidential = exclude_confidential) %>%
    mutate(Country = recode_country(Country), Area = group_areas(Area))
}

oldataset <- ol %>%
  filter(Species == st$species_code, Area %in% st$catch_dataset_areas) %>%
  mutate(Country = recode_country(Country),
         Area = group_areas(Area)) %>%
  group_by(Species, Area, Units, Country, Year) %>%
  summarise(Landings = sum(value, na.rm = TRUE))

olbll      <- bind_rows(oldataset, olhist, prelim(FALSE))
olbll_conf <- bind_rows(oldataset, olhist, prelim(TRUE))   # the confidential preliminary landings left out

write.taf(olbll, file = paste0(prefix, ".official.landings.csv"), dir = "data")
write.taf(olbll_conf, file = paste0(prefix, ".official.landings.confidencial.csv"), dir = "data")
write.taf(olbll %>% group_by(Year) %>% summarise(Landings = sum(Landings)),
          file = paste0(prefix, ".official.landings.summary.csv"), dir = "data")

write_data(read_prelim_bms(prelim_files), "ol.bms")

write.taf(prelim_confidentiality(prelim_files, by = c("Species", "Area", "Units", "Country", "Year"), recode = TRUE),
          file = paste0(prefix, ".official.landings.summary.confidential.csv"), dir = "data")


###################################
msg("Data: Loading IC data")
###################################

## The raised landings and discards of InterCatch (LandingOnly.txt), by year, area, country, catch category and stratum
icd <- read_intercatch_landing_only("boot/data/LandingOnly.txt") %>%
  filter(Year >= first_commercial_year)
write.taf(icd, file = paste0(prefix, ".intercatch.raised.csv"), dir = "data", quote = TRUE)

IC_Catches <- catches_by_area_country(icd)
write.taf(IC_Catches, file = paste0(prefix, ".intercatch.catches.csv"), dir = "data")


###################################
msg("Data: Loading RDBES-SCF data")
###################################

## The Catch Combined (CC) and the Distribution Combined (DC) downloaded from the RDBES by boot/RDBES_SCF.R (a zip per year)
zipfiles <- list.files("boot/data/RDBES_SCF", "^export_.*[.]zip$", full.names = TRUE)
scf_cc <- read_rdbes_table(zipfiles, "CatchCombined")
scf_dc <- read_rdbes_table(zipfiles, "DistributionCombined")
msg("Data: RDBES SCF of ", toString(sort(unique(scf_cc$year))), " from ", length(zipfiles), " download(s)")
write.taf(scf_cc, file = paste0(prefix, ".rdbes.catch.combined.csv"), dir = "data")
write.taf(scf_dc, file = paste0(prefix, ".rdbes.distribution.combined.csv"), dir = "data")

## The allocation tables of the SCF (CAR, CAS, DAR, DAS): not in the RDBES yet, so from boot/data/rdbes_SCF_<table>_<year>.csv.gz
scf_alloc <- read_scf_allocation("boot/data")
missing <- names(scf_alloc)[vapply(scf_alloc, is.null, logical(1))]
if (length(missing)) msg("Data: SCF allocation tables not in boot/data: ", toString(missing))
if (!setequal(unique(scf_alloc$CAR$year), unique(scf_cc$year)))
  warning("The years of the CAR (", toString(sort(unique(scf_alloc$CAR$year))), ") and of the CC (",
          toString(sort(unique(scf_cc$year))), ") differ", call. = FALSE)

## The catch by area and country of the Catch Allocation Results, in the format of the InterCatch catch
scf_ic <- scf_to_intercatch(scf_alloc$CAR)
RDBES_Catches <- catches_by_area_country(scf_ic)
write.taf(RDBES_Catches, file = paste0(prefix, ".rdbes.catches.csv"), dir = "data")

## The CC, the CAR and InterCatch side by side: landings and discards by year (t)
totals <- function(x, source, to_t = 1 / 1000) {
  x %>%
    group_by(Year) %>%
    summarise(Landings = sum(Landings, na.rm = TRUE) * to_t, Discards = sum(Discards, na.rm = TRUE) * to_t) %>%
    mutate(Source = source, .before = 1)
}
cc_t <- scf_cc %>%
  filter(variableType == "WeightLive") %>%
  mutate(Year = year, t = total * c(kg = 1 / 1000, t = 1)[variableUnit],
         Landings = ifelse(catchCategory == "Lan", t, 0), Discards = ifelse(catchCategory == "Dis", t, 0))
rdbes_vs_ic <- bind_rows(totals(cc_t, "RDBES CC", to_t = 1),
                         totals(RDBES_Catches, "RDBES CAR"),
                         totals(IC_Catches %>% filter(Year %in% cc_t$Year), "InterCatch"))
write.taf(rdbes_vs_ic, file = paste0(prefix, ".rdbes.vs.intercatch.csv"), dir = "data")

## The commercial catch of the catch series: InterCatch, with the years of the SCF from the RDBES when commercial_source is "RDBES"
stopifnot(commercial_source %in% c("RDBES", "IC"))
commercial <- if (commercial_source == "RDBES") {
  bind_rows(IC_Catches %>% filter(!Year %in% RDBES_Catches$Year), RDBES_Catches)
} else IC_Catches
msg("Data: commercial catch from ", commercial_source, " for ", toString(sort(unique(RDBES_Catches$Year))))


###################################
msg("Data: Merging Official + commercial data")
###################################

catches <- build_catch_series(olbll, commercial, first_commercial_year)
write_data(catches, "catches")


###################################
msg("Data: Calculating TAC uptake 2000-2023")
###################################

landings2a4 <- species_landings_by_year(tac_species_2a4,
                                        hist = hist_landings, hist_divisions = tac_2a4_hist_divisions, hist_years = 2000:2006,
                                        catch_dataset = ol, dataset_areas = tac_2a4_dataset_areas,
                                        prelim_files = prelim_files, prelim_areas = tac_2a4_prelim_areas)

Uptake2a4 <- landings2a4 %>%
  full_join(read_csv("boot/data/TAC.csv", show_col_types = FALSE), by = "Year") %>%
  mutate(Uptake = ifelse(!is.na(Total), Total / `Agreed TAC 2a4` * 100, NA))
write_data(Uptake2a4, "uptake.TAC.2a4")

write.taf(prelim_confidentiality(prelim_files, by = c("Species", "Year"), species_latin = tac_species_2a4$latin,
                                 areas = tac_2a4_prelim_areas, species_code = "BLL+TUR"),
          file = paste0(prefix, ".prel2a4.confidential.csv"), dir = "data")


###################################
msg("Data: Calculating TAC (NEW AREA AND SPECIES) uptake 2025")
###################################

Uptake2a3a47de <- read_prelim_landings(prelim_files, areas = tac_2a3a47de_areas) %>%
  group_by(Year, Area) %>%
  summarise(Landings = sum(Landings, na.rm = TRUE), .groups = "drop") %>%
  full_join(read_csv("boot/data/TAC_BLL_2a3a47de.csv", show_col_types = FALSE), by = c("Year", "Area")) %>%
  as.data.frame()
write_data(Uptake2a3a47de, "uptake.TAC.2a3a47de")

prel2a43a7de <- prelim_confidentiality(prelim_files, by = c("Species", "Year"), areas = tac_2a3a47de_areas)
write.taf(prel2a43a7de, file = paste0(prefix, ".prel2a43a7de.confidential.csv"), dir = "data")


###################################
msg("Data: Survey indices")
###################################

## The indices of 2026_bll.27.3a47de_survey (boot/data/survey_*) under the names of data/
copy_survey_files("boot/data", "data", c(
  survey_index_sem1.csv                 = paste0(prefix, ".survey.index.sem1.csv"),
  survey_index_sem2.csv                 = paste0(prefix, ".survey.index.sem2.csv"),
  survey_index_by_area_sem1.csv         = paste0(prefix, ".survey.index.by.area.sem1.csv"),
  survey_index_by_area_sem2.csv         = paste0(prefix, ".survey.index.by.area.sem2.csv"),
  survey_index_historical_1983_1998.csv = paste0(prefix, ".survey.index.1983.1998.csv")))

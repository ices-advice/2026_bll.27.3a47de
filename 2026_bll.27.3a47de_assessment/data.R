## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)
library(dplyr)
library(tidyr)
library(grid) # for unit()
library(RColorBrewer)
library(readr)
library(ggplot2)


# taf.bootstrap()
mkdir("data")
mkdir("report")
mkdir("report/tables")
mkdir("report/figures")
mkdir("report/figures_rds")

cp("boot/initial/00_functions.R", "./")
source("./00_functions.R")

###################################
msg("Data: Loading Official Landings data")
###################################
  
  ## Loading data and settings ----

  olhist <-
    read.csv("boot/data/ICES_1950-2010.csv",
             stringsAsFactors = FALSE, header = TRUE) %>% as_tibble() %>%
    reshape2:::melt.data.frame(id.vars = c("Species", "Division",  "Country"), variable.name = "Year") %>%
    as_tibble() %>%
    mutate(Year = as.integer(substr(Year, start=2, stop=5)),
           Country = ifelse(Country %in% levels_country, Country, "Other"),
           Country = factor(Country, levels = levels_country, labels = labels_country, ordered = TRUE),
           Area = factor(Division, levels = levels_division_hist, labels = labels_division_hist, ordered = TRUE)) %>% 
    filter(Species == "Brill", Area %in% labels_division_hist,
           Year >= 1950,
           ! value %in% c("-", ".", "<0.5")) %>%
    group_by(Species,Year, Country,Area) %>%
    summarize(Landings = sum(as.numeric(value), na.rm = TRUE))%>%
    mutate( Species= "BLL", Units = "TLW")%>%
    filter(Year < 2006, Landings > 0)

  ol <- read_csv("boot/data/ICESCatchDataset2006-2023.csv", na = "0 c")%>%
    as_tibble() 
  
  
  prels <- bind_rows(
    readPrelFlexible("boot/data/Preliminary_landings_allSpecies_2024.csv", 
                     latin = "Scophthalmus rhombus",
                     areas = c("27_3_A", "27_3_A_20", "27_3_A_21", "27_4_A", "27_4_B", "27_4_C", "27_7_D", "27_7_E", "27.4.a", "27.4.b", "27.4.c", "27.3.a.20", "27.3.a.21", "27.3.a", "27.7.d", "27.7.e"),
                     speciesout = "BLL",
                     exclude.confidential = FALSE),
    readPrelFlexible("boot/data/Preliminary_landings_allSpecies_2025.csv", 
                     latin = "Scophthalmus rhombus",
                     areas = c("27_3_A", "27_3_A_20", "27_3_A_21", "27_4_A", "27_4_B", "27_4_C", "27_7_D", "27_7_E", "27.4.a", "27.4.b", "27.4.c", "27.3.a.20", "27.3.a.21", "27.3.a", "27.7.d", "27.7.e"),
                     speciesout = "BLL",
                     exclude.confidential = FALSE) )%>%
    mutate(Country = factor(Country, 
                            levels = levels_country_ol, 
                            labels = labels_country_ol, 
                            ordered = TRUE))
  
  olbll <- ol %>% 
    filter(Species == "BLL", Area %in% c("27.3.a","27.4", "27.7.d", "27.7.e")) %>% # Do not include specific areas for 3.a and 4 because they duplicate the data.
    reshape2:::melt.data.frame(id.vars = c("Species", "Area", "Units", "Country"), variable.name = "Year") %>%
    as_tibble() %>%
    mutate(
           Year = as.integer(as.character(Year)),
           Country = factor(Country, levels = levels_country_ol, labels = labels_country_ol, ordered = TRUE),
           Area = case_when(Area %in% c("27.3.a", "27.3.a_NK", "27.3.a.20", "27.3.a.21")~ "27.3.a",
                            Area %in% c("27.4", "27.4.a", "27.4.b", "27.4.c")~ "27.4",
                            Area %in% c("27.7.d", "27.7.e")~ "27.7.de")) %>%
    
    group_by(Species, Area, Units, Country, Year) %>%
    summarise(Landings = sum(as.numeric(value), na.rm = TRUE)) %>%
    bind_rows(olhist, prels)

  write.taf(olbll, file = "bll.27.3a47de.official.landings.csv", dir = "data")
  
  olbll_summ <- olbll %>% 
    group_by(Year) %>% 
    summarise( Landings = sum(Landings))
    
  write.taf(olbll, file = "bll.27.3a47de.official.landings.summary.country.area.csv", dir = "report/tables")
  
  write.taf(olbll_summ, file = "bll.27.3a47de.official.landings.summary.csv", dir = "report/tables")
  
  bms <- bind_rows(
    getBMS("boot/data/Preliminary_landings_allSpecies_2024.csv"),
    getBMS("boot/data/Preliminary_landings_allSpecies_2025.csv"))
  
  write.taf(bms , file = "bll.27.3a47de.ol.bms.csv", dir = "data")
  write.taf(bms , file = "bll.27.3a47de.ol.bms.csv", dir = "report/tables")
  
  prels_conf <- 
    bind_rows(
      readPrelFlexible("boot/data/Preliminary_landings_allSpecies_2024.csv", 
                       latin = c("Scophthalmus rhombus"),
                       areas = c("27_3_A", "27_3_A_20", "27_3_A_21", "27_4_A", "27_4_B", "27_4_C", "27_7_D", "27_7_E", "27.4.a", "27.4.b", "27.4.c", "27.3.a.20", "27.3.a.21", "27.3.a", "27.7.d", "27.7.e"),
                       speciesout = "BLL",
                       exclude.confidential = TRUE),
      readPrelFlexible("boot/data/Preliminary_landings_allSpecies_2025.csv", 
                       latin = "Scophthalmus rhombus",
                       areas = c("27_3_A", "27_3_A_20", "27_3_A_21", "27_4_A", "27_4_B", "27_4_C", "27_7_D", "27_7_E", "27.4.a", "27.4.b", "27.4.c", "27.3.a.20", "27.3.a.21", "27.3.a", "27.7.d", "27.7.e"),
                       speciesout = "BLL",
                       exclude.confidential = TRUE)
    ) %>%
    mutate(Country = factor(Country, 
                            levels = levels_country_ol, 
                            labels = labels_country_ol, 
                            ordered = TRUE)) %>% 
    rename(Not_Conf = Landings) 
  
  prels_sum <- prels %>%
    group_by(Species, Area, Units, Country, Year) %>%
    summarise(Landings = sum(Landings), .groups = "drop")
  
  prels_conf_sum <- prels_conf %>%
    group_by(Species, Area, Units, Country, Year) %>%
    summarise(Not_Conf = sum(Not_Conf), .groups = "drop")
  
  prels_joined <- prels_sum %>%
    left_join(prels_conf_sum,
              by = c("Species", "Area", "Units", "Country", "Year"))
  
  write.taf(prels_joined, file = "bll.27.3a47de.official.landings.summary.confidential.csv", dir = "report/tables") 
  
  ###################################
  msg("Data: Loading IC data")
  ###################################
  
  # Now load Intercatch data to merge with OL
  icd <- readRDS(file = "boot/initial/data/brill.3a47de.InterCatch_raised_discards.Rds") %>% filter(Year>2013)
  write.taf(icd, file = "bll.27.3a47de.intercatch.raised.csv", dir = "data", quote = TRUE)

  lastyr <- max(icd$Year)
  
    ## Catches ----
  #Get Landings and Discards for IC data
  IC_Catches <- lapply(split(icd, icd$Year), get_catchcat_percent)%>%
    bind_rows() %>%
    filter (!Area == "Total")%>%
    reframe (Species ="BLL",Area, Units = "TLW", Country, Year, Landings, Discards) 

  write.taf(IC_Catches, file = "bll.27.3a47de.intercatch.catches.csv", dir = "data")
  
  ## Discard rate and estimation
  percent_discards <- lapply(split(icd, icd$Year), get_catchcat_percent)%>%
    bind_rows() %>%
    reshape2:::melt.data.frame(id.vars = c("Area", "Year", "Country"))

  ###################################
  msg("Data: Merging Official + IC data")
  ###################################
  
  bll.27.3a47de_catch<- olbll %>% group_by(Year) %>%
    summarise(Landings = sum(Landings)) %>%
    filter (Year <2014 
            )%>%
    bind_rows(
      IC_Catches %>%
        filter (!Year <2014 
                )%>%
        group_by(Year)%>%
        summarise(Landings = sum(Landings/1000,na.rm = TRUE ),
                  Discards = sum (Discards/1000,na.rm = TRUE )))%>%
    arrange(Year) %>% 
    mutate(Total = Landings + Discards )
  
  write.taf(bll.27.3a47de_catch, file = "bll.27.3a47de.catches.csv", dir = "data")
  write.taf(bll.27.3a47de_catch, file = "bll.27.3a47de.catches.csv", dir = "report/tables")
  ###################################
  msg("Data: Calculating TAC uptake 2000-2023")
  ###################################
  
  #Historial Official landngs 2000-2005
  olhist2a4 <-
    read.csv("boot/data/ICES_1950-2010.csv",
             stringsAsFactors = FALSE, header = TRUE) %>% as_tibble() %>%
    reshape2:::melt.data.frame(id.vars = c("Species", "Division",  "Country"), variable.name = "Year") %>%
    as_tibble() %>%
    mutate(Year = as.integer(substr(Year, start=2, stop=5)),
           Country = ifelse(Country %in% levels_country, Country, "Other"),
           Country = factor(Country, levels = levels_country, labels = labels_country, ordered = TRUE),
           Country = factor(Country, levels = levels_division_hist, labels = labels_division_hist, ordered = TRUE)) %>%
    filter(Species %in% c("Brill", "Turbot"), 
           Division %in% c("II a (not specified)","II a2", "IV a","IV a+b (not specified)","IV b","IV c"),
           ! value %in% c("-", ".", "<0.5"),
           Year >= 2000 & Year <=2006) %>%
    group_by(Species,Year) %>%
    summarize(Landings = sum(as.numeric(value), na.rm = TRUE))%>%
    pivot_wider (names_from= "Species", values_from = "Landings")%>%
    mutate( Total = Brill + Turbot)
  
  # Official landngs 2006-2021
  ol2a4 <-  ol %>%
    filter(Species %in% c("BLL", "TUR"), Area %in% c("27.2.a","27.4")) %>%
    reshape2:::melt.data.frame(id.vars = c("Species", "Area", "Units", "Country"), variable.name = "Year") %>%
    as_tibble() %>%
    mutate(
      Year = as.integer(as.character(Year)),
      Country = factor(Country, levels = levels_country_ol, labels = labels_country_ol)) %>% 
    group_by(Species, Year) %>%
    summarize(Landings = sum(as.numeric(value), na.rm = TRUE))%>%
    pivot_wider (names_from= "Species", values_from = "Landings")%>%
    rename(Brill = BLL, Turbot = TUR)%>%
    mutate( Total = Brill + Turbot)
  

  #Preliminary Landings 2022-2023
  prel2a4<-
    bind_rows(
    read.csv("boot/data/Preliminary_landings_allSpecies_2024.csv",
             stringsAsFactors = FALSE, header = TRUE) %>% 
    filter(Species.Latin.Name %in% c("Scophthalmus rhombus", "Scophthalmus maximus"), Area %in% c("27.2.a.2","27.4.a", "27.4.b", "27.4.c")) %>%
    rename(Species= Species.Latin.Name)%>%
    mutate (BMS.Catch.TLW. = case_when(is.na(BMS.Catch.TLW.)~0,
                                  TRUE~as.double(BMS.Catch.TLW.)),
            Species = case_when(Species == "Scophthalmus rhombus"~"Brill",
                                Species == "Scophthalmus maximus"~"Turbot"))%>%
    group_by(Year,Species)%>%
    summarize (AMS_Catch = sum(AMS.Catch.TLW.),
               BMS_Catch = sum(BMS.Catch.TLW.),
               Total = AMS_Catch+BMS_Catch),
    read.csv("boot/data/Preliminary_landings_allSpecies_2025.csv",
             stringsAsFactors = FALSE, header = TRUE) %>% 
      filter(Species.Latin.Name %in% c("Scophthalmus rhombus", "Scophthalmus maximus"), Area %in% c("27.2.a.2","27.4.a", "27.4.b", "27.4.c")) %>%
      rename(Species= Species.Latin.Name)%>%
      mutate (BMS.Catch.TLW. = case_when(is.na(BMS.Catch.TLW.)~0,
                                         TRUE~as.double(BMS.Catch.TLW.)),
              Species = case_when(Species == "Scophthalmus rhombus"~"Brill",
                                  Species == "Scophthalmus maximus"~"Turbot"))%>%
      group_by(Year,Species)%>%
      summarize (AMS_Catch = sum(AMS.Catch.TLW.),
                 BMS_Catch = sum(BMS.Catch.TLW.),
                 Total = AMS_Catch+BMS_Catch))%>%
    select(Year, Species, Total )%>%
    pivot_wider(names_from = Species, values_from = Total)%>%
    mutate( Total = Brill + Turbot)
  
  
  Uptake2a4 <- rbind(olhist2a4, ol2a4, prel2a4) %>%
    full_join(read_csv("boot/data/TAC.csv"), by = "Year") %>%
    mutate(Uptake = ifelse(!is.na(Total), Total / `Agreed TAC 2a4` * 100, NA))
  
  write.taf(Uptake2a4, file = "bll.27.3a47de.uptake.TAC.2a4.csv", dir = "data")
  write.taf(Uptake2a4, file = "bll.27.3a47de.uptake.TAC.2a4.csv", dir = "report/tables") 
  
  prel2a4_conf <- 
    bind_rows(
      readPrelFlexible("boot/data/Preliminary_landings_allSpecies_2024.csv", 
                       latin = c("Scophthalmus rhombus","Scophthalmus maximus"),
                       areas = c("27.2.a.2","27.4.a", "27.4.b", "27.4.c"),
                       speciesout = "BLL+TUR",
                       exclude.confidential = TRUE),
      readPrelFlexible("boot/data/Preliminary_landings_allSpecies_2025.csv", 
                       latin = c("Scophthalmus rhombus","Scophthalmus maximus"),
                       areas = c("27.2.a.2","27.4.a", "27.4.b", "27.4.c"),
                       speciesout = "BLL+TUR",
                       exclude.confidential = TRUE))%>%
    group_by(Species, Year) %>%
    summarise(Not_Conf = sum(Landings), .groups = "drop")
  
  prel2a4_combined <- left_join(prel2a4 %>% 
                                select(Year, Total) %>% 
                                rename(Landings = "Total"),
                                prel2a4_conf) %>% 
    select(Species,Year,Landings, Not_Conf)
  
  write.taf(prel2a4_combined, file = "bll.27.3a47de.prel2a4.confidential.csv", dir = "report/tables") 
  ###################################
  msg("Data: Calculating TAC (NEW AREA AND SPECIES) uptake 2025")
  ###################################

  prel2a43a7de <- bind_rows(
    summarizePrelFlexible("boot/data/Preliminary_landings_allSpecies_2024.csv",
                          latin = "Scophthalmus rhombus",
                          areas = c("27.2.a.2","27.2.a.1", "27.2.a", 
                                    "27.4.a", "27.4.b", "27.4.c",
                                    "27.3.a", "27.3.a.20", "27.3.a.21",
                                    "27.7.d", "27.7.e")),
    
    summarizePrelFlexible("boot/data/Preliminary_landings_allSpecies_2025.csv",
                          latin = "Scophthalmus rhombus",
                          areas = c("27.2.a.2","27.2.a.1", "27.2.a", 
                                    "27.4.a", "27.4.b", "27.4.c",
                                    "27.3.a", "27.3.a.20", "27.3.a.21",
                                    "27.7.d", "27.7.e"))) 
    
  Uptake2a3a47de <- as.data.frame(prel2a43a7de %>%
                                    mutate(Area = case_when(Area %in% c("27.4", "27.2a")~ "27.2a4",
                                                            TRUE ~Area)) %>% 
                                    group_by(Year, Area) %>% 
                                    summarise(Landings=sum(Landings)) %>% 
    full_join(read_csv("boot/data/TAC_BLL_2a3a47de.csv"), by = c("Year","Area")) %>%
    ungroup())

  write.taf(Uptake2a3a47de, file = "bll.27.3a47de.uptake.TAC.2a3a47de.csv", dir = "data")
  write.taf(Uptake2a3a47de, file = "bll.27.3a47de.uptake.TAC.2a3a47de.csv", dir = "report/tables") 
  
  
  prel2a43a7de_conf <- 
    bind_rows(
      readPrelFlexible("boot/data/Preliminary_landings_allSpecies_2024.csv", 
                       latin = c("Scophthalmus rhombus"),
                       areas = c("27.2.a.2","27.2.a.1", "27.2.a", 
                                 "27.4.a", "27.4.b", "27.4.c",
                                 "27.3.a", "27.3.a.20", "27.3.a.21",
                                 "27.7.d", "27.7.e"),
                       speciesout = "BLL",
                       exclude.confidential = TRUE),
      readPrelFlexible("boot/data/Preliminary_landings_allSpecies_2025.csv", 
                       latin = c("Scophthalmus rhombus"),
                       areas = c("27.2.a.2","27.2.a.1", "27.2.a", 
                                 "27.4.a", "27.4.b", "27.4.c",
                                 "27.3.a", "27.3.a.20", "27.3.a.21",
                                 "27.7.d", "27.7.e"),
                       speciesout = "BLL",
                       exclude.confidential = TRUE))%>%
    group_by(Species, Year) %>%
    summarise(Not_Conf = sum(Landings), .groups = "drop")
  
  prel2a43a7de_combined <- left_join(prel2a43a7de %>% 
                                  group_by(Year) %>% 
                                  summarize(Landings = sum(Landings)),
                                prel2a43a7de_conf %>% select(Species, Year, Not_Conf), by = "Year") %>% 
    select(Species, Year, Landings, Not_Conf)
  
  write.taf(prel2a43a7de_combined, file = "bll.27.3a47de.prel2a43a7de.confidential.csv", dir = "report/tables") 
  ###################################
  msg("Data: Writting in Data repository")
  ###################################

  write.taf(bll.27.3a47de_catch, file = "bll.27.3a47de.catches.csv", dir = "data")

  dest_path   <- "data"
  
  # List CSV files
  files <- list.files(source_path, pattern = "index_wgnssk_26_sem.*\\.csv$", full.names = TRUE)
  
  for (f in files) {
    fname <- basename(f)
    
    # Remove prefix and suffix
    core <- sub("^index_", "", fname)
    core <- sub("\\.csv$", "", core)
    
    # Split into model description and semester
    parts <- strsplit(core, "_sem")[[1]]
    model <- parts[1]
    semester <- paste0("sem.", parts[2])  # sem.1, sem.2, etc.
    
    # Optional: clean model name (replace underscores with dots)
    model_clean <- gsub("_", ".", model)
    
    # Construct new filename
    new_name <- paste0(
      "bll.27.3a47de.idx.99.25.",
      model_clean, ".", semester, ".csv"
    )
    
    # Copy with new name
    file.copy(from = f,
              to = file.path(dest_path, new_name),
              overwrite = TRUE)
  }



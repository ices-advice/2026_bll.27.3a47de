library(dplyr)
library(ggplot2)
library(rlang)
library(RColorBrewer)
library(kableExtra)

levels_country <- c("Belgium","Germany","Germany  Fed. Rep. Of", "Germany, Fed. Rep. of", "Denmark", "France", "Ireland", "Netherlands", "Norway","Sweden", "Channel Is.- Guernsey","Channel Is.- Jersey","Isle of Man", "Channel Islands (ns)", "UK","UKS", "UK(Scotland)", "UK - Scotland", "UKE","UK (England)", "UK(Northern Ireland)", "UK - Eng+Wales+N.Irl.", "UK - England & Wales", "GB")
labels_country <- c("BE","DE", "DE", "DE", "DK" , "FR", "IE", "NL", "NO", "SE","UK" , "UK","UK", "UK", "UK","UK", "UK","UK","UK", "UK", "UK","UK", "UK", "UK")
levels_country_ol <- c("BE", "DE", "DK", "FR", "IE", "NL", "NO", "SE", "UK", "CI", "GG", "IM", "JE", "GB")
labels_country_ol <- c("BE", "DE", "DK", "FR", "IE", "NL", "NO", "SE", "UK", "UK", "UK", "UK", "UK", "UK")

levels_gear <- c("OTB", "SDN", "TBB", "GNS", "GTR", "LLS", "MIS", "SSC", "FPO", "DRB", "LHP", "OTM", "OTT")
labels_gear <-  c("Otter and Seine", "Otter and Seine", "Beam", "Trammel/gillnets", "Trammel/gillnets", "Other", "Other", "Otter and Seine", "Other", "Other", "Other", "Other", "Otter and Seine")

labels_gear_report <- c("OTB", "OTB", "TBB", "GTR", "GTR", "Other", "Other", "OTB", "Other", "Other", "Other", "OTB", "OTB")

levels_area <- c("27.3.a", "27.3.a.21", "27.3.a.20", "27.4" , "27.4.a", "27.4.b", "27.4.c","27.7.d", "27.7.e")
labels_area <-  c("27.3.a", "27.3.a", "27.3.a", "27.4" , "27.4", "27.4", "27.4","27.7.de", "27.7.de")
levels_division_hist <- c("III a","IV (not specified)", "IV a","IV a+b (not specified)","IV b","IV c", "VII d","VII e","VII d+e (not specified)")
labels_division_hist <- c("27.3.a", "27.4", "27.4", "27.4", "27.4", "27.4", "27.7.de", "27.7.de", "27.7.de")


## Functions
split_to_numbers <- function(x) as.numeric(strsplit(gsub(",", "", x), '[[:blank:]]{1,}')[[1]])


NAorCode <- function(x) {
  res <- unique(na.omit(x))
  if(length(res) == 0) return(NA_character_)
  res
}

# Normalize the units by dividing each value by its mean
normalize <- function(x) {
  return(x / mean(x))
}

## Helper functions ----
vsum <- function(x, y) {
  sum(x, y, na.rm = TRUE )
}

getBMS <- function(fn) {
  res <- read.csv(fn,
                  stringsAsFactors = FALSE, header = TRUE) %>% as_tibble() %>%
    filter(Species.Latin.Name %in% c("Scophthalmus rhombus"),
           Area %in% c("27_3_A", "27_3_A_20", "27_3_A_21", "27.3.a.20", "27.3.a.21",
                       "27_4_A" , "27_4_B", "27_4_C", "27.4.a","27.4.b", "27.4.c",
                       "27_7_D", "27_7_E", "27.7.d", "27.7.e"
           )) %>%
    mutate(Country = ifelse(Country == "GB", "UK", Country)) %>%
    group_by(Year, Country, Area)
  if ("AMS_Catch" %in% names(res)) {
    res$AMS.Catch.TLW. <- res$AMS_Catch
    suppressWarnings(res$BMS.Catch.TLW. <- as.numeric(res$BMS_Catch))
  }
  res %>% summarise(BMS = sum(as.numeric(BMS.Catch.TLW.), na.rm = TRUE))
}

get_catchcat_percent <- function(dat) {
  perarea <- dat %>%
    filter(!CatchCategory  %in% c("Logbook Registered Discard", "BMS landing")) %>%
    group_by(CatchCategory , Area, Country) %>%
    summarise(catchpercatchcat = sum(Caton), .groups = "drop") %>%
    ungroup() %>%
    reshape2::dcast(Area + Country ~ CatchCategory, value.var = "catchpercatchcat") %>%
    mutate(Total = Discards + Landings,
           `Discard ratio` = ifelse(Total > 0, Discards / (Landings + Discards) * 100, 0),
           DLratio = ifelse(Total > 0, Discards / Landings, 0),
           Year = first(dat$Year),
           Area = case_when(Area %in% c("27.3.a", "27.3.a.21", "27.3.a.20")~ "27.3.a",
                            Area %in% c("27.4" , "27.4.a", "27.4.b", "27.4.c")~ "27.4",
                            Area %in% c("27.7.d", "27.7.e")~ "27.7.de"),
           Country = factor(Country, levels = levels_country, labels = labels_country, ordered = TRUE))
  total <- perarea %>%
    summarise(Area = "Total",
              Discards = sum(Discards, na.rm = TRUE),
              Landings = sum(Landings, na.rm = TRUE),
              Total = sum(Total, na.rm = TRUE),
              Year = first(Year),
              .groups = "drop") %>%
    mutate(#`Raising factor` = Discards / Landings * 100,
      DLratio = ifelse(Total > 0, Discards / Landings, 0),
      `Discard ratio` = Discards / (Landings + Discards) * 100)
  bind_rows(perarea, total)
}


create_correctRetro <- function(retroindex, peels) {
  retroinps <- lapply(retroindex[2:(peels+1)], function(indx) {
    retroinp <- baseinp
    retroinp$timeI <- list(indx[[1]]$timeI,indx[[2]]$timeI+0.66, IndexSem83_98$timeI)
    retroinp$obsI <- list(indx[[1]]$ObsI,indx[[2]]$ObsI,IndexSem83_98$ObsI)
    retroinp$stdevfacI <- list(indx[[1]]$CV/mean(indx[[1]]$CV),indx[[2]]$CV/mean(indx[[2]]$CV),IndexSem83_98$CV/mean(IndexSem83_98$CV))
    keep <- retroinp$timeC %in% c(1950:1998, indx[[1]]$timeI)
    retroinp$timeC <- retroinp$timeC[keep]
    retroinp$obsC<- retroinp$obsC[keep]
    
    lstyr <- max(indx[[1]]$timeI)
    retroinp$stdevfacC <- c(rep(2, length(1950:1998)), rep(1, length(1999:lstyr)))
    retroinp$maneval <- NULL
    retroinp$maninterval <- NULL
    check.inp(retroinp)
  })
  
  inpretro <- c(list(baseinp), retroinps)
  correctRetro$retro <- lapply(inpretro, fit.spict)
  
  return(correctRetro)
}


#' Reads ICES preliminary catch statistics and returns only
#'
#' @param fn file name, it should contain the
#' @param latin string or vector, latin names of species to select
#' @param areas vector with ICES areas, e.g. "27_3_A_20"
#' @param areaout string, the name of the combined area
#' @param speciesout string, species name in the returned data.frame

#' @return data.frame of selected species by country
#' @export

readPrelFlexible <- function(fn,
                             latin,
                             areas,
                             speciesout,
                             exclude.confidential = FALSE) {
  
  if (!file.exists(fn)) return(NULL)
  
  res <- read.csv(fn, stringsAsFactors = FALSE) %>% as_tibble()
  
  # Optionally strip confidential rows
  if (exclude.confidential && "ConfidentialityFlag" %in% names(res)) {
    res <- res %>% filter(ConfidentialityFlag != "Y")
  }
  
  # 2023-style: dots in column names (Species.Latin.Name)
  if ("Species.Latin.Name" %in% names(res)) {
    
    yr <- unique(res$Year)
    stopifnot(length(yr) == 1)
    
    res %>%
      filter(Species.Latin.Name %in% latin, Area %in% areas) %>%
      distinct(Species.Latin.Name, Area, Year, Country, AMS.Catch.TLW., .keep_all = TRUE) %>%
      group_by(Species.Latin.Name, Area, Year, Country) %>%
      summarise(
        Landings    = vsum(AMS.Catch.TLW., BMS.Catch.TLW.),
        Units       = "TLW",
        .groups     = "drop"
      ) %>%
      rename(SpeciesName = Species.Latin.Name) %>% 
      mutate(Species = speciesout)
    
    # 2024-style: spaces in column names (`Species Latin Name`)
  } else {
    warning("Unrecognised column format in: ", fn)
    return(NULL)
  }
}

summarizePrelFlexible <- function(fn, latin = "Scophthalmus rhombus", areas = NULL, speciesout =NULL) {
  readPrelFlexible(fn, latin = latin, areas = areas, speciesout=speciesout) %>%
    group_by(Year, Area) %>%
    summarise(Landings = sum(Landings, na.rm = TRUE), .groups = "drop") %>%
    arrange(Year)
}

assign_percent_to_unallocated_landings <- function(s) {
  s %>%
    filter(CatchCategory  != "Discards", geargroup != "MIS") %>%
    group_by(group) %>%
    mutate(weight = ifelse(count == 2, Caton, NA)) %>%
    mutate(percraise = ifelse(count == 1,
                              weighted.mean(ifelse(count == 2, perc, NA), ifelse(count == 2, weight, NA), na.rm = TRUE),
                              ##weighted.mean(perc, Caton, na.rm = TRUE),
                              NA)) %>%
    mutate(percraise_includes = ifelse(count == 1,
                                       paste0(perc, collapse = ", "),
                                       NA),
           percraise_weights = ifelse(count == 1,
                                      paste0(weight, collapse = ", "),
                                      NA)) %>%
    ungroup()
}

get_gear_percent <- function(dat) {
  dat %>% mutate(gearcat = substr(Fleets, 1, 3)) %>%
    group_by(gearcat) %>%
    summarise(catchpergear = sum(Caton)) %>%
    ungroup() %>%
    mutate(perc=catchpergear / sum(catchpergear)*100,
           year = first(dat$Year))
}

get_gear_fine_percent <- function(dat) {
  dat %>% mutate(gearcat = substr(Fleets, 1, 7)) %>%
    group_by(gearcat) %>%
    summarise(catchpergear = sum(Caton)) %>%
    ungroup() %>%
    mutate(perc=catchpergear / sum(catchpergear)*100,
           year = first(dat$Year))
}

readPrel <- function(fn,
                     latin = c("Scophthalmus rhombus"),
                     areas = c("27_3_A", "27_3_A_20", "27_3_A_21",
                               "27_4_A" , "27_4_B", "27_4_C" ,
                               "27_7_D", "27_7_E"),
                     speciesout = "BLL") {
  res <- read.csv(fn, stringsAsFactors = FALSE, header = TRUE) %>% as_tibble()
  yr <- unique(res$Year)
  stopifnot(length(yr) == 1)
  if ("AMS_Catch" %in% names(res)) {
    res$AMS.Catch.TLW. <- res$AMS_Catch
    suppressWarnings(res$BMS.Catch.TLW. <- as.numeric(res$BMS_Catch))
  }
  res %>%
    filter(Species.Latin.Name %in% latin,
           Area %in% areas) %>%
    mutate(Country = ifelse(Country == "GB", "UK", Country)) %>%
    group_by(Year, Country) %>%
    mutate(Area = case_when(Area %in% c("27_3_A", "27_3_A_20", "27_3_A_21")~ "27.3.a",
                            Area %in% c("27_4_A" , "27_4_B", "27_4_C")~ "27.4",
                            Area %in% c("27_7_D", "27_7_E")~ "27.7.de"))%>%
    group_by(Species.Latin.Name,Area, Year, Country )%>%
    summarise(Landings = vsum(AMS.Catch.TLW., BMS.Catch.TLW.),
              Units = "TLW",
              Species = speciesout)%>%
    ungroup() %>%
    select(Species,Area,Units,Country, Year,Landings)
}

getBMS <- function(fn) {
  res <- read.csv(fn,
                  stringsAsFactors = FALSE, header = TRUE) %>% as_tibble() %>%
    filter(Species.Latin.Name %in% c("Scophthalmus rhombus"),
           Area %in% c("27_3_A", "27_3_A_20", "27_3_A_21", "27.3.a.20", "27.3.a.21",
                       "27_4_A" , "27_4_B", "27_4_C", "27.4.a","27.4.b", "27.4.c",
                       "27_7_D", "27_7_E", "27.7.d", "27.7.e"
           )) %>%
    mutate(Country = ifelse(Country == "GB", "UK", Country)) %>%
    group_by(Year, Country, Area)
  if ("AMS_Catch" %in% names(res)) {
    res$AMS.Catch.TLW. <- res$AMS_Catch
    suppressWarnings(res$BMS.Catch.TLW. <- as.numeric(res$BMS_Catch))
  }
  res %>% summarise(BMS = sum(as.numeric(BMS.Catch.TLW.), na.rm = TRUE))
}

vsum <- function(x, y) {
  sum(x, y, na.rm = TRUE )
}

# get_catchcat_percent <- function(dat) {
#     perarea <- dat %>%
#     filter(!CatchCategory  %in% c("Logbook Registered Discard", "BMS landing")) %>%
#     group_by(CatchCategory , Area, Country) %>%
#     summarise(catchpercatchcat = sum(Caton), .groups = "drop") %>%
#     ungroup() %>%
#     reshape2::dcast(Area + Country ~ CatchCategory, value.var = "catchpercatchcat") %>%
#     mutate(Total = Discards + Landings,
#            `Discard ratio` = ifelse(Total > 0, Discards / (Landings + Discards) * 100, 0),
#            DLratio = ifelse(Total > 0, Discards / Landings, 0),
#            Year = first(dat$Year),
#            Area = case_when(Area %in% c("27.3.a", "27.3.a.21", "27.3.a.20")~ "27.3.a",
#                             Area %in% c("27.4" , "27.4.a", "27.4.b", "27.4.c")~ "27.4",
#                             Area %in% c("27.7.d", "27.7.e")~ "27.7.de"),
#            Country = ifelse(Country %in% levels, Country, "Other"),
#            Country = factor(Country, levels = levels, labels = labels, ordered = TRUE))
#   total <- perarea %>%
#     summarise(Area = "Total",
#               Discards = sum(Discards, na.rm = TRUE),
#               Landings = sum(Landings, na.rm = TRUE),
#               Total = sum(Total, na.rm = TRUE),
#               Year = first(Year),
#               .groups = "drop") %>%
#     mutate(#`Raising factor` = Discards / Landings * 100,
#       DLratio = ifelse(Total > 0, Discards / Landings, 0),
#       `Discard ratio` = Discards / (Landings + Discards) * 100)
#   bind_rows(perarea, total)
# }

##### From Intercatch_Output_Analysis_Functions.r ####
readStockOverview <- function(StockOverviewFile, NumbersAtAgeLengthFile, CatchAndSampleDataTables){
  
  ###### LOAD SAMPLE WEIGHT DATA #####
  Wdata <- read.table(StockOverviewFile, header=TRUE, sep="\t")
  names(Wdata)[7]  <- "Fleet"
  names(Wdata)[10] <- "CatchWt"
  names(Wdata)[11] <- "CatchCat"
  names(Wdata)[12] <- "ReportCat"
  
  temp <- sum(Wdata$CatchWt[Wdata$CatchCat=="Logbook Registered Discard"])
  print(paste("The sum of catches from Logbook Registered Discard is ", temp, " kg. They are excluded from the summary", sep=""))
  temp <- sum(Wdata$CatchWt[Wdata$CatchCat=="BMS landing"])
  print(paste("The sum of catches from BMS landing is ", temp, " kg. They are re-categorized as Discards", sep=""))
  Wdata <- Wdata[Wdata$CatchCat != "Logbook Registered Discard", ]
  Wdata$CatchCat[Wdata$CatchCat == "BMS landing"] <- "Discards"
  Wdata$CatchCat <- as.character(Wdata$CatchCat)
  Wdata$CatchCat <- substr(Wdata$CatchCat, 1, 1)
  Wdata <- Wdata[, -ncol(Wdata)]
  
  ###### LOAD LENGTH FREQUENCY DATA #####
  Ndata <- read.table(NumbersAtAgeLengthFile, header=TRUE, sep="\t", skip=0)
  names(Ndata)[7] <- "CatchCat"
  names(Ndata)[9] <- "Fleet"
  
  Wdata <- merge(Wdata, Ndata[, c(3,4,5,7,9,10,11)], by=c("Area","Season","Fleet","Country","CatchCat"), all.x=TRUE)
  # existing length sampling flag
  Wdata$Sampled <- ifelse(is.na(Wdata$SampledCatch), FALSE, TRUE)
  
  ###### LOAD TABLE 2 (catch + sample data) #####
  test <- scan(CatchAndSampleDataTables, what='character', sep='\t')
  table2 <- test[(which(test == "TABLE 2.") + 3):length(test)]
  tmp <- table2[-c(1:56)]
  table2_bis <- data.frame(matrix(tmp, ncol=27, byrow=TRUE), stringsAsFactors=FALSE)
  colnames(table2_bis) <- table2[1:27]
  table2_bis <- data.table(table2_bis)
  table2_bis[, CATON          := as.numeric(as.character(CATON))]
  table2_bis[, CATON          := CATON / 1000]
  table2_bis[, CANUM          := as.numeric(as.character(CANUM))]
  table2_bis[, WECA           := as.numeric(as.character(WECA))]
  table2_bis[, AgeOrLength    := as.numeric(as.character(AgeOrLength))]
  table2_bis[, `No. of Length Samples` := as.numeric(as.character(`No. of Length Samples`))]
  table2_bis[, `No. of Age Samples`    := as.numeric(as.character(`No. of Age Samples`))]
  table2_bis[, Area    := as.factor(Area)]
  table2_bis[, Fleet   := factor(Fleet)]
  table2_bis[, Season  := factor(Season)]
  table2_bis[, Country := factor(Country)]
  table2_bis[, id      := paste(Stock, Country, Area, Season, Fleet)]
  table2_bis[Area == "IIIaN                                                       ", Area := "IIIaN"]
  table2_bis[, CatchCategory := factor(CatchCategory)]
  colnames(table2_bis)[colnames(table2_bis) == 'CATONRaisedOrImported'] <- 'RaisedOrImported'
  
  # collapse to one row per stratum
  table2_ter <- unique(table2_bis[, -c("Sex", "AgeOrLength", "CANUM", "WECA", "LECA")])
  
  # ── flag 1: landing strata that have an associated discard stratum (Imported_Data) ──
  ids_with_imported_disc <- table2_ter[
    RaisedOrImported == "Imported_Data" & CatchCategory == "Discards", 
    unique(id)]
  
  # ── flag 2: strata with at least one length sample ────────────────────────
  ids_sampled_length <- table2_ter[
    , .(has_length = any(`No. of Length Samples` > 0)), by = .(id, CatchCategory)
  ][has_length == TRUE]
  
  # ── flag 3: strata with at least one age sample ───────────────────────────
  ids_sampled_age <- table2_ter[
    , .(has_age = any(`No. of Age Samples` > 0)), by = .(id, CatchCategory)
  ][has_age == TRUE]
  
  # ── join flags back onto table2_ter strata (one row per stratum) ──────────
  strata_flags <- unique(table2_ter[, .(id, CatchCategory, CATON = sum(CATON)), 
                                    by = .(id, CatchCategory)])
  # cleaner: summarise CATON per stratum + attach flags
  strata_summary <- table2_ter[
    , .(CATON = sum(CATON)), by = .(id, CatchCategory, RaisedOrImported)
  ]
  strata_summary[, HasAssociatedDiscard := id %in% ids_with_imported_disc]
  strata_summary <- merge(
    strata_summary,
    ids_sampled_length[, .(id, CatchCategory, SampledLength = TRUE)],
    by = c("id", "CatchCategory"), all.x = TRUE
  )
  strata_summary <- merge(
    strata_summary,
    ids_sampled_age[, .(id, CatchCategory, SampledAge = TRUE)],
    by = c("id", "CatchCategory"), all.x = TRUE
  )
  strata_summary[is.na(SampledLength), SampledLength := FALSE]
  strata_summary[is.na(SampledAge),     SampledAge     := FALSE]
  
  # ── merge flags into Wdata ────────────────────────────────────────────────
  # Wdata uses single-char CatchCat ("L" / "D"), table2 uses full words — align first
  strata_flags_for_merge <- strata_summary[, .(
    Area    = sub(".* (\\S+)$", "\\1", id),   # if id structure allows; safer to keep key cols
    HasAssociatedDiscard,
    SampledLength,
    SampledAge
  )]
  
  # The safest join key is the same fields used to build `id` in table2_bis
  # Extract them back and map CatchCategory to single char to match Wdata$CatchCat
  strata_for_merge <- unique(table2_ter[
    , .(Country, Area, Season, Fleet, CatchCategory, id)
  ])
  strata_for_merge[, CatchCat := substr(as.character(CatchCategory), 1, 1)]
  
  strata_for_merge <- merge(
    strata_for_merge,
    strata_summary[, .(id, CatchCategory, HasAssociatedDiscard, SampledLength, SampledAge)],
    by = c("id", "CatchCategory")
  )
  
  Wdata <- merge(
    Wdata,
    unique(strata_for_merge[, .(Country, Area, Season, Fleet, CatchCat,
                                HasAssociatedDiscard, SampledLength, SampledAge)]),
    by = c("Country", "Area", "Season", "Fleet", "CatchCat"),
    all.x = TRUE
  )
  Wdata[is.na(Wdata$HasAssociatedDiscard), "HasAssociatedDiscard"] <- FALSE
  Wdata[is.na(Wdata$SampledLength),       "SampledLength"]       <- FALSE
  Wdata[is.na(Wdata$SampledAge),           "SampledAge"]           <- FALSE
  
  return(Wdata)
}

plotStockOverview <- function(dat, plotType="LandPercent", byFleet=TRUE, byCountry=TRUE, bySampled=TRUE, bySeason=FALSE, byArea=FALSE, countryColours=NULL, set.mar=TRUE, markSampled=TRUE, individualTotals=TRUE, ymax=NULL, fcex.names=0.7){
  
  plotTypes <- c("LandWt","LandPercent","CatchWt","DisWt","DisRatio","DiscProvided")
  if (!(plotType %in% plotTypes)) stop(paste("PlotType needs to be one of the following:", paste(plotTypes)))
  stock <- dat$Stock[1]
  
  impLand <- dat[dat$CatchCat=="L",]
  impDis  <- dat[dat$CatchCat=="D",]
  nArea <- nSeason <- nCountry <- nFleet <- 1
  SeasonNames <- sort(unique(impLand$Season))
  AreaNames   <- sort(unique(impLand$Area))
  
  countryLegend <- FALSE
  
  if (byFleet)   nFleet   <- length(unique(c(impLand$Fleet,   impDis$Fleet)))
  if (byCountry) { nCountry <- length(unique(c(impLand$Country, impDis$Country))); countryLegend <- TRUE }
  if (byArea)    nArea    <- length(AreaNames)
  if (bySeason)  nSeason  <- length(SeasonNames)
  if (!bySampled) markSampled <- FALSE
  
  if (length(countryColours)==1 && countryColours){
    cols <- colorRampPalette(brewer.pal(12, "Paired"))
    countryColours <- data.frame(
      "Country" = unique(dat$Country)[order(unique(dat$Country))],
      "Colour"  = cols(length(unique(dat$Country))),
      stringsAsFactors = FALSE)
  }
  if (length(countryColours)==1 && countryColours==FALSE){
    countryLegend  <- FALSE
    countryColours <- data.frame(
      "Country" = unique(dat$Country)[order(unique(dat$Country))],
      stringsAsFactors = FALSE)
    countryColours$Colour <- rep("grey", length(countryColours$Country))
  }
  
  LsummaryList <- list()
  DsummaryList <- list()
  summaryNames <- NULL
  i <- 1
  if (byFleet)   { LsummaryList[[i]] <- impLand$Fleet;   DsummaryList[[i]] <- impDis$Fleet;   summaryNames <- c(summaryNames,"Fleet");   i <- i+1 }
  if (byCountry) { LsummaryList[[i]] <- impLand$Country; DsummaryList[[i]] <- impDis$Country; summaryNames <- c(summaryNames,"Country"); i <- i+1 }
  if (bySeason)  { LsummaryList[[i]] <- impLand$Season;  DsummaryList[[i]] <- impDis$Season;  summaryNames <- c(summaryNames,"Season");  i <- i+1 }
  if (byArea)    { LsummaryList[[i]] <- impLand$Area;    DsummaryList[[i]] <- impDis$Area;    summaryNames <- c(summaryNames,"Area");    i <- i+1 }
  if (bySampled) { LsummaryList[[i]] <- impLand$Sampled; DsummaryList[[i]] <- impDis$Sampled; summaryNames <- c(summaryNames,"Sampled"); i <- i+1 }
  
  byNames      <- summaryNames
  summaryNames <- c(summaryNames, "CatchWt")
  
  # ── aggregate ──────────────────────────────────────────────────────────────
  if (plotType %in% c("LandWt","LandPercent")) {
    
    Summary <- aggregate(impLand$CatchWt, LsummaryList, sum)
    names(Summary) <- summaryNames
    names(Summary)[names(Summary)=="CatchWt"] <- "LandWt"
    prop_length <- sum(Summary$LandWt[Summary$Sampled]) / sum(Summary$LandWt)
    
    # age: swap Sampled slot for SampledAge
    LsummaryList_age        <- LsummaryList
    LsummaryList_age[[i-1]] <- impLand$SampledAge
    Summary_age             <- aggregate(impLand$CatchWt, LsummaryList_age, sum)
    names(Summary_age)      <- summaryNames
    names(Summary_age)[names(Summary_age)=="CatchWt"]  <- "LandWt"
    names(Summary_age)[names(Summary_age)=="Sampled"]  <- "SampledAge"
    prop_age <- sum(Summary_age$LandWt[Summary_age$SampledAge]) / sum(Summary_age$LandWt)
    
  } else if (plotType == "DisWt") {
    
    Summary <- aggregate(impDis$CatchWt, DsummaryList, sum)
    names(Summary) <- summaryNames
    names(Summary)[names(Summary)=="CatchWt"] <- "DisWt"
    prop_length <- sum(Summary$DisWt[Summary$Sampled]) / sum(Summary$DisWt)
    
    DsummaryList_age        <- DsummaryList
    DsummaryList_age[[i-1]] <- impDis$SampledAge
    Summary_age             <- aggregate(impDis$CatchWt, DsummaryList_age, sum)
    names(Summary_age)      <- summaryNames
    names(Summary_age)[names(Summary_age)=="CatchWt"] <- "DisWt"
    names(Summary_age)[names(Summary_age)=="Sampled"] <- "SampledAge"
    prop_age <- sum(Summary_age$DisWt[Summary_age$SampledAge]) / sum(Summary_age$DisWt)
    
  } else if (plotType == "DisRatio") {
    
    SummaryD <- aggregate(impDis$CatchWt, DsummaryList, sum)
    SummaryL <- aggregate(impLand$CatchWt, LsummaryList, sum)
    if (bySampled) {
      testN <- colnames(SummaryD)[grep('Group', colnames(SummaryD)) - 1]
    } else {
      testN <- colnames(SummaryD)[grep('Group', colnames(SummaryD))]
    }
    Summary        <- merge(SummaryD, SummaryL, all=TRUE, by=testN)
    Summary$DRatio <- Summary$x.x / (Summary$x.x + Summary$x.y)
    Summary        <- Summary[!is.na(Summary$DRatio),
                              c(testN,
                                paste0('Group.', length(testN)+1, '.x'),
                                paste0('Group.', length(testN)+1, '.y'),
                                'DRatio')]
    prop_length <- prop_age <- NULL
    
  } else if (plotType == "DiscProvided") {
    
    DsummaryList_red <- if (bySampled) DsummaryList[1:(length(DsummaryList)-1)] else DsummaryList[length(DsummaryList)]
    LsummaryList_red <- if (bySampled) LsummaryList[1:(length(LsummaryList)-1)] else LsummaryList[length(LsummaryList)]
    SummaryD <- aggregate(impDis$CatchWt, DsummaryList_red, sum)
    SummaryL <- aggregate(impLand$CatchWt, LsummaryList_red, sum)
    testN    <- colnames(SummaryD)[grep('Group', colnames(SummaryD))]
    Summary  <- merge(SummaryD, SummaryL, all=TRUE, by=testN)
    # Summary$Land_with_dis <- !is.na(Summary$x.x)
    Summary$DRatio        <- Summary$x.x / (Summary$x.x + Summary$x.y)
    Summary  <- Summary[, c(testN,'DRatio','x.y')]
    Summary$DRatio <- !is.na(Summary$DRatio)
    Summary  <- unique(Summary)
    Summary  <- Summary %>% arrange(-DRatio       , x.y)
    
    names(Summary) <- c(summaryNames)
    
    ProvidedDiscards <<- Summary
    prop <- sum(Summary$CatchWt [Summary$Sampled])/sum(Summary$CatchWt )
    prop_length <- prop_age <- prop
    
  } else if (plotType == "CatchWt") {
    
    Summary     <- aggregate(impLand$CatchWt, LsummaryList, sum)
    names(Summary) <- summaryNames
    prop_length <- prop_age <- NULL
    
  }
  
  # ── rename columns for non-DisRatio types ─────────────────────────────────
  if (plotType == "DisRatio") {
    names(Summary) <- c(
      summaryNames[-c(grep('Sampled', summaryNames), grep('CatchWt', summaryNames))],
      "SampledD","SampledL","DRatio")
  }
  
  stratumSummary <- Summary
  
  if (plotType %in% c("LandWt","LandPercent","DiscProvided","CatchWt")) {
    if ("CatchWt" %in% names(stratumSummary))
      names(stratumSummary)[names(stratumSummary)=="CatchWt"] <- "LandWt"
  } else if (plotType == "DisWt") {
    if ("CatchWt" %in% names(stratumSummary))
      names(stratumSummary)[names(stratumSummary)=="CatchWt"] <- "DisWt"
  }
  
  # ── sort stratumSummary ────────────────────────────────────────────────────
  if (bySampled && "Sampled" %in% names(stratumSummary)) {
    if (plotType != "DisRatio") {
      stratumSummary <- stratumSummary[
        rev(order(stratumSummary$Sampled,
                  stratumSummary[, ncol(stratumSummary)])),]
    } else {
      stratumSummary <- stratumSummary[
        rev(order(stratumSummary$SampledL,
                  stratumSummary$SampledD,
                  stratumSummary[, ncol(stratumSummary)])),]
    }
  } else {
    stratumSummary <- stratumSummary[
      rev(order(stratumSummary[, ncol(stratumSummary)])),]
  }
  
  catchData <- stratumSummary[, ncol(stratumSummary)]
  
  if (set.mar) par(mar=c(8,4,1,1)+0.1)
  
  # ── inner helper: draw one barplot panel ───────────────────────────────────
  .draw_panel <- function(ss, yvals, indx, prop_val, sampled_flag,
                          label_sampled, label_unsampled, title_suffix) {
    
    colVec <- if (byCountry) countryColours$Colour[match(ss$Country[indx], countryColours$Country)] else "grey"
    
    if (!is.null(ymax)) newYmax <- ymax
    if (is.null(ymax))  newYmax <- max(yvals, na.rm=TRUE)
    if (is.null(ymax) & plotType=="LandPercent") newYmax <- max(cumsum(yvals), na.rm=TRUE)
    if (markSampled)    newYmax <- 1.06 * newYmax
    
    if (byFleet) {
      namesVec <- ss$Fleet[indx]
    } else if (byArea & bySeason) {
      namesVec <- paste(ss$Area[indx], ss$Season[indx])
    } else if (!byCountry & !byArea & bySeason) {
      namesVec <- paste(ss$Season[indx])
    } else if (byCountry) {
      namesVec <- paste(ss$Country[indx])
    } else if (bySampled & !byCountry & nArea==1 & nSeason==1) {
      namesVec <- paste(ss$Season[indx])
    } else {
      namesVec <- as.character(seq_len(sum(indx)))
    }
    
    cumulativeY        <- cumsum(yvals)
    yvals[yvals > newYmax] <- newYmax
    
    if (newYmax == -Inf) {
      plot(0,0,type="n",axes=FALSE,xlab="",ylab="")
      box()
    } else {
      b <- barplot(yvals, names=namesVec, las=2, cex.names=fcex.names,
                   col=colVec, ylim=c(0, newYmax), yaxs="i")
      
      if (bySampled & markSampled & length(b) > 1) {
        nSampled  <- sum(sampled_flag[indx])
        prop_lab  <- round(prop_val * 100, 0)
        bar_width <- b[2] - b[1]
        if (nSampled > 0) {
          arrows(b[1]-bar_width/2,        newYmax*102/106,
                 b[nSampled]+bar_width/2, newYmax*102/106, code=3, length=0.1)
          arrows(b[nSampled]+bar_width/2, newYmax*102/106,
                 b[length(b)]+bar_width/2, newYmax*102/106, code=3, length=0.1)
          text((b[nSampled]+b[1])/2,
               newYmax*104/106, paste0(label_sampled,   " (", prop_lab,       "%)"), cex=0.8)
          text((b[length(b)]+b[nSampled])/2+bar_width/2,
               newYmax*104/106, paste0(label_unsampled, " (", 100-prop_lab,   "%)"), cex=0.8)
        } else {
          arrows(b[1]-bar_width/2,         newYmax*102/106,
                 b[length(b)]+bar_width/2, newYmax*102/106, code=3, length=0.1)
          text(b[length(b)]+bar_width/4, newYmax*104/106, label_unsampled, cex=0.8)
        }
      }
      
      if (countryLegend) legend("topright", inset=0.05,
                                legend=countryColours$Country,
                                col=countryColours$Colour, pch=15)
      box()
      if (plotType=="LandPercent") {
        lines(b-( b[2]-b[1])/2, cumulativeY, type="s")
        abline(h=c(1,5,90,95,99,100), col="grey", lty=1)
      }
    }
    title(title_suffix)
  }
  
  # ── main loop ──────────────────────────────────────────────────────────────
  for (a in 1:nArea) {
    if (bySeason & !(byCountry | byFleet)) nSeason <- 1
    for (s in 1:nSeason) {
      area   <- AreaNames[a]
      season <- SeasonNames[s]
      
      indx <- rep(TRUE, nrow(stratumSummary))
      if  (bySeason & !byArea & (byCountry | byFleet))            indx <- stratumSummary$Season==season
      if  (!bySeason & byArea)                                    indx <- stratumSummary$Area==area
      if  (bySeason & byArea & (byCountry | byFleet | bySampled)) indx <- stratumSummary$Area==area & stratumSummary$Season==season
      
      # base title
      title.txt <- stock
      if (!bySeason & byArea)                                    title.txt <- paste(stock, area)
      if (bySeason & !byArea & (byCountry | byFleet | bySampled)) title.txt <- paste(stock, season)
      if (bySeason & byArea)                                      title.txt <- paste(stock, area, season)
      
      # ── LandWt / LandPercent / DisWt: two plots (length + age) ────────────
      if (plotType %in% c("LandWt", "LandPercent", "DisWt")) {
        
        # sort each summary by its own sampling flag
        wt_col <- if (plotType == "DisWt") "DisWt" else "LandWt"
        
        ss_len <- stratumSummary[rev(order(stratumSummary$Sampled,
                                           stratumSummary[[wt_col]])),]
        ss_age <- Summary_age[rev(order(Summary_age$SampledAge,
                                        Summary_age[[wt_col]])),]
        
        # recompute indx for each (same logic, different data frame)
        make_indx <- function(ss) {
          idx <- rep(TRUE, nrow(ss))
          if (bySeason & !byArea & (byCountry | byFleet))            idx <- ss$Season==season
          if (!bySeason & byArea)                                    idx <- ss$Area==area
          if (bySeason & byArea & (byCountry | byFleet | bySampled)) idx <- ss$Area==area & ss$Season==season
          idx
        }
        indx_len <- make_indx(ss_len)
        indx_age <- make_indx(ss_age)
        
        yvals_fn <- function(ss, idx) {
          yv <- ss[[wt_col]][idx]
          if (plotType=="LandPercent") {
            sumW <- if (individualTotals) sum(ss[[wt_col]][idx], na.rm=TRUE) else sum(ss[[wt_col]], na.rm=TRUE)
            yv   <- 100 * yv / sumW
          }
          yv
        }
        
        # plot 1 — length sampling
        .draw_panel(
          ss             = ss_len,
          yvals          = yvals_fn(ss_len, indx_len),
          indx           = indx_len,
          prop_val       = prop_length,
          sampled_flag   = ss_len$Sampled,
          label_sampled  = "length sampled",
          label_unsampled= "length unsampled",
          title_suffix   = paste(title.txt, plotType, "- Length sampling")
        )
        
        # plot 2 — age sampling
        .draw_panel(
          ss             = ss_age,
          yvals          = yvals_fn(ss_age, indx_age),
          indx           = indx_age,
          prop_val       = prop_age,
          sampled_flag   = ss_age$SampledAge,
          label_sampled  = "age sampled",
          label_unsampled= "age unsampled",
          title_suffix   = paste(title.txt, plotType, "- Age sampling")
        )
        
        # ── DisRatio: original 2x2 panel logic ────────────────────────────────
      } 
      else if (plotType == "DisRatio") {
        
        par(mfrow=c(2,2))
        listSample <- unique(paste(stratumSummary$SampledD, stratumSummary$SampledL))
        for (ii in seq_along(listSample)) {
          idx <- which(
            stratumSummary$SampledD[indx] == strsplit(listSample[ii],' ')[[1]][1] &
              stratumSummary$SampledL[indx] == strsplit(listSample[ii],' ')[[1]][2])
          if (length(idx) > 0) {
            colVec  <- if (byCountry) countryColours$Colour[match(stratumSummary$Country[indx][idx], countryColours$Country)] else "grey"
            newYmax <- if (!is.null(ymax)) ymax else max(stratumSummary$DRatio, na.rm=TRUE)
            if (markSampled) newYmax <- 1.06 * newYmax
            namesVec <- if (byFleet) stratumSummary$Fleet[indx][idx] else stratumSummary$Country[indx][idx]
            b <- barplot(stratumSummary$DRatio[indx][idx], names=namesVec, las=2,
                         cex.names=fcex.names, col=colVec, ylim=c(0, newYmax), yaxs="i")
            if (countryLegend) legend("topright", inset=0.05,
                                      legend=countryColours$Country,
                                      col=countryColours$Colour, pch=15)
            box()
            title(paste(title.txt, plotType, "D/L", listSample[ii]))
          }
        }
        
        # ── DiscProvided / CatchWt / LandPercent fallback: single plot ─────────
      } else {
        
        if (plotType %in% c("LandWt","LandPercent","DiscProvided","CatchWt")) yvals <- stratumSummary$LandWt[indx]
        if (plotType %in% c("DiscProvided")) yvals <- stratumSummary$LandWt[indx]
        if (plotType == "LandPercent") {
          sumW  <- if (individualTotals) sum(stratumSummary$LandWt[indx], na.rm=TRUE) else sum(stratumSummary$LandWt, na.rm=TRUE)
          yvals <- 100 * stratumSummary$LandWt[indx] / sumW
        }
        if (plotType == "DisWt")   yvals <- stratumSummary$DisWt[indx]
        if (plotType == "CatchWt") yvals <- catchData[indx]
        
        .draw_panel(
          ss             = stratumSummary,
          yvals          = yvals,
          indx           = indx,
          prop_val       = if (!is.null(prop_length)) prop_length else 0,
          sampled_flag   = stratumSummary$Sampled,
          label_sampled  = if (plotType=="DiscProvided") "Landings with discards" else "sampled",
          label_unsampled= if (plotType=="DiscProvided") "no discards"            else "unsampled",
          title_suffix   = paste(title.txt, plotType)
        )
      }
    }
  }
}

# Define a function to calculate Mohn's rho
extract_retro_semester <- function(retroindex, semester) {
  df <- retroindex[[1]]
  # filter reference year series
  extract_retro <- list()
  extract_retro$Year <- df[df$semester == semester, "timeI"]
  extract_retro$base <- df[df$semester == semester, "ObsI"]
  n <- length(extract_retro$Year)
  # Loop through retro peels
  for (i in 2:length(retroindex)) {
    col_name <- paste0("peel", i - 1)
    df <- retroindex[[i]]
    vals <- df[df$semester == semester, "ObsI"]
    
    # align length safely (THIS is what you were missing)
    extract_retro[[col_name]] <- c(
      vals,
      rep(NA, max(0, n - length(vals)))
    )
  }
  # convert to data frame
  extract_retro <- as.data.frame(extract_retro)
  # set row names safely
  rownames(extract_retro) <- extract_retro$Year
  # drop Year column
  extract_retro <- extract_retro[, -1, drop = FALSE]
  return(extract_retro)
}

################## FUnction to get the % of landings with discard #############
# CatchAndSampleDataTables <- "../boot/data/CatchAndSampleDataTables.txt"
get_perc_land_with_disc <- function(CatchAndSampleDataTables) {
  # --- existing parsing code unchanged ---
  test <- scan(CatchAndSampleDataTables, what = 'character', sep = '\t')
  table2 <- test[(which(test == "TABLE 2.") + 3):length(test)]
  tmp <- table2[-c(1:56)]			  
  table2_bis <- data.frame(matrix(tmp, ncol = 27, byrow = TRUE), stringsAsFactors = FALSE)
  colnames(table2_bis) <- table2[1:27]
  table2_bis <- data.table(table2_bis)
  table2_bis <- table2_bis[, CATON := as.numeric(as.character(CATON))]
  table2_bis <- table2_bis[, CATON := CATON / 1000]
  table2_bis <- table2_bis[, CANUM := as.numeric(as.character(CANUM))]
  table2_bis <- table2_bis[, WECA := as.numeric(as.character(WECA))]
  table2_bis <- table2_bis[, AgeOrLength := as.numeric(as.character(AgeOrLength))]
  table2_bis <- table2_bis[, `No. of Length Samples` := as.numeric(as.character(`No. of Length Samples`))]
  table2_bis <- table2_bis[, `No. of Age Samples`    := as.numeric(as.character(`No. of Age Samples`))]
  table2_bis <- table2_bis[, Area    := as.factor(Area)]
  table2_bis <- table2_bis[, Fleet   := factor(Fleet)]
  table2_bis <- table2_bis[, Season  := factor(Season)]
  table2_bis <- table2_bis[, Country := factor(Country)]
  table2_bis <- table2_bis[, id := paste(Stock, Country, Area, Season, Fleet)]
  table2_bis[Area == "IIIaN                                                       ", 'Area'] <- "IIIaN"
  table2_bis$CatchCategory <- factor(table2_bis$CatchCategory)
  colnames(table2_bis)[colnames(table2_bis) == 'CATONRaisedOrImported'] <- 'RaisedOrImported'
  
  # --- collapse to one row per stratum (drop length/age rows) ---
  table2_ter <- unique(table2_bis[, -c("Sex", "AgeOrLength", "CANUM", "WECA", "LECA")])
  
  # ── 1. % landings with associated discards (your original metric) ─────────
  landingsWithAssociatedDiscards <- {
    agg <- table2_ter[, .(CATON = sum(CATON)), by = .(id, RaisedOrImported, CatchCategory)]
    ids_with_disc <- agg[RaisedOrImported == "Imported_Data" & CatchCategory == "Discards", id]
    agg[id %in% ids_with_disc & CatchCategory == "Landings", sum(CATON)] /
      table2_ter[CatchCategory == "Landings", sum(CATON)] * 100
  }
  
  # ── helper: CATON of strata that have at least one biological sample ──────
  # "sampled" = SampledOrEstimated contains "Sampled" (covers Sampled_Distribution etc.)
  has_length_sample <- function(dt) {
    dt[, sampled_length := any(`No. of Length Samples` > 0), by = id]
    dt[sampled_length == TRUE]
  }
  has_age_sample <- function(dt) {
    dt[, sampled_age := any(`No. of Age Samples` > 0), by = id]
    dt[sampled_age == TRUE]
  }
  
  # ── 2. % landings with sampled lengths ───────────────────────────────────
  total_landings_caton   <- table2_ter[CatchCategory == "Landings",  sum(CATON)]
  total_discards_caton   <- table2_ter[CatchCategory == "Discards",  sum(CATON)]
  
  perc_land_sampled_length <- {
    land <- table2_ter[CatchCategory == "Landings"]
    sampled_ids <- land[, .(has_sample = any(`No. of Length Samples` > 0)), by = id][has_sample == TRUE, id]
    land[id %in% sampled_ids, sum(CATON)] / total_landings_caton * 100
  }
  
  # ── 3. % discards with sampled lengths ───────────────────────────────────
  perc_disc_sampled_length <- {
    disc <- table2_ter[CatchCategory == "Discards"]
    sampled_ids <- disc[, .(has_sample = any(`No. of Length Samples` > 0)), by = id][has_sample == TRUE, id]
    disc[id %in% sampled_ids, sum(CATON)] / total_discards_caton * 100
  }
  
  # ── 4. % landings with sampled ages ──────────────────────────────────────
  perc_land_sampled_age <- {
    land <- table2_ter[CatchCategory == "Landings"]
    sampled_ids <- land[, .(has_sample = any(`No. of Age Samples` > 0)), by = id][has_sample == TRUE, id]
    land[id %in% sampled_ids, sum(CATON)] / total_landings_caton * 100
  }
  
  # ── 5. % discards with sampled ages ──────────────────────────────────────
  perc_disc_sampled_age <- {
    disc <- table2_ter[CatchCategory == "Discards"]
    sampled_ids <- disc[, .(has_sample = any(`No. of Age Samples` > 0)), by = id][has_sample == TRUE, id]
    disc[id %in% sampled_ids, sum(CATON)] / total_discards_caton * 100
  }
  
  # ── return all metrics as a named list ───────────────────────────────────
  list(
    perc_land_with_discards      = landingsWithAssociatedDiscards,
    perc_land_sampled_length     = perc_land_sampled_length,
    perc_disc_sampled_length     = perc_disc_sampled_length,
    perc_land_sampled_age        = perc_land_sampled_age,
    perc_disc_sampled_age        = perc_disc_sampled_age
  )
}

###### PLOT SPICT RETRO WITH ZOOM #########
plotspict.retro2 <- function (rep, stamp = get.version(), add.mohn = TRUE, CI = 0.95, xlim = NULL) {
  opar <- par(mfrow = c(2, 2), mar = c(2.5, 3.3, 4, 0.8))
  on.exit(par(opar))
  if (!"spictcls" %in% class(rep)) 
    stop("This function only works with a fitted spict object (class 'spictcls'). Please run `fit.spict` first.")
  if (!"retro" %in% names(rep)) 
    stop("No results of the retro function found. Please run the retrospective analysis using the `retro` function.")
  if (add.mohn) {
    mr <- suppressMessages(mohns_rho(rep, what = c("FFmsy", 
                                                   "BBmsy")))
    mrr <- round(mr, 3)
  }
  nruns <- length(rep$retro)
  bs <- bbs <- fs <- ffs <- time <- conv <- list()
  for (i in 1:nruns) {
    bs[[i]] <- get.par("logB", rep$retro[[i]], exp = TRUE, 
                       CI = CI)[rep$retro[[i]]$inp$indest, 1:3]
    bbs[[i]] <- get.par("logBBmsy", rep$retro[[i]], exp = TRUE, 
                        CI = CI)[rep$retro[[i]]$inp$indest, 1:3]
    fs[[i]] <- get.par("logFnotS", rep$retro[[i]], exp = TRUE, 
                       CI = CI)[rep$retro[[i]]$inp$indest, 1:3]
    ffs[[i]] <- get.par("logFFmsynotS", rep$retro[[i]], 
                        exp = TRUE, CI = CI)[rep$retro[[i]]$inp$indest, 
                                             1:3]
    time[[i]] <- rep$retro[[i]]$inp$time[rep$retro[[i]]$inp$indest]
    conv[[i]] <- rep$retro[[i]]$opt$convergence
  }
  conv <- ifelse(unlist(conv) == 0, TRUE, FALSE)
  sel <- function(x) x[, 2]
  cols <- spict:::cols()
  ylim <- range(0, sapply(bs[conv], sel), na.rm = TRUE) * 
    1.2
  plot(time[[1]], sel(bs[[1]]), type = "n", ylim = ylim, xlab = "", 
       ylab = "", lwd = 1.5, xlim = xlim)
  title(ylab = expression(B[t]), line = 2.2)
  polygon(c(time[[1]], rev(time[[1]])), c(bs[[1]][, 1], rev(bs[[1]][, 
                                                                    3])), col = "lightgrey", border = NA)
  for (i in seq(nruns)) {
    if (conv[i]) {
      lines(time[[i]], sel(bs[[i]]), col = cols[i], lwd = 2)
    }
  }
  par(lend = 2)
  s <- seq(nruns)
  lbls <- c("All", ifelse(conv[-1], paste0("-", s[-length(s)]), 
                          ""))[conv]
  cls <- cols[s][conv]
  if (sum(conv) <= 10) {
    ncol <- sum(conv)
  }
  else {
    ncol <- ceiling(sum(conv)/2)
    a <- seq(ncol)
    b <- seq(ncol + 1, sum(conv) + sum(conv)%%2)
    w <- unique(c(rbind(a, b)))
    lbls <- lbls[w]
    cls <- cls[w]
  }
  usr <- par("usr")
  xx <- usr[2] + diff(usr[c(1, 2)]) * 0.1
  yy <- mean(usr[4])
  legend(xx, yy, title = "Number of retrospective years", 
         xjust = 0.5, yjust = 0.1, legend = lbls, ncol = ncol, 
         col = cls, lty = 1, seg.len = 1, lwd = 6, x.intersp = 0.5, 
         bg = "transparent", box.lwd = 0, box.lty = 0, xpd = NA)
  par(lend = 1)
  box(lwd = 1.5)
  plot(time[[1]], sel(fs[[1]]), typ = "n", ylim = range(0, 
                                                        sapply(fs[conv], sel), na.rm = TRUE) * 1.2, xlab = "", 
       ylab = "", lwd = 1.5, xlim = xlim)
  title(ylab = expression(F[t]), line = 2.2)
  polygon(c(time[[1]], rev(time[[1]])), c(fs[[1]][, 1], rev(fs[[1]][, 
                                                                    3])), col = "lightgrey", border = NA)
  for (i in seq(nruns)) {
    if (conv[i]) {
      lines(time[[i]], sel(fs[[i]]), col = cols[i], lwd = 2)
    }
  }
  box(lwd = 1.5)
  par(mar = c(4, 3.3, 2.5, 0.8))
  plot(time[[1]], sel(bbs[[1]]), typ = "n", ylim = range(0, 
                                                         sapply(bbs[conv], sel), na.rm = TRUE) * 1.2, xlab = "", 
       ylab = "", lwd = 1.5, xlim = xlim)
  title(ylab = expression(B[t]/B[MSY]), line = 2.2)
  polygon(c(time[[1]], rev(time[[1]])), c(bbs[[1]][, 1], rev(bbs[[1]][, 
                                                                      3])), col = "lightgrey", border = NA)
  for (i in seq(nruns)) {
    if (conv[i]) {
      lines(time[[i]], sel(bbs[[i]]), col = cols[i], lwd = 2)
    }
  }
  if (add.mohn) 
    mtext(bquote("Mohn's " * rho[B/B[MSY]] * " = " * .(unname(mrr["BBmsy"]))), 
          3, 0.1)
  box(lwd = 1.5)
  plot(time[[1]], sel(ffs[[1]]), typ = "n", ylim = range(0, 
                                                         sapply(ffs[conv], sel), na.rm = TRUE) * 1.2, xlab = "", 
       ylab = "", lwd = 1.5, xlim = xlim)
  title(ylab = expression(F[t]/F[MSY]), line = 2.2)
  polygon(c(time[[1]], rev(time[[1]])), c(ffs[[1]][, 1], rev(ffs[[1]][, 
                                                                      3])), col = "lightgray", border = NA)
  for (i in seq(nruns)) {
    if (conv[i]) {
      lines(time[[i]], sel(ffs[[i]]), col = cols[i], lwd = 2)
    }
  }
  if (add.mohn) 
    mtext(bquote("Mohn's " * rho[F/F[MSY]] * " = " * .(unname(mrr["FFmsy"]))), 
          3, 0.1)
  box(lwd = 1.5)
  txt.stamp(stamp, do.flag = TRUE)
  nnotconv <- sum(!conv)
  if (nnotconv > 0) {
    message("Excluded ", nnotconv, " retrospective runs that ", 
            if (nnotconv == 1) 
              "was"
            else "were", " not converged: ", paste(which(!conv) - 
                                                     1, collapse = ", "))
  }
  if (add.mohn) 
    mr
  else invisible(NULL)
}

get_percent_raised <- function(fn) {
  so <- read.delim(fn, stringsAsFactors = FALSE)
  so %>%
    ##filter(CatchCategory  == "Discards") %>%
    group_by(CATONRaisedOrImported ) %>%
    summarise(sum(Caton))
}

get_discard_ratio_per_gear <- function(fn) {
  so <- read.delim(fn, stringsAsFactors = FALSE)
  so %>%
    mutate(gearcat = substr(Fleets, 1, 3)) %>%
    group_by(Area, gearcat, CatchCategory ) %>%
    summarise(Caton = sum(Caton)) %>%
    group_by(gearcat, Area) %>%
    mutate(Percent = Caton / sum(Caton) * 100) %>%
    filter(CatchCategory  == "Discards")
}


# raise_discards <- function(fn) {
#   so <- read.delim(fn, stringsAsFactors = FALSE)
#   s <- 0
#   for (fl in unique(so$Fleets)) {
#     s <- s + nrow(so[which(so$Fleets == fl),])
#   }
# }


get_quarter_percent <- function(dat) {
  dat %>% group_by(Season) %>% summarise(tot = sum(Caton)) %>%
    mutate(perc = tot / sum(tot)*100,
           year = first(dat$Year))
}

get_country_percent <- function(dat) {
  dat %>%
    filter(CatchCategory  == "Landings") %>%
    group_by(Country, Area) %>%
    summarise(tot = sum(Caton), .groups = "drop_last") %>%
    mutate(perc = tot / sum(tot)*100,
           year = first(dat$Year)) %>%
    ungroup() %>%
    transmute(Area, Country, Year = year, Landings = tot, perc)
}

read_noraise_lfq <- function(fn) {
  lfq <- read.table(fn, skip = 2, header = TRUE, sep = "\t")
  summary((lfq))
  lngt_cols <- which(grepl("Lngt", names(lfq)))
  lengths <- inteRcatch::get_numbers(names(lfq[lngt_cols]))
  data.frame(length = lengths, freq = unname(colSums(lfq[lngt_cols])))
}

read_noraise_lfq_split_land_dis_gear <- function(fn) {
  lfq <- read.table(fn, skip = 2, header = TRUE, sep = "\t")
  year <- inteRcatch::get_numbers(fn)
  summary((lfq))
  lngt_cols <- which(grepl("Lngt", names(lfq)))
  splitlfq <- split(lfq, list(lfq$CatchCategory ))
  dd <- mapply(function(x, n) {
    lengths <- inteRcatch::get_numbers(names(lfq[lngt_cols]))
    x <- x %>% mutate(gearcat = substr(Fleets, 1, 3))
    lapply(unique(x$gearcat), function(gc) {
      ttt <- x %>% filter(gearcat == gc)
      data.frame(length = lengths, freq = unname(colSums(ttt[lngt_cols])) , catch_cat = n,
                 year = year, stringsAsFactors = FALSE, gearcat = gc)
    })
  },
  splitlfq, names(splitlfq), SIMPLIFY = FALSE)
  bind_rows(dd$D %>% bind_rows(),
            dd$L %>% bind_rows())
}

read_noraise_lfq_split_land_dis <- function(fn) {
  lfq <- read.table(fn, skip = 2, header = TRUE, sep = "\t")
  year <- inteRcatch::get_numbers(fn)
  lngt_cols <- which(grepl("Lngt", names(lfq)))
  splitlfq <- split(lfq, list(lfq$CatchCategory ))
  dd <- mapply(function(x, n) {
    lengths <- inteRcatch::get_numbers(names(lfq[lngt_cols]))
    data.frame(length = lengths, freq = unname(colSums(x[lngt_cols])) , catch_cat = n,
               year = year, stringsAsFactors = FALSE)
  },
  splitlfq, names(splitlfq), SIMPLIFY = FALSE)
  bind_rows(dd$D %>% bind_rows(),
            dd$L %>% bind_rows())
}

plotQuantity <- function(x, add = FALSE, col = 1, ylim = NULL, xlim = NULL,
                         addUncertainty = FALSE, col.unc = 'lightgrey',
                         what = "logBBmsy", ylab = sub("log", "", what)) {
  bbmsy <- get.par(what, x, exp = TRUE)
  idx <- x$inp$indest
  time <- x$inp$time[idx]
  if ( ! add) {
    plot(time, bbmsy[idx, 2], col = col, lwd = 2, type = "n", 
         ylim = ylim, xlim = xlim,
         xlab = "", ylab = "")
    title(xlab = "Year", ylab = ylab)
  }
  if (addUncertainty) {
    polygon(c(time, rev(time)), 
            c(bbmsy[idx, 1], rev(bbmsy[idx, 3])), 
            col = col.unc, border = 'darkgrey')
  }
  lines(time, bbmsy[idx, 2], col = col, lwd = 2, type = "l", ylim = ylim)
}
getFractileSeries <- function(fit, what = c("BBmsy", "FFmsy"), fractiles = c(0.35, 0.65), when = last) {
  stopifnot(length(what) == length(fractiles))
  time <- fit$inp$time
  t <- annual(time, time, when)$annvec
  res <- mapply(function(w, f) {
    g <- get.par(paste0("log", w), fit, exp = FALSE)
    gm <- annual(time, g[, 2], when)$annvec
    gsd <- annual(time, g[, 4], when)$annvec
    exp(qnorm(f, gm, gsd))
  }, what, fractiles) 
  res <- cbind(t, res)
  colnames(res) <- c("Year", paste(what, fractiles, sep = "_"))
  res
}

plotStockStatus <- function(fit, when, plot = TRUE){
  time <- fit$inp$time
  idx <- which(time == when)
  
  bbmsy <- get.par("logBBmsy", fit, exp = FALSE)
  ffmsy <- get.par("logFFmsy", fit, exp = FALSE)
  bbmsyLast <- bbmsy[idx, ,drop = FALSE]
  ffmsyLast <- ffmsy[idx, ,drop = FALSE]
  
  fractile <- 0.35
  bbmsyLast_pa <- exp(qnorm(fractile,bbmsyLast[,2],bbmsyLast[,4]))
  ffmsyLast_pa <- exp(qnorm(1 - fractile,ffmsyLast[,2],ffmsyLast[,4]))
  
  if (plot) {
    par(mfrow = c(2, 1), mar = c(4,4,0.5, 0.5))
    plotQuantity(fit, addUncertainty = TRUE, ylim = c(0, 3.5), ylab = "B/Bmsy")
    abline(h = 0.5, lty = 2, col = "darkgrey")
    points(when, bbmsyLast_pa, pch = 20)
    plotQuantity(fit, addUncertainty = TRUE, ylim = c(0, 3.5), what = "logFFmsy", ylab = "F/Fmsy")
    abline(h = 1, lty = 2, col = "darkgrey")
    points(when, ffmsyLast_pa, pch = 20) 
  }
  list(BBmsy_last = bbmsyLast_pa, FFmsy_last = ffmsyLast_pa,
       BBmsy = bbmsy, FFmsy = ffmsy)
}

normalize <- function(x) {
  return(x / mean(x))
}


getmanline <- function(fit, assessment_basis = "catch", n_years_man = 2) {
  # Validate inputs
  assessment_basis <- tolower(assessment_basis)
  if (!assessment_basis %in% c("landings", "catch")) {
    stop("assessment_basis must be either 'landings' or 'catch'")
  }
  
  if (n_years_man < 1) {
    stop("n_years_man must be at least 1")
  }
  
  # Initialize list to store results for each year
  results <- list()
  
  # First management year
  cfy_base <- fit$inp$maninterval[1]
  
  # Loop through each management year
  for (i in 1:n_years_man) {
    
    # Current forecast year and biomass year
    cfy <- cfy_base + (i - 1)
    by <- fit$inp$maneval - 1 + (i - 1)
    
    # Find indices
    find <- which(fit$inp$time == cfy)
    bind <- which(fit$inp$time == by)
    cind <- which(getAnnualCatch(fit)$anntime == cfy)
    
    # Check if indices are valid
    if (length(cind) == 0 || length(bind) == 0 || length(find) == 0) {
      warning(paste("Year", cfy, "not found in model output. Stopping at year", i-1))
      break
    }
    
    # Get catch from model
    cpred <- getAnnualCatch(fit)[cind, 2]
    
    # Calculate catch, landings, and discards based on assessment basis
    if (assessment_basis == "landings") {
      # Assessment is based on landings, need to raise to catch
      lan <- cpred
      ct <- lan / (1 - meandis/100)
      dis <- ct * meandis/100
    } else {
      # Assessment is based on catch
      ct <- cpred
      lan <- ct * (1 - meandis/100)
      dis <- ct * meandis/100
    }
    
    # Get fishing mortality at start and end of year
    fstart <- get.par("logFFmsy", fit, TRUE)[find, 2]
    if (length(find) + 3 <= nrow(get.par("logFFmsy", fit, TRUE))) {
      fend <- get.par("logFFmsy", fit, TRUE)[find + 3, 2]
    } else {
      fend <- fstart
    }
    
    if (fstart < 0.000001) fstart <- 0
    if (fend < 0.000001) fend <- 0
    
    # Get biomass
    b <- get.par("logBBmsy", fit, TRUE)[bind, 2]
    
    # Determine reference values for comparison
    if (i == 1) {
      # First year: compare to forecast baseline
      b_ref <- b_for
      ct_ref <- as.numeric(lastAdvice)
    } else {
      # Subsequent years: ALWAYS compare to FIRST YEAR of this scenario
      # Not the previous year, but year 1 of the current management scenario
      b_ref <- as.numeric(b_man)  # Changed from i-1 to 1
      ct_ref <- as.numeric(ct_man)  # Changed from i-1 to 1
    }
    
    # Create row names
    nms <- gsub("YYY", by,
                gsub("XXX", cfy, c("Total catch (XXX)",  
                                   "Projected landings (XXX)", 
                                   "Projected discards (XXX)", 
                                   "Fishing mortality (FXXX/FMSY)",
                                   "Stock size (BYYY/BMSY)", 
                                   "% BYYY/BMSY change**", 
                                   "% TAC change***",
                                   "% advice change^")))
    
    # Create table for this year
    year_table <- setNames(data.frame(
      round(ct), 
      round(lan), 
      round(dis),
      icesRound(fend),
      icesRound(b),
      icesRound(safe_div((b - b_ref) * 100, b_ref)),
      icesRound(safe_div((round(ct, 0) - lastTAC) * 100, lastTAC)),  # Fixed: use lastTAC as denominator
      icesRound(safe_div((round(ct, 0) - ct_ref) * 100, ct_ref))
    ), nms)
    
    # Store results for this year (including raw values for next iteration)
    results[[i]] <- list(
      table = year_table
    )
    
    # Name the list element
    names(results)[i] <- paste0(cfy)
  }
  
  # Return results
  if (n_years_man == 1) {
    return(results[[1]]$table)
  } else if (n_years_man == 2) {
    # Maintain backward compatibility with original output format
    return(list(
      first_year = results[[1]]$table,
      second_year = results[[2]]$table
    ))
  } else {
    # For multiple years, return list of tables
    tables <- lapply(results, function(x) x$table)
    names(tables) <- names(results)
    
    return(list(
      tables = tables
    ))
  }
}

safe_div <- function(numerator, denominator) {
  # Handle division safely to avoid Inf, NaN, or errors
  result <- ifelse(
    is.na(denominator) | is.na(numerator) | denominator == 0,
    NA_real_,
    numerator / denominator
  )
  return(result)
}

getAnnualCatch <- function(rep) {
  b <- get.par("logB", rep, exp = TRUE) [,2]
  f <- get.par("logF", rep, exp = TRUE) [,2]
  data.frame(annual(rep$inp$time, b * f * rep$inp$dteuler, sum))}

plotspict.retro2 <- function (rep, stamp = get.version(), add.mohn = TRUE, CI = 0.95, xlim = NULL) {
  opar <- par(mfrow = c(2, 2), mar = c(2.5, 3.3, 4, 0.8))
  on.exit(par(opar))
  if (!"spictcls" %in% class(rep)) 
    stop("This function only works with a fitted spict object (class 'spictcls'). Please run `fit.spict` first.")
  if (!"retro" %in% names(rep)) 
    stop("No results of the retro function found. Please run the retrospective analysis using the `retro` function.")
  if (add.mohn) {
    mr <- suppressMessages(mohns_rho(rep, what = c("FFmsy", 
                                                   "BBmsy")))
    mrr <- round(mr, 3)
  }
  nruns <- length(rep$retro)
  bs <- bbs <- fs <- ffs <- time <- conv <- list()
  for (i in 1:nruns) {
    bs[[i]] <- get.par("logB", rep$retro[[i]], exp = TRUE, 
                       CI = CI)[rep$retro[[i]]$inp$indest, 1:3]
    bbs[[i]] <- get.par("logBBmsy", rep$retro[[i]], exp = TRUE, 
                        CI = CI)[rep$retro[[i]]$inp$indest, 1:3]
    fs[[i]] <- get.par("logFnotS", rep$retro[[i]], exp = TRUE, 
                       CI = CI)[rep$retro[[i]]$inp$indest, 1:3]
    ffs[[i]] <- get.par("logFFmsynotS", rep$retro[[i]], 
                        exp = TRUE, CI = CI)[rep$retro[[i]]$inp$indest, 
                                             1:3]
    time[[i]] <- rep$retro[[i]]$inp$time[rep$retro[[i]]$inp$indest]
    conv[[i]] <- rep$retro[[i]]$opt$convergence
  }
  conv <- ifelse(unlist(conv) == 0, TRUE, FALSE)
  sel <- function(x) x[, 2]
  cols <- spict:::cols()
  ylim <- range(0, sapply(bs[conv], sel), na.rm = TRUE) * 
    1.2
  plot(time[[1]], sel(bs[[1]]), type = "n", ylim = ylim, xlab = "", 
       ylab = "", lwd = 1.5, xlim = xlim)
  title(ylab = expression(B[t]), line = 2.2)
  polygon(c(time[[1]], rev(time[[1]])), c(bs[[1]][, 1], rev(bs[[1]][, 
                                                                    3])), col = "lightgrey", border = NA)
  for (i in seq(nruns)) {
    if (conv[i]) {
      lines(time[[i]], sel(bs[[i]]), col = cols[i], lwd = 2)
    }
  }
  par(lend = 2)
  s <- seq(nruns)
  lbls <- c("All", ifelse(conv[-1], paste0("-", s[-length(s)]), 
                          ""))[conv]
  cls <- cols[s][conv]
  if (sum(conv) <= 10) {
    ncol <- sum(conv)
  }
  else {
    ncol <- ceiling(sum(conv)/2)
    a <- seq(ncol)
    b <- seq(ncol + 1, sum(conv) + sum(conv)%%2)
    w <- unique(c(rbind(a, b)))
    lbls <- lbls[w]
    cls <- cls[w]
  }
  usr <- par("usr")
  xx <- usr[2] + diff(usr[c(1, 2)]) * 0.1
  yy <- mean(usr[4])
  legend(xx, yy, title = "Number of retrospective years", 
         xjust = 0.5, yjust = 0.1, legend = lbls, ncol = ncol, 
         col = cls, lty = 1, seg.len = 1, lwd = 6, x.intersp = 0.5, 
         bg = "transparent", box.lwd = 0, box.lty = 0, xpd = NA)
  par(lend = 1)
  box(lwd = 1.5)
  plot(time[[1]], sel(fs[[1]]), typ = "n", ylim = range(0, 
                                                        sapply(fs[conv], sel), na.rm = TRUE) * 1.2, xlab = "", 
       ylab = "", lwd = 1.5, xlim = xlim)
  title(ylab = expression(F[t]), line = 2.2)
  polygon(c(time[[1]], rev(time[[1]])), c(fs[[1]][, 1], rev(fs[[1]][, 
                                                                    3])), col = "lightgrey", border = NA)
  for (i in seq(nruns)) {
    if (conv[i]) {
      lines(time[[i]], sel(fs[[i]]), col = cols[i], lwd = 2)
    }
  }
  box(lwd = 1.5)
  par(mar = c(4, 3.3, 2.5, 0.8))
  plot(time[[1]], sel(bbs[[1]]), typ = "n", ylim = range(0, 
                                                         sapply(bbs[conv], sel), na.rm = TRUE) * 1.2, xlab = "", 
       ylab = "", lwd = 1.5, xlim = xlim)
  title(ylab = expression(B[t]/B[MSY]), line = 2.2)
  polygon(c(time[[1]], rev(time[[1]])), c(bbs[[1]][, 1], rev(bbs[[1]][, 
                                                                      3])), col = "lightgrey", border = NA)
  for (i in seq(nruns)) {
    if (conv[i]) {
      lines(time[[i]], sel(bbs[[i]]), col = cols[i], lwd = 2)
    }
  }
  if (add.mohn) 
    mtext(bquote("Mohn's " * rho[B/B[MSY]] * " = " * .(unname(mrr["BBmsy"]))), 
          3, 0.1)
  box(lwd = 1.5)
  plot(time[[1]], sel(ffs[[1]]), typ = "n", ylim = range(0, 
                                                         sapply(ffs[conv], sel), na.rm = TRUE) * 1.2, xlab = "", 
       ylab = "", lwd = 1.5, xlim = xlim)
  title(ylab = expression(F[t]/F[MSY]), line = 2.2)
  polygon(c(time[[1]], rev(time[[1]])), c(ffs[[1]][, 1], rev(ffs[[1]][, 
                                                                      3])), col = "lightgray", border = NA)
  for (i in seq(nruns)) {
    if (conv[i]) {
      lines(time[[i]], sel(ffs[[i]]), col = cols[i], lwd = 2)
    }
  }
  if (add.mohn) 
    mtext(bquote("Mohn's " * rho[F/F[MSY]] * " = " * .(unname(mrr["FFmsy"]))), 
          3, 0.1)
  box(lwd = 1.5)
  txt.stamp(stamp, do.flag = TRUE)
  nnotconv <- sum(!conv)
  if (nnotconv > 0) {
    message("Excluded ", nnotconv, " retrospective runs that ", 
            if (nnotconv == 1) 
              "was"
            else "were", " not converged: ", paste(which(!conv) - 
                                                     1, collapse = ", "))
  }
  if (add.mohn) 
    mr
  else invisible(NULL)
}

plotQuantity <- function(x, add = FALSE, col = 1, ylim = NULL, xlim = NULL,
                         addUncertainty = FALSE, col.unc = 'lightgrey',
                         what = "logBBmsy", ylab = sub("log", "", what)) {
  bbmsy <- get.par(what, x, exp = TRUE)
  idx <- x$inp$indest
  time <- x$inp$time[idx]
  if ( ! add) {
    plot(time, bbmsy[idx, 2], col = col, lwd = 2, type = "n", 
         ylim = ylim, xlim = xlim,
         xlab = "", ylab = "")
    title(xlab = "Year", ylab = ylab)
  }
  if (addUncertainty) {
    polygon(c(time, rev(time)), 
            c(bbmsy[idx, 1], rev(bbmsy[idx, 3])), 
            col = col.unc, border = 'darkgrey')
  }
  lines(time, bbmsy[idx, 2], col = col, lwd = 2, type = "l", ylim = ylim)
}
getFractileSeries <- function(fit, what = c("BBmsy", "FFmsy"), fractiles = c(0.35, 0.65), when = last) {
  stopifnot(length(what) == length(fractiles))
  time <- fit$inp$time
  t <- annual(time, time, when)$annvec
  res <- mapply(function(w, f) {
    g <- get.par(paste0("log", w), fit, exp = FALSE)
    gm <- annual(time, g[, 2], when)$annvec
    gsd <- annual(time, g[, 4], when)$annvec
    exp(qnorm(f, gm, gsd))
  }, what, fractiles) 
  res <- cbind(t, res)
  colnames(res) <- c("Year", paste(what, fractiles, sep = "_"))
  res
}

plotStockStatus <- function(fit, when, plot = TRUE){
  time <- fit$inp$time
  idx <- which(time == when)
  
  bbmsy <- get.par("logBBmsy", fit, exp = FALSE)
  ffmsy <- get.par("logFFmsy", fit, exp = FALSE)
  bbmsyLast <- bbmsy[idx, ,drop = FALSE]
  ffmsyLast <- ffmsy[idx, ,drop = FALSE]
  
  fractile <- 0.35
  bbmsyLast_pa <- exp(qnorm(fractile,bbmsyLast[,2],bbmsyLast[,4]))
  ffmsyLast_pa <- exp(qnorm(1 - fractile,ffmsyLast[,2],ffmsyLast[,4]))
  
  if (plot) {
    par(mfrow = c(2, 1), mar = c(4,4,0.5, 0.5))
    plotQuantity(fit, addUncertainty = TRUE, ylim = c(0, 3.5), ylab = "B/Bmsy")
    abline(h = 0.5, lty = 2, col = "darkgrey")
    points(when, bbmsyLast_pa, pch = 20)
    plotQuantity(fit, addUncertainty = TRUE, ylim = c(0, 3.5), what = "logFFmsy", ylab = "F/Fmsy")
    abline(h = 1, lty = 2, col = "darkgrey")
    points(when, ffmsyLast_pa, pch = 20) 
  }
  list(BBmsy_last = bbmsyLast_pa, FFmsy_last = ffmsyLast_pa,
       BBmsy = bbmsy, FFmsy = ffmsy)
}


# Generic helper to create polygon data
make_polygon_data <- function(df, time_col, upper_col, lower_col, group_cols, name = NULL) {
  df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      x = c(.data[[time_col]], rev(.data[[time_col]])),
      y = c(.data[[upper_col]], rev(.data[[lower_col]])),
      .groups = "drop"
    ) %>%
    mutate(name = name)
}

normalize_index <- function(df, group_vars = NULL, name = NULL, time_col = "timeI") {
  # Log-transform first
  df <- df %>%
    mutate(
      ObsI = log(ObsI),
      upper = log(upper),
      lower = log(lower)
    )
  
  if (!is.null(group_vars)) {
    df <- df %>%
      group_by(across(all_of(group_vars))) %>%
      mutate(
        mean_idx = mean(ObsI, na.rm = TRUE),
        idx = ObsI / mean_idx,
        upper = upper / mean_idx,
        lower = lower / mean_idx,
        name = name
      ) %>%
      ungroup()
  } else {
    mean_val <- mean(df$ObsI, na.rm = TRUE)
    df <- df %>%
      mutate(
        idx = ObsI / mean_val,
        upper = upper / mean_val,
        lower = lower / mean_val,
        name = name
      )
  }
  
  poly <- make_polygon_data(df, time_col, "upper", "lower", group_vars %||% "name", name = name)
  list(index_df = df, polygon_df = poly)
}

prepare_raw_index <- function(df, group_vars, name = NULL, time_col = "timeI") {
  # Log-transform first
  df <- df %>%
    mutate(
      ObsI = log(ObsI),
      upper = log(upper),
      lower = log(lower)
    )
  
  if (!is.null(name)) df$name <- name
  poly <- make_polygon_data(df, time_col, "upper", "lower", group_vars)
  list(index_df = df, polygon_df = poly)
}

plot_index <- function(index_df, polygon_df = NULL, x = "timeI", y = "idx",
                       group = "name", color_title = NULL, y_label = NULL,
                       facet = FALSE, title = NULL, file = NULL) {
  
  unique_groups <- unique(index_df[[group]])
  colors <- brewer.pal(min(length(unique_groups), 12), "Dark2")
  color_mapping <- setNames(colors[1:length(unique_groups)], unique_groups)
  
  p <- ggplot() +
    {if (!is.null(polygon_df)) geom_polygon(data = polygon_df, aes(x = x, y = y, fill = .data[[group]], group = .data[[group]]), alpha = 0.2)} +
    geom_line(data = index_df, aes_string(x = x, y = y, color = group, group = group), linewidth = 1.2) +
    scale_color_manual(values = color_mapping, name = color_title) +
    scale_fill_manual(values = color_mapping, guide = "none") +
    scale_x_continuous(breaks = unique(index_df[[x]]), labels = unique(index_df[[x]])) +
    # coord_cartesian(ylim = c(0, 2 * max(index_df$idx, na.rm = TRUE))) +
    labs(x = "Year", y = y_label, title = title) +
    theme_minimal() +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(colour = guide_legend(nrow = 1)) +
    {if (facet) facet_grid(. ~ name, scales = "free_y")}
  
  if (!is.null(file)) {
    ggsave(filename = file, plot = p, width = 15, height = 12, dpi = 300)
  }
  
  return(p)
}

make_block <- function(source, year, type, values) {
  data.frame(
    Source   = source,
    Year     = year,
    Type     = type,
    Variable = c("B/Bmsy", "F/Fmsy", "Catches"),
    Value    = as.numeric(values),
    stringsAsFactors = FALSE
  )
}

load_assessment_data <- function(year, base_path) {
  
  assess <- read.csv(file.path(
    base_path,
    paste0("WGNSSK", year),
    paste0(year, "_bll.27.3a47de_assessment/report/tables/bll.27.3a47de.assessment.summary.csv")
  ))
  
  ind <- read.csv(file.path(
    base_path,
    paste0("WGNSSK", year),
    paste0(year, "_bll.27.3a47de_assessment/report/tables/bll.27.3a47de.intermediate.year.csv")
  ))
  
  catch <- read.csv(file.path(
    base_path,
    paste0("WGNSSK", year),
    paste0(year, "_bll.27.3a47de_assessment/report/tables/bll.27.3a47de.catch.scenario.table.csv")
  ))%>% 
    filter(X == "F=Fmsy_C_fractile")
  
  # 🔥 Correct extraction
  current  <- assess %>% filter(Year == year)
  previous <- assess %>% filter(Year == year - 1)
  
  data_values <- data.frame(
    B.Bmsy   = current$B.Bmsy,
    F.Fmsy   = previous$F.Fmsy,
    Catches = previous$Catches
  )
  
  source <- paste0("WGNSSK ", year)
  
  rbind(
    make_block(
      source, year - 1, "Data year",
      data_values
    ),
    make_block(
      source, year, "Intermediate year",
      ind[c(1, 2, 3)]
    ),
    make_block(
      source, year + 1, "Advice year",
      catch[1, c(6, 5, 2)]
    )
  )
}


spict2flbeia <- function(
    spict_fit, # fitted spict object
    wt_units = "kg", # units for weight at age
    n_units = "10^3", # units for numbers
    catch_units = "t", # units for catch
    stock_name = "stk", # optional stock name
    disc = NULL # discard time series
){
  
  # extract biomass and make FLStock
  Bs <- as.data.frame(get.par("logB", spict_fit, exp = TRUE))
  Bs$time <- as.numeric(rownames(Bs))
  Bs$year <- floor(Bs$time)
  yrs <- sort(unique(Bs$year))
  tmp <- data.frame(year = yrs)
  tmp$B <- Bs$est[match(tmp$year, Bs$time)]
  flq <- FLQuant(tmp$B, dim=c(1,nrow(tmp)), dimnames=list(age=1, year=tmp$year), units="t")
  
  stock <- FLStock(stock=flq, name = stock_name)
  stock@stock.wt[1,] <- 1
  stock@stock.n <- stock@stock / stock@stock.wt
  stock@stock.wt@units <- wt_units
  stock@stock.n@units <- n_units
  
  # F or harvest rate (averaged over year)
  Fs <- as.data.frame(get.par("logF", spict_fit, exp = TRUE))
  Fs$time <- as.numeric(rownames(Fs))
  Fs$year <- floor(Fs$time)
  tmp <- aggregate(Fs$est, list(year=Fs$year), FUN = mean) # take mean over year?
  names(tmp)[which(names(tmp)=="x")] <- "f"
  stock@harvest[,ac(yrs)] <- tmp$f[match(yrs, tmp$year)]
  stock@harvest@units <- "f"
  
  # catches
  Cs <- as.data.frame(get.par("logB", spict_fit, exp = TRUE) *
                        get.par("logF", spict_fit, exp = TRUE) *
                        spict_fit$inp$dt)
  Cs$time <- as.numeric(rownames(Cs))
  Cs$year <- floor(Cs$time)
  tmp <- aggregate(Cs$est, list(year=Cs$year), FUN = sum)
  names(tmp)[which(names(tmp)=="x")] <- "catch"
  
  stock@catch[,ac(yrs)] <- tmp$catch[match(yrs, tmp$year)]
  stock@catch.wt[,] <- 1
  stock@catch.n[] <- c(stock@catch / stock@catch.wt)
  stock@catch@units <- catch_units
  stock@catch.wt@units <- wt_units
  stock@catch.n@units <- n_units
  
  # discards
  if(!is.null(disc)){
    stock@discards[,ac(yrs)] <- disc
  }else{
    stock@discards[,ac(yrs)] <- 0
  }
  stock@discards.wt[1,] <- 1
  stock@discards.n[] <- c(stock@discards / stock@discards.wt)
  stock@discards@units <- catch_units
  stock@discards.wt@units <- wt_units
  stock@discards.n@units <- n_units
  
  # landings
  stock@landings <- stock@catch - stock@discards
  stock@landings.wt[1,] <- 1
  stock@landings.n[] <- c(stock@landings / stock@landings.wt)
  stock@landings@units <- catch_units
  stock@landings.wt@units <- wt_units
  stock@landings.n@units <- n_units
  
  ## Other pars (not relevant?) ===============================
  stock@mat[1,] <- 1
  stock@harvest.spwn[1,] <- 0
  stock@m[1,] <- 0
  stock@m.spwn[1,] <- 0
  
  
  # create BD data for FLBEIA ----------------------------------------------
  
  tab1 <- sumspict.parest(spict_fit)
  tab3 <- sumspict.states(spict_fit) # intermediate year
  tab5 <- sumspict.predictions(spict_fit) # forecast
  
  r.stk <- (get.par("logm", spict_fit, exp=T)[2]*
              get.par("logn", spict_fit, exp=T)[2]^
              (get.par("logn", spict_fit, exp=T)[2]/
                 (get.par("logn", spict_fit, exp=T)[2]-1)))/
    get.par("logK", spict_fit, exp=T)[2]
  K.stk <- get.par("logK", spict_fit, exp=T)[2]
  p.stk <- get.par("logn", spict_fit, exp=T)[2] - 1
  
  res <- list()
  res$stk <- stock
  res$BDinfo$par.fixed <- spict_fit$par.fixed
  res$BDinfo$cov.fixed <- spict_fit$cov.fixed
  res$BDinfo$PellaTomlinson_pars <- c("r"=r.stk, "K"= K.stk, "p"=p.stk)
  
  # Add reference points
  res$BDinfo$refPts <- c(
    "Fmsy" = get.par("logFmsy", spict_fit, exp = TRUE)[,"est"],
    "Bmsy" = get.par("logBmsy", spict_fit, exp = TRUE)[,"est"],
    "MSY" = get.par("MSY", spict_fit)[,"est"]
  )
  
  return(res)
}


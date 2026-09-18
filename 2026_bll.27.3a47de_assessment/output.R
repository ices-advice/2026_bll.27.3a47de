## Extract results of interest, write TAF output tables

## Before:
## After:
##################################CONTINUE HEEEEEEEEEEEEEEEEEEEEERRRRRRRRRRRRRRRRRRRRREEEEEEEEEEEEEEEEE
###################################
msg("Output: Load packages")
###################################
library(spict)
library(FLCore)
library(icesTAF)
library(icesSAG)
library(icesASD)
library(mixfishtools)


options(dplyr.summarise.inform=FALSE)

mkdir("output")
mkdir("output/MIXFISH")
mkdir("output/SAG")
source("./00_functions.R")

## ggplot options
theme_set(theme_minimal())
type <- "qual"
palette <- 3

levels_country <- c("Belgium","Germany", "Denmark", "France", "Ireland", "Netherlands", "Norway", "Sweden", "UK", "Channel Is.- Guernsey", "Channel Is.- Jersey", "Isle of Man", "UK (England)", "UK(Scotland)", "UK(Northern Ireland)")
labels_country <- c("BE", "DE", "DK" , "FR", "IE", "NL", "NO", "SE", "UK","GG" , "JE", "IM","UK","UK", "UK")
levels_country_ol <- c("BE", "DE", "DK", "FR", "IE", "NL", "NO", "SE", "UK", "CI", "GG", "IM", "JE")
labels_country_ol <- c("BE", "DE", "DK", "FR", "IE", "NL", "NO", "SE", "UK", "UK", "UK", "UK", "UK")
levels_gear <- c("OTB", "SDN", "TBB", "GNS", "GTR", "LLS", "MIS", "SSC", "FPO", "DRB", "LHP", "OTM")
labels_gear <-  c("Otter and Seine", "Otter and Seine", "Beam", "Trammel/gillnets", "Trammel/gillnets", "Other", "Other", "Otter and Seine", "Other", "Other", "Other", "Other")
levels_area <- c("27.3.a", "27.3.a.21", "27.3.a.20", "27.4" , "27.4.a", "27.4.b", "27.4.c","27.7.d", "27.7.e")
labels_area <-  c("27.3.a", "27.3.a", "27.3.a", "27.4" , "27.4", "27.4", "27.4","27.7.de", "27.7.de")

###################################
msg("Output: Load data")
###################################
# olbll <- readRDS("./../data/official_landings_bll.27.3a47de.Rds")
olbll <- read.taf("./data/bll.27.3a47de.official.landings.csv")
lastyr <- max(olbll$Year)

# bllcatch <- read.taf("./../data/bll.27.3a47de.catches.csv")
bllcatch <- read.taf("./data/bll.27.3a47de.catches.csv")

# assessmentsummary <- read.taf("./tables/bll.27.3a47de.assessment.summary.csv")
assessmentsummary <- read.taf("./report/tables/bll.27.3a47de.assessment.summary.csv")

############################# MAKING FLSTOCK OBJECTS FOR MIXFISH ###########################################

# fit <- readRDS("./../model/bll.27.3a47de.fit.Rds")
fit <- readRDS("./model/bll.27.3a47de.fit.Rds")
spict_fit <- fit

fit_correct_retro <- readRDS("./model/bll.27.3a47de.fit.correctRetro.Rds")

rho_f <- mohns_rho(fit_correct_retro)[1]
rho_b <- mohns_rho(fit_correct_retro)[2]

res <- spict2flbeia(spict_fit = fit)
save(res, file = "./output/MIXFISH/bll.27.3a47de.FLBEIA.RData")

# extract spict estimate

stock_estimated <- res$stk
op <- par(mfcol = c(3,1), mar = c(3,4,2,2))
plotspict.biomass(fit)
plotspict.catch(fit)
plotspict.f(fit)
par(op)

# compare FLStock
plot(stock_estimated, metrics = list(SSB = ssb, Catch = catch, F = fbar))

#####MAKING SAG PLOTS ################

info <- stockInfo(StockCode = "bll.27.3a47de",
                  AssessmentYear = lastyr+1,
                  Purpose = "Advice",
                  StockCategory = 2,
                  ContactPerson = "damian.villagra@ilvo.vlaanderen.be",
                  # B: Biomass model (like SPiCT)
                  ModelType = "B",
                  ModelName = "SPiCT",
                  # B/Bmsy
                  StockSizeDescription="Biomass relative to Bmsy", StockSizeUnits="",
                  # F/Fmsy
                  
                  FishingPressureDescription="F/Fmsy", FishingPressureUnits="ratio",
                  # catch
                  CatchesLandingsUnits="t",
                  # REFPTS
                  # F/FMSY
                  FMSY = 1,
                  # BMSY = fit$report$Bmsy,
                  # B/BMSY
                  MSYBtrigger = 0.5,
                  # CIs
                  ConfidenceIntervalDefinition="95%")

fishdata <-
  stockFishdata(
    Year = assessmentsummary$Year,
# Catches
# Catches = assessmentsummary$Catches,
Landings = assessmentsummary$Landings,
Discards = assessmentsummary$Discards )

# F/Fmsy   
fishdata$FishingPressure <- assessmentsummary$`F/Fmsy`
fishdata$Low_FishingPressure <- assessmentsummary$`F/Fmsy_lower`
fishdata$High_FishingPressure <- assessmentsummary$`F/Fmsy_upper`
# B/Bmsy
fishdata$StockSize <- assessmentsummary$`B/Bmsy`
fishdata$Low_StockSize <- assessmentsummary$`B/Bmsy_lower`
fishdata$High_StockSize <- assessmentsummary$`B/Bmsy_upper`

options(icesSAG.use_token = TRUE)
options(icesSAG.messages = FALSE)

xml <- createSAGxml(info, fishdata)
capture.output(cat(xml), file="output/bll.27.3a47de_SAG.xml")

# ADD retro
# Note: ICES SAG expects commas as decimal separators in retro-bias values
fmt_comma <- function(x) gsub("\\.", ",", as.character(x))

retro_xml <- paste0(
  "\n<Assessment_retro-bias>\n",
  "<TerminalYear>", lastyr , "</TerminalYear>\n",
  "<RetroAssessment>5</RetroAssessment>\n",
  "<Fbarrho>", fmt_comma(round(rho_f, 4)), "</Fbarrho>\n",
  "<SSBrhoYear>Y</SSBrhoYear>\n",
  "<SSBrho>", fmt_comma(round(rho_b, 4)), "</SSBrho>\n",
  "<RecruitmentrhoYear>Y</RecruitmentrhoYear>\n",
  "<Recruitmentrho>", 0, "</Recruitmentrho>\n",
  "</Assessment_retro-bias>\n"
)

# Insert retro-bias block before the closing </Assessment> tag
xml <- sub("</Assessment>", 
               paste0(retro_xml, 
                      "<Chart_Settings><GraphKey>0</GraphKey><SettingKey>0</SettingKey><SettingValue>yes</SettingValue></Chart_Settings>\n", "</Assessment>"), xml)

# SAVE to file
cat(xml, file="output/SAG/bll.27.3a47de_SAG.xml")

key <- icesSAG::uploadStock(file="output/SAG/bll.27.3a47de_SAG.xml",upload = TRUE,verbose = TRUE)

key <- findAssessmentKey(stock = "bll.27.3a47de")
key <- 22409  

Catches_Plot <- icesSAG::getLandingsGraph(key)
F_Plot <- icesSAG::getFishingMortalityGraph(key)
B_Plot <- icesSAG::getSpawningStockBiomassGraph(key)
F_Histo_plot <- icesSAG::getFishingMortalityHistoricalPerformance(key)
B_Histo_plot <- icesSAG::getSSBHistoricalPerformance(key)
# icesSAG::setSAGSettingForAStock(key, chartKey = 10, settingKey = 58, settingValue = length(seq(lastyr:2022)))

plot(icesSAG::getStockStatusTable(key))
getStockStatusValues(key)

plot(Catches_Plot)
plot(F_Plot)
plot(B_Plot)
plot(Histo_plot)
plot(B_Histo_plot)


key <- findAssessmentKey(stock = "bll.27.3a47de", year = lastyr+1)
a <- icesSAG::getStockStatusTable(key)
plot(a)

## Output to share outside the working group: ASD (the catch scenario table of the advice sheet), SAG (the xml, the upload
## and the graphs SAG makes) and MIXFISH (FLBEIA)

## Before: model/bll.27.3a47de.fit.Rds, model/bll.27.3a47de.fit.correctRetro.Rds (model.R),
##         output/assessment/bll.27.3a47de.{assessment.summary, catch.scenario.table, assessment.settings}.csv
##         (output_assessment.R), boot/data/TAC_BLL_2a3a47de.csv
## After:  output/share/ASD/bll.27.3a47de_ASD.xml, output/share/SAG/bll.27.3a47de_SAG.xml (and sag_key.txt, the key of the
##         assessment in SAG), output/share/MIXFISH/bll.27.3a47de.FLBEIA.RData; the graphs SAG makes (Figure 1 and 2 of
##         the advice sheet, also in the presentation): output/advice_sheet/Figure_*.png

library(icesTAF)
library(spict)
library(FLCore)
library(icesSAG)
library(dplyr)

cp("boot/data/utilities.R", ".")      # the functions and stock settings, set up by taf.boot() from boot/DATA.bib
source("utilities.R")

options(dplyr.summarise.inform = FALSE)

contact <- "damian.villagra@ilvo.vlaanderen.be"

## TRUE screens output/share/SAG/bll.27.3a47de_SAG.xml in DATSU and uploads it to SAG (sag_upload() in utilities.R: a new
## assessment key each time; it asks for your ICES login). FALSE takes the assessment of this year that is in SAG
## already, to download its graphs. The xml can also be uploaded by hand: https://standardgraphs.ices.dk/manage/index.aspx
upload_to_sag <- FALSE

## The catch scenarios of the advice sheet (ASD), in their order: the scenario of model.R, and the row of the ASD table
asd_scenarios <- data.frame(
  Scenario   = c("F=Fmsy_C_fractile", "F=Fmsy", "F=Fsq", "F=0"),
  AR_ID      = c(12, 16, 17, 18),
  CS_Purpose = c("BasisAdvice", "OtherScenarios", "OtherScenarios", "OtherScenarios"),
  CS_Basis   = c("MSY", "Other", "Other", "Other"),
  CS_Label   = c("Maximum sustainable yield (MSY) approach (35th percentile of predicted catch distribution under F = FMSY)",
                 "FMSY", "F=F{last_yr}", "F=0"))       # {last_yr}: the last data year

asd_dir     <- "output/share/ASD"
sag_dir     <- "output/share/SAG"
mixfish_dir <- "output/share/MIXFISH"
mkdir(c(asd_dir, sag_dir, mixfish_dir))

fit <- readRDS("model/bll.27.3a47de.fit.Rds")
fit_correct_retro <- readRDS("model/bll.27.3a47de.fit.correctRetro.Rds")
assessmentsummary <- table_in("bll.27.3a47de.assessment.summary", "output/assessment")
settings <- table_in("bll.27.3a47de.assessment.settings", "output/assessment")
lastyr <- settings$last_yr


###################################
msg("Output: ASD")
###################################

## The agreed TACs of the intermediate year (without the third-country catches, TCC) and the catch limit (with them)
tac <- read.csv("boot/data/TAC_BLL_2a3a47de.csv")
sumTAC <- sum(tac$TAC[tac$Year == lastyr + 1 & !grepl("^TCC", tac$Area)], na.rm = TRUE)

asd_scenarios$CS_Label <- gsub("{last_yr}", lastyr, asd_scenarios$CS_Label, fixed = TRUE)
asd <- asd_tables(table_in("bll.27.3a47de.catch.scenario.table", "output/assessment"), asd_scenarios,
                  ay = lastyr + 1, lastTAC = settings$lastTAC, sumTAC = sumTAC, lastAdvice = settings$lastAdvice,
                  contact = contact)
asd_file <- file.path(asd_dir, paste0(stock_settings$code, "_ASD.xml"))
writeLines(asd_create_xml(asd$info, asd$columns, asd$rows, asd$values, asd$notes), asd_file, useBytes = TRUE)
msg("Output: ASD xml ", asd_file, " (upload it to the Advice and Scenarios Database by hand)")


###################################
msg("Output: SAG")
###################################

info <- stockInfo(StockCode = stock_settings$code,
                  AssessmentYear = lastyr + 1,
                  Purpose = "Advice",
                  StockCategory = 2,
                  ContactPerson = contact,
                  ModelType = "B",                    # biomass model (SPiCT)
                  ModelName = "SPiCT",
                  StockSizeDescription = "Biomass relative to Bmsy", StockSizeUnits = "",
                  FishingPressureDescription = "F/Fmsy", FishingPressureUnits = "ratio",
                  CatchesLandingsUnits = "t",
                  FMSY = 1,                           # F/Fmsy
                  MSYBtrigger = 0.5,                  # B/Bmsy
                  ConfidenceIntervalDefinition = "95%")

fishdata <- stockFishdata(Year = assessmentsummary$Year,
                          Landings = assessmentsummary$Landings,
                          Discards = assessmentsummary$Discards)
fishdata$FishingPressure      <- assessmentsummary$`F/Fmsy`
fishdata$Low_FishingPressure  <- assessmentsummary$`F/Fmsy_lower`
fishdata$High_FishingPressure <- assessmentsummary$`F/Fmsy_upper`
fishdata$StockSize            <- assessmentsummary$`B/Bmsy`
fishdata$Low_StockSize        <- assessmentsummary$`B/Bmsy_lower`
fishdata$High_StockSize       <- assessmentsummary$`B/Bmsy_upper`

sag_file <- file.path(sag_dir, paste0(stock_settings$code, "_SAG.xml"))
cat(sag_xml(info, fishdata,
            rho_f = mohns_rho(fit_correct_retro)[1],
            rho_b = mohns_rho(fit_correct_retro)[2],
            terminal_year = lastyr),
    file = sag_file)

key <- if (upload_to_sag) sag_upload(sag_file) else sag_key(lastyr + 1)
msg("Output: SAG assessment key ", key)
writeLines(as.character(key), file.path(sag_dir, "sag_key.txt"))

## The graphs SAG makes of the assessment: Figure 1 and 2 of the advice sheet (also in the presentation)
sag_save_graphs(key, "output/advice_sheet")


###################################
msg("Output: MIXFISH")
###################################

res <- spict2flbeia(spict_fit = fit)
save(res, file = file.path(mixfish_dir, "bll.27.3a47de.FLBEIA.RData"))

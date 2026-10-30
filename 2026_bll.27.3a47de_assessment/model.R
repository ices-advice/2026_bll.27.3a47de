## Run analysis, write model results

## Before: data/bll.27.3a47de.catches.csv, data/bll.27.3a47de.survey.index.*.csv (data.R) and
##         boot/data/survey_index_retro.rds (the survey indices of the retro peels)
## After:  model/bll.27.3a47de.fit*.Rds

library(icesTAF)
library(TMB)
library(spict)
library(icesAdvice)

# sourceTAF("model")


mkdir("model")
cp("boot/data/utilities.R", ".")      # the functions and stock settings, set up by taf.boot() from boot/DATA.bib
source("utilities.R")

## TRUE fits the extra management scenarios, one per F from 0.01 to the upper bound of Fmsy (several hours), and saves
## them to model/bll.27.3a47de.fit.extraFoptions.Rds; nothing else in the analysis uses them
run_extra_foptions <- FALSE

###################################
msg("Model: read in data")
###################################

#Loading catch and indices data
catch   <- read.taf("data/bll.27.3a47de.catches.csv")
IndexSem83_98   <- read.taf("data/bll.27.3a47de.survey.index.1983.1998.csv")
IndexSem1 <- read.taf(file="data/bll.27.3a47de.survey.index.sem1.csv")
IndexSem2 <- read.taf(file="data/bll.27.3a47de.survey.index.sem2.csv")

#Calulating the range of data and last data year
catchYears <- min(catch$Year):max(catch$Year) #Range of years
nyears <- length(catchYears)#Counting the records for catches
lastYear <- max(catch$Year)

#Defining priors as agreed in WGNSSK 2023
priors <- list(logn = c(log(2), 0.01, 1),      
               logr = c(log(0.647),0.322,1), # This prior was changed due to issues with Retro's
               logalpha = c(0,0,0), # prior is desactivated
               logbeta = c(0,0,0),# prior is desactivated
               logbkfrac = c(log(0.8),0.3,1),
               logsdi = list(c(log(0.25),0.3,1), c(log(0.5), 0.3, 1), c(0,0,0)))
                        
#Defining management interval and evualtion year
maninterval <- c(max(catchYears) + 2, max(catchYears) + 3)
maneval <- maninterval[2]

#Creating the input object
baseinp <- inp <- list(
  timeC = catch$Year ,
  obsC  = catch$Landings,
  timeI = list (IndexSem1$timeI,IndexSem2$timeI+0.66,IndexSem83_98$timeI),
  obsI  = list (IndexSem1$ObsI,IndexSem2$ObsI,IndexSem83_98$ObsI),
  stdevfacI = list (IndexSem1$CV/mean(IndexSem1$CV,na.rm = T),IndexSem2$CV/mean(IndexSem2$CV,na.rm = T),IndexSem83_98$CV/mean(IndexSem83_98$CV,na.rm = T)),
  stdevfacC = c(rep(2, length(1950:1998)), rep(1, length(1999:lastYear))),
  priors = priors,
  maninterval = maninterval,
  maneval = maneval,
  optimiser.control = list(iter.max = 1e4, eval.max = 1e4)
)

inp <- check.inp(inp)
inp_old <- check.inp(inp)

###################################
msg("Model: model fit")
###################################
# fit <- bll.27.3a47de.fit 
#Fitting the input  object to SPiCT
fit <- fit.spict(inp)

saveRDS(fit, "model/bll.27.3a47de.fit.Rds")

calc.om(fit)
fit <- calc.osa.resid(fit)
fit <- calc.process.resid(fit)

msg("Model: Sensitivity")
fit <- check.ini(fit, ntrials = 30)

msg("Model: retro")
fit <- retro(fit)

msg("Model: hindcast")
fit <- hindcast(fit)

msg("Model: saving the results")
saveRDS(fit, "model/bll.27.3a47de.fit.Rds")
readRDS("model/bll.27.3a47de.fit.Rds")
fitManagement <- fit

msg("Model: management scenarios")
#Getting management scenarios (no asumption on the catch for intermediate year Fsq)
fitManagement <- add.man.scenario(fitManagement, "F=Fmsy_C_fractile", fractiles = list(catch = 0.35), breakpointB = c(1/2))
fitManagement <- add.man.scenario(fitManagement, "F=Fmsy", breakpointB = c(1/2))
fitManagement <- add.man.scenario(fitManagement, "F=Fsq", ffac = 1)
fitManagement <- add.man.scenario(fitManagement, "F=0", ffac = 0)
fitManagement <- add.man.scenario(fitManagement, "F=Fmsy_All_fractiles", fractiles = list(catch = 0.35, bbmsy = 0.35, ffmsy = 0.35), breakpointB = c(1/2))

msg("Model: saving the management scenario results")
saveRDS(fitManagement, "model/bll.27.3a47de.fit.management.Rds")

if (run_extra_foptions) {
  msg("Model: extra Foption management scenarios")

  fitExtraFoptions <- fit

  # F options from 0.01 to upper 95% bound of Fmsy estimate
  for (fopt in seq(0.01, get.par("Fmsy", fit)[3], 0.01 )) {
    progress <- (fopt - 0.01) / (get.par("Fmsy", fit)[3] - 0.01) * 100
    msg(paste("Fitting extra Foptions (F = ",fopt,") - progress:", round(progress, 2), "%"))
    fitExtraFoptions <- add.man.scenario(fitExtraFoptions, paste0("F=", fopt), fractiles = list(catch = 0.35), fabs = fopt)}

  # meandis <- mean((catch$Discards / catch$Total * 100) [catch$Year %in% seq(lastyr-2, lastyr)])
  #
  # catchscenariotableExtra <- t(sapply(fitExtraFoptions$man, getmanline)) %>% write.csv("./report/tables/bll.27.3a47de.fit.catch.scenario.table.extraFoptions.csv", row.names = TRUE)

  saveRDS(fitExtraFoptions, "model/bll.27.3a47de.fit.extraFoptions.Rds")
} else msg("Model: extra Foption management scenarios skipped (run_extra_foptions = FALSE)")

msg("Model: correct retro")
retro_indices <- readRDS("boot/data/survey_index_retro.rds")   # the full index, then the 5 peels

## The retro peels with the survey indices recalculated for each peel (the NS-IBTS Q1 1983-1998 index stays as it is)
correctRetro <- correct_retro(fit, baseinp, retro_indices, peels = 5,
                              fixed_indices = list(IndexSem83_98),
                              fixed_catch_years = 1950:1998)

saveRDS(correctRetro, file = "model/bll.27.3a47de.fit.correctRetro.Rds")
# sourceTAF("model")

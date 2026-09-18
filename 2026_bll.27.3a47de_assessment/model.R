## Run analysis, write model results

## Before:
## After:

library(icesTAF)
library(TMB)
library(spict)
library(icesAdvice)

# sourceTAF("model")


mkdir("model")
source("./00_functions.R")

###################################
msg("Model: read in data")
###################################

#Loading catch and indices data
catch   <- read.taf("data/bll.27.3a47de.catches.csv")
IndexSem83_98   <- read.taf("data/bll.27.3a47de.idx.83.98.csv")
IndexSem1 <- read.taf(file="data/bll.27.3a47de.idx.99.25.wgnssk.26.sem.1.csv")
IndexSem2 <- read.taf(file="data/bll.27.3a47de.idx.99.25.wgnssk.26.sem.2.csv")

# IndexSem1 <- IndexSem1 %>% filter (timeI != 2022)
# IndexSem1$ObsI[IndexSem1$timeI == 2022] <- NA
# IndexSem1$lower     [IndexSem1$timeI == 2022] <- NA
# IndexSem1$upper        [IndexSem1$timeI == 2022] <- NA
# IndexSem1$CV        [IndexSem1$timeI == 2022] <- NA
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

#Fitting the input  object to SPiCT
fit <- fit.spict(inp)
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

msg("Model: correct retro")
correctRetro <- fit
load("../2026_bll.27.3a47de_survey/output/index_retros/index_retro_wgnssk_26.RData")
retroindex <- retro_indices
# load("boot/data/retroindex.RData")
# Define the function with num_retro parameter

create_correctRetro <- function(peels) {
  retroinps <- lapply(retroindex[2:(peels+1)], function(indx) {
    retroinp <- baseinp
    indx <- split(indx, indx$semester)
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

# Create correctRetro with peels = 5
correctRetro <- create_correctRetro(peels = 5)

saveRDS(correctRetro, file = "model/bll.27.3a47de.fit.correctRetro.Rds")
# sourceTAF("model")

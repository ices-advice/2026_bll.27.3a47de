## Run the TAF assessment of brill (bll.27.3a47de) for WGNSSK 2026 (catch year 2025): a step at a time, or all of it at once
##
## The steps, in this order:
##   1. taf.boot()             boot/data from boot/DATA.bib: every file of boot/initial/data (one entry per file: the
##                             landings, TAC, InterCatch, survey indices and figures, previous assessments, the SCF
##                             allocation tables CAR/CAS/DAR/DAS, the report sources and utilities.R) and the RDBES
##                             download RDBES_SCF of 2024 and 2025 (boot/RDBES_SCF.R: it asks for your ICES login)
##   2. data.R                 official landings + InterCatch + RDBES SCF + survey indices -> data/
##   3. model.R                the SPiCT fit, management scenarios and retro correction -> model/
##                             (run_extra_foptions = TRUE in model.R adds the extra F scenarios: several hours)
##   4. output.R               runs the three output scripts in this order:
##        output_catch.R            the figures and tables of the catch data (official landings, InterCatch, RDBES, discards)
##        output_assessment.R       the figures and tables of the survey indices, the assessment, the forecast and the advice
##        output_ASD_SAG_MIXFISH.R  the ASD xml (catch scenarios), the SAG xml and MIXFISH (FLBEIA) -> output/share; the
##                                  SAG graphs -> output/advice_sheet (upload_to_sag = TRUE in it uploads to SAG)
##   5. report.R               the WG report section (Word, ICES template) and the presentation -> report/
##
## The output, each figure and table made once and named after where it is shown:
##   output/report/        Figure_3.x.x_*.png, Table_3.x.x_*.csv: the WG report section (numbered as in the report)
##   output/advice_sheet/  Figure_1/2_*.png (SAG), Table_1/2/6/7/8a-c/9_*.csv: the advice sheet (the report shows them too)
##   output/presentation/  the figures and tables only the presentation shows
##   output/assessment/    the unrounded results SAG and next year's assessment read (copy them to
##                         boot/initial/data/WGNSSK<year>_*.csv next year) and the values the report quotes
##   output/share/         SAG (xml), MIXFISH (FLBEIA) and ASD, to share outside the working group
##
## The functions and the stock settings of all the scripts and the reports are in utilities.R.
##
## Every script is run by source.taf(). With clean = TRUE it empties the folder named after the script first (data/,
## model/, report/). output.R runs with clean = FALSE: output/share/ASD holds a file no script makes. An output script
## can also be run on its own, e.g. source.taf("output_catch.R", clean = FALSE).
## Nothing is run when this file is sourced: run the lines you want (Ctrl+Enter).

library(TAF)
library(here)

setwd(here())   # taf.boot() and source.taf() work in the folder of the analysis


## ###########################################################################
## Step by step
## ###########################################################################
taf.boot(clean = FALSE)                 # clean = FALSE keeps the RDBES download that is there: no new download, no login
source.taf("data.R", clean = TRUE)
source.taf("model.R", clean = TRUE)
source.taf("output.R", clean = FALSE)          # output_catch.R, output_assessment.R, output_ASD_SAG_MIXFISH.R
source.taf("report.R", clean = TRUE)


## ###########################################################################
## All at once: stops at the first script that fails
## ###########################################################################
# taf.boot(software = FALSE)
# for (s in c("data.R", "model.R", "output.R", "report.R"))
#   if (!source.taf(s, clean = s %in% c("data.R", "model.R", "report.R"))) stop(s, " failed: see the error above")

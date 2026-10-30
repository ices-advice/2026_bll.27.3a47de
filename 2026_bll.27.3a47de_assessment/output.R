## Output: run the three output scripts, in this order (each in an environment of its own)
##
##   output_catch.R            the figures and tables of the catch data (official landings, InterCatch, RDBES, discards)
##   output_assessment.R       the figures and tables of the survey indices, the assessment, the forecast and the advice
##   output_ASD_SAG_MIXFISH.R  the ASD xml, the SAG xml (and upload, upload_to_sag in it) and MIXFISH -> output/share;
##                             the SAG graphs -> output/advice_sheet
##
## Before: data/ (data.R), model/ (model.R), boot/data/
## After:  output/report, output/advice_sheet, output/presentation, output/assessment, output/share
##
## A script can also be run on its own: source.taf("output_catch.R", clean = FALSE). Run this one with clean = FALSE:
## output/share/ASD holds the advice sheet workbook (bll.27.3a47de_ASD.xlsm), which no script makes.

library(icesTAF)

for (script in c("output_catch.R", "output_assessment.R", "output_ASD_SAG_MIXFISH.R")) {
  msg("Output: ", script)
  source(script, local = new.env())
}

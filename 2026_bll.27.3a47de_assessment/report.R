## Prepare plots and tables for report

## Before:
## After:



# source.taf("report")


cp("boot/initial/report/*", "report/")
cp("boot/initial/00_functions.R", "report/")

outdir <- "report/"

output_format <- NULL # "all"
quiet <- FALSE

msg("Report: Making catch working document")
render("report/bll.3a47de_catch_WD.qmd", output_dir = outdir,
       #output_format = output_format,
       clean = TRUE, quiet = quiet,  encoding = 'UTF-8')

icesTAF::msg("Report: Making assessment working document")
render("report/bll.3a47de_assessment_WD.qmd", output_dir = outdir,
       #output_format = output_format,
       clean = TRUE, quiet = quiet,  encoding = 'UTF-8')

icesTAF::msg("Report: Making catch and assessment presentation")
render("report/bll.3a47de_catch_assessment_Presentation_html.qmd",
       output_dir = outdir,
       #output_format = output_format,
       clean = TRUE, quiet = quiet,  encoding = 'UTF-8')



## Render the WG report section of brill (Word, on the ICES Scientific Reports template) and the presentation (html)

## The report and the presentation only show what the output scripts made: the figures and tables of output/report,
## output/advice_sheet and output/presentation (and the survey figures of boot/data/survey_*). Nothing is
## calculated or plotted here.

## Before: boot/data/ (set up by taf.boot() from boot/DATA.bib): report.qmd, presentation.qmd, ices_reference.docx and
##         ices_fixups.R (the ICES template), the tables that are text (dutch_po_measures, spict_settings,
##         reference_points, benchmark_issues .csv), styles.css, footer.html and IVLO_logo.png for the presentation;
##         boot/data/utilities.R; output/ (output_catch.R, output_assessment.R, output_ASD_SAG_MIXFISH.R)
## After:  report/bll.27.3a47de_WGNSSK_report.docx, report/bll.27.3a47de_presentation.html

library(icesTAF)

mkdir("report")
## The sources of the report and the presentation, from boot/data
report_files <- c("report.qmd", "presentation.qmd", "styles.css", "footer.html", "IVLO_logo.png", "rhombus.jpg", "ices_reference.docx",
                  "ices_fixups.R", "dutch_po_measures.csv", "spict_settings.csv", "reference_points.csv", "benchmark_issues.csv")
cp(file.path("boot/data", report_files), "report/")
cp("boot/data/utilities.R", ".")      # the reports source ../utilities.R

if (is.null(quarto::quarto_path()))
  stop("Quarto was not found: install it (https://quarto.org; RStudio has one) or give its path in the variable QUARTO_PATH")

msg("Report: the WG report section (Word)")
quarto::quarto_render("report/report.qmd", output_format = "docx")
source("report/ices_fixups.R")
ices_fix_docx("report/report.docx")
file.rename("report/report.docx", "report/bll.27.3a47de_WGNSSK_report.docx")

msg("Report: the presentation (html)")
quarto::quarto_render("report/presentation.qmd")
file.rename("report/presentation.html", "report/bll.27.3a47de_presentation.html")

## Only the rendered report and presentation stay in report/
keep <- c("bll.27.3a47de_WGNSSK_report.docx", "bll.27.3a47de_presentation.html")
unlink(setdiff(list.files("report", full.names = TRUE, all.files = TRUE, no.. = TRUE), file.path("report", keep)), recursive = TRUE)

## Download the SCF (Catch Combined and Distribution Combined) of brill (bll.27.3a47de) from the RDBES, for the catch years below (one zip per year)
##
## A boot script: taf.boot() runs it through the entry RDBES_SCF of boot/DATA.bib (source = {script}), and the zip it
## downloads goes to boot/data/RDBES_SCF for data.R to read. taf.boot() empties that folder before it runs the script, so
## every taf.boot() downloads again; taf.boot(clean = FALSE) keeps the zip that is there and skips the download.
## The download asks for your ICES login (icesConnect) the first time in a session.

library(icesRDBES)

stock_code <- "bll.27.3a47de"
catch_years <- 2024:2025          # the RDBES data of the stock start in 2024

## taf.boot() runs the script in boot/data/RDBES_SCF; when it is run by hand from the project folder, the zip goes there too
dest_dir <- if (basename(getwd()) == "RDBES_SCF") getwd() else file.path("boot", "data", "RDBES_SCF")
dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
for (catch_year in catch_years) {
  message("  RDBES download of the SCF of ", stock_code, " for the catch year ", catch_year)
  my_payload <- list(
    dataType    = "SCF",
    format      = "CsvFilePerTable",
    hierarchies = list("HCI"),
    scfFilters  = list(
      ccStock = list(stock_code),
      ccYear  = list(as.character(catch_year))
    ))
  zipfile <- rdbes_download_data(payload = my_payload, dest_dir = dest_dir)
  message("  saved ", basename(zipfile), " in ", dest_dir)
}

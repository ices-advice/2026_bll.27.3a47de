# install.packages("icesTAF")
library(icesTAF)
library(dplyr)
library(readr)
update.packages(checkBuilt = TRUE, ask = FALSE)
# taf.skeleton()

# taf.bootstrap()

# #### ADDING CATCH DATA #######
# draft.data(
#   data.files = "catch.csv",
#   data.scripts = NULL,
#   originator = "ICES Official Landings/Intercatch",
#   year = 2026,
#   title = "Catch data for bll.27.3a47de",
#   period = "1950-2025",
#   file = TRUE
# )

# mkdir("boot/initial/data/Survey Indices")
# 
# draft.data(
#   data.files = "Survey Indices",
#   data.scripts = NULL,
#   originator = "DATRAS",
#   title = "Exploitable biomass indices (Combined surveys GAM) for bll.27.3a47de",
#   source = "folder",
#   period = "1983-2025",
#   file = TRUE,
#   append = TRUE # add to DATA.bib
# )
# 
# draft.data(
#   data.files = "retroindex.RData",
#   data.scripts = NULL,
#   originator = "",
#   year = 2026,
#   title = "Retrospective of the Index",
#   period = "1983-2025",
#   file = TRUE,
#   append = TRUE
# )
# 
# draft.data(
#   data.files = "retroindex.BSAS.RData",
#   data.scripts = NULL,
#   originator = "",
#   year = 2026,
#   title = "Retrospective of the Index with BSAS",
#   period = "1983-2025",
#   file = TRUE,
#   append = TRUE
# )

lines <- readLines("boot/initial/data/LandingOnly.txt")
pattern <- " Landings per year area country"
header_lines <- grep(pattern, lines)
data_start <- header_lines[1] + 2
data_end <- header_lines[2] - 2
data_lines <- lines[data_start:data_end]
temp_file <- tempfile()
writeLines(data_lines, temp_file)
df <- read.table(temp_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE)

write_csv(df, "boot/initial/data/LandingOnly.csv")

brill.3a47de.InterCatch_raised_discards <- read_csv("boot/initial/data/LandingOnly.csv") %>%
  filter(Year >= 2012)

saveRDS(brill.3a47de.InterCatch_raised_discards, file = "./boot/initial/data/brill.3a47de.InterCatch_raised_discards.Rds")

draft.data(
  data.files = "brill.3a47de.InterCatch_raised_discards.Rds",
  data.scripts = NULL,
  originator = "Intercatch, create this file from the LandingsOnly.xlsx file",
  year = 2026,
  title = "Intercatch Raised Discards",
  period = "2014-2025",
  file = TRUE,
  append = TRUE
)

draft.data(
  data.files = "ICES_1950-2010.csv",
  data.scripts = NULL,
  originator = "ICES",
  year = 2026,
  title = "Historical Official Landings",
  period = "1950-2010",
  file = TRUE,
  append = TRUE
)

draft.data(
  data.files = "ICESCatchDataset2006-2023.csv",
  data.scripts = NULL,
  originator = "ICES",
  year = 2026,
  title = "Official Landings",
  period = "2006-2023",
  file = TRUE,
  append = TRUE
)

draft.data(
  data.files = "Preliminary_landings_allSpecies_2024.csv",
  data.scripts = NULL,
  originator = "ICES",
  year = 2026,
  title = "Preliminary  Landings",
  period = "2024",
  file = TRUE,
  append = TRUE
)

draft.data(
  data.files = "Preliminary_landings_allSpecies_2025.csv",
  data.scripts = NULL,
  originator = "ICES",
  year = 2026,
  title = "Preliminary  Landings",
  period = "2025",
  file = TRUE,
  append = TRUE
)

draft.data(
  data.files = "StockOverview_2025.txt",
  data.scripts = NULL,
  originator = "ICES",
  year = 2026,
  title = "Stock Overview IC",
  period = "2025",
  file = TRUE,
  append = TRUE
)

draft.data(
  data.files = "NumbersAtAgeLength_2025.txt",
  data.scripts = NULL,
  originator = "ICES",
  year = 2026,
  title = "Numbers at length IC",
  period = "2025",
  file = TRUE,
  append = TRUE
)

draft.data(
  data.files = "CatchAndSampleDataTables_2025.txt",
  data.scripts = NULL,
  originator = "ICES",
  year = 2026,
  title = "CatchAndSampleDataTables IC",
  period = "2025",
  file = TRUE,
  append = TRUE
)

draft.data(
  data.files = "TAC.csv",
  data.scripts = NULL,
  originator = "ICES",
  year = 2026,
title = "Agreed TAC for BLL and TUR in 2a4",
  period = "2000-2025",
  file = TRUE,
  append = TRUE
)

draft.data(
  data.files = "TAC_BLL_2a3a47de.csv",
  data.scripts = NULL,
  originator = "ICES",
  year = 2026,
  title = "Agreed TAC for BLL in 2a3a47de",
  period = "2025",
  file = TRUE,
  append = TRUE
)

taf.bootstrap()
mkdir("boot/initial/report/figures")


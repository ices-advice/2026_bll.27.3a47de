## Output of the catch data: the figures and tables of the catch part of the WG report, of the advice sheet and of the
## presentation, each made once and named after where it is shown
##
## Before: data/ (data.R), boot/data/rdbes_SCF_*_<year>.csv.gz (the SCF allocation tables), boot/data/survey_map_cpue_*.png
## After:  output/report/        Figure_3.x.x_*.png and Table_3.x.x_*.csv of the WG report (section 3)
##         output/advice_sheet/  Table_7_*, Table_8*_*.csv of the advice sheet (also shown in the WG report)
##         output/presentation/  the figures only the presentation shows

library(icesTAF)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(patchwork)
library(RColorBrewer)
library(janitor)
library(data.table)

cp("boot/data/utilities.R", ".")      # the functions and stock settings, set up by taf.boot() from boot/DATA.bib
source("utilities.R")

options(dplyr.summarise.inform = FALSE)
theme_set(theme_minimal())

report_dir <- "output/report"
advice_dir <- "output/advice_sheet"
pres_dir   <- "output/presentation"
mkdir(c(report_dir, advice_dir, pres_dir))

## The tables and figures go to the WG report folder unless another one is given
formals(table_out)$dir <- report_dir
formals(figure_out)$dir <- report_dir

## The gears the report shows on their own; the others are "Others"
main_gears <- c("TBB", "OTB", "GTR", "GNS", "SSC", "SDN", "OTT")

olbll      <- read.taf("data/bll.27.3a47de.official.landings.csv")
olbll_conf <- read.taf("data/bll.27.3a47de.official.landings.confidencial.csv")
icd        <- read.taf("data/bll.27.3a47de.intercatch.raised.csv")
OL_BMS     <- read.taf("data/bll.27.3a47de.ol.bms.csv")
Uptake2a4  <- read.taf("data/bll.27.3a47de.uptake.TAC.2a4.csv")
Uptake2a3a47de <- read.taf("data/bll.27.3a47de.uptake.TAC.2a3a47de.csv")
lastyr <- max(icd$Year)

## The official landings as published: without the confidential preliminary landings. The years of the preliminary
## landings are marked "*", the years with confidential landings "**": their totals are not given, and the countries
## with confidential landings show "c".
prelim_years <- sort(unique(read.taf("data/bll.27.3a47de.official.landings.summary.confidential.csv")$Year))
confidential <- olbll %>% group_by(Year, Area, Country) %>% summarise(all = sum(Landings), .groups = "drop") %>%
  left_join(olbll_conf %>% group_by(Year, Area, Country) %>% summarise(open = sum(Landings), .groups = "drop"),
            by = c("Year", "Area", "Country")) %>%
  filter(is.na(open) | abs(all - open) > 1e-6)
conf_years <- sort(unique(confidential$Year))
year_label <- function(y) ifelse(y %in% conf_years, paste0(y, "**"), ifelse(y %in% prelim_years, paste0(y, "*"), as.character(y)))


###################################
msg("Output catch: TAC (report 3.1)")
###################################

## Table 3.1.1 and Figure 3.1.3.1: the combined TAC of brill and turbot in 2.a and 4, and its uptake
table_out(Uptake2a4 %>% filter(!is.na(`Agreed TAC 2a4`), Year <= 2023) %>% select(Year, TAC = `Agreed TAC 2a4`),
          "Table_3.1.1_TAC_brill_turbot_2a4")

p <- Uptake2a4 %>%
  select(!Uptake) %>%
  mutate(Brill = Brill / `Agreed TAC 2a4`, Turbot = Turbot / `Agreed TAC 2a4`, Total = Total / `Agreed TAC 2a4`,
         Agreed.TAC.2a4 = `Agreed TAC 2a4` / `Agreed TAC 2a4`) %>%
  filter(Year <= min(lastyr, 2023)) %>%
  pivot_longer(cols = c("Brill", "Turbot", "Total", "Agreed.TAC.2a4"), names_to = "Species", values_to = "Tonnes") %>%
  ggplot(aes(x = as.integer(Year), y = Tonnes, color = Species)) +
  geom_point() +
  geom_line() +
  scale_color_discrete(breaks = c("Agreed.TAC.2a4", "Brill", "Turbot", "Total")) +
  ylab("Proportion of TAC") + xlab("Year") + labs(color = NULL) +
  scale_x_continuous(breaks = seq(2000, lastyr, 2)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  theme(legend.position = "bottom", legend.key.size = unit(0.1, "in"))
figure_out(p, "Figure_3.1.3.1_TAC_uptake_brill_turbot_2a4")

## Table 3.1.2 and Figure 3.1.3.2: the TAC of brill (from 2024, sum over the TAC areas) and its uptake by area
tac_bll <- Uptake2a3a47de %>% filter(Year > 2023) %>% select(Year, Area, Landings, TAC)
table_out(tac_bll %>% group_by(Year) %>% summarise(TAC = round(sum(TAC, na.rm = TRUE))), "Table_3.1.2_TAC_brill_2a3a47de")

p <- tac_bll %>%
  mutate(Uptake = replace_na(Landings, 0), Remainder = pmax(TAC - Uptake, 0)) %>%
  pivot_longer(cols = c("Uptake", "Remainder"), names_to = "Part", values_to = "Value") %>%
  filter(!is.na(Value)) %>%
  ggplot() +
  geom_col(aes(x = Year, y = Value, fill = Area, alpha = Part), position = "stack") +
  geom_col(data = tac_bll %>% filter(!is.na(TAC)), aes(x = Year, y = TAC, group = Area),
           fill = NA, color = "black", linewidth = 0.5, position = position_stack()) +
  geom_text(data = tac_bll %>%
              group_by(Year) %>%
              summarise(TotalCatch = sum(Landings, na.rm = TRUE), TotalTAC = sum(TAC, na.rm = TRUE), .groups = "drop") %>%
              filter(TotalCatch > 0) %>%
              mutate(Label = paste0(round(100 * TotalCatch / TotalTAC, 1), "%"), ypos = TotalTAC + 100),
            aes(x = Year, y = ypos, label = Label), fontface = "bold", size = 4, color = "black") +
  scale_alpha_manual(values = c("Uptake" = 1, "Remainder" = 0.5)) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(breaks = seq(2024, max(tac_bll$Year), 1)) +
  labs(x = "Year", y = "Tonnes", fill = "Area", alpha = "Uptake")
figure_out(p, "Figure_3.1.3.2_TAC_uptake_brill_2a3a47de")


###################################
msg("Output catch: official landings (report 3.2.1, advice sheet table 8)")
###################################

by_area <- olbll_conf %>% group_by(Year, Area) %>% summarise(Landings = sum(Landings, na.rm = TRUE), .groups = "drop")
area_wide <- by_area %>%
  pivot_wider(names_from = Area, values_from = Landings, values_fill = 0) %>%
  mutate(TOTAL = ifelse(Year %in% conf_years, NA, `27.3.a` + `27.4` + `27.7.de`))

## Table 3.2.1: the official landings by area, 1950 to the last year
table_out(area_wide %>% transmute(Year = year_label(Year), `3.a` = round(`27.3.a`), `4` = round(`27.4`),
                                  `7.de` = round(`27.7.de`), TOTAL = round(TOTAL)),
          "Table_3.2.1_official_landings_area")

## Table 3.2.2: the official landings by area of the last 15 years, absolute and as proportion
table_out(area_wide %>%
            filter(Year > lastyr - 16) %>%
            transmute(Year = year_label(Year), `3a` = round(`27.3.a`), `4` = round(`27.4`), `7de` = round(`27.7.de`),
                      TOTAL = round(TOTAL),
                      `3a %` = round(100 * `27.3.a` / TOTAL, 1), `4 %` = round(100 * `27.4` / TOTAL, 1),
                      `7de %` = round(100 * `27.7.de` / TOTAL, 1)),
          "Table_3.2.2_official_landings_area_last_15_years")

## Figures 3.2.1-3.2.4: the official landings by area and by country, absolute and as proportion (without the
## confidential landings)
landings_plot <- function(fill, position = "stack", ylab = "Landings (tonnes)") {
  ggplot(olbll_conf) +
    geom_bar(aes(Year, weight = Landings, fill = .data[[fill]]), position = position, color = "black") +
    ylab(ylab) +
    scale_x_continuous(breaks = seq(1950, lastyr, 5)) +
    scale_fill_manual(values = brewer.pal(max(3, length(unique(olbll_conf[[fill]]))), "Paired")) +
    theme(legend.position = "bottom", legend.key.size = unit(0.1, "in"))
}
figure_out(landings_plot("Area"), "Figure_3.2.1_official_landings_area")
figure_out(landings_plot("Area", "fill", "Proportion of the landings"), "Figure_3.2.2_official_landings_area_proportion")
figure_out(landings_plot("Country") + guides(fill = guide_legend(nrow = 1)), "Figure_3.2.3_official_landings_country")
figure_out(landings_plot("Country", "fill", "Proportion of the landings"), "Figure_3.2.4_official_landings_country_proportion")

## Advice sheet tables 8a-c (and the supplementary tables of the WG report): the official landings by country of each
## area, with the BMS landings of the preliminary landings; "c" for confidential, no total in a confidential year
countries_of <- c("27.4" = "Belgium|BE,Germany|DE,Denmark|DK,France|FR,UK|UK,Netherlands|NL,Norway|NO,Sweden|SE",
                  "27.3.a" = "Belgium|BE,Germany|DE,Denmark|DK,Netherlands|NL,Norway|NO,Sweden|SE",
                  "27.7.de" = "Belgium|BE,Germany|DE,Denmark|DK,France|FR,UK|UK,Ireland|IE,Netherlands|NL")
bms_area <- OL_BMS %>% mutate(Area = group_areas(Area)) %>% group_by(Year, Area) %>% summarise(BMS = sum(BMS), .groups = "drop")
landings_by_country <- function(area) {
  cc <- do.call(rbind, strsplit(strsplit(countries_of[[area]], ",")[[1]], "|", fixed = TRUE))
  open <- olbll_conf %>% filter(Area == area) %>% group_by(Year, Country) %>% summarise(L = sum(Landings), .groups = "drop")
  conf <- confidential %>% filter(Area == area) %>% distinct(Year, Country) %>% mutate(c = TRUE)
  tab <- expand_grid(Year = sort(unique(olbll$Year)), Country = cc[, 2]) %>%
    left_join(open, by = c("Year", "Country")) %>%
    left_join(conf, by = c("Year", "Country")) %>%
    mutate(value = ifelse(!is.na(c), "c", as.character(round(replace_na(L, 0)))))
  wide <- tab %>% select(Year, Country, value) %>%
    pivot_wider(names_from = Country, values_from = value) %>%
    left_join(bms_area %>% filter(Area == area) %>% transmute(Year, `BMS landings` = round(BMS, 1)), by = "Year") %>%
    left_join(open %>% group_by(Year) %>% summarise(Total = sum(L)), by = "Year") %>%
    mutate(Total = ifelse(Year %in% conf_years, "NC", as.character(round(Total))), Year = year_label(Year))
  names(wide) <- c("Year", cc[, 1], "BMS landings", "Total")
  wide
}
table_out(landings_by_country("27.4"), "Table_8a_official_landings_country_27.4", advice_dir)
table_out(landings_by_country("27.3.a"), "Table_8b_official_landings_country_27.3.a", advice_dir)
table_out(landings_by_country("27.7.de"), "Table_8c_official_landings_country_27.7.de", advice_dir)


###################################
msg("Output catch: InterCatch (report 3.2.1)")
###################################

icd <- icd %>% mutate(AreaGroup = group_areas(Area), Gear = str_sub(Fleet, 1, 3))
ic_by <- function(category, by) {
  icd %>% filter(CatchCategory == category) %>%
    group_by(across(all_of(c("Year", by)))) %>%
    summarise(t = sum(Caton) / 1000, .groups = "drop")
}

## Figures 3.2.5-3.2.6: the InterCatch landings by area and by country
ic_plot <- function(fill, dat) {
  ggplot(dat) +
    geom_bar(aes(Year, fill = .data[[fill]], weight = t), position = position_stack(), color = "black") +
    scale_fill_manual(values = brewer.pal(max(3, length(unique(dat[[fill]]))), "Paired")) +
    xlab("Year") + ylab("Landings (tonnes)") +
    scale_x_continuous(breaks = seq(2014, lastyr, 1)) +
    theme(legend.position = "bottom", legend.key.size = unit(0.1, "in"))
}
figure_out(ic_plot("AreaGroup", ic_by("Landings", "AreaGroup")) + labs(fill = "Area"), "Figure_3.2.5_intercatch_landings_area")
figure_out(ic_plot("Country", ic_by("Landings", "Country") %>% mutate(Country = recode_country(Country))) +
             guides(fill = guide_legend(nrow = 1)), "Figure_3.2.6_intercatch_landings_country")

## Figure 3.2.7: InterCatch against the official landings (without the confidential ones) by country, last year
comp <- bind_rows(
  icd %>% filter(Year == lastyr, CatchCategory %in% c("Landings", "BMS landing")) %>%
    mutate(Country = recode_country(Country)) %>% group_by(Country) %>%
    summarise(Landings = sum(Caton) / 1000) %>% mutate(Source = "Intercatch (IC)"),
  olbll_conf %>% filter(Year == lastyr) %>% group_by(Country) %>%
    summarise(Landings = sum(Landings)) %>% mutate(Source = "Official landings (OL)"))
p <- ggplot(comp) +
  geom_bar(aes(Country, weight = Landings, fill = Source), color = "black", position = "dodge") +
  scale_fill_manual("", values = brewer.pal(3, "Paired")[1:2]) +
  xlab("") + ylab("Landings (tonnes)") +
  theme(legend.position = "bottom")
figure_out(p, "Figure_3.2.7_intercatch_vs_official_landings")

## Tables 3.2.3-3.2.4: the InterCatch landings and discards by area
ic_area_table <- function(category) {
  ic_by(category, "AreaGroup") %>%
    pivot_wider(names_from = AreaGroup, values_from = t, values_fill = 0) %>%
    transmute(Year, `3.a` = round(`27.3.a`), `4` = round(`27.4`), `7.de` = round(`27.7.de`),
              TOTAL = round(`27.3.a` + `27.4` + `27.7.de`))
}
table_out(ic_area_table("Landings"), "Table_3.2.3_intercatch_landings_area")
table_out(ic_area_table("Discards"), "Table_3.2.4_intercatch_discards_area")

## Table 3.2.5: the InterCatch catch of the last year by country, area and catch category
cat_short <- c("Landings" = "LAN", "Discards" = "DIS", "BMS landing" = "BMS")
by_cat <- icd %>%
  filter(Year == lastyr, CatchCategory %in% names(cat_short)) %>%
  mutate(Cat = cat_short[CatchCategory]) %>%
  group_by(Country, AreaGroup, Cat) %>%
  summarise(t = sum(Caton) / 1000, .groups = "drop") %>%
  complete(Country, AreaGroup = c("27.3.a", "27.4", "27.7.de"), Cat = c("LAN", "DIS", "BMS"), fill = list(t = 0))
tab325 <- bind_rows(by_cat, by_cat %>% group_by(Country, Cat) %>% summarise(t = sum(t), .groups = "drop") %>% mutate(AreaGroup = "Total")) %>%
  mutate(col = paste(AreaGroup, Cat)) %>%
  select(Country, col, t) %>%
  pivot_wider(names_from = col, values_from = t) %>%
  mutate(`Total ALL` = `Total LAN` + `Total DIS` + `Total BMS`) %>%
  select(Country, all_of(paste(rep(c("27.3.a", "27.4", "27.7.de", "Total"), each = 3), c("LAN", "DIS", "BMS"))), `Total ALL`) %>%
  adorn_totals("row") %>%
  mutate(across(-Country, round))
table_out(tab325, "Table_3.2.5_intercatch_catch_country_area")

## Table 3.2.6: the InterCatch landings of the last year by gear and area
table_out(icd %>%
            filter(Year == lastyr, CatchCategory == "Landings") %>%
            mutate(Gear = ifelse(Gear %in% main_gears, Gear, "Others")) %>%
            group_by(Gear, AreaGroup) %>% summarise(t = sum(Caton) / 1000, .groups = "drop") %>%
            pivot_wider(names_from = AreaGroup, values_from = t, values_fill = 0) %>%
            transmute(`Gear type` = Gear, `3.a` = `27.3.a`, `4` = `27.4`, `7.d-e` = `27.7.de`) %>%
            mutate(Total = `3.a` + `4` + `7.d-e`) %>%
            arrange(`Gear type` == "Others", desc(Total)) %>%
            adorn_totals("row", name = "Total") %>%
            mutate(across(-`Gear type`, round)),
          "Table_3.2.6_intercatch_landings_gear_area")


###################################
msg("Output catch: RDBES and the three sources compared (report 3.2.1)")
###################################

## The catch of the years the RDBES has (from the SCF Catch Allocation Results, by area and country, kg)
rdbes <- read.taf("data/bll.27.3a47de.rdbes.catches.csv") %>%
  pivot_longer(c(Landings, Discards), names_to = "Catch category", values_to = "kg") %>%
  mutate(t = kg / 1000)
rdbes_years <- sort(unique(rdbes$Year))

## Figure 3.2.15: the RDBES landings and discards by area and by country
rdbes_plot <- function(fill) {
  ggplot(rdbes) +
    geom_bar(aes(factor(Year), weight = t, fill = .data[[fill]]), position = position_stack(), color = "black") +
    facet_wrap(~`Catch category`, scales = "free_y") +
    scale_fill_manual(values = brewer.pal(max(3, length(unique(rdbes[[fill]]))), "Paired")) +
    labs(x = "Year", y = "Tonnes") +
    theme(legend.position = "bottom", legend.key.size = unit(0.1, "in"))
}
figure_out(rdbes_plot("Area") / (rdbes_plot("Country") + guides(fill = guide_legend(nrow = 1))),
           "Figure_3.2.15_rdbes_catch_area_country", width = 2800, height = 2800)

## Table 3.2.14 and Figure 3.2.16: the landings of the years of the RDBES by country from the three sources: the official
## landings (without the confidential ones), InterCatch and the RDBES (SCF). BMS landings are not in the landings.
three <- bind_rows(
  olbll_conf %>% filter(Year %in% rdbes_years) %>% group_by(Year, Country) %>%
    summarise(t = sum(Landings), .groups = "drop") %>% mutate(Source = "Official landings"),
  icd %>% filter(Year %in% rdbes_years, CatchCategory == "Landings") %>% mutate(Country = recode_country(Country)) %>%
    group_by(Year, Country) %>% summarise(t = sum(Caton) / 1000, .groups = "drop") %>% mutate(Source = "InterCatch"),
  rdbes %>% filter(`Catch category` == "Landings") %>% group_by(Year, Country) %>%
    summarise(t = sum(t), .groups = "drop") %>% mutate(Source = "RDBES")) %>%
  mutate(Country = as.character(Country), Source = factor(Source, levels = c("Official landings", "InterCatch", "RDBES")))
table_out(three %>%
            mutate(t = round(t, 1)) %>%
            pivot_wider(names_from = Source, values_from = t) %>%
            arrange(Year, Country) %>%
            group_by(Year) %>%
            group_modify(~ adorn_totals(.x, "row")) %>%
            ungroup() %>%
            mutate(`RDBES - InterCatch` = round(RDBES - InterCatch, 1)),
          "Table_3.2.14_landings_official_intercatch_rdbes")
p <- ggplot(three) +
  geom_bar(aes(Country, weight = t, fill = Source), color = "black", position = "dodge") +
  facet_wrap(~Year, ncol = 1) +
  scale_fill_brewer("", palette = "Paired") +
  labs(x = "", y = "Landings (tonnes)") +
  theme(legend.position = "bottom")
figure_out(p, "Figure_3.2.16_landings_official_intercatch_rdbes", width = 2800, height = 2400)


###################################
msg("Output catch: discards and BMS (report 3.2.2-3.2.3)")
###################################

## Table 3.2.7: the catch of the last year imported and raised, by catch category
imported <- icd %>%
  filter(Year == lastyr) %>%
  group_by(`Catch category` = CatchCategory, Survey = CATONRaisedOrImported) %>%
  summarise(`CATON (kg)` = sum(Caton), .groups = "drop") %>%
  group_by(`Catch category`) %>%
  mutate(Percentage = ifelse(sum(`CATON (kg)`) > 0, round(100 * `CATON (kg)` / sum(`CATON (kg)`)), NA)) %>%
  ungroup() %>%
  mutate(Survey = recode(Survey, "Imported_Data" = "Imported data", "Raised_Discards" = "Raised discards"),
         `CATON (kg)` = round(`CATON (kg)`))
table_out(bind_rows(imported, tibble(`Catch category` = "TOTAL", `CATON (kg)` = sum(imported$`CATON (kg)`), Percentage = 100)),
          "Table_3.2.7_intercatch_imported_raised")

## Figures 3.2.8-3.2.9: the share of the discards imported and raised by year, by area and by country; and for the stock
## (presentation)
imported_plot <- function(by = NULL, label_offset = 0.03) {
  icd %>%
    filter(CatchCategory == "Discards") %>%
    mutate(Area = AreaGroup, Country = recode_country(Country)) %>%
    group_by(across(all_of(c("Year", by, "CATONRaisedOrImported")))) %>%
    summarise(Discards = sum(Caton), .groups = "drop_last") %>%
    mutate(tot = sum(Discards), frac = Discards / tot) %>%
    ggplot() +
    geom_bar(position = "fill", color = "black", alpha = 0.55,
             aes(Year, weight = Discards, fill = factor(CATONRaisedOrImported, levels = c("Raised_Discards", "Imported_Data")))) +
    scale_fill_manual("", values = brewer.pal(3, "Paired")[1:2]) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + ylab("Imported discards (%)") +
    scale_x_continuous(breaks = seq(2014, lastyr, 1)) +
    {if (!is.null(by)) facet_grid(as.formula(paste(by, "~ .")))} +
    geom_text(aes(Year, Discards / tot + label_offset,
                  label = ifelse(Discards != 0 & CATONRaisedOrImported == "Imported_Data", paste0(round(frac * 100), "%"), "")),
              size = 2.9) +
    theme(legend.position = "bottom")
}
figure_out(imported_plot("Area"), "Figure_3.2.8_discards_imported_raised_area", width = 1800, height = 2800)
figure_out(imported_plot("Country", 0.05), "Figure_3.2.9_discards_imported_raised_country", width = 1800, height = 2800)
figure_out(imported_plot(), "discards_imported_raised", pres_dir)

## Figure 3.2.10: the InterCatch landings (a) and discards (b) by gear
gear_plot <- function(category, ylab) {
  icd %>%
    filter(CatchCategory == category, Fleet != "All") %>%
    mutate(gearcat = factor(Gear, levels = levels_gear, labels = labels_gear_report)) %>%
    group_by(Year, gearcat) %>%
    summarise(t = sum(Caton) / 1000, .groups = "drop") %>%
    mutate(gearcat = factor(gearcat, levels = rev(c("OTB", "TBB", "GTR", "Other")), ordered = TRUE)) %>%
    ggplot() +
    geom_bar(aes(Year, weight = t, fill = gearcat), position = "stack", color = "black") +
    scale_fill_brewer("Gear type", palette = "Paired", direction = -1, drop = FALSE) +
    scale_x_continuous(breaks = seq(2014, lastyr, 1)) +
    ylab(ylab) +
    theme(legend.key.size = unit(0.1, "in"))
}
figure_out((gear_plot("Landings", "Landings (tonnes)") + ggtitle("(a)")) /
             (gear_plot("Discards", "Discards (tonnes)") + ggtitle("(b)")) + plot_layout(guides = "collect"),
           "Figure_3.2.10_intercatch_landings_discards_gear", width = 2200, height = 2200)

## Tables 3.2.8-3.2.10: the discard rates (discards over landings + discards, %) of the last three years by country and by
## area, and of all years
rate <- function(dat, by) {
  dat %>%
    filter(CatchCategory %in% c("Landings", "Discards")) %>%
    group_by(across(all_of(c("Year", by)))) %>%
    summarise(Discards = sum(Caton[CatchCategory == "Discards"]) / 1000,
              rate = round(100 * Discards / (sum(Caton) / 1000), 2), .groups = "drop")
}
last3 <- icd %>% filter(Year > lastyr - 3)
rate_wide <- function(by, label) {
  bind_rows(rate(last3, by), rate(last3, NULL) %>% mutate("{by}" := "Overall")) %>%
    select(-Discards) %>%
    filter(!is.na(rate)) %>%
    pivot_wider(names_from = Year, values_from = rate) %>%
    rename("{label}" := all_of(by))
}
table_out(rate_wide("Country", "Country"), "Table_3.2.8_discard_rate_country")
table_out(rate_wide("AreaGroup", "Subarea/Division"), "Table_3.2.9_discard_rate_area")
table_out(rate(icd, NULL) %>% transmute(Year, Discards = round(Discards), `Discard rate (%)` = rate), "Table_3.2.10_discard_rate")

## Table 3.2.11: the BMS landings (kg), official and in InterCatch, from 2018
table_out(full_join(OL_BMS %>% group_by(Year) %>% summarise(`BMS official landings` = round(sum(BMS) * 1000)),
                    icd %>% filter(CatchCategory == "BMS landing") %>% group_by(Year) %>%
                      summarise(`BMS InterCatch landings` = round(sum(Caton))), by = "Year") %>%
            filter(Year >= 2018) %>% arrange(Year),
          "Table_3.2.11_BMS_landings")

## Figure 3.2.11: the CPUE of the surveys (the maps of 2026_bll.27.3a47de_survey, a file per period)
for (f in list.files("boot/data", "^survey_map_cpue_.*[.]png$", full.names = TRUE))
  file.copy(f, file.path(report_dir, paste0("Figure_3.2.11_survey_CPUE_", sub("^survey_map_cpue_", "", basename(f)))),
            overwrite = TRUE)   # the name the report looks for


###################################
msg("Output catch: advice sheet table 7")
###################################

## Table 7: the catch of the last year: catch, landings and discards (incl. BMS) in tonnes, and the share of each gear group
## in the landings
fleet <- icd %>%
  filter(Year == lastyr, CatchCategory == "Landings") %>%
  mutate(Fleet = as.character(factor(Gear, levels = levels_gear, labels = labels_gear)),
         Fleet = case_when(Fleet == "Beam" ~ "Beam trawls", Fleet == "Otter and Seine" ~ "Otter trawls",
                           Fleet == "Trammel/gillnets" ~ "Trammel/gillnets", TRUE ~ "Other gears")) %>%
  group_by(Fleet) %>% summarise(t = sum(Caton) / 1000, .groups = "drop") %>%
  transmute(Fleet, `Share of the landings (%)` = round(100 * t / sum(t), 1)) %>%
  arrange(Fleet == "Other gears", desc(`Share of the landings (%)`))
table_out(bind_rows(
  tibble(Fleet = c("Catch", "Landings", "Discards (incl. BMS)"),
         Tonnes = round(c(sum(icd$Caton[icd$Year == lastyr & icd$CatchCategory %in% c("Landings", "Discards", "BMS landing")]),
                          sum(icd$Caton[icd$Year == lastyr & icd$CatchCategory == "Landings"]),
                          sum(icd$Caton[icd$Year == lastyr & icd$CatchCategory %in% c("Discards", "BMS landing")])) / 1000)),
  fleet),
  "Table_7_catch_by_fleet", advice_dir)

###################################
msg("Output catch: presentation")
###################################

## The discard sampling of the years of the RDBES, from the SCF Catch Allocation Results (rcef::plot_coverage_bar): the
## landings of a stratum count as sampled when the CAR has imported discards for it
cov_catch <- scf_coverage_catch(read_scf_allocation("boot/data", "CAR")$CAR)
cov_plot <- function(...) rcef::plot_coverage_bar(cov_catch, facet = "year", ...) + theme(legend.position = "bottom")
figure_out(cov_plot(y = "fleetValue", fill = "vesselFlagCountry") + theme(axis.text.y = element_text(size = 5)),
           "rdbes_discard_sampling_fleet_country", pres_dir, width = 3600, height = 5000)
figure_out(cov_plot(y = "ICESDivision"), "rdbes_discard_sampling_area", pres_dir, width = 3200, height = 1600)

## The length sampling of the landings and of the discards of the years with a DAR (the SCF Distribution Allocation
## Results): a stratum is sampled when its length distribution was imported, not allocated from other strata
len_cov <- scf_length_coverage(read_scf_allocation("boot/data", "DAR")$DAR)
categories <- c(Lan = "landings", Dis = "discards")
for (cc in names(categories)) {
  figure_out(rcef::plot_coverage_bar(len_cov, y = "fleetValue", fill = "vesselFlagCountry", facet = "year", catchCategory = cc) +
               theme(legend.position = "bottom", axis.text.y = element_text(size = 5)),
             paste0("rdbes_length_sampling_", categories[[cc]], "_fleet_country"), pres_dir, width = 3000, height = 4000)
}

## The share of the landings of the last year with discard sampling (quoted in the WG report, section 3.2.2)
coverage_ly <- rcef::build_coverage_strata(cov_catch %>% filter(year == lastyr), dims = "year")
table_out(data.frame(perc_landings_with_discards = round(coverage_ly$coverage)), "bll.27.3a47de.catch.settings", "output/assessment")

## The discard rate of the last year by area and country
table_out(icd %>%
            filter(Year == lastyr, CatchCategory %in% names(cat_short)) %>%
            mutate(Country = recode_country(Country)) %>%
            group_by(Area = AreaGroup, Country) %>%
            summarise(`DR (%)` = round(100 * sum(Caton[CatchCategory == "Discards"]) / sum(Caton), 2), .groups = "drop"),
          "discard_rate_area_country", pres_dir)

## Output of the assessment: the figures and tables of the survey indices, the SPiCT assessment, the short-term forecast
## and the advice, for the WG report, the advice sheet and the presentation, each made once

## Before: model/bll.27.3a47de.fit*.Rds (model.R), data/ (data.R), boot/data/TAC_BLL_2a3a47de.csv,
##         boot/data/WGNSSK<year>_bll.27.3a47de.*.csv (the assessments of the two previous years), advice_history.csv
##         (the history of the advice) and boot/data/survey_index_retro.rds
## After:  output/report/        Figure_3.x.x_*.png and Table_3.x.x_*.csv of the WG report (section 3)
##         output/advice_sheet/  Table_1_*, Table_2_*, Table_6_*, Table_9_*.csv of the advice sheet (also in the WG report)
##         output/presentation/  the SPiCT figures only the presentation shows
##         output/assessment/    the unrounded results that output_ASD_SAG_MIXFISH.R (SAG) and next year's assessment (copied to
##                               boot/initial/data/WGNSSK<year>_*.csv) read: assessment summary, intermediate
##                               year, catch scenarios, advice; and the values the reports quote (settings)

library(icesTAF)
library(spict)
library(icesAdvice)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(RColorBrewer)

cp("boot/data/utilities.R", ".")      # the functions and stock settings, set up by taf.boot() from boot/DATA.bib
source("utilities.R")

options(dplyr.summarise.inform = FALSE)

report_dir <- "output/report"
advice_dir <- "output/advice_sheet"
pres_dir   <- "output/presentation"
assess_dir <- "output/assessment"
mkdir(c(report_dir, advice_dir, pres_dir, assess_dir))

formals(table_out)$dir <- report_dir
formals(figure_out)$dir <- report_dir
formals(save_base_plot)$dir <- report_dir
assess_out <- function(x, name) table_out(x, paste0("bll.27.3a47de.", name), assess_dir)

fit             <- readRDS("model/bll.27.3a47de.fit.Rds")
fitCorrectRetro <- readRDS("model/bll.27.3a47de.fit.correctRetro.Rds")
fitManagement   <- readRDS("model/bll.27.3a47de.fit.management.Rds")
bllcatch        <- read.taf("data/bll.27.3a47de.catches.csv")

last_yr <- max(fit$inp$timeI[[1]], fit$inp$timeC[[1]])     # the last data year; the assessment is of last_yr + 1

## The advice of the two previous assessments (the advice for last_yr + 1 and for last_yr) and the TAC of the
## intermediate year (the sum over the TAC areas)
previous_advice <- function(year) read.csv(sprintf("boot/data/WGNSSK%d_bll.27.3a47de.new.advice.csv", year))$NewAdviceCatch
lastAdvice     <- previous_advice(last_yr)        # given by WGNSSK last_yr for last_yr + 1
previousAdvice <- previous_advice(last_yr - 1)    # given by WGNSSK last_yr - 1 for last_yr
lastTAC <- with(read.csv("boot/data/TAC_BLL_2a3a47de.csv"), sum(TAC[Year == last_yr + 1], na.rm = TRUE))


###################################
msg("Output assessment: survey indices (report 3.2.5)")
###################################

idx_hist <- read.taf("data/bll.27.3a47de.survey.index.1983.1998.csv")
idx_sem1 <- read.taf("data/bll.27.3a47de.survey.index.sem1.csv")
idx_sem2 <- read.taf("data/bll.27.3a47de.survey.index.sem2.csv")

## Tables 3.2.12-3.2.13: the indices and their CV standardised to the mean (as SPiCT uses them)
table_out(idx_hist %>% transmute(Year = timeI, `Biomass index` = round(ObsI, 1), CV = round(CV / mean(CV), 2)),
          "Table_3.2.12_survey_index_1983_1998")
table_out(full_join(idx_sem1 %>% transmute(Year = timeI, `Semester 1 index` = round(ObsI, 3), `Semester 1 CV` = round(CV / mean(CV, na.rm = TRUE), 3)),
                    idx_sem2 %>% transmute(Year = timeI, `Semester 2 index` = round(ObsI, 3), `Semester 2 CV` = round(CV / mean(CV, na.rm = TRUE), 3)),
                    by = "Year"),
          sprintf("Table_3.2.13_survey_index_1999_%d", last_yr))

## Figure 3.2.12: the three indices, each standardised to its mean, with their 95% confidence interval
std <- function(x, name) {
  m <- mean(x$ObsI, na.rm = TRUE)
  transmute(x, Year = timeI, Index = name, value = ObsI / m, low = pmin(lower, upper) / m, high = pmax(lower, upper) / m)
}
indices <- bind_rows(std(idx_hist, "Quarter 1, 1983-1998"), std(idx_sem1, "Semester 1"), std(idx_sem2, "Semester 2"))
p <- ggplot(indices, aes(Year, value, colour = Index, fill = Index)) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 1) +
  scale_colour_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2") +
  labs(x = "Year", y = "Standardised exploitable biomass index", colour = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")
figure_out(p, "Figure_3.2.12_survey_indices")

## Figures 3.2.13-3.2.14: the retrospective of the indices of each semester (recalculated without the last 1-5 years)
retro_indices <- readRDS("boot/data/survey_index_retro.rds")   # the full index, then the 5 peels
mohn_index <- sapply(1:2, function(s) icesAdvice::mohn(extract_retro_semester(retro_indices, semester = s), peels = 5))
index_retro_plot <- function(index, semester) {
  peels <- lapply(retro_indices[-1], function(x) x %>% filter(semester == !!semester))
  palette <- c("black", brewer.pal(n = length(peels), name = "Paired"))
  ggplot() +
    geom_polygon(data = data.frame(x = c(index$timeI, rev(index$timeI)), y = c(log(index$upper), rev(log(index$lower)))),
                 aes(x = x, y = y), fill = "lightgrey", color = NA, alpha = 0.5) +
    geom_line(data = index, aes(x = timeI, y = log(ObsI), color = "base"), linewidth = 1) +
    geom_line(data = index, aes(x = timeI, y = log(upper), color = "base"), linewidth = 0.5, linetype = 2) +
    geom_line(data = index, aes(x = timeI, y = log(lower), color = "base"), linewidth = 0.5, linetype = 2) +
    lapply(seq_along(peels[1:5]), function(i)
      geom_line(data = data.frame(peels[[i]]), aes(x = timeI, y = log(ObsI), color = paste0("peel -", i - 1)), linewidth = 1)) +
    lapply(seq_along(peels[1:5]), function(i)
      geom_point(data = data.frame(peels[[i]][nrow(peels[[i]]), ]), aes(x = timeI, y = log(ObsI), color = paste0("peel -", i - 1)), size = 4)) +
    scale_color_manual(name = "Retros", values = palette, labels = c("base", paste0("peel -", seq_along(peels)))) +
    scale_x_continuous(breaks = unique(index$timeI), labels = unique(index$timeI)) +
    labs(y = "Exploitable biomass index (log)", x = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    guides(colour = guide_legend(nrow = 1))
}
figure_out(index_retro_plot(idx_sem1, 1), "Figure_3.2.13_survey_index_retro_semester_1", width = 15, height = 9, units = "in")
figure_out(index_retro_plot(idx_sem2, 2), "Figure_3.2.14_survey_index_retro_semester_2", width = 15, height = 9, units = "in")


###################################
msg("Output assessment: SPiCT (report 3.3.1)")
###################################

if (fit$opt$convergence != 0) warning("The SPiCT fit has not converged", call. = FALSE)
save_base_plot({
  par(mfrow = c(2, 2), mar = c(4, 4, 1, 1))
  plotspict.catch(fit, qlegend = FALSE)
  plotspict.bbmsy(fit, qlegend = FALSE, lineat = 0.5)
  plotspict.ffmsy(fit, qlegend = FALSE)
  plotspict.fb(fit)
}, "Figure_3.3.1_spict_results")
save_base_plot({
  par(mfrow = c(3, 2), mar = c(4, 4, 0.5, 0.5))
  plotspict.priors(fit, automfrow = FALSE)
}, "Figure_3.3.2_spict_priors_posteriors")
save_base_plot(plotspict.diagnostic(fit), "Figure_3.3.3_spict_osa_residuals")
save_base_plot(plotspict.diagnostic.process(fit), "Figure_3.3.4_spict_process_residuals")
save_base_plot(plotspict.retro(fitCorrectRetro), "Figure_3.3.5_spict_retro_recalculated_index")
save_base_plot(plotspict.hindcast(fit, legend.pos = "bottomleft"), "Figure_3.3.6_spict_hindcast")

## The presentation shows more of the fit
save_base_plot(plotspict.production(fit, main = ""), "spict_production_curve", pres_dir)
save_base_plot(plotspict.retro(fit), "spict_retro_analytical", pres_dir)
save_base_plot(plotspict.retro2(fitCorrectRetro, xlim = c(2014, last_yr + 1)), "spict_retro_recalculated_index_zoom", pres_dir)
save_base_plot(plotspict.retro.fixed(fitCorrectRetro), "spict_retro_recalculated_index_parameters", pres_dir)

## Table 3.3.3: the sensitivity to the initial values (check.ini)
resmat <- as.data.frame(fit$check.ini$resmat)
names(resmat) <- make.unique(colnames(fit$check.ini$resmat), sep = "")    # the q and sdi of each index: q, q1, q2
table_out(data.frame(Run = rownames(fit$check.ini$resmat), round(resmat, 2), check.names = FALSE),
          "Table_3.3.3_spict_sensitivity_initial_values")


###################################
msg("Output assessment: assessment summary (report table 3.3.2, advice sheet table 9)")
###################################

bf <- plotStockStatus(fit, when = last_yr + 1, plot = FALSE)
time <- as.numeric(rownames(bf$BBmsy))
res <- bind_cols(
  Year = annual(time, time, first)$anntime,
  exp(setNames(as.data.frame(sapply(1:3, function(x) annual(time, bf$BBmsy[, x], first)$annvec)), c("B/Bmsy_lower", "B/Bmsy", "B/Bmsy_upper"))),
  exp(setNames(as.data.frame(sapply(1:3, function(x) annual(time, bf$FFmsy[, x], mean)$annvec)), c("F/Fmsy_lower", "F/Fmsy", "F/Fmsy_upper"))))

wholefit <- bind_cols(
  data.frame(Year = seq(1950, last_yr + 2)),
  as.data.frame(relist(as.matrix(res[-1]), as.matrix(res[-1]))),
  rbind(bllcatch %>% transmute(Year, Discards, Landings, Total),
        data.frame(Year = seq(last_yr + 1, last_yr + 2), Discards = NA, Landings = NA, Total = NA))[, c(4, 3, 2)]) %>%
  rename(Catches = Total)

## Up to the assessment year, without F in the assessment year: for SAG and next year
assessmentsummary <- wholefit %>%
  filter(Year <= last_yr + 1) %>%
  mutate(across(c(`F/Fmsy`, `F/Fmsy_lower`, `F/Fmsy_upper`), ~replace(., row_number() == nrow(.), NA_real_)))
assess_out(assessmentsummary, "assessment.summary")

table_out(assessmentsummary %>%
            transmute(Year, `Low B/BMSY` = icesRound(`B/Bmsy_lower`), `B/BMSY` = icesRound(`B/Bmsy`), `High B/BMSY` = icesRound(`B/Bmsy_upper`),
                      `Landings (t)` = round(Landings), `Discards (t)` = round(Discards),
                      `Low F/FMSY` = icesRound(`F/Fmsy_lower`), `F/FMSY` = icesRound(`F/Fmsy`), `High F/FMSY` = icesRound(`F/Fmsy_upper`)),
          "Table_9_assessment_summary", advice_dir)


###################################
msg("Output assessment: short-term forecast and advice (report 3.3.2-3.4, advice sheet tables 1, 2 and 6)")
###################################

find <- which(fitManagement$inp$time == last_yr + 1)
bind <- which(fitManagement$inp$time == last_yr + 2)
lind <- which(fitManagement$inp$timeCpred == last_yr + 1)

b_for <- exp(bf$BBmsy[bind, 2])
f_for <- mean(exp(bf$FFmsy[find:bind, 2]))

## SPiCT is fitted on landings: the projected landings, and the catch and the discards from the mean discard rate of the
## last three years
lan_for <- get.par("logCpred", fitManagement, exp = TRUE, CI = 0.95)[lind, 2]
meandis <- with(assessmentsummary, mean((Discards / Catches * 100)[Year %in% seq(last_yr - 2, last_yr)]))
c_for <- round(lan_for / (100 - meandis) * 100, 1)
dis_for <- round(c_for * meandis / 100, 1)
lan_for <- round(lan_for, 1)
assess_out(data.frame(b_for, f_for, c_for, lan_for, dis_for, meandis), "intermediate.year")

## Table 1 (report table 3.3.4): the intermediate year
table_out(data.frame(
  Variable = c(sprintf("F%d/FMSY", last_yr + 1), sprintf("B%d/BMSY", last_yr + 2), sprintf("Projected landings (%d)", last_yr + 1),
               sprintf("Discard rate (%d, %d)", last_yr + 1, last_yr + 2), sprintf("Projected discards (%d)", last_yr + 1),
               sprintf("Catch (%d)", last_yr + 1)),
  Value = c(icesRound(f_for), icesRound(b_for), round(lan_for), round(meandis, 1), round(dis_for), round(c_for)),
  Notes = c(sprintf("Status quo F/FMSY: F%d/FMSY", last_yr), "Short-term forecast",
            "Landings under status quo F/FMSY; tonnes",
            sprintf("Average %d-%d; percentage by weight of the total catch", last_yr - 2, last_yr),
            sprintf("Based on the average discard rate (%d-%d); tonnes", last_yr - 2, last_yr),
            sprintf("Based on the average discard rate (%d-%d); tonnes", last_yr - 2, last_yr))),
  "Table_1_interim_year", advice_dir)

## The catch scenarios (one per management scenario of model.R) and the advice: the first scenario
catchscenariotable <- do.call(rbind, lapply(fitManagement$man, getmanline, assessment_basis = "landings", n_years_man = 1,
                                            meandis = meandis, b_for = b_for, lastAdvice = lastAdvice, lastTAC = lastTAC))
catchscenariotable <- cbind(Scenario = names(fitManagement$man), catchscenariotable)
assess_out(catchscenariotable, "catch.scenario.table")

NewAdvice <- data.frame(NewAdviceLand  = catchscenariotable[1, 3],
                        NewAdviceCatch = catchscenariotable[1, 2],
                        NewAdviceDis   = catchscenariotable[1, 4],
                        AdviceChange   = catchscenariotable[1, 9])
assess_out(NewAdvice, "new.advice")

## Table 2 (report table 3.4.1): the catch scenarios
scenario_names <- c("F=Fmsy_C_fractile" = "MSY approach (35th percentile of predicted catch distribution under F = FMSY)",
                    "F=Fmsy" = "F = FMSY", "F=Fsq" = "F = Fsq", "F=0" = "F = 0",
                    "F=Fmsy_All_fractiles" = "F = FMSY (35th percentile of catch, B/BMSY and F/FMSY)")
table_out(catchscenariotable %>% mutate(Scenario = ifelse(Scenario %in% names(scenario_names), scenario_names[Scenario], Scenario)),
          "Table_2_catch_scenarios", advice_dir)

## Table 6: the ICES advice, the TACs, the official landings and the ICES estimates of the catch by year. The text and
## the catch of the advice come from boot/data/advice_history.csv (add a row each year), the
## advice of this assessment is added
history <- read_csv("boot/data/advice_history.csv", col_types = "icc") %>%
  filter(Year != last_yr + 2) %>%
  bind_rows(tibble(Year = last_yr + 2, `ICES advice` = "MSY approach",
                   `Catch corresponding to advice` = as.character(NewAdvice$NewAdviceCatch)))
olbll_conf <- read.taf("data/bll.27.3a47de.official.landings.confidencial.csv")
olbll <- read.taf("data/bll.27.3a47de.official.landings.csv")
conf_years <- setdiff(unique(olbll$Year), NULL)[sapply(unique(olbll$Year), function(y)
  abs(sum(olbll$Landings[olbll$Year == y]) - sum(olbll_conf$Landings[olbll_conf$Year == y])) > 1e-6)]
icd <- read.taf("data/bll.27.3a47de.intercatch.raised.csv")
Uptake2a4 <- read.taf("data/bll.27.3a47de.uptake.TAC.2a4.csv")
table_out(history %>%
            full_join(read.taf("data/bll.27.3a47de.uptake.TAC.2a3a47de.csv") %>% group_by(Year) %>%
                        summarise(`Agreed TAC brill` = round(sum(TAC, na.rm = TRUE))) %>% filter(`Agreed TAC brill` > 0), by = "Year") %>%
            full_join(olbll_conf %>% group_by(Year) %>% summarise(`Official landings` = round(sum(Landings))) %>%
                        mutate(`Official landings` = ifelse(Year %in% conf_years, "NC", as.character(`Official landings`))), by = "Year") %>%
            full_join(icd %>% filter(CatchCategory %in% c("Landings", "Discards", "BMS landing")) %>% group_by(Year) %>%
                        summarise(`ICES landings` = sum(Caton[CatchCategory == "Landings"]) / 1000,
                                  `ICES discards` = sum(Caton[CatchCategory != "Landings"]) / 1000) %>%
                        mutate(`ICES catches` = `ICES landings` + `ICES discards`, across(-Year, round)), by = "Year") %>%
            full_join(Uptake2a4 %>% transmute(Year, `Agreed TAC turbot and brill 4 and 2.a` = `Agreed TAC 2a4`,
                                              `Official landings turbot and brill 4 and 2.a` = round(Total)), by = "Year") %>%
            filter(Year >= 2000) %>%
            arrange(Year),
          "Table_6_advice_TAC_landings_catch", advice_dir)

## Figure 3.4.1: the management scenarios
save_base_plot(plotspict.hcr(fitManagement), "Figure_3.4.1_management_scenarios", width = 1000, height = 1000)


###################################
msg("Output assessment: change in advice (report figure 3.4.2)")
###################################

## This assessment (WGNSSK last_yr + 1) and the two before it (boot/data/WGNSSK<year>_bll.27.3a47de.*.csv)
change_advice <- bind_rows(
  lapply(c(last_yr - 1, last_yr), function(y) load_assessment_data(y, "boot/data", prefix = paste0("WGNSSK", y, "_"))),
  load_assessment_data(last_yr + 1, assess_dir)) %>%
  filter(as.numeric(Year) > (last_yr - 3)) %>%
  mutate(Type = factor(Type, levels = c("Data year", "Intermediate year", "Advice year")),
         Variable = factor(Variable, levels = c("B/Bmsy", "F/Fmsy", "Catches")),
         Value = as.numeric(Value))

var_levels <- c("B/Bmsy", "F/Fmsy", "Catches")
type_levels <- c("Data year", "Intermediate year", "Advice year")

## The change in the advised catch (advice year) and in B/Bmsy and F/Fmsy (data year) from one assessment to the next
brackets <- change_advice %>%
  filter(Type == "Advice year", Variable == "Catches") %>%
  arrange(Year) %>%
  mutate(prev_value = lag(Value), pct_change = 100 * (Value - prev_value) / prev_value) %>%
  filter(!is.na(pct_change)) %>%
  mutate(x_start = Year - 1, x_end = Year,
         y_pos = pmax(Value, prev_value), y_top = y_pos * 1.02,
         y_left = prev_value, y_right = Value, label_y = y_pos * 1.03,
         label = paste0(ifelse(pct_change > 0, "+", ""), round(pct_change, 1), "%**"),
         Variable = factor("Catches", levels = var_levels))
bf_brackets <- change_advice %>%
  filter(Type == "Data year", Variable %in% c("B/Bmsy", "F/Fmsy")) %>%
  arrange(Variable, Year) %>%
  group_by(Variable) %>%
  mutate(prev_value = lag(Value), pct_change = 100 * (Value - prev_value) / prev_value) %>%
  filter(!is.na(pct_change)) %>%
  mutate(x_start = Year - 1, x_end = Year,
         y_pos = pmax(Value, prev_value),
         range = max(Value, na.rm = TRUE) - min(Value, na.rm = TRUE),
         y_top = y_pos + 0.30 * range, y_left = prev_value, y_right = Value, label_y = y_pos + 0.40 * range,
         label = paste0(ifelse(pct_change > 0, "+", ""), round(pct_change, 1), "%*")) %>%
  ungroup() %>%
  mutate(Variable = factor(Variable, levels = var_levels), Type = factor(Type, levels = type_levels))

bracket_layers <- function(d) {
  list(geom_segment(data = d, aes(x = x_start, xend = x_end, y = y_top, yend = y_top), inherit.aes = FALSE),
       geom_segment(data = d, aes(x = x_start, xend = x_start, y = y_left, yend = y_top), inherit.aes = FALSE),
       geom_segment(data = d, aes(x = x_end, xend = x_end, y = y_right, yend = y_top), inherit.aes = FALSE),
       geom_text(data = d, aes(x = (x_start + x_end) / 2, y = label_y, label = label), inherit.aes = FALSE, size = 3))
}
p <- ggplot(change_advice, aes(x = Year, y = Value, colour = Source, shape = Type)) +
  geom_point(size = 3) +
  theme_bw() +
  labs(x = "", y = "", colour = "", shape = "Year type",
       caption = "* Change in B/Bmsy and F/Fmsy (Data year)\n** Change in advised catch (Advice year)") +
  scale_shape_manual(values = c("Data year" = 16, "Intermediate year" = 2, "Advice year" = 0)) +
  scale_x_continuous(breaks = seq(min(change_advice$Year), max(change_advice$Year), by = 1)) +
  theme(legend.position = "bottom") +
  bracket_layers(brackets) +
  bracket_layers(bf_brackets) +
  facet_wrap(~Variable, scales = "free_y")
figure_out(p, "Figure_3.4.2_change_in_advice", width = 12, height = 8, units = "in")


###################################
msg("Output assessment: the values the reports quote")
###################################

assess_out(data.frame(last_yr, lastAdvice, previousAdvice, lastTAC, meandis,
                      convergence = fit$opt$convergence,
                      RhoBBMSY = as.numeric(mohns_rho(fitCorrectRetro)[2]),
                      RhoFFMSY = as.numeric(mohns_rho(fitCorrectRetro)[1]),
                      MohnIndexSemester1 = mohn_index[1],
                      MohnIndexSemester2 = mohn_index[2],
                      spict_version = spict::get.version()),
           "assessment.settings")

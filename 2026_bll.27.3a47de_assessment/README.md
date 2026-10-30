# 2026_bll.27.3a47de_assessment
2026 - Brill (Scophthalmus rhombus) in Subarea 4 and in divisions 3.a, 7.d, and 7.e (North Sea, Skagerrak, Kattegat, English Channel) - WGNSSK (ADGNS)

## Files from the other repositories

`boot/initial/data` holds the files of the two repositories this assessment is built on, named after the one they come from:

- `survey_*`: from `2026_bll.27.3a47de_survey` (`output/to_assessment`, written by its `output_to_assessment.R`): the survey indices (`survey_index_sem1.csv`, `survey_index_sem2.csv`, `survey_index_historical_1983_1998.csv`, `survey_index_retro.rds`, ...) and the survey figures of the report and the presentation
- `rdbes_*`: from `2026_bll.27.3a47de_RDBES_combined` (`output/to_assessment`, written by its `output_to_assessment.R`): the SCF allocation tables `rdbes_SCF_<CAR|CAS|DAR|DAS>_<catch year>.csv.gz`

When they are made again, copy them here and run `taf.boot()`; `boot/DATA.bib` lists every one.

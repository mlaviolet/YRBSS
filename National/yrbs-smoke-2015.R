
library(srvyr)
yrbs15 <- readr::read_csv("//hzndhhsvf2/data/OCPH/EPI/BHSDM/Group/Bethany Poulin/YRBS/YRBS_Custom/Create Analytic Dataset/NHHqn2015.csv") %>% 
  as_survey_design(weights = ov_wgt, strata = stratum, nest = TRUE)

yrbs15 %>% 
  summarize(pct = survey_mean(qn33 == 1 | qn40 == 1, na.rm = TRUE,
                              vartype = "ci"))
  

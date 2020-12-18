library(here)
library(srvyr)
library(segmented) 
library(survey) 
library(tidyverse)

options(survey.lonely.psu = "adjust")

# "Have you ever tried cigarette smoking, even one or two puffs?" 
#  1 = Yes, 2 = No
# 1991: q23
# 1993, 2001-2009: q28
# 1995-1997: q26
# 1999: q27
# 2011: q29

f1 <- function(year, begin, end) {
  read_fwf(here("National", paste0("yrbs", year, ".dat")), 
           fwf_positions(begin, end, 
                         c("sex", "grade", "smoking", "weight", "psu", "stratum")))
  }

survyear <- seq(1991, 2011, 2)
begin_lst <- list(c(2, 3, 23, 76, 84, 87), 
                  c(2, 3, 28, 88, 98, 103), 
                  c(2, 3, 26, 89, 101, 106), 
                  c(2, 3, 26, 90, 102, 107), 
                  c(2, 3, 41, 107, 118, 115), 
                  c(2, 3, 42, 110, 118, 124), 
                  c(2, 3, 43, 216, 224, 230), 
                  c(18, 3, 53, 358, 370, 375), 
                  c(18, 3, 59, 358, 370, 375), 
                  c(18, 3, 59, 363, 376, 373), 
                  c(18, 3, 60, 364, 374, 377))
end_lst <- list(c(2, 3, 23, 83, 86, 91), 
                c(2, 3, 28, 97, 102, 103), 
                c(2, 3, 26, 100, 105, 106), 
                c(2, 3, 26, 101, 106, 107), 
                c(2, 3, 41, 114, 123, 115), 
                c(2, 3, 42, 117, 123, 124), 
                c(2, 3, 43, 223, 229, 230), 
                c(18, 3, 53, 369, 374, 378), 
                c(18, 3, 59, 369, 374, 378), 
                c(18, 3, 59, 372, 382, 373),
                c(18, 3, 60, 373, 376, 382))

chk_tbl <- tibble(year = survyear, x2 = begin_lst, x3 = end_lst)

combine_tbl <- chk_tbl %>% 
  mutate(df = pmap(list(year, x2, x3), f1)) %>% 
  select(year, df) %>% 
  unnest(cols = df)

combine_tbl %>% 
  as_survey_design(id = psu, strata = c(year, stratum), 
                   weights = weight, nest = TRUE) %>% 
  group_by(year) %>% 
  summarize(pct = survey_mean(smoking == 1, na.rm = TRUE))

# Damico's race-Hispanic coding
# raceeth = ifelse(year %in% 1991:1997,
#                  ifelse(q4 %in% 1:3, q4, 
#                         ifelse(q4 %in% 4:6, 4, NA)),
#                  ifelse(year %in% 1999:2005,
#                         ifelse(q4 %in% 6, 1,
#                                ifelse(q4 %in% 3, 2,
#                                       ifelse(q4 %in% c(4, 7), 3,
#                                              ifelse( q4 %in% c(1, 2, 5, 8), 4, NA)))),
#                         ifelse(year %in% 2007:2011,
#                                ifelse(raceeth %in% 5, 1,
#                                       ifelse(raceeth %in% 3, 2,
#                                              ifelse(raceeth %in% c(6, 7), 3,
#                                                     ifelse(raceeth %in% c(1, 2, 4, 8), 4, NA)))),

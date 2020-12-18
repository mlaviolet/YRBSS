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
                         c("sex", "grade", "race",
                           "smoking", "weight", "psu", "stratum")))
  }

survyear <- seq(1991, 2011, 2)
begin_lst <- list(c(2, 3, 4, 23, 76, 84, 87), 
                  c(2, 3, 4, 28, 88, 98, 103), 
                  c(2, 3, 4, 26, 89, 101, 106), 
                  c(2, 3, 4, 26, 90, 102, 107), 
                  c(2, 3, 4, 41, 107, 118, 115), 
                  c(2, 3, 4, 42, 110, 118, 124), 
                  c(2, 3, 4, 43, 216, 224, 230), 
                  c(18, 3, 20, 53, 358, 370, 375), 
                  c(18, 3, 384, 59, 358, 370, 375), 
                  c(18, 3, 393, 59, 363, 376, 373), 
                  c(18, 3, 388, 60, 364, 374, 377))
end_lst <- list(c(2, 3, 4, 23, 83, 86, 91), 
                c(2, 3, 4, 28, 97, 102, 103), 
                c(2, 3, 4, 26, 100, 105, 106), 
                c(2, 3, 4, 26, 101, 106, 107), 
                c(2, 3, 4, 41, 114, 123, 115), 
                c(2, 3, 4, 42, 117, 123, 124), 
                c(2, 3, 5, 43, 223, 229, 230), 
                c(18, 3, 21, 53, 369, 374, 378), 
                c(18, 3, 385, 59, 369, 374, 378), 
                c(18, 3, 394, 59, 372, 382, 373),
                c(18, 3, 389, 60, 373, 376, 382))

yrbs_dat <- tibble(year = survyear, begin_lst, end_lst) %>% 
  mutate(df = pmap(list(year, begin_lst, end_lst), f1)) %>% 
  select(year, df) %>% 
  unnest(cols = df) %>% 
  # recode race to 1 = White, 2 = Black, 3 Hispanic any race, 4 = other
  mutate(raceeth = case_when(
    year %in% 1991:1997 & race == 1                ~ 1,
    year %in% 1991:1997 & race == 2                ~ 2,
    year %in% 1991:1997 & race == 3                ~ 3,
    year %in% 1991:1997 & race %in% 4:6            ~ 4,
    year %in% 1999:2005 & race == 6                ~ 1,
    year %in% 1999:2005 & race == 3                ~ 2,
    year %in% 1999:2005 & race %in% c(4, 7)        ~ 3,
    year %in% 1999:2005 & race %in% c(1, 2, 5, 8)  ~ 4,
    year %in% 2007:2011 & race == 5                ~ 1,
    year %in% 2007:2011 & race == 3                ~ 2,
    year %in% 2007:2011 & race %in% 6:7            ~ 3,
    year %in% 2007:2011 & race %in% c(1, 2, 4, 8) ~ 4,
    TRUE                                           ~ NA_real_)) %>% 
  select(year, grade, sex, raceeth, smoking, psu, stratum, weight) %>% 
  mutate(grade = factor(grade, 1:4, c("9th", "10th", "11th", "12th")),
         sex = factor(sex, 1:2, c("Female", "Male")),
         raceeth = factor(raceeth, 1:4, 
                          c("White", "Black", "Hispanic", "Other")),
         smoking = factor(smoking, 1:2, c("Yes", "No")))

count(yrbs_dat, year)

yrbs_svy <- yrbs_dat %>% 
  as_survey_design(id = psu, strata = c(year, stratum), 
                   weights = weight, nest = TRUE) 

yrbs_svy %>% 
  group_by(year) %>% 
  summarize(pct = survey_mean(smoking == "Yes", na.rm = TRUE))



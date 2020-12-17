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

# open 1991 to check
# column begin-end in user guide is incorrect
#   correct psu is 84-86, stratum 87-91

yrbs91 <- 
  read_fwf(here("National", "YRBS1991.dat"), 
           fwf_positions(c(23, 76, 84, 87),
                         c(23, 83, 86, 91),
                         c("q23", "weight", "psu", "stratum")))

yrbs91 %>% 
  count(psu)

svy91 <- yrbs91 %>% 
  as_survey_design(id = psu, strata = stratum, weights = weight, nest = TRUE)
# this is correct, finally
svy91 %>% 
  summarize(pct = survey_mean(q23 == 1, na.rm = TRUE))

# try 2011
yrbs11 <- 
  read_fwf(here("National", "yrbs2011.dat"), 
           fwf_positions(c(60, 364, 374, 377),
                         c(60, 373, 376, 382),
                         c("q29", "weight", "psu", "stratum")))
svy11 <- yrbs11 %>% 
  as_survey_design(id = psu, strata = stratum, weights = weight, nest = TRUE)
# this is correct, finally
svy11 %>% 
  summarize(pct = survey_mean(q29 == 1, na.rm = TRUE))

# arguments for function to read each year
# year, beginning vector, ending vector, smoking question

f1 <- function(year, begin, end) {
  read_fwf(here("National", paste0("yrbs", year, ".dat")), 
           fwf_positions(begin, end, 
                         c("smoking", "weight", "psu", "stratum")))
  }

chk91 <- f1(1991, c(23, 76, 84, 87), c(23, 83, 86, 91))
chk11 <- f1(2011, c(60, 364, 374, 377), c(60, 373, 376, 382))
chk99 <- f1(1999, c(27, 107, 118, 115),  c(27, 114, 123, 115))

chk99 %>% 
  as_survey_design(id = psu, strata = stratum, weights = weight, nest = TRUE) %>% 
  summarize(pct = survey_mean(smoking == 1, na.rm = TRUE)) 

# des <- svydesign(id = ~psu, strata = ~interaction(stratum, year),
#                  data = y, weights = ~weight, nest = TRUE)

survyear <- seq(1991, 2011, 2)
# qsmoke <- c(23, 28, 26, 26, 27, 28, 28, 28, 28, 28, 29)
# chksmoke <- paste0("q", c(23, 29))

begin_lst <- list(c(23, 76, 84, 87), 
                  c(28, 88, 98, 103), 
                  c(26, 89, 101, 106), 
                  c(26, 90, 102, 107), 
                  c(41, 107, 118, 115), 
                  c(42, 110, 118, 124), 
                  c(43, 216, 224, 230), 
                  c(53, 358, 370, 375), 
                  c(59, 358, 370, 375), 
                  c(59, 363, 376, 373), 
                  c(60, 364, 374, 377))
end_lst <- list(c(23, 83, 86, 91), 
                c(28, 97, 102, 103), 
                c(26, 100, 105, 106), 
                c(26, 101, 106, 107), 
                c(41, 114, 123, 115), 
                c(42, 117, 123, 124), 
                c(43, 223, 229, 230), 
                c(53, 369, 374, 378), 
                c(59, 369, 374, 378), 
                c(59, 372, 382, 373),
                c(60, 373, 376, 382))
chkyear <- c(1991, 2011)

chk_tbl <- tibble(year = survyear, x2 = begin_lst, x3 = end_lst)

combine_tbl <- chk_tbl %>% 
  mutate(df = pmap(list(year, x2, x3), f1)) %>% 
  select(year, df) %>% 
  unnest(cols = df)

count(combine_tbl, year, is.na(stratum))

combine_tbl %>% 
  as_survey_design(id = psu, strata = c(year, stratum), 
                   weights = weight, nest = TRUE) %>% 
  group_by(year) %>% 
  summarize(pct = survey_mean(smoking == 1, na.rm = TRUE))



# tbl1 <- tibble(max = c(3, 4, 5), seq = list(1:3, 1:4, 1:5))
# tbl1 %>% 
#   mutate(chk = map2(max, seq, `+`)) %>% 
#   unnest(cols = c(seq, chk))



# Replicate and enhance results in CDC publication
#   "Conducting Trend Analyses of YRBS Data," June 2014
# Using YRBSS data 1991, 1993, ..., 2011

# data and documentation at https://www.cdc.gov/healthyyouth/data/yrbs/data.htm
library(here)
library(srvyr)
library(segmented) 
library(survey) 
library(polypoly)
library(tidyverse)
library(ggthemes)   
library(magrittr)
# load extra themes, scales, and geoms for ggplot2
library(texreg)     
# converts output to latex tables 

options(survey.lonely.psu = "adjust")

# "Have you ever tried cigarette smoking, even one or two puffs?" 
#  1 = Yes, 2 = No
# 1991: q23
# 1993, 2001-2009: q28
# 1995-1997: q26
# 1999: q27
# 2011: q29

# Import data -------------------------------------------------------------
# beginning and ending columns for fields sex, grade, race or race-ethnicity,
#   ever-smoked indicator, survey analysis weight, primary sampling unit,
#   and stratum
survyear <- seq(1991, 2011, 2)
begin_lst <- list(c(2, 3, 4, 23, 76, 84, 87), 
                  c(2, 3, 4, 28, 88, 98, 103), 
                  c(2, 3, 4, 26, 89, 101, 106), 
                  c(2, 3, 4, 26, 90, 102, 107), 
                  c(2, 3, 4, 41, 107, 118, 115), 
                  c(2, 3, 4, 42, 110, 118, 124), 
                  c(2, 3, 4, 43, 216, 224, 230), 
                  c(18, 19, 20, 53, 358, 370, 375), 
                  c(18, 19, 384, 59, 358, 370, 375), 
                  c(18, 19, 393, 59, 363, 376, 373), 
                  c(18, 19, 388, 60, 364, 374, 377))
end_lst <- list(c(2, 3, 4, 23, 83, 86, 91), 
                c(2, 3, 4, 28, 97, 102, 103), 
                c(2, 3, 4, 26, 100, 105, 106), 
                c(2, 3, 4, 26, 101, 106, 107), 
                c(2, 3, 4, 41, 114, 123, 115), 
                c(2, 3, 4, 42, 117, 123, 124), 
                c(2, 3, 5, 43, 223, 229, 230), 
                c(18, 19, 21, 53, 369, 374, 378), 
                c(18, 19, 385, 59, 369, 374, 378), 
                c(18, 19, 394, 59, 372, 382, 373),
                c(18, 19, 389, 60, 373, 376, 382))

import_data <- function(year, begin, end) {
  read_fwf(here("National", paste0("yrbs", year, ".dat")), 
           fwf_positions(begin, end, 
                         c("sex", "grade", "race",
                           "smoking", "weight", "psu", "stratum")))
  }

# Combine tables ----------------------------------------------------------
# combine biannual data tables into a single table
yrbs_dat <- tibble(year = survyear, begin_lst, end_lst) %>% 
  mutate(df = pmap(list(year, begin_lst, end_lst), import_data)) %>% 
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
    year %in% 2007:2011 & race %in% c(1, 2, 4, 8)  ~ 4,
    TRUE                                           ~ NA_real_)) %>% 
  select(year, grade, sex, raceeth, smoking, psu, stratum, weight) %>% 
  # make all categorical variables into factors
  mutate(grade = factor(grade, 1:4, c("9th", "10th", "11th", "12th")),
         sex = factor(sex, 1:2, c("Female", "Male")),
         raceeth = factor(raceeth, 1:4, 
                          c("White", "Black", "Hispanic", "Other")),
         smoking = factor(smoking, 1:2, c("Yes", "No"))) %>% 
  poly_add_columns(year, 3)

# check_grade <- count(yrbs_dat, year, grade) %>% 
#   pivot_wider(year, names_from = "grade", values_from = "n") %T>% 
#   write_csv(here("National", "check_grade.csv"))

# Create survey object ----------------------------------------------------
yrbs_svy <- as_survey_design(yrbs_dat, id = psu, strata = c(year, stratum), 
                             weights = weight, nest = TRUE) 

# calculate unadjusted "ever smoked" rates by year
# looks good; matches document
unadjusted <- yrbs_svy %>% 
  group_by(year) %>% 
  summarize(pct = survey_mean(smoking == "Yes", vartype = c("ci", "se"),
                              na.rm = TRUE)) %T>% 
  write_csv("unadjusted.csv")

# plot unadjusted trend
ggplot(unadjusted, aes(x = year, y = pct)) +
  geom_point() + 
  geom_errorbar(aes(ymax = pct_upp, ymin = pct_low), 
                width = 0.2 ) +
  geom_line() +
  theme_tufte() +
  ggtitle("Figure 1. Unadjusted smoking prevalence 1999-2011") +
  theme(plot.title = element_text(size = 9, face = "bold"))

# calculate the Number of Joinpoints Needed
# Calculate the "ever smoked" binomial regression, adjusted by sex, age, 
#   race-ethnicity, and a linear year contrast.
# do models one at a time?
cubic_year <- 
  svyglm(I(smoking == "Yes") ~ sex + raceeth + grade + year1 + year2 + year3,
         design = yrbs_svy, family = quasibinomial)
summary(cubic_year)

# (7) Calculate the Adjusted Prevalence and Predicted Marginals
#   First, calculate the survey-year-independent predictor effects and store 
#   these results into a separate object.
marginals <- svyglm(I(smoking == "Yes") ~ sex + raceeth + grade, 
                    design = yrbs_svy, family = quasibinomial)

means_for_joinpoint <- svypredmeans(marginals, ~ year) %>% 
  merge(confint(.), by = 0) %>% 
  setNames(c("year", "pct", "pct_se", "pct_low", "pct_upp")) %>% 
  arrange(year) %>% 
  mutate(year = as.numeric(year)) %>% 
  tibble()
# names(means_for_joinpoint) <- c("yr", "mean", "se") # is this still necessary?

# plot adjusted prevalence
ggplot(means_for_joinpoint, aes(x = year, y = pct)) +
  geom_point() + 
  geom_errorbar(aes(ymax = pct_upp, ymin = pct_low), width = 0.2) +
  geom_line() +
  theme_tufte() +
  ggtitle("Figure 2. Adjusted smoking prevalence 1999-2011") +
  theme(plot.title = element_text(size = 9, face = "bold"))

# (8) Identify the Breakpoint/Changepoint
# Carrying out a trend analysis requires creating new weights to fit a 
#   piecewise linear regression. Figure 3 shows the relationship between 
#   variance at each datum and weighting. Larger circles display greater 
#   uncertainty and therefore lower weight.
ggplot(means_for_joinpoint, aes(x = year, y = pct)) +
  geom_point(aes(size = pct_se)) +
  theme_tufte() +
  ggtitle( "Figure 3. Standard Error at each timepoint\n(smaller dots indicate greater confidence in each adjusted value)")

# First, create that weight variable.
means_for_joinpoint <- means_for_joinpoint %>% 
  mutate(wgt = (pct / pct_se ) ^ 2) # or 1 / pct_se ^ 2,
# Second, fit a piecewise linear regression.
# Estimate the 'starting' linear model with the usual "lm" function using 
#   the log values and the weights.
o <- lm(log(pct) ~ year, weights = wgt, data = means_for_joinpoint)
os <- segmented(o, ~year)

# Note that the Estimated Break-Point is not an integer.
(your_breakpoint <- round(as.vector(os$psi[, "Est."])))
slope(os, APC = TRUE)

# RESUME AT LINE 268 OF DAMICO SCRIPT

# poly_add_columns generates warning
# Warning message:
#   Prefixing `UQS()` with the rlang namespace is deprecated as of rlang 0.3.0.
# Please use the non-prefixed form or `!!!` instead.
# 
# # Bad:
# rlang::expr(mean(rlang::UQS(args)))
# 
# # Ok:
# rlang::expr(mean(UQS(args)))
# 
# # Good:
# rlang::expr(mean(!!!args))
# Issue reported on GitHub 
# https://github.com/tjmahr/polypoly/issues
# Pull request?

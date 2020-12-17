# reproducing results from "Conducting Trend Analyses of YRBSS data"
#  (2015 edition)
# http://www.cdc.gov/healthyyouth/data/yrbs/pdf/2015/2015_yrbs_conducting_trend_analyses.pdf

# following methods in 
# http://www.asdfree.com/2015/11/statistically-significant-trends-with.html

# 1. Data Importation -----------------------------------------------------
# "Prior to running this analysis script, the Youth Risk Behavioral 
# Surveillance System (YRBSS) 1991-2011 single-year files must all be 
# loaded as R data files (.rda) on your local machine.  Running the 
# download automation script will create the appropriate files."

# library(downloader)
# setwd( "C:/My Directory/YRBSS/" )
# source_url( "https://raw.github.com/ajdamico/asdfree/master/Youth%20Risk%20Behavior%20Surveillance%20System/download%20all%20microdata.R" , prompt = FALSE , echo = TRUE )

# 2. Load Required Packages, Options --------------------------------------
# Muggeo V. (2008) Segmented: an R package to fit regression models with 
#   broken-line relationships. R News, 8, 1: 20-25.
library(segmented)  
  # determine segmented relationships in regression models
library(downloader) 
  # downloads and then runs the source() function on scripts from github
library(plyr)       
  # contains the rbind.fill() function, which stacks two 
  #   data frames even if they don't contain the same 
  #  columns. the rbind() function does not do this
library(survey)     
  # load survey package (analyzes complex design surveys)
library(ggplot2)    
  # load ggplot2 package (plots data according to the grammar of graphics)
library(ggthemes)   
  # load extra themes, scales, and geoms for ggplot2
library(texreg)     
  # converts output to latex tables 
library(dplyr)
library(tibble)
# library(mosaic)
# set R to produce conservative standard errors instead of crashing
# http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options(survey.lonely.psu = "adjust")
# this setting matches the MISSUNIT option in SUDAAN
# SAS uses "remove" instead of "adjust" by default,
# the table target replication was generated with SAS,
# so if you want to get closer to that, use "remove"
# No external functions needed--"svypredmeans" is now part of survey package

# 3. Harmonize and Stack Multiple Years of Survey Data --------------------
# This step is clearly dataset-specific.  In order for your trend analysis
#   to work, you'll need to figure out how to align the variables from 
#   multiple years of data into a trendable, stacked data.frame object.

setwd("C:/Survey data/YRBSS")
# initiate an empty `y` object
y <- NULL
# loop through each year of YRBSS microdata
for (year in seq(2005, 2015, 2)) {
  # load the current year
  load(paste0("yrbs", year, ".rda"))
  # tack on a `year` column
  x$year <- year
  # stack that year of data alongside the others, 
  #   ignoring mis-matching columns
  y <- rbind.fill(x, y)
  # clear the single-year of microdata from RAM
  rm(x); gc()
  }

# MODIFY FOLLOWING FOR 2005-15 DATA
# REPLACE QN29 with QN10 (Rode with a drinking driver, past 30 days)
# remove all unnecessary columns from the 1991-2011 multi-year stack
# y <- y[c("q2", "q3", "q4", "q23", "q26", "q27", "q28", "q29", "year",
#          "psu", "stratum", "weight", "raceeth")]

# convert every column to numeric type
# y[, ] <- sapply(y[, ], as.numeric)

# construct year-specific recodes so that
# "rode with a drinking driver" // grade // sex // race-ethnicity align 
#    across years
y <- y %>% 
  select(sex = q2, grade = q3, q4, q10, riding = qn10, year, psu, stratum, 
         weight, raceeth) %>% 
  mutate_each(funs(as.numeric)) %>% 
  mutate(riding = factor(riding, 1:2),
         grade = factor(grade, 1:4),
         sex = factor(sex, 2:1)) %>% 
  mutate(
    raceeth = mosaic::derivedFactor(
      .default = NA,
      '1' = (year == 2005 & q4 == 6) |
        (year %in% 2007:2015 & raceeth == 5), # White
      '2' = (year == 2005 & q4 == 3) | 
        (year %in% 2007:2015 & raceeth == 3), # Black
      '3' = (year == 2005 & q4 %in% c(4, 7)) |
        (year %in% 2007:2015 & raceeth %in% c(6, 7)), # Hispanic
      '4' = (year == 2005 & q4 %in% c(1, 2, 5, 8)) | 
        (year %in% 2007:2015 & raceeth %in% c(1, 2, 4, 8))))  %>% # Other
  select(year, psu, stratum, weight, riding, raceeth, sex, grade)

levels(y$raceeth) <- as.character(1:4)
str(y)

lapply(y, summary)

# 4. Construct a Multi-Year Stacked Complex Survey Design Object ----------
# Before constructing a multi-year stacked design object, check out 
#   ?contr.poly - this function implements polynomials used in our trend 
#   analysis during step 6. For more detail on this subject, see page 216 of 
#   "Applied Multiple Regression/Correlation Analysis for the Behavioral 
#   Sciences" by Jacob Cohen, Patricia Cohen, Stephen G. West, Leona S. Aiken:
#   "The polynomials we have used as predictors to this point are natural 
#   polynomials, generated from the linear predictor by centering and then 
#   powering the linear predictor."

# extract a linear contrast vector of length six,
# because we have six distinct years of yrbs data 2005, 2007, ..., 2015
c11l <- contr.poly(6)[, 1]
# also extract a quadratic (squared) contrast vector
c11q <- contr.poly(6)[, 2]
# just in case, extract a cubic contrast vector
c11c <- contr.poly(6)[, 3]
# for each record in the data set, tack on the linear, quadratic, and cubic 
#   contrast value
#   these contrast values will serve as replacement for the linear `year` 
#   variable in any regression.

# year^1 term (linear)
y$t11l <- c11l[match(y$year, seq(2005, 2015, 2))]
# year^2 term (quadratic)
y$t11q <- c11q[match(y$year, seq(2005, 2015, 2))]
# year^3 term (cubic)
y$t11c <- c11c[match(y$year, seq(2005, 2015, 2))]

# construct a complex sample survey design object
# stacking multiple years and accounting for `year` in the nested strata
des <- svydesign(id = ~psu, strata = ~interaction(stratum, year),
                 data = y, weights = ~weight, nest = TRUE)

# 5. Review the unadjusted results ----------------------------------------
# Here's the change over time for riding with drinker prevalence among youth. 
#   Unadjusted prevalence rates (Figure 1) suggest a significant change in 
#   riding prevalence.

# immediately remove records with missing riding status
des_ns <- subset(des, !is.na(riding))

# calculate unadjusted, un-anythinged "ever smoked" rates by year
#   note that this reproduces the unadjusted "ever smoked" statistics at 
#   the top of pdf page 6 of 
#   http://www.cdc.gov/healthyyouth/data/yrbs/pdf/2015/2015_yrbs_conducting_trend_analyses.pdf
unadjusted <- 
  svyby(~riding, ~year, svymean, design = des_ns, vartype = c("ci", "se"))

# coerce that result into a `data.frame` object
my_plot <- data.frame(unadjusted)

# plot the unadjusted decline in riding
ggplot(my_plot, aes(x = year, y = riding1)) +
  geom_point() + 
  geom_errorbar(aes(ymax = ci_u.riding1, ymin = ci_l.riding1), width = 0.2) +
  geom_line() +
  theme_tufte() +
  ggtitle("Figure 1. Unadjusted riding prevalence 2005-2015") +
  theme(plot.title = element_text(size = 9, face = "bold"))

# 6. Calculate the Number of Joinpoints Needed ----------------------------
# Calculate the "rode in past 30 days" binomial regression, adjusted by 
#   sex, age, race-ethnicity, and a linear year contrast.
linyear <- 
  svyglm(I(riding == 1) ~ sex + raceeth + grade + t11l, 
         design = subset(des_ns, riding %in% 1:2), family = quasibinomial)
summary(linyear)

# Calculate the "rode in past 30 days" binomial regression, adjusted by sex, 
#   age, race-ethnicity, and both linear and quadratic year contrasts. Notice 
#   the addition of t11q.
quadyear <-
  svyglm(I(riding == 1) ~ sex + raceeth + grade + t11l + t11q, 
         design = subset(des_ns, riding %in% 1:2), family = quasibinomial)
summary(quadyear)

# Calculate the "rode in past 30 days" binomial regression, adjusted by sex, 
#   age, race-ethnicity, and linear, quadratic, and cubic year contrasts. 
#   Notice the addition of t11c.
cubyear <-
  svyglm(I(riding == 1) ~ sex + raceeth + grade + t11l + t11q + t11c , 
         design = subset(des_ns, riding %in% 1:2), family = quasibinomial)
summary(cubyear)

# 7. Calculate the Adjusted Prevalence and Predicted Marginals ------------
#   First, calculate the survey-year-independent predictor effects and store 
#   these results into a separate object.
marginals <- 
  svyglm(formula = I(riding == 1) ~sex + raceeth + grade,
         design = des_ns, family = quasibinomial)

# Second, run these marginals through the svypredmeans function 
means_for_joinpoint <- 
  svypredmeans(marginals, ~factor(year)) %>% 
  # coerce the results to a data.frame object
  data.frame(.) %>% 
  # extract the row names as the survey year
  rownames_to_column("year") %>% 
  # must be sorted, just in case it's not already
  arrange(year) %>% 
  # rename columns so they do not conflict with variables in memory
  select(mean, se = SE, yr = year) %>% 
  mutate(yr = as.numeric(yr))
  
another_plot <- means_for_joinpoint %>% 
  mutate(ci_l.mean = mean - (1.96 * se),
         ci_u.mean = mean + (1.96 * se))

ggplot(another_plot, aes(x = yr, y = mean, group = 1)) +
  geom_point() + 
  geom_errorbar(aes(ymax = ci_u.mean, ymin = ci_l.mean), width = 0.2) +
  geom_line() +
  theme_tufte() +
  ggtitle("Figure 2. Adjusted riding prevalence 2005-2015") +
  theme(plot.title = element_text(size = 9, face = "bold"))

# 8. Identify the Breakpoint/Changepoint ----------------------------------
# Carrying out a trend analysis requires creating new weights to fit a 
#   piecewise linear regression. Figure 3 shows the relationship between 
#   variance at each datum and weighting. Larger circles display greater 
#   uncertainty and therefore lower weight.

ggplot(means_for_joinpoint, aes(x = yr, y = mean)) +
  geom_point(aes(size = se)) +
  theme_tufte() +
  ggtitle( "Figure 3. Standard Error at each timepoint\n(smaller dots indicate greater confidence in each adjusted value)")

# First, create that weight variable.
means_for_joinpoint$wgt <- with(means_for_joinpoint, (mean / se ) ^ 2) 
# Second, fit a piecewise linear regression.
# Estimate the 'starting' linear model with the usual "lm" function using 
#   the log values and the weights.
o <- lm(log(mean) ~ yr, weights = wgt, data = means_for_joinpoint)

# Now that the regression has been structured correctly, estimate the year 
#   that our complex survey trend should be broken into two segments 
#  (the changepoint/breakpoint/joinpoint).
# add a segmented variable (`yr` in this example) with 1 breakpoint
set.seed(42)
os <- segmented(o, ~as.numeric(yr))
# `os` is now a `segmented` object, which means it includes information 
#    on the fitted model, such as parameter estimates, standard errors, 
#    residuals.
summary(os) 

# Note that the Estimated Break-Point is not an integer.
(your_breakpoint <- round(as.vector(os$psi[, "Est."])))
# obtain the annual percent change (APC=) estimates for each time point
slope(os, APC = TRUE)
# The returned CIs for the annual percent change (APC) may be different from
#   those returned by NCI's Joinpoint Software.

# 9. Make statistically defensible statements about trends ----------------
# After identifying the change point for smoking prevalence, we can create 
#   two regression models (one for each time segment). (If we had two 
#   joinpoints, we would need three regression models.) The first model covers 
#   the years leading up to (and including) the changepoint (i.e., 2005 to 
#   2011). The second model includes the years from the changepoint forward
#   (i.e., 1999 to 2011).
predict(os)
plot(os)
os$psi[, "Est."]
# 2005-09
# calculate a three-timepoint linear contrast vector
c5l <- contr.poly(3)[, 1]

# tack the three-timepoint linear contrast vectors onto the current survey 
#   design object
des_ns <- update(des_ns, t5l = c5l[match(year, seq(2005, 2009, 2))])
pre_05_09 <-
  svyglm(I(riding == 1) ~sex + raceeth + grade + t5l ,
         design = subset(des_ns, riding %in% 1:2 & year <= 2009),
         family = quasibinomial)
summary(pre_05_09) 
# There was "no significant change in the prevalence of riding with a
#   drinking driver during 2005-2009 ..." (see model term t5l)

# 2009-2015
# calculate a four-timepoint linear contrast vector
c7l <- contr.poly(4)[, 1]
# tack the four-timepoint linear contrast vectors onto the current survey 
#   design object
des_ns <- update(des_ns, t7l = c7l[match(year, seq(2009, 2015, 2))])
post_05_09 <-
  svyglm(I(riding == 1) ~sex + raceeth + grade + t7l ,
         design = subset(des_ns, riding %in% 1:2 & year >= 2009), 
         family = quasibinomial)
summary(post_05_09)
# "... followed by a significant linear decrease during 2009-2015."

# P-VALUES AND MODEL COEFFICIENTS MATCH

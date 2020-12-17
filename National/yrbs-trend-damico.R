# http://www.asdfree.com/2015/11/statistically-significant-trends-with.html

# (1) Data Importation
# "Prior to running this analysis script, the Youth Risk Behavioral 
# Surveillance System (YRBSS) 1991-2011 single-year files must all be 
# loaded as R data files (.rda) on your local machine.  Running the 
# download automation script will create the appropriate files."

# library(downloader)
# setwd( "C:/My Directory/YRBSS/" )
# source_url( "https://raw.github.com/ajdamico/asdfree/master/Youth%20Risk%20Behavior%20Surveillance%20System/download%20all%20microdata.R" , prompt = FALSE , echo = TRUE )

# (2) Load Required Packages, Options, External Functions
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

# set R to produce conservative standard errors instead of crashing
# http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options(survey.lonely.psu = "adjust")
# this setting matches the MISSUNIT option in SUDAAN
# SAS uses "remove" instead of "adjust" by default,
# the table target replication was generated with SAS,
# so if you want to get closer to that, use "remove"

# No external functions needed--"svypredmeans" is now part of survey package

# (3) Harmonize and Stack Multiple Years of Survey Data
# This step is clearly dataset-specific.  In order for your trend analysis
#   to work, you'll need to figure out how to align the variables from 
#   multiple years of data into a trendable, stacked data.frame object.

setwd("C:/Survey data/YRBSS")
# initiate an empty `y` object
y <- NULL
# loop through each year of YRBSS microdata
for (year in seq(1991, 2011, 2)) {
  # load the current year
  load( paste0( "yrbs" , year , ".rda" ) )
  # tack on a `year` column
  x$year <- year
  # stack that year of data alongside the others, 
  #   ignoring mis-matching columns
  y <- rbind.fill(x, y)
  # clear the single-year of microdata from RAM
  rm(x); gc()
  }

# remove all unnecessary columns from the 1991-2011 multi-year stack
y <- y[c("q2", "q3", "q4", "q23", "q26", "q27", "q28", "q29", "year",
         "psu", "stratum", "weight", "raceeth")]

# convert every column to numeric type
y[, ] <- sapply(y[, ], as.numeric)

# construct year-specific recodes so that
# "ever smoked a cigarette" // grade // sex // race-ethnicity align across 
#   years
y <- transform(y,
               smoking = 
                 as.numeric(ifelse(year == 1991, q23,
                                   ifelse(year %in% c(1993, 2001:2009), q28,
                                   ifelse(year %in% 1995:1997, q26,
                                   ifelse(year %in% 1999, q27,
                                   ifelse(year %in% 2011, q29, NA)))))),
               raceeth = ifelse(year %in% 1991:1997,
                                ifelse(q4 %in% 1:3, q4, 
                                       ifelse(q4 %in% 4:6, 4, NA)),
                         ifelse(year %in% 1999:2005,
                                ifelse(q4 %in% 6, 1,
                                ifelse(q4 %in% 3, 2,
                                ifelse(q4 %in% c(4, 7), 3,
                                ifelse( q4 %in% c(1, 2, 5, 8), 4, NA)))),
                         ifelse(year %in% 2007:2011,
                                ifelse(raceeth %in% 5, 1,
                                ifelse(raceeth %in% 3, 2,
                                ifelse(raceeth %in% c(6, 7), 3,
                                ifelse(raceeth %in% c(1, 2, 4, 8), 4, NA)))),
                                NA))),
               grade = ifelse(q3 == 5, NA, as.numeric(q3)),
               sex = ifelse(q2 %in% 1:2, q2, NA))

# again remove unnecessary variables, keeping only the complex sample survey 
#   design columns plus independent/dependent variables to be used in the 
#   regression analyses
y <- y[c("year", "psu", "stratum", "weight", "smoking", "raceeth", "sex",
         "grade")]

# set female to the reference group
y$sex <- relevel(factor(y$sex), ref = "2")

# set ever smoked = yes // white // 9th graders as the reference groups
for (i in c('smoking', 'raceeth', 'grade')) 
  y[, i] <- relevel(factor(y[, i]), ref = "1")

# (4) Construct a Multi-Year Stacked Complex Survey Design Object
# Before constructing a multi-year stacked design object, check out 
#   ?contr.poly - this function implements polynomials used in our trend 
#   analysis during step 6. For more detail on this subject, see page 216 of 
#   "Applied Multiple Regression/Correlation Analysis for the Behavioral 
#   Sciences" by Jacob Cohen, Patricia Cohen, Stephen G. West, Leona S. Aiken:
#   "The polynomials we have used as predictors to this point are natural 
#   polynomials, generated from the linear predictor by centering and then 
#   powering the linear predictor."

# extract a linear contrast vector of length eleven,
# because we have eleven distinct years of yrbss data `seq( 1999 , 2011 , 2 )`
c11l <- contr.poly(11)[, 1]
# also extract a quadratic (squared) contrast vector
c11q <- contr.poly(11)[, 2]
# just in case, extract a cubic contrast vector
c11c <- contr.poly(11)[, 3]
# for each record in the data set, tack on the linear, quadratic, and cubic 
#   contrast value
#   these contrast values will serve as replacement for the linear `year` 
#   variable in any regression.

# year^1 term (linear)
y$t11l <- c11l[match(y$year, seq(1999, 2011, 2))]
# year^2 term (quadratic)
y$t11q <- c11q[match(y$year, seq(1999, 2011, 2))]
# year^3 term (cubic)
y$t11c <- c11c[match(y$year, seq(1999, 2011, 2))]

# construct a complex sample survey design object
# stacking multiple years and accounting for `year` in the nested strata
des <- svydesign(id = ~psu, strata = ~interaction(stratum, year),
                 data = y, weights = ~weight, nest = TRUE)

# (5) Review the unadjusted results
# Here's the change over time for smoking prevalence among youth. 
#   Unadjusted prevalence rates (Figure 1) suggest a significant change in 
#   smoking prevalence.

# immediately remove records with missing smoking status
des_ns <- subset(des, !is.na(smoking))

# calculate unadjusted, un-anythinged "ever smoked" rates by year
#   note that this reproduces the unadjusted "ever smoked" statistics at 
#   the top of pdf page 6 of 
#   http://www.cdc.gov/healthyyouth/yrbs/pdf/yrbs_conducting_trend_analyses.pdf
unadjusted <- 
  svyby(~smoking, ~year, svymean, design = des_ns, vartype = c("ci", "se"))

# coerce that result into a `data.frame` object
my_plot <- data.frame(unadjusted)

# plot the unadjusted decline in smoking
ggplot(my_plot, aes(x = year, y = smoking1)) +
  geom_point() + 
  geom_errorbar(aes(ymax = ci_u.smoking1, ymin = ci_l.smoking1), 
                width = 0.2 ) +
  geom_line() +
  theme_tufte() +
  ggtitle("Figure 1. Unadjusted smoking prevalence 1999-2011") +
  theme(plot.title = element_text(size = 9, face = "bold"))

# (6) Calculate the Number of Joinpoints Needed
# Calculate the "ever smoked" binomial regression, adjusted by sex, age, 
#   race-ethnicity, and a linear year contrast.
linyear <- 
  svyglm(I(smoking == 1) ~ sex + raceeth + grade + t11l, 
         design = subset(des_ns, smoking %in% 1:2), family = quasibinomial)
summary(linyear)

# Calculate the "ever smoked" binomial regression, adjusted by sex, age, 
#   race-ethnicity, and both linear and quadratic year contrasts. Notice 
#   the addition of t11q.

quadyear <-
  svyglm(I(smoking == 1) ~ sex + raceeth + grade + t11l + t11q, 
         design = subset(des_ns, smoking %in% 1:2), family = quasibinomial)
summary(quadyear)

# Calculate the "ever smoked" binomial regression, adjusted by sex, age, 
#   race-ethnicity, and linear, quadratic, and cubic year contrasts. Notice 
#   the addition of t11c.
cubyear <-
  svyglm(I(smoking == 1) ~ sex + raceeth + grade + t11l + t11q + t11c , 
         design = subset(des_ns, smoking %in% 1:2), family = quasibinomial)
summary(cubyear)

# (7) Calculate the Adjusted Prevalence and Predicted Marginals
#   First, calculate the survey-year-independent predictor effects and store 
#   these results into a separate object.
marginals <- 
  svyglm(formula = I(smoking == 1) ~sex + raceeth + grade,
         design = des_ns, family = quasibinomial)

# Second, run these marginals through the svypredmeans function 
means_for_joinpoint <- svypredmeans(marginals, ~factor(year))
means_for_joinpoint

# coerce the results to a data.frame object
means_for_joinpoint <- as.data.frame(means_for_joinpoint)

# extract the row names as the survey year
means_for_joinpoint$year <- as.numeric(rownames( means_for_joinpoint))

# must be sorted, just in case it's not already
means_for_joinpoint <- 
  means_for_joinpoint[order(means_for_joinpoint$year), ]

# rename columns so they do not conflict with variables in memory
names(means_for_joinpoint) <- c('mean', 'se', 'yr')
# the above line is only because the "segmented" function (used below)
#   does not work if an object of the same name is also in memory.

another_plot <- means_for_joinpoint
another_plot$ci_l.mean <- another_plot$mean - (1.96 * another_plot$se)
another_plot$ci_u.mean <- another_plot$mean + (1.96 * another_plot$se)

ggplot(another_plot, aes(x = yr, y = mean)) +
  geom_point() + 
  geom_errorbar(aes(ymax = ci_u.mean, ymin = ci_l.mean), width = 0.2) +
  geom_line() +
  theme_tufte() +
  ggtitle("Figure 2. Adjusted smoking prevalence 1999-2011") +
  theme(plot.title = element_text(size = 9, face = "bold"))

# (8) Identify the Breakpoint/Changepoint
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
os <- segmented(o, ~yr)
# `os` is now a `segmented` object, which means it includes information 
#    on the fitted model, such as parameter estimates, standard errors, 
#    residuals.
summary(os) # A HAIR DIFFERENT FROM PUBLISHED

# Note that the Estimated Break-Point is not an integer.
(your_breakpoint <- round(as.vector(os$psi[, "Est."])))
# obtain the annual percent change (APC=) estimates for each time point
slope(os, APC = TRUE)
# The returned CIs for the annual percent change (APC) may be different from
#   those returned by NCI's Joinpoint Software.

# (9) Make statistically defensible statements about trends with complex 
#   survey data

# After identifying the change point for smoking prevalence, we can create 
#   two regression models (one for each time segment). (If we had two 
#   joinpoints, we would need three regression models.) The first model covers 
#   the years leading up to (and including) the changepoint (i.e., 1991 to 
#   1999). The second model includes the years from the changepoint forward
#   (i.e., 1999 to 2011).

# 1991-99
# calculate a five-timepoint linear contrast vector
c5l <- contr.poly(5)[, 1]

# tack the five-timepoint linear contrast vectors onto the current survey 
#   design object
des_ns <- update(des_ns, t5l = c5l[ match(year, seq(1991, 1999, 2))])

pre_91_99 <-
  svyglm(I(smoking == 1) ~sex + raceeth + grade + t5l ,
         design = subset(des_ns, smoking %in% 1:2 & year <= 1999),
         family = quasibinomial)
summary(pre_91_99) 
# There was "no significant change in the prevalence of ever smoking a 
#   cigarette during 1991-1999." (see model term t5l)

# 1999-2011
# calculate a seven-timepoint linear contrast vector
c7l <- contr.poly(7)[, 1]
# tack the seven-timepoint linear contrast vectors onto the current survey 
#   design object
des_ns <- update(des_ns, t7l = c7l[match(year, seq(1999, 2011, 2))])

post_99_11 <-
  svyglm(I(smoking == 1) ~sex + raceeth + grade + t7l ,
         design = subset(des_ns, smoking %in% 1:2 & year >= 1999), 
         family = quasibinomial)
summary(post_99_11)
# There was a "significant linear decrease in the prevalence of ever 
#   smoking a cigarette during 1999-2011."

# plot points with joinpoint lines
df <- data.frame(year = seq(1991, 2011, 2),
                 smoke = round(100 * unadjusted$smoking1, 1))
ggplot(df, aes(x = year, y = smoke)) +
  geom_point() +
  scale_x_continuous(breaks = seq(1991, 2011, 2)) +
  theme_bw()
# how to add correct lines?

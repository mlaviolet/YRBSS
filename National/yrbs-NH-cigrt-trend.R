library(survey)
# library(haven)
# library(plyr)
library(dplyr)
library(segmented)  
library(ggplot2)
library(scales)
options(survey.lonely.psu = "adjust")

setwd("R:/OCPH/EPI/BHSDM/Group/yrbs analytical data access/Reference material/CDC provided data")
yrbs03 <- haven::read_sas("nhh2003_yrbs_data_without_codes.sas7bdat")
yrbs05 <- haven::read_sas("nhh2005_yrbs_data.sas7bdat")
yrbs07 <- haven::read_sas("nhh2007_yrbs_data.sas7bdat")
yrbs09 <- haven::read_sas("nhh2009_yrbs_data.sas7bdat")
yrbs11 <- haven::read_sas("nhh2011_yrbs_data.sas7bdat")
setwd("R:/OCPH/EPI/BHSDM/Group/Bethany Poulin/YRBS/YRBS_Custom/Create Analytic Dataset")
yrbs13 <- readr::read_csv("NHHqn2013.csv")
yrbs15 <- readr::read_csv("NHHqn2015.csv")
setwd("R:/OCPH/EPI/BHSDM/Group/Michael Laviolette/_Projects/YRBSS trend")

core.vars <- c("psu", "weight", "stratum")

# get variable for current smoking
# 2003, 2005, 2007, 2009: qn30
# 2011: qn31
# 2013, 2015: qn33
# http://stackoverflow.com/questions/30562819/error-message-when-running-simple-rename-function-in-r
# doing individually until figure out how to wrap into function
f1 <- function(yr, var) {
  # assemble data set showing year, common name for variable across years,
  #  psu, weight, strata
  get(paste0("yrbs", substr(yr, 3, 4))) %>% 
    select_(.dots = c(core.vars, var)) %>% 
    # rename_(newname = oldname) %>% 
    mutate(YEAR = yr) %>% 
    select(YEAR, everything())
  }

# construct data frame with variable "currsmok" representing current 
#   cigarette smoking
step1 <- bind_rows(list(f1("2003", "qn30"), f1("2005", "qn30"), 
                        f1("2007", "qn30"), f1("2009", "qn30"))) %>% 
  rename(currsmok = qn30)
step2 <- f1("2011", "qn31")%>% 
  rename(currsmok = qn31) %>% 
  bind_rows(step1, .)
step3 <- f1("2013", "qn33") %>% 
  rename(currsmok = qn33) %>% 
  bind_rows(step2, .)
# next step produces final stacked data frame with all data years
yrbs <- yrbs15 %>% 
  select(psu, stratum, weight = ov_wgt, currsmok = qn33) %>% 
  mutate(YEAR = "2015") %>% 
  select(YEAR, psu, weight, stratum, currsmok) %>% 
  bind_rows(step3, .) %>% 
  mutate(currsmok = factor(currsmok, 1:2, c("Yes", "No")))
# clean up  
rm(step1, step2, step3, yrbs03, yrbs05, yrbs07, yrbs09, yrbs11, yrbs13,
   yrbs15); gc()

# check--looks OK
table(yrbs$YEAR, exclude = NULL)  
xtabs(~YEAR + currsmok, data = yrbs)

# extract a linear contrast vector of length seven for the seven distinct
#   years of yrbs data 2003, 2005, ..., 2015
# construct data frame consisting of seven distinct data years (2003, 2005, ...
#   2015 for merging
# add coefficients for linear, quadratic, cubic trends
trend <- data.frame(YEAR = as.character(seq(2003, 2015, 2)),
                   trend.l = contr.poly(7)[, 1],
                   trend.q = contr.poly(7)[, 2],
                   trend.c = contr.poly(7)[, 3])
yrbs <- yrbs %>%
  inner_join(trend, by = "YEAR") %>%
  mutate(YEAR = factor(YEAR))

# construct survey object  
yrbs.svy <- svydesign(id = ~psu, weight = ~weight, 
                      strata = ~interaction(stratum, YEAR),
                      data = yrbs, nest = TRUE)
# prevalence by year
# smok.yr <- 
  # svyby(~currsmok, ~YEAR, yrbs.svy, svymean, na.rm = TRUE, vartype = "ci")
# XLConnect::writeWorksheetToFile("//Hzndhhsvf2/data/OCPH/EPI/BHSDM/Group/Michael Laviolette/~Projects/YRBSS trend/yrbs-smoke.xlsx", 
#                                 smok.yr, "trend")

# model with linear trend
linyear <- 
  svyglm(I(currsmok == "Yes") ~ trend.l, yrbs.svy, family = quasibinomial)
summary(linyear)
# model with quadratic trend
quadyear <- 
  svyglm(I(currsmok == "Yes") ~ trend.l + trend.q, yrbs.svy,
         family = quasibinomial)
summary(quadyear)
# model with cubic trend
cubyear <- 
  svyglm(I(currsmok == "Yes") ~ trend.l + trend.q + trend.c, yrbs.svy,
         family = quasibinomial)
summary(cubyear)

# get prevalence by year--no coveriates, so just using svymean
# RESUME EDITING HERE--CHANGE OBJECT NAMES TO GET RID OF SINGLE-LETTER NAMES
means_for_joinpoint <- 
  svyby(~currsmok, ~YEAR, yrbs.svy, svymean, na.rm = TRUE) %>% 
  select(YEAR, mean = currsmokYes, SE = se.currsmokYes) %>% 
  # add weights for weigted regression--inverse of relative variance
  # larger sample gives greater weight
  mutate(wgt = (mean / SE) ^ 2,
         yr = as.numeric(YEAR))
     
fit.lm <- lm(log(mean) ~ yr, weights = wgt, data = means_for_joinpoint)
set.seed(42)
fit.seg <- segmented(fit.lm, ~yr)
summary(fit.seg) 
jp <- as.vector(fit.seg$psi[, "Est."])
# joinpoint at year 5 (2011)

# get annual percent change (APC)
slope(fit.seg, APC = TRUE)

# my.fitted <- predict(fit.seg)
# add joinpoint to data frame for plotting
df <- data.frame(yr = means_for_joinpoint$yr, 
                 logmean = predict(fit.seg)) %>% 
  bind_rows(data.frame(yr = jp, 
                       logmean = predict(fit.seg, data.frame(yr = jp)))) %>% 
  arrange(yr)

ggplot(df, aes(x = yr, y = logmean)) + 
  geom_line() +
  geom_point(data = means_for_joinpoint, aes(x = yr, y = log(mean)),
             size = 2) +
  scale_x_continuous(breaks = 1:7, 
                     labels = as.character(seq(2003,2015, 2))) +
  labs(x = "Survey year", y = "") +
  scale_y_continuous(label = function(x){percent(round(exp(x),2))}) +
  theme_bw() +
  theme(axis.text.x = element_text(face = "bold", size = rel(1.15))) +
  theme(axis.text.y = element_text(face = "bold", size = rel(1.25))) +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_text(face = "bold", size = rel(1.15))) +
  ggtitle("             Currently smoking cigarettes, NH YRBSS") +  
  coord_fixed(ratio = 1.62) 
 
# ggsave("yrbs-smoke.png")

# TEST FOR SIGNIFICANT REGRESSION EACH SEGMENT
# five-point contrast vector for 2003-2011
c5l <- contr.poly(5)[, 1]

# three-point contrast vector for 2011-2015
c3l <- contr.poly(3)[, 1]

# split data frames
# list of models--using "do" from dplyr?

# OK TO HERE --------------------------------------------------------------


# # tack the five-timepoint linear contrast vectors onto the current survey 
# #   design object
# des_ns <- update(des_ns, t5l = c5l[match(year, seq(2005, 2009, 2))])
# pre_05_09 <-
#   svyglm(I(riding == 1) ~sex + raceeth + grade + t5l ,
#          design = subset(des_ns, riding %in% 1:2 & year <= 2009),
#          family = quasibinomial)
# summary(pre_05_09) 
# 
# 
# step1 <- yrbs03 %>% 
#   select_(.dots = c(core.vars, "qn30")) %>% 
#   plyr::rename(c("qn30" = "currsmok")) %>% 
#   mutate(YEAR = "2003") %>% 
#   select(YEAR, everything())
# head(step1)
# 
# oldname <- "qn30"
# newname <- "currsmok"
# http://stackoverflow.com/questions/30562819/error-message-when-running-simple-rename-function-in-r
# step1a <- yrbs03 %>% 
#   select_(.dots = c(core.vars, oldname)) %>% 
#   plyr::rename(c(oldname = newname)) %>% 
#   mutate(YEAR = "2003") %>% 
#   select(YEAR, everything())
# head(step1a)




# step1a <- yrbs03 %>% 
#   select_(.dots = c(core.vars, "qn30")) %>% 
#   rename_(.dots = paste0("qn30", "=", "currsmok") ) %>% 
#   mutate(YEAR = "2003") %>% 
#   select(YEAR, everything())
# head(step1a)
# 
# 
# 
# yr <- "2003"
# 
# chk1 <- get(paste0("yrbs", substr(yr, 3, 4))) %>% 
#   select_(.dots = c(core.vars, oldname)) %>% 
#   plyr::rename(c(oldname = newname)) %>% 
#   mutate(YEAR = yr) %>% 
#   select(YEAR, everything()) 
#  
# head(chk1)
#   
# 
# 
# f1 <- function(yr, newname, oldname) {
#   # assemble data set showing year, common name for variable across years,
#   #  psu, weight, strata
#   get(paste0("yrbs", substr(yr, 3, 4))) %>% 
#     select_(.dots = c(core.vars, oldname)) %>% 
#     rename_(newname = oldname) %>% 
#     mutate(YEAR = yr) %>% 
#     select(YEAR, everything())
#   }
# step1 <- f1("2003", "currsmok", "qn30")
# step2 <- f1("2005", "currsmok", "qn30")
# looks good!
# STOP HERE ---------------------------------------------------------------




# brfs.all <- 
#   bind_rows(
#     nh11brfs %>% select_(.dots = c(var.list, "RACE2")),
#     nh12brfs %>% select_(.dots = c(var.list, "RACE2")),
#     nh13brfs %>% select_(.dots = c(var.list, "X.RACE")),
#     nh14brfs %>% select_(.dots = c(var.list, "X.RACE"))
#     ) %>% 
#   mutate(
#     IYEAR = factor(ifelse(SURVYEAR == "2015", "2014", SURVYEAR)),
#     race = ifelse(SURVYEAR %in% c("2011", "2012"), RACE2, X.RACE),
#     race = mapvalues(race, 1:8, c(1, 2, 3, rep(4, 4), 5)),
#     race = factor(race, 1:5, race5.lbl)
#     ) %>% 
#   data.frame()
# 
# brfs.all %>% 
#   group_by(race) %>% 
#   summarize(wt=sum(X.NHWT))
# sum(brfs.all$X.NHWT)
# 
# 
# summary(brfs.all$race)
# rm(nh11brfs, nh12brfs, nh13brfs, nh14brfs); gc()
# # str(brfs.all)
# 
# nh.svy <- svydesign(id = ~0, strata = ~X.STSTR + IYEAR, weights = ~X.NHWT,
#                     data = brfs.all)
# # with CDC weights
# nh2.svy <- svydesign(id = ~0, strata = ~X.STSTR + IYEAR, weights = ~X.LLCPWT,
#                     data = brfs.all)
# 
# rm(brfs.all, var.list); gc()
# 
# getPrev <- function(svyvar, outcome, design = nh.svy){
#   # get level corresponding to outcome
#   b1 <- with(with(design, variables), levels(get(svyvar)))
#   b1 <- b1[grepl(outcome, b1)]
#   svyby(make.formula(svyvar), ~race, design, svymean, na.rm = TRUE,
#         vartype = c("ci")) %>% 
#     select(race, contains(outcome, ignore.case = FALSE)) %>% 
#     mutate_each(funs(round(100 * ., 1)), -race) %>% 
#     inner_join(
#       svyby(make.formula(svyvar), ~race, design, unwtd.count, 
#             keep.var = FALSE), .,
#       by = "race") %>% 
#     mutate(Outcome = b1)  %>% 
#     select(c(6, 1:5)) %>% 
#     setNames(c("Outcome", "Race", "N", "Pct", "LCI", "UCI")) %>% 
#     mutate(Reliable = ifelse((UCI - LCI)/2 <= 10, "", "*"))
#   }  
# 
# a1 <- getPrev("X.RFSMOK3", "Current")
# a2 <- getPrev("DIABETE3", "Told")
# a3 <- getPrev("X.BMI5CAT", "Obese")
# a4 <- getPrev("phys.hlth", "14")


# 
# getPrev <- function(svyvar, outcome){
#   svyby(make.formula(svyvar), ~race, nh.svy, svymean, na.rm = TRUE,
#         vartype = c("ci")) %>% 
#     select(race, contains(outcome, ignore.case = FALSE)) %>% 
#     mutate_each(funs(round(100 * ., 1)), -race) %>% 
#     inner_join(
#       svyby(make.formula(svyvar), ~race, nh.svy, unwtd.count, 
#             keep.var = FALSE), .,
#       by = "race") %>% 
#     mutate(Variable = svyvar)  %>% 
#     select(c(6, 1:5)) %>% 
#     setNames(c("Variable", "Race", "N", "Pct", "LCI", "UCI"))
#   }  
# 
# a1 <- getPrev("X.RFSMOK3", "Current")
# a2 <- getPrev("DIABETE3", "Told")
# a3 <- getPrev("X.BMI5CAT", "Obese")
# 
# b1 <- with(nh.svy$variables, levels(X.RFSMOK3))
# b1 <- with(with(nh.svy, variables), 
#            levels(X.RFSMOK3))
# outcome <- "X.BMI5CAT"
# b1 <- with(with(nh.svy, variables), 
#            levels(eval(parse(text = "X.RFSMOK3"))))
# 
# svyvar <- "X.BMI5CAT"
# svyvar <- "X.RFSMOK3"
# b1 <- with(with(nh.svy, variables), 
#            levels(eval(parse(text = svyvar))))
# b1[grepl("Current", b1)]

# a1 <- svyby(~X.RFSMOK3, ~IYEAR, nh2.svy, svymean, na.rm = TRUE)

library(srvyr)
y15 <- yrbs15 %>% 
  select(psu, stratum, weight = ov_wgt, qn40) %>% 
  # mutate(qn40 = factor(qn40, 1:2, c("Yes", "No"))) %>% 
  as_survey_design(weight = weight, strata = stratum, nest = TRUE) %>%
  summarize(pct = survey_mean(qn40 == 1, na.rm = TRUE, vartype = "ci"))
  

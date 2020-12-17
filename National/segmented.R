# reproducing results from 
# Muggeo, V.M.R. (2008) Segmented: an R package to fit regression models with 
#   broken-line relationships. R News 8/1, 20-25.
# https://cran.r-project.org/doc/Rnews/Rnews_2008-1.pdf

library(dplyr)
library(ggplot2)
library(segmented)
data("down")

# data are in "successes/trials" format--add logits of Down birth for each age
down <- down %>% 
  mutate(p = cases/births,
         logit = log(p / (1 - p))) %>% 
  select(-p)

# fit initial model
fit.glm <- 
  glm(cases/births ~ age, weight = births, family = binomial, data = down)
# fit segmented model with initial guess for joinpoint
fit.seg <- segmented(fit.glm, seg.Z = ~age, psi = 25)
# get the joinpoint
jp <- as.vector(fit.seg$psi[, "Est."])

# data frame of predicted logits 
df <- 
  data.frame(age = down$age, 
             logit = predict(fit.seg, type = "link")) %>% 
  # add the joinpoint; may not be a value occurring in the data
  bind_rows(data.frame(age = jp, 
                       logit = predict(fit.seg, data.frame(age = jp)))) %>% 
  arrange(age)

# this reproduces Muggeo's Figure 1 (using "fit.seg" model only)
# plot fitted segmented model on logit scale with data points
ggplot(df, aes(x = age, y = logit)) + 
  geom_line() +
  # add original data points on logit scale
  geom_point(data = down, aes(x = age, y = logit)) +
  # polish
  scale_x_continuous(breaks = seq(15, 50, 5)) +
  labs(x = "Mother's age", y = "logit(cases/births)") +
  theme_bw()

# other references
# https://rpubs.com/MarkusLoew/12164
# http://stackoverflow.com/questions/13487625/overlaying-two-graphs-using-ggplot2-in-r
# http://stackoverflow.com/questions/33164639/how-to-use-ggplot2-to-plot-results-from-segmented-package
# https://www.r-bloggers.com/using-segmented-regression-to-analyse-world-record-running-times/
# https://github.com/vaibhavdobriyal/R_Tutorials/wiki/Piecewise-or-Segmented-Regression



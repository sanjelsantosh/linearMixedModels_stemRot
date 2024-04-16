# this code runs linear mixed effects model using inoculate field trial data
# it is a hierarchical data 

# load R data (sr_audpc.RData )
load("data/sr_audpc.RData")

# variables
summary(dta)

# load needed libraries for LMM
library(lmerTest)
library(emmeans)
library(multcomp)
library(multcompView)
library(performance)
library(ggResidpanel)
library(dplyr)


# Fit linear mixed effect model: start with the most elaborate one
# fixed effects -> Variety (main-plot) and trt-inoculum (sub-plot)
# random effect -> complicated due to the nesting (year, location, replication, mainplot and subplot)

model1 <- lmer(sAudpc_cumSyInc ~ variety*trt  + 
                 (1 | year) + 
                 (1 | year:location) +
                 (1 | year:location:block) +
                 (1 | year:location:block:variety), data = dta)

# this model has singularity issue probably due to too complicated random effect structure
summary(model1)
# since the random effect "year:location:block" could not be calculated (see the variance = 0) 
# remove problematic random component

#-------------------------------------------------------------------------------
model2 <- lmer(sAudpc_cumSyInc ~ variety*trt  + 
                 (1 | year) + 
                 (1 | year:location) +
                 (1 | year:location:block:variety), data = dta) 

# no singularity issue; better model
# summary and anova
summary(model2)
ggResidpanel::resid_panel(model2, plots = "default")
# residual plot has a pattern; index plot is showing positive bias on residuals
# QQ plot not ideal

# model performance
performance::model_performance(model2)
# poor R-squared (marg.) = 0.097 but high R-sq (cond.) = 0.538


# Mean separation
# pairwise mean separation; slicing by variety
# table 
model2 |> 
  emmeans(pairwise ~ trt | variety,
          lmer.df = "kenward-roger", 
          adjust = "tukey") |> 
  cld(Letters = letters, decreasing = TRUE) 


# figure
model2 |> 
  emmeans(pairwise ~ trt | variety,
          lmer.df = "kenward-roger", 
          adjust = "tukey") |> 
  cld(Letters = letters, decreasing = TRUE) |> 
  plot()


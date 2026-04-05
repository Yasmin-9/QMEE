library(DHARMa)
library(dplyr)
library(performance)
library(effects)

## BMB: ideally you would have *one* cleanup script, not copy it
##  to different places (although this does make individual assignments
##  less free-standing)

## this isn't quite right, I had to move some stuff around
##  (you're still saving the output to the A6 directory ...)

# Load RDS object 
df_clean <- readRDS("Assignment_7/data/clean_dry_eye_dataset.rds")



# For my model, I'll be performing a multi-parameter interaction model. 

# Git a model with Dry Eye Disease as the predictor 
glm_model <- glm(Dry_Eye_Disease ~ Average_screen_time * Sleep_duration,
                 data=df_clean,
                 family= binomial)
 
# Plotting diagnostic plot 
check_model(glm_model)

# Discussion: 
# The posterior predictive check shows no discrepancy between observed and model-predicted data, 
# the glm is not over or under-predicting the prevalence of Dry Eye Disease 
# No influential observations can be detected, as all points lie within Cook's distance
# The binned residual plot show that most points are within the error points, only a minority of the points are out of the error bounds
# Looking at the Colinearity plot, it appears that average_screen_time has high colinearity while sleep_duration is moderate
# Such as observed colinearity is in fact expected for an interaction model such as this one 
# Lastly, the residual quantiles follow the 1:1 line very closely
# Overall, The diagnostic plots confirm that the model is well-specified with no concerns of influential observations

# Inference
summary(glm_model)
# Based on the summary statistics, there's no clear evidence showing screen time or sleep duration independently predicting Dry Eye Disease 
# Similarly, no clear evidence that screen time depends on sleep duration. 
# The model is unable to explain the variation as evidenced by the null and residual deviance being very similar
# Overall, the daya does not clearly support the hypothesis that screen time or sleep duration meaningfully affect the odds of Dry Eye Disease, neither independently or interactively. 

## BMB: can you say anything about the *magnitudes* of the effects/CIs? ('not clear' could mean a noisy observation/experiment
## or a small effect)

# Plotting the interaction 
plot(allEffects(glm_model))
# From the effects plot, the lines for the separate plots are nearly flat,
# indicating that screen time does not substantially change the predicated probability of dry eye disease 
# This is in agreement with what the summary statistics previously showed: there's no clear effect of screen time on dry eye disease detected with this dataset/model
# This trend holds across different sleeping duration, only for 8-10 hours of sleep is there a slight increase 
# However, the confidence interval bands appear quite large
# Overall, the effects plot corroborate what the summary statistics have demonstrated. 

## BMB: again, what can you say about these magnitudes? Are these small changes?


# Check overdispersion
check_overdispersion(glm_model)
# No overdispersion detected
## BMB: overdispersion check unnecessary for binary data

## mark: 2


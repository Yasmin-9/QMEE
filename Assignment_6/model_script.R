library(DHARMa)

# Load RDS object 
df_clean <- readRDS("Assignment_6/data/clean_dry_eye_dataset.rds")



# For my model, I'll be performing a multi-parameter interaction model. 

# Git a model with Dry Eye Disease as the predictor 
glm_model <- glm(Dry_Eye_Disease ~ Average_screen_time * Sleep_duration,
                 data=df_clean,
                 family= binomial)

# From the dataset documentation, Average_screen_time and Sleep_duration are both in hours
# Possible limitation: it's not clear that the Sleep_duration is an average over a period of time (if so what is it) or a single point measure
# Similarly, the documentation does not clarify what's the period of time the Average_screen_time was calculated over

# Plot diagnostic plots using base R
plot(glm_model, id.n=4)

# Interpreting the diagnostic plots:

# The residual vs. fitted plot shows two bands which is expected for binary outcome/response variable 
# (the Y cases would have positive residuals while the N cases would have negative residuals )
# The horizontal line at 0 is a good indication for a linear relationship 

# The Q-Q residuals shows a deviation from the normality - which is typically observed for a linear model) that is expected for a GLM. 

# In the Scale-location plot,t he two bands observed, again, arise from the binary nature of the response variable.
# The residuals are clustered and don't show a pronounced widening pattern resembling a funnel,
# So the spread appears to be constant across the fitted values
# Therefore, The plots don't show strong heteroscedasticity but further exploration is warranted as this is a glm

# Lastly, for the Residuals vs. Leverage plot, there are no obvious high-influence points (so no points with unusually high leverage or residual)
# There's a uniform low levarge and no points exceed Cook's distance. 

# Using base R to run diagnostics of the model appears to give sensible results, but since this is a not a regular linear model so there's a lot of nuance
# So, I will also try using DHARMa to run diagnostics

# Using DHARMa to diagnose the model 
plot(simulateResiduals(glm_model))

# From the DHARMa residual plots, the the QQ plot shows no clear KS test, dispersion or outliers 
# For the residual vs. predicted plot, because of size of the dataset (20,000), the scatter plot was replaced by graphics::smoothScatter. 
# Nonetheless, it still appears that there are no clear deviations. 
# I used this discussion here as a reference: https://github.com/florianhartig/DHARMa/issues/396

# Still, I subset the data (exactly 2000) and see if the diagnostic plots are more informative 
df_subset <- df_clean %>%
  sample_n(2000,replace = FALSE)

# Run the model on the subset on the subset df
glm_subset <- glm(Dry_Eye_Disease ~ Average_screen_time * Sleep_duration,
                            data=df_subset,
                            family= binomial)

# Run diagnostics on the new model 
plot(simulateResiduals(glm_subset))

# QQ plot residuals still, of course, shows no clear deviations
# The residual vs. predicted plot shows the dotted lines and solid lines at the quantiles overlapping with no clear deviations. 
# No outliers noted either

# Overall, based on the inital diagnostics and the additional DHARMa analysis, there is no clear indication of a misspecification. 
# Using a logistic-linear model appears to be adequate for the predictors selected




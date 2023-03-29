##################
### Hometask 4 ###
##################

### In this task, we will check if assumption that residuals are i.i.d (independent and identically distributed) holds.
### Read Chapter 6.2 of Monogan's III cook, fill the blank spots in the code, and interpret the results.

library(tidyverse)                       # load tidyverse library
getwd()                                  # check the working directory
region <- read.csv2("~/0_HSE_study/нис/5/regional_data.csv") # Load the data
glimpse(region)                          # check the data

# Build a model explaining Turnout in 2011 by the following set of predictors:
# democracy_index  = Petrov-Titkov regional democratization index, 2013
# urbanpopulation = share of urban populaiton in the region, % of regional population, 2011
# internet = internet penetration, % of regional population, 2011
# reg_grp = regional GRP, thousands rub., 2011
# protest_events = aggregated count of protest events in the region, 2097-2012
# ethnic = region's ethnic status

model <- lm(turnout_2011 ~ democracy_index + urbanpopulation + internet + reg_grp + protest_events + ethnic, data = region)
# For which variables we reject the null hypothesis of no relations (beta = 0) with the outcome?
# Check the summary of the model, interpret the coefficients.
summary(model)

##############
### ANSWER ###
##############
#
# We can reject H0 for: democracy_index, internet, reg_grp and ethnic variables.
#
# Interpretation of coefficients:
# increase in democracy index in the region by 1 percent is associated with an average decrease in turnout by 0.8702 percent all things being equal 
# increase in internet penetration in the region by 1 percent is associated with an average decrease in turnout by 0.2489 percent all things being equal
# increase in regional GRP in the region by 1 percent is associated with an average increase in turnout by 3.214e-06 percent all things being equal
# being an ethnic region is associated with an average increase in turnout by 9.175 percent all things being equal

# Check the linearity condition. Which variables should be transformed?
# Check the linearity condition. Which variables should be transformed?
region_pairs <- region[ , c("reg_ur2011", "democracy_index", "urbanpopulation", "internet", "reg_grp", "protest_events")]
pairs(region_pairs, pch = 19, lower.panel = NULL) 

##############
### ANSWER ###
##############
#
# reg_grp and protest_events

##############
### ANSWER ###
##############

# You can access residuals from the model fitted values via $.
summary(model$residuals)

# Check the histogram of the residuals, does it look normal?
hist(model$residuals)

##############
### ANSWER ###
##############
#
# It kind of does, however with a visible diproportion at the right side (tails are not even)

# One way to assess if residuals are i.i.d. is to plot residuals vs. fitted. Use base R to plot it.
plot(model$fitted.values, model$residuals, xlab = "Fitted Values", ylab = "Residuals")

# Can you see any pattern in the plot? Write down the answer.

##############
### ANSWER ###
##############
#
# The pattern is clearly seen - a line from left-up to right-bottom corners, therefore we can say that residuals show that it is not normally dist

# You can run four diagnostic plots simply putting your model in plot() function:
plot(model) # a sequence of four diagnostic plots

# Fitted vs. residuals, we expect the residuals to be evenly distributed across fitted values
# Normal Q-Q plot, we expect observations to follow the theoretical line
# Fitted vs. standardized residuals, we expect the red line to be horizontal
# Leverage vs. standardized residuals, no observation should exceed Cook's distance

# Can you see any pattern in Residuals vs. Fitter plot? In Normal Q-Q plot?
##############
### ANSWER ###
##############
#
# In this case most of the observations are on the line except the cases of lower values (76, 78) and higher (13).
# 
#

# What observations exhibit high leverage (i.e. cross Cook's distance)? Use their numbers to check out the regions' names
region[76, "Region"] # Moscow
region[71, "Region"] # Tyumen

# Formal test for homoskedasticity:
#install.packages("lmtest")
library(lmtest)
# Perform Breusch-Pagan test with null hypothesis of residuals are homoskedastic.
# The null hypothesis is the absence of heteroskedasticity.
bptest(model)

# What does the p-value tell you?
##############
### ANSWER ###
##############
#
# p-value = 0.0004415 < 0.05 -> can reject H0 -> there is heteroskedasticity

# You can adjust for heteroskedasticity using several methods.
# Adjust for heteroskedasticity via robust (heteroskedasticity-consistent, HC) standard errors:
#install.packages("sandwich")
library(sandwich)
coeftest(model, vcov = vcovHC(model, type = "HC3"))
# Which variable(s) are no longer statistically significant?
##############
### ANSWER ###
##############
#
# reg_grp is no longer statistically significant


# You can access directly the HC errors using sjPLot:
library(sjPlot)
tab_model(model, vcov.fun = "HC3", show.se = TRUE)


# Log-transform the variables that violate linearity and use them instead of original variables, use log() function inside lm().
model_log <- lm(turnout_2011 ~ democracy_index + urbanpopulation + internet + log(reg_grp+1) + log(protest_events + 1 ) + ethnic, data = region)

# Run Breusch-Pagan Test:
bptest(model_log)

# Are the residuals homoskedastic?
##############
### ANSWER ###
##############
#
# p-value = 0.0005411 < 0.05 -> can reject H0 -> heteroscedasticity is present

# Run diagnostic plots.
plot(model_log)

# Are there any remaining outliers?
##############
### ANSWER ###
##############
#
# 76 - Moscow

# Adjust for heteroskedasticity via HC3 standard errors (actually, you can try several types):
coeftest(model_log, vcov = vcovHC(model_log, type = "HC3"))

# Which variables remained statistically significant?
##############
### ANSWER ###
##############
#
# democracy_index, internet, log(reg_grp+1) and ethnic variables

# Now we are more certain about our results, but let's consider removing some suspicous observations.
# First, let's check what are the regions with turnout above 80%:
region %>% filter(turnout_2011 >= 80) %>% select(Region)

# What features unite these regions?
##############
### ANSWER ###
##############
#
# they are ethnic

# Let's remove observations with high turnout using dplyr():
region_clean <- region %>% filter(turnout_2011 < 80)

# Take a look at the cleaned dataset, how many observations remained?
glimpse(region_clean)
##############
### ANSWER ###
##############
#
# 74 remained

#Use the following code to standardize the variables:
# Standardize the variables (log-transform GRP and protests).
region$democracy_centered <- as.vector(scale(region$democracy_index))
region$urbanpopulation_centered <- as.vector(scale(region$urbanpopulation))
region$internet_centered <- as.vector(scale(region$internet))
region$log_grp_centered <- as.vector(scale(log(region$reg_grp)))
region$log_protest_centered <- as.vector(scale(log(region$protest_events+1)))

# Build the same model using the cleaned dataset and standardized variables:
model_centered <- lm(turnout_2011 ~ democracy_centered + urbanpopulation_centered + internet_centered + log_grp_centered + log_protest_centered + ethnic, data = region)

# Print model summary with robust SEs:
summary(model_centered)
tab_model(model_centered, vcov.fun = "HC3", show.se = TRUE)

# Interpret the final model substantially. What would you do to increase the turnout for federal elections?

##############
### ANSWER ###
##############
#
# democracy_centered, internet_centered, log_grp_centered, ethnic are statistically significant factors, p-value of the model is also acceptable
# Adjusted R-squared is about 60%
#
# The biggest effect among given variables is ethnic effect - in this case encouraging the population growth in ethnic regions is worth looking at as the more people in ethnic regions the bigger turnout overall will be
# Then the effect of log grp centered goes, increasing regional GRP might also increase the turnout
# After it with a negative effect internet centered goes. Minimising internet penetration might be a way of increasing turnout
# Finally with the biggest negative effect democracy centered goes. Making regions less democratic might increase the turnout

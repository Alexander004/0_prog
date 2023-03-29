##################
### Hometask 2 ###
##################

### We continue to work with regional_data dataset.
library(tidyverse)                       # load tidyverse library
getwd()                                  # check the working directory (you should put your data here)
region <- read.csv2("regional_data.csv") # load the data, user other function like read.csv() or read.delim() if this funciton does not work
glimpse(region)                          # check the data

# We will work with UR votes share in 2011 as an outcome variable (reg_ur2011)
# Check the summary:
summary(region$reg_ur2011)

# We will work with the following variables:
# democracy_index  = Petrov-Titkov regional democratization index, 2011
# urbanpopulation = share of urban populaiton in the region, % of regional population, 2011
# internet = internet penetration, % of regional population, 2011
# reg_mincome = regional median income, in rubles, 2011
# reg_alcbeer = beer consumption, liter per 1000 inhabitants, 2011
# protest_events = aggregated count of protest events in the region, 2007-2012

# Build a scatterplot with reg_ur2011 as a DV and democracy_index as IV. Add linear approximation.  Is the relation linear? Comment below with argumentation.
ggplot(region, aes(x = democracy_index, y = reg_ur2011 )) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  theme_bw()

##############
### ANSWER ###
##############
###
### The relationship is negative linear.
### From the visual point of view it means that dots are located in a certain linear pattern.

# Build a scatterplot with reg_ur2011 as a DV and urbanpopulation as IV. Check for linearity. Comment below with argumentation.
ggplot(region, aes(x = urbanpopulation, y = reg_ur2011 )) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  theme_bw()

##############
### ANSWER ###
##############
###
### The relationship is negative linear.
###

# Build a scatterplot with reg_ur2011 as a DV and internet as IV. Check for linearity. Comment below with argumentation.
ggplot(region, aes(x = internet, y = reg_ur2011 )) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  theme_bw()

##############
### ANSWER ###
##############
###
### In this case I would say that the relation is still negative linear. 
### However, it is rather weak as the majority of cases are located in the square 40-60x30-70 and there are around 10 different cases.

# Build a scatterplot with reg_ur2011 as a DV and reg_mincome as IV. Check for linearity. Comment below with argumentation.
ggplot(region, aes(x = reg_mincome, y = reg_ur2011 )) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  theme_bw()

##############
### ANSWER ###
##############
###
### There is no linear relationship in this case. Absolute majority of cases are located on one line at around 20000 income
### which gives us little to no information about the dependency at all.

# Build a scatterplot with reg_ur2011 as a DV and reg_alcbeer as IV. Check for linearity. Comment below with argumentation.
ggplot(region, aes(x = reg_alcbeer, y = reg_ur2011 )) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  theme_bw()

##############
### ANSWER ###
##############
###
### The relationship is linear negative.
### Cases definitely have negative pattern.

# Build a scatterplot with reg_ur2011 as a DV and protest_events as IV. Check for linearity. Comment below with argumentation.
ggplot(region, aes(x = protest_events, y = reg_ur2011 )) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  theme_bw()

##############
### ANSWER ###
##############
###
### No linear relationship.
### All except 3 cases are on the one vertical line and close to 0. Basically 0 protest actions can get a ur result from 30% to 100%

# Summarise: where do you see the linear relations? What variables you thank can explain the variation in the outcome variable?

##############
### ANSWER ###
##############
###
### I see linear relation in the following cases:
### democracy index, urban population, internet and beer consumption.

# Build a model with all predictors above:
summary(lm(reg_ur2011 ~ democracy_index + urbanpopulation + internet + reg_mincome + reg_alcbeer + protest_events, data = region))
# Interpret the model: what are the statistically significant variables?

##############
### ANSWER ###
##############
###
###  Only statistically significant variables are "democracy_index" and "internet"
###

# Write down the substantial interpretation for each significant predictor.

##############
### ANSWER ###
##############
###
### decrease in democracy index in the region by 1.23 percent will make an average increase of UR results by 1 percent all things being equal 
### and decrease in internet penetration in the region by 3 percent will make an average increase of UR results by 1 percent all things being equal
###

# Exclude the predictors without linear relations with the outcome variable.
summary(lm(reg_ur2011 ~ democracy_index + urbanpopulation + reg_alcbeer + internet, data = region))

# How did the exclusion of non-linear predictors change the model? Pay attention to R-squared.

##############
### ANSWER ###
##############
###
### The Adjusted R-squared decreased from 0.5527to 0.5451 therefore the model became slightly less accurate.
### Though decrease by less than 1% is not so significant.

# Use natural log transformation for protest_events
summary(lm(reg_ur2011 ~ democracy_index + urbanpopulation + reg_alcbeer + internet + log(protest_events+1), data = region))

# What changes do you observe?

##############
### ANSWER ###
##############
###
### Firstly, the log(protest_events + 1) became a statistically significant variable with p-value of 0.0135. But 'internet''s p-value increased slightly upper the limit to 0.0502 making it statistically insignificant.
### Secondly, the Adjusted R-squared increased up to 0.5749 (by 2,9%) which tells us that the model became more accurate. 
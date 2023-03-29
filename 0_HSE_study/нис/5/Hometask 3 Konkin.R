##################
### Hometask 3 ###
##################

### We continue to work with regional_data dataset.
library(tidyverse)                       # load tidyverse library
getwd()                                  # check the working directory (you should put your data here)
region <- read.csv2("0_HSE_study/нис/5/regional_data.csv") # load the data, user other function like read.csv() or read.delim() if this funciton does not work
glimpse(region)                          # check the data

# Codebook:
# democracy_index  = Petrov-Titkov regional democratization index, 2013
# urbanpopulation = share of urban populaiton in the region, % of regional population, 2011
# internet = internet penetration, % of regional population, 2011
# transfers = gross transfers from the federation, in mln. rubles, 2011
# reg_disttomoscow = distcnce to Moscow, in km.
# reg_tempjan = average temperature in January, Celsius, 2011
# reg_kdshare = percent of KPRF votes in 2011 State Duma elections
# reg_ur2011 = percent of UR votes in 2011 State Duma elections
# turnout_2011 = turnout in 2011 State Duma elections
# turnout_2016 = turnout in 2016 State Duma elections
# reg_mincome = regional median income, in rubles, 2011
# reg_levelofunempl = level of unemployment, % of regional population, 2011
# reg_heductoempd = share of population with high education, % of regional population
# reg_alcvodka = vodka consumption, liter per 1000 inhabitants, 2011
# reg_alcbeer = beer consumption, liter per 1000 inhabitants, 2011
# reg_mobile = mobile network users, thousands, 2011
# reg_grp = regional GRP, thousands rub., 2011
# protest_events = aggregated count of protest events in the region, 2097-2012
# ethnic = region's ethnic status

# Suppose we want to build a model like:
model <- lm(reg_ur2011 ~ democracy_index + urbanpopulation + internet + protest_events + reg_disttomoscow + reg_alcbeer + reg_grp, data = region)

# Check the summary of the model:
summary(model)

# Write a short summary of the results: what variables are significant and what is the direction.

##############
### ANSWER ###
##############
#
# Based on the output of the summary function, we can make the following conclusions:
#
# The democracy index variable is statistically significant with a p-value of 0.0000144 and a negative coefficient estimate, indicating that as the democracy index increases, the UR result in the region tends to decrease.
#
# The internet variable is statistically significant with a p-value of 0.0109 and a negative coefficient estimate, indicating that as the availability of the internet increases, the UR result in the region tends to decrease.
#
# The reg_grp variable is statistically significant with a p-value of 0.0239 and a positive coefficient estimate, indicating that as the regional GRP increases, the UR result tends to increase as well.
#
# The adjusted R-squared value is 0.576, indicating that the model explains 57.6% of the variance in the response variable while accounting for the number of variables in the model.

# Some variables (protest_events, reg_disttomoscow, reg_grp) are not distributed normally.
# Visualize their distribution using density plots:

region %>% ggplot(aes(x = protest_events)) + 
  geom_density() + 
  theme_bw()

# Now, build scatterplots with reg_ur2011 as an outcome variable:

region %>% ggplot(aes(x = democracy_index, y = reg_ur2011)) + 
  geom_point() + 
  theme_bw()


# Is there linearity on any of these plots? Check it out using linear approximation

region %>% ggplot(aes(x = democracy_index, y = reg_ur2011)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()

### In democracy_index we can see lineriarity

region %>% ggplot(aes(x = protest_events, y = reg_ur2011)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()

### In protest_eventsthere is no lineriarity


# Take natural log of these three variables, add 1 if necessary:
region <- region %>%
  mutate(log_dist = log(reg_disttomoscow + 1),
         log_protest = log(protest_events + 1),
         log_grp = log(reg_grp + 1))

# Build a bivariate model with reg_ur2011 as a DV and log distance to Moscow as an IV:
model_m <- lm(reg_ur2011 ~ log_dist, data = region)
summary(model_m)

# Is there a statistical significance?

### No, it is still not statistically significant

# Repeat the same for log protest events and log GRP:
summary(lm(reg_ur2011 ~ log_protest, data = region))
summary(lm(reg_ur2011 ~ log_grp, data = region))

### They both became statistically significant

# Interpret the results for log protests and log GRP for 10% increment in the IV:

# A 10% increase in the log of protest events is associated with an average decrease of 26% in the UR result, all things being equal (p-value = 0.0387).
# A 10% increase in the log of regional GRP is associated with an average decrease of 42% in the UR result, all things being equal (p-value = 0.00966).

# Use log-transformed variables to repeat the model from line 33:
model1 <- lm(reg_ur2011 ~ democracy_index + urbanpopulation + internet + log_protest + log_dist + reg_alcbeer + log_grp, data = region)


# Compare the results from both models useing tab_model function in sjPlot:
#install.packages("sjPlot")
library(sjPlot)
tab_model(model1, model, show.se=F)

# What are the differences between the models? Write down 2-3 sentences.

##############
### ANSWER ###
##############
#
# such variables as democracy index and internet remained statistically significant
# both log grp and reg grp variables are statistically significant in represented models
# the R^2 adjasted in the new model is slightly higher stating that the accuracy increased

# Lastly, suppose that we assume an interaction between democracy level and KPRF party vote share affecting UR 2011 votes share.
# Suppose we believe that the stronger KPRF is in the region, the more pronounced negatve effect of demoracy on DV becomes.
summary(lm(reg_ur2011 ~ democracy_index*reg_kdshare, data = region))

# What does the summary show us? Does it confirm our hypothesis?

##############
### ANSWER ###
##############
#
# The coefficient for the interaction term is positive and statistically 
# significant, suggesting that the negative effect of democracy index on UR 2011 
# vote share is more pronounced in regions with stronger KPRF support. 
# This result is consistent with the hypothesis that KPRF support may increase the 
# negative effects of democracy index on UR support in the region.

# Run the following code and try to understand the resulting plot:
# Use margins library to profile the marginal effects plot for the effects of democracy on UR 2016 vote share within a range ov internet penetration values.
#install.packages("margins")
require(margins)
model <- lm(reg_ur2011 ~ democracy_index*reg_kdshare, data = region)
margins.model <- cplot(model, dx = "democracy_index", x = "reg_kdshare", what = "effect", draw=F)
ggplot(data = margins.model, aes(x = xvals, y = yvals)) + 
  geom_line(color = "red") + 
  geom_ribbon(alpha = .2, aes(ymin = lower, ymax = upper)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(title = "Marginal Effect of Democracy Index on UR Share 2011 as a Function of KPRF Vote Share",
       subtitle = "Marginal Effect of Democracy Index Across Range of KPRF Vote Share",
       y = "Estimated Effect of DI on UR % (BetaX)",
       x = "KPRF Vote Share", 
       colour = "") +
  theme_bw()

# Build your own interaction model with UR 2011 Vote Share as a DV, interpret the results.

summary(lm(reg_ur2011 ~ democracy_index*urbanpopulation, data = region))

# the relation is indeed statistically significant

model_x <- lm(reg_ur2011 ~ democracy_index*urbanpopulation, data = region)
margins.model <- cplot(model_x, dx = "democracy_index", x = "urbanpopulation", what = "effect", draw=F)
ggplot(data = margins.model, aes(x = xvals, y = yvals)) + 
  geom_line(color = "blue") + 
  geom_ribbon(alpha = .2, aes(ymin = lower, ymax = upper)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(title = "Marginal Effect of Democracy Index on UR Share 2011 as a Function of Share of urban populaiton in the region",
       subtitle = "Marginal Effect of Democracy Index Across Range of Share of urban populaiton in the region",
       y = "Estimated Effect of DI on UR % (BetaX)",
       x = "Share of urban populaiton in the region", 
       colour = "") +
  theme_bw()

##############
### ANSWER ###
##############
#
# The coefficient for the interaction term is positive and statistically 
# significant, suggesting that the negative effect of democracy index on UR 2011 
# vote share is more pronounced in regions with bigger share of urban population. 
# This result is consistent with the hypothesis that hihger share of urban 
# population in the region may increase the negative effects of democracy index on UR support in the region.

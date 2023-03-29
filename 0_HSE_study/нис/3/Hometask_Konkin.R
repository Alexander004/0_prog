####################
### R Hometask 1 ###
####################

### Make sure to install tidyverse packages for dplyr and ggplot operations.
library(tidyverse)                       # load tidyverse library
getwd()                                  # check the working directory
region <- read.csv2("Data/regional_data.csv") # load the data
glimpse(region)                          # check the data

### In this task, you will have to expand the analysis from the previous seminar. 
### Use the replication code to produce plots and statistics.

### Last time, we explained UR's vote share in 2016. Let's try to explain the varition in UR's support in previous elections.
# Check the summary of the DV (reg_ur2011): 
summary(region$reg_ur2011)

# Plot the histogram using ggplot, set number of bins to 50:
ggplot(region, aes(reg_ur2011)) +   
  geom_histogram(bins = 50) +       
  theme_bw() +                     
  labs(x = "State Duma Elections Turnout in 2011 by Region", 
       y = "Frequency", caption = "Source: CIK RF" ) 

hist(region$reg_ur2016, breaks = 25, main = "Votes for UR 2016")
hist(region$reg_ur2011, breaks = 50, main = "Votes for UR 2011")

##############
### ANSWER ###
##############
#
# In both cases we see anomalies (like 100% votes). However, in 2011 after the clear peak at around 40%
# the votes were decreasing more or less evenly. In 2016 on the other hand the peak have moved to the right 
# and a lot of values are now between 40% and 60%. Therefore the share of votes for UR increased in 2016
# rather dramatically. 
#

# Do you see any difference from 2016 elections? Write down 2-3 sentences.

# Use density plot, add geom_rug():
ggplot(region, aes(reg_ur2011)) +   
  geom_density() +       
  geom_rug() +
  theme_bw() +                     
  labs(x = "State Duma Elections Turnout in 2011 by Region", 
       y = "Frequency", caption = "Source: CIK RF" ) 

# Plot differences in means in 2011 turnout by ethnic status of regions.
# Calculate the means:
region %>%                              
  group_by(ethnic) %>%                
  summarise(mean = mean(reg_ur2011))  
# You can check the global mean with summary()
summary(region$reg_ur2011) # it is 49.16

ggplot(region, aes(reg_ur2011, fill = factor(ethnic))) + 
  geom_density(alpha = 0.6) +
  theme_bw() +
  geom_vline(xintercept = mean(region$reg_ur2011)) +    
  geom_vline(xintercept = c(42.4, 64.0), col = "red") + 
  labs(x = "State Duma Elections UR results in 2011 by Region", fill = "Ethnic status")

ggplot(region, aes(turnout_2016, fill = factor(ethnic))) + # by "fill - ()" argument we tell ggplot how to divide your sample into subgroups
  geom_density(alpha = 0.6) + # alpha argument allows you to change the transparency of densities
  theme_bw() +
  geom_vline(xintercept = mean(region$reg_ur2016)) + # plots vertical line at the mean of the distribution
  geom_vline(xintercept = c(44.5, 63.1), col = "red") + # plots vertical lines at the means of subsamples
  labs(x = "State Duma Elections UR results in 2016 by Region", fill = "Ethnic status") # "fill" argument here tells ggplot to change the legend's titile


# Use linear model to check if the difference in means in significant:
summary(lm(reg_ur2011 ~ factor(ethnic), data=region)) 
summary(lm(reg_ur2016 ~ factor(ethnic), data=region)) 
# Based on these results, can we safely conclude that the ethnic status has an effect on UR's vote share in 2011 elections as well?
# Compare this effect with 2016 elections. What migth explin the difference?

##############
### ANSWER ###
##############
#
# 2011
# Indeed the results we got can be named statistically significant as P-value is way smaller than 0.05
# 
# Coefficients:
#
#                     Estimate  Std.Error   t value   Pr(>|t|)    
#   (Intercept)       42.378      1.808     23.44     < 2e-16 ***
#   factor(ethnic)1   21.641      3.230      6.70     2.54e-09 ***
#
# 2016
#
#
# Coefficients:
#                     Estimate  Std. Error t value    Pr(>|t|)    
#   (Intercept)       0.47111    0.01683  27.994      < 2e-16 ***
#   factor(ethnic)1   0.13748    0.03007   4.572      1.71e-05 ***
#
#
#  Basically the results we got in 2011 have smaller P-value than ones in 2016. And also we can look at our
#  estimated results graph to say that in 2016 the difference between ethnic and non ethnic regions was bigger
#  than in 2016. The estimated result in non ethnic regions increased by 5% which can be explained Crimean
#  effect and smaller participation of the opposition in the elections. Personally, I imagined a higher 
#  increase but maybe low turnout made the 2016 very much invisible to the broader population. 
#  What concerns ethnic increase in 2011 it was 21,6% while in 2016 it was 13,7%. The influence of results from
#  ethnic regions played bigger role in 2011. I personally think that it is because the level of support for
#  UR in non ethnic regions in 2011 was much smaller than the results created in ethnic. While in 2016 the
#  support for UR among non ethnic regions increased and the ethnic factor was a smaller.

##########################
### Code for Seminar 3 ###
##########################
install.packages("tidyverse")
### This R script reproduces the figures and estimates for QPS Lecture 2. 
### Feel free to familiarize with the dataset, we will use it in the future.
### Make sure to install tidyverse packages for dplyr and ggplot operations.
library(tidyverse)                       # load tidyverse library
getwd()                                  # check the working directory
region <- read.csv2("C:/Users/Alex/OneDrive/Документы/0_HSE_study/нис/3/Data/regional_data.csv") # Load the data
glimpse(region)                          # check the data

# Analyse the univariate distribution for Turnout 2016 variable (turnout)
# This is a base R histogram plot
hist(region$turnout_2016)

# You can manually set the number of bins for histogram, i.e. increase or decrease
# Try the next line of code and notice the difference.
hist(region$turnout_2016, breaks = 25, main = "Turnout 2016")

# The same historgam using ggplot requires a bit of an effort but comes with flexibility:
ggplot(region, aes(turnout_2016)) + # First, define your dataset ("region"), then tell ggplot what variable(s) you want to plot insisde aes() argument 
  geom_histogram(bins = 50) +       # Set the geomtery ("histogram") and the number of bins
  theme_bw() +                      # Choose alternative theme
  labs(x = "State Duma Elections Turnout in 2016 by Region", 
       y = "Frequency", caption = "Source: CIK RF" ) # Change axis titles and add a caption

# ggplot is an extremely powerful tool for plotting the data, you can use alternative geometry and add other layers
ggplot(region, aes(turnout_2016)) + 
  geom_density() + # uses density plot instead of histogram
  geom_rug() +     # adds number of observations at the bottom of the plot
  theme_bw() + 
  labs(x = "State Duma Elections Turnout in 2016 by Region", caption = "Source: CIK RF" )

# Compare the distribution of 2016 turnout to 2011 elections 
ggplot(region, aes(turnout_2011)) + geom_density() + geom_rug() + theme_bw() + labs(x = "State Duma Elections Turnout in 2011 by Region", caption = "Source: CIK RF" )

# PLot differences in means in 2016 turnout by ethcni status of regions:
ggplot(region, aes(turnout_2016, fill = factor(ethnic))) + # by "fill - ()" argument we tell ggplot how to divide your sample into subgroups
  geom_density(alpha = 0.6) + # alpha argument allows you to change the transparency of densities
  theme_bw() +
  geom_vline(xintercept = mean(region$turnout_2016)) + # plots vertical line at the mean of the distribution
  geom_vline(xintercept = c(44.5, 63.1), col = "red") + # plots vertical lines at the means of subsamples
  labs(x = "State Duma Elections Turnout in 2016 by Region", fill = "Ethnic status") # "fill" argument here tells ggplot to change the legend's titile

# Use dplyr to calculate the subsamples' means. Note the pipe operator (%>%).
region %>% 
  group_by(factor(ethnic)) %>%         # groups the data into subgroups
  summarise(mean = mean(turnout_2016)) # caluclates means for subgroups

# You can add other statistics to the output. Note that you can do that sequantially.
region %>% 
  group_by(factor(ethnic)) %>%        
  summarise(n = n(), mean = mean(turnout_2016), sd = sd(turnout_2016), se = sd/sqrt(n))

# Conduct t-test for differences in means
with(region, t.test(turnout_2016 ~ factor(ethnic)))

# Conduct linear regression for ethnic status effect on turnout in 2016
summary(lm(turnout_2016 ~ factor(ethnic), data=region))

# Compare the turnout by the level of media freedom (media2017)
region$media2017 <- factor(region$media2017)
ggplot(region, aes(turnout_2016, fill = factor(media2017))) + geom_density(alpha = 0.6) + theme_bw() + geom_vline(xintercept = mean(region$turnout_2016)) + labs(x = "State Duma Elections Turnout in 2016 by Region", fill = "Media Freedom") # Not very informtaive
with(region, boxplot(turnout_2016 ~ media2017, xlab = "Media Fredom Rank in 2017", ylab = "Turnout 2016"))
abline(h = mean(region$turnout_2016), col="red") # add mean value for the entire sample

# ANOVA for differences in means between the levels
model <- aov(turnout_2016 ~ factor(media2017), data = region)
summary(model)									# overall assessment of the model
TukeyHSD(model, conf.level=0.95)	# significance between particular levels
model.tables(model, type="effects")  # the individual effects for each group

# Linear regression for the effects of media freedom on turnout
summary(lm(turnout_2016 ~ factor(media2017), data=region))

# Note the difference between the ANOVA and LM output. What does significant effects mean in the latter?

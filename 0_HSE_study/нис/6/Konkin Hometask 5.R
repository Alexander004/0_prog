######################
### RS Hometask 5 ###
######################
# In this hometask, we will work with public opinion data from the World Values Survey -- a large-scale project aiming to capture public attitudes across the entire world.
# You can find more information about the project on its website: http://www.worldvaluessurvey.org/
# You can also explore the data (including online) here: https://www.worldvaluessurvey.org/WVSContents.jsp

library(tidyverse)  # for ggplot2 and dplyr
library(readxl)     # for working with .xlsx format

# WVS data come in waves, we will work with the most recent (2017-2022) round.
# Upload the data on the share of respondents who answered that they recently attended lawful/peaceful demonstrations.
# It is Q211.- Political action: Attending lawful/peaceful demonstrations in the data
data_wvs <- read_excel("~/0_HSE_study/нис/6/Data/wvs_demonstrations_2017.xlsx")

# Take a look at the data:
glimpse(data_wvs)

# The "demonstrators_share" variable should be numeric, transform it if need be:
data_wvs$demonstrators_share <- as.numeric(as.character(data_wvs$demonstrators_share))

# Explore the distribution of the share of demonstrators in the country (use histogram, density plot, and summary statistics). What can you infer?
library(ggplot2)
library(dplyr)
# Histogram
data_wvs %>% ggplot(aes(x = demonstrators_share)) +
  geom_histogram(binwidth = 0.02, color = "black", fill = "blue") +
  theme_bw() +
  labs(x = "Share of respondents", y = "Number of countries", title = "Share of respondents claiming that they attended peaceful demonstrations recently", caption = "Source: WVS 2017")

# Density plot
ggplot(data_wvs, aes(x = demonstrators_share)) +
  geom_density(color = "blue") +
  theme_bw() +
  labs(x = "Share of respondents", y = "Density", title = "Share of respondents claiming that they attended peaceful demonstrations recently", caption = "Source: WVS 2017")

# Summary
summary(data_wvs$demonstrators_share)

# Min.    1st Qu. Median  Mean    3rd Qu. Max. 
# 0.400   8.575   12.000  14.062  19.025  40.800 

# Write a summary of your thoughts regarding the DV.

#The distribution is not normally distributed in the left side of the graph with values under 20 but the right tail makes it not normal

# Now, we turn to explanatory variables, we will obtain the data from World Development Indicators maintened by the World Bank.
# Check out the documentation and data here https://data.worldbank.org/
# Check out the documentation for R-package here https://www.r-project.org/nosvn/pandoc/WDI.html
#
library(WDI)

# We will work with following indicators, which will use as independent variables:
# SP.POP.TOTL = Population, total
# NY.GDP.PCAP.PP.KD = GDP per capita, PPP (constant 2017 international $)
# SE.TER.ENRR = School enrollment, tertiary (% gross)
# IT.NET.USER.ZS = Individuals using the Internet (% of population)
# SP.DYN.LE00.IN = Life expectancy at birth, total (years)

# Create a vector of names:
indicators <- c("SP.POP.TOTL", "NY.GDP.PCAP.PP.KD", "SE.TER.ENRR", "IT.NET.USER.ZS", "SP.DYN.LE00.IN")

# Download the data for 2017:
data_wdi = WDI(indicator=indicators, start=2017, end=2017)

# Check out the data:
glimpse(data_wdi)

# Merge WDI with WVS data using left_join() function in dplyr:
data <- left_join(data_wvs, data_wdi, by = "country")

# Check out the bivariate relations between the demonstrators_share and each IV above:

summary(lm(demonstrators_share ~ `SP.POP.TOTL` , data = data))
#SP.POP.TOTL -9.160e-09  5.870e-09   -1.56    0.123    
#Multiple R-squared:  0.03186,	Adjusted R-squared:  0.01877 

summary(lm(demonstrators_share ~ `NY.GDP.PCAP.PP.KD` , data = data))
#NY.GDP.PCAP.PP.KD 1.845e-04  4.742e-05    3.89 0.000219 ***
#Multiple R-squared:  0.1717,	Adjusted R-squared:  0.1603 

summary(lm(demonstrators_share ~ `SE.TER.ENRR` , data = data))
#SE.TER.ENRR  0.08685    0.04234   2.051 0.044464 * 
#Multiple R-squared:  0.06356,	Adjusted R-squared:  0.04845 

summary(lm(demonstrators_share ~ `IT.NET.USER.ZS` , data = data))
#IT.NET.USER.ZS  0.12953    0.04247   3.050  0.00319 **
#Multiple R-squared:  0.113,	Adjusted R-squared:  0.1009 

summary(lm(demonstrators_share ~ `SP.DYN.LE00.IN` , data = data))
#SP.DYN.LE00.IN   0.4870     0.1589   3.066  0.00304 **
#Multiple R-squared:  0.1141,	Adjusted R-squared:  0.1019 

# Think about log-transformation for your IVs: what are the obvious candidates?
# Probably the SP.POP.TOTL = Population, total should be a candidate for log-transformation as it is the only non-significant variable

# Build a basic linear model (without transformations), check the results
model <- lm(demonstrators_share ~ `SP.POP.TOTL` + `NY.GDP.PCAP.PP.KD` + `SE.TER.ENRR` + `IT.NET.USER.ZS` + `SP.DYN.LE00.IN`, data = data)
summary(model)

#SP.POP.TOTL       -6.938e-09  5.935e-09  -1.169   0.2472  
#NY.GDP.PCAP.PP.KD  1.581e-04  9.024e-05   1.752   0.0851 .
#SE.TER.ENRR       -1.435e-02  6.289e-02  -0.228   0.8203  
#IT.NET.USER.ZS    -4.588e-02  9.487e-02  -0.484   0.6304  
#SP.DYN.LE00.IN     2.600e-01  3.323e-01   0.782   0.4371  

# No results so far, let's try to log-transform population size and life expectancy.

summary(lm(demonstrators_share ~ log(`SP.POP.TOTL` + 1), data = data))
#log(SP.POP.TOTL + 1)  -0.7314     0.5722  -1.278  0.20514      
#Multiple R-squared:  0.0216,	Adjusted R-squared:  0.008383 

summary(lm(demonstrators_share ~ log(`SP.DYN.LE00.IN` + 1) , data = data))
#log(SP.DYN.LE00.IN + 1)    32.08      11.62   2.759  0.00732 **
#Multiple R-squared:  0.09445,	Adjusted R-squared:  0.08204

# Check the resulting bivariate relations with the DV, what worked?

# Nothing changed actually

# Build a model with log-transformed variables
model <- lm(demonstrators_share ~ log(`SP.POP.TOTL`+1) + `NY.GDP.PCAP.PP.KD` + `SE.TER.ENRR` + `IT.NET.USER.ZS` + log(`SP.DYN.LE00.IN` +1), data = data)
summary(model)
#log(SP.POP.TOTL + 1)     1.663e-01  6.700e-01   0.248    0.805  
#NY.GDP.PCAP.PP.KD        1.636e-04  9.120e-05   1.794    0.078 .
#SE.TER.ENRR             -2.417e-03  6.375e-02  -0.038    0.970  
#IT.NET.USER.ZS          -9.169e-03  9.724e-02  -0.094    0.925  
#log(SP.DYN.LE00.IN + 1)  7.570e+00  2.292e+01   0.330    0.742
#
# Still nothing

# The SEs are still large, let's identify an (obvious) outlier, which lies beyond Cook's distance:
plot(model)

#71 is Singapore

# Remove the obvious outlier from the sample:
data_clean <- data %>% filter(!country == "Singapore")

# Re-fit the model above with the new data:
model <- lm(demonstrators_share ~ log(`SP.POP.TOTL`+1) + `NY.GDP.PCAP.PP.KD` + `SE.TER.ENRR` + `IT.NET.USER.ZS` + log(`SP.DYN.LE00.IN` +1), data = data_clean)
summary(model)

#log(SP.POP.TOTL + 1)    -2.153e-01  6.010e-01  -0.358  0.72148    
#NY.GDP.PCAP.PP.KD        4.031e-04  9.968e-05   4.044  0.00016 ***
#SE.TER.ENRR             -1.163e-02  5.654e-02  -0.206  0.83773    
#IT.NET.USER.ZS          -1.446e-01  9.227e-02  -1.567  0.12274    
#log(SP.DYN.LE00.IN + 1)  4.205e+00  2.033e+01   0.207  0.83687  

# What variable(s) becomes significant? Write down the answer
# NY.GDP.PCAP.PP.KD = GDP per capita, PPP (constant 2017 international $) became significant

# You can increase the interpretability of this variable by dividing the current value by 1000, and re-fit the model:
data_clean <- data_clean %>% mutate(NY.GDP.PCAP.PP.KD = NY.GDP.PCAP.PP.KD/1000)
model <- lm(demonstrators_share ~ log(`SP.POP.TOTL`+1) + `NY.GDP.PCAP.PP.KD` + `SE.TER.ENRR` + `IT.NET.USER.ZS` + log(`SP.DYN.LE00.IN` +1), data = data_clean)
summary(model)

# Check for heteroskedasticity using diagnostic plots:
plot(model)

# It kind of resembles a straight line however with an arc at 10-15

# Use HC3 SEs instead of default:
library(sjPlot)
tab_model(model, vcov.fun = "HC3")

# Check out for multicollinearity in the data:
library(corrplot)
mc_check <- data %>% select(SP.POP.TOTL, NY.GDP.PCAP.PP.KD, SE.TER.ENRR, IT.NET.USER.ZS, SP.DYN.LE00.IN)

# Log-transform the population size and GDP per capita variables:
mc_check$SP.POP.TOTL <- log(mc_check$SP.POP.TOTL)
mc_check$SP.DYN.LE00.IN <- log(mc_check$SP.DYN.LE00.IN)
colnames(mc_check) <- c("Log Population", "GDP pc 2017", "Education", "Internet Users", "Life Expectancy")
mat <- cor(mc_check, use = "complete.obs")
corrplot(mat, method = "number", type = "lower", order = "FPC")

# What is the pair of variables with the strongest correlation?
# Life expectancy - Internet Users are the strongest at the rate of 0.8

# Use variance inflation factor to identify the variable, which has the strongest impact on inflating the variance of coefficients in your current model:
library(car)
vif(model)

# Remove the variable with the largest VIF and re-fit the model:
data_clean <- data_clean %>% select(-IT.NET.USER.ZS)
model <- lm(demonstrators_share ~ log(`SP.POP.TOTL`+1) + `NY.GDP.PCAP.PP.KD` + `SE.TER.ENRR` + log(`SP.DYN.LE00.IN` +1), data = data_clean)
summary(model)

# Check the summary of this model with robust SEs:
tab_model(model, vcov.fun = "HC3")

# Anything changed?

# Nothing changed, GDP per capita, PPP (constant 2017 international $) remains the only significant variable

# Finally, let's explore the OVB.
# Fit the model with only GDP per capita variable.
model_2 <- lm(demonstrators_share ~ NY.GDP.PCAP.PP.KD , data = data_clean)
summary(model_2)
# Suppose, population size is an omitted variable here, add it to the model and compare the results.
model_3 <- lm(demonstrators_share ~ NY.GDP.PCAP.PP.KD + SP.DYN.LE00.IN, data = data_clean)
summary(model_3)

tab_model(model_2,model_3,  vcov.fun = "HC3")

# Does population size cause omitted variable bias for GRP per capita?
# Not really, nothing really changed, gdp remained significant

# In the main model with five predictors above, what else can cause OVB? Share your thoughts.
# Many variables like index of democracy or freedom of economy as they might correlate with gdp
# Load necessary libraries
library(plm)
library(readstata13)
library(dplyr)
library(RStata)
library(haven)
library(lmtest)
library(sandwich)
library(stargazer)


options("RStata.StataPath" = "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp")
options("RStata.StataVersion" = 18)

#load the data 
setwd("/Users/thomas/Downloads/ma675/consulting project/china/data")
#data <- read.dta13("citypanel_base.dta")
# load "DO14_process_before_main" 
stata('do "/Users/thomas/Downloads/ma675/consulting project/china/data/DO14_process_before_main.do"', echo = TRUE)
data <- read.dta13("/Users/thomas/Downloads/ma675/consulting project/china/data/processed_data.dta", nonint.factors = TRUE)




# Define global variables
# Create a vector with names of variables that start with certain patterns
# These might be vectors or lists of column names depending on what start*_loggdp represents
econ <- grep("start.*_loggdp|start.*_logpop|start.*_loginvest|dep|startm*gdpidx", names(data), value = TRUE)
career <- grep("msec_female|mayor_female|msec_edu|mayor_edu|mayorage|msecage|m*in1|m*in3|m*in5", names(data), value = TRUE)

# Subset and modify the data
data <- data %>%
  mutate(samp = as.numeric(year >= 2000))
data <- subset(data, provid != 540000)
fe <- ~ year + provid + year:provid

# Set panel data structure
pdata <- pdata.frame(data, index = c("cityid", "year"))

# A.4 Models
#m1
# Assuming df is your data frame and the variables are correctly defined
# Filter the data for samp == 1
data_A4 <- data %>% filter(samp == 1)

# Fit the fixed effects model
m1 <- plm(gdpidx ~ bin_mleader2currentsec, 
          data = data_m1, 
          model = "within", 
          index = c("cityid", "year"))

# Clustered standard errors
m1_se <- coeftest(m1, vcovHC(m1, type = "HC1", cluster = "group", cluster.id = data_A4$cityid))

#m2
# Fit the fixed effects model with economic variables
m2 <- plm(gdpidx ~ bin_mleader2currentsec + econ, 
          data = data_A4, 
          model = "within", 
          index = c("cityid", "year"))

# Clustered standard errors
m2_se <- coeftest(m2, vcovHC(m2, type = "HC1", cluster = "group", cluster.id = data_A4$cityid))




#A.5 Models
data_A5 <- data %>%
  mutate(fusheng = as.numeric(citylevel == "副省级城市"))
#m1
# Combine all variables into a formula string
formula_str <- paste("gdpidx ~ bin_mleader2currentsec +", paste(c(econ, career, "year", "provid"), collapse = " + "))
model_formula <- as.formula(formula_str)

# Fit the model with fixed effects
m1 <- plm(model_formula,
          data = data_A5,
          model = "within",
          index = c("cityid", "year"))
# Clustered standard errors
m1_se <- coeftest(m1, vcovHC(m1, type = "HC1", cluster = "group", cluster.id = data_A5$cityid))

#m2
# Filter the data for the condition auto == 0
data_m2 <- data %>%
  filter(samp == 1 & auto == 0)

# Fit the model with fixed effects
m2 <- plm(model_formula,
          data = data_m2,
          model = "within",
          index = c("cityid", "year"))

# Clustered standard errors
m2_se <- coeftest(m2, vcovHC(m2, type = "HC1", cluster = "group", cluster.id = data_m2$cityid))


#m3
# Filter the data for the condition east == 1
data_m3 <- data %>%
  filter(samp == 1 & east == 1)

# Fit the model with fixed effects
m3 <- plm(model_formula,
          data = data_m3,
          model = "within",
          index = c("cityid", "year"))

# Clustered standard errors
m3_se <- coeftest(m3, vcovHC(m3, type = "HC1", cluster = "group", cluster.id = data_m3$cityid))


#m4
# Filter the data for the condition east == 0
data_m4 <- data %>%
  filter(samp == 1 & east == 0)

# Fit the model with fixed effects
m4 <- plm(model_formula,
          data = data_m4,
          model = "within",
          index = c("cityid", "year"))

# Clustered standard errors
m4_se <- coeftest(m4, vcovHC(m4, type = "HC1", cluster = "group", cluster.id = data_m4$cityid))

#combined
stargazer(m1, m2, m3, m4, type = "text",
          se = list(sqrt(diag(vcovHC(m1))),
                    sqrt(diag(vcovHC(m2))),
                    sqrt(diag(vcovHC(m3))),
                    sqrt(diag(vcovHC(m4)))))

library(dplyr)
library(Hmisc)
library(haven)
library(plm)
library(labelled)

# Set working directory and Load Data
setwd("/Users/thomas/Downloads/ma675/consulting project/china/data")
data <- read_dta("citypanel_base.dta")

# Load the econ_panel dataset
econ_panel <- read_dta("econ_panel.dta")

# Creating new variables
data <- data %>%
  mutate(msec_female = msec_sex == "女",
         mayor_female = mayor_sex == "女")

# Merge with other city covariates
# Assuming 'cityid' and 'year' are the columns to merge on in both data frames
data <- merge(data, econ_panel, by = c("cityid", "year"), all.x = TRUE)

# Dropping _merge (if it exists)
#data <- select(data, -_merge)

# Labeling variables (Example)
data <- labelled::set_variable_labels(data,
                                      msec2currentsec = "City secretary connected to prov sec",
                                      mayor2currentsec = "Mayor connected to prov sec")

# Creating and labeling variables
data <- data %>%
  mutate(mleader2currentsec = msec2currentsec + mayor2currentsec,
         bin_mleader2currentsec = as.integer(mleader2currentsec > 0),
         bin_mleader2currentsec2 = as.integer(msec2currentsec2 == 1 | mayor2currentsec2 == 1),
         bin_mleader2currentgvn = as.integer(msec2currentgvn == 1 | mayor2currentgvn == 1),
         bin_mleader2pleader = as.integer(bin_mleader2currentsec == 1 | bin_mleader2currentgvn == 1))

# Adding labels
label(data$bin_mleader2currentsec) <- "Connected to prov sec"
label(data$bin_mleader2currentsec2) <- "Connected to prov sec (include promotions under governor)"
label(data$bin_mleader2currentgvn) <- "Connection to governor"
label(data$bin_mleader2pleader) <- "Connected to prov sec or governor"


pdata <- pdata.frame(data, index = c("cityid", "year"))
pdata <- pdata %>% select(-c(prior1, prior2, prior3, prior4, prior5, post1, post2, post3, post4, conn1, conn2, conn3))



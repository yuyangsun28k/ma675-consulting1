setwd("/Users/thomas/Downloads/ma675/consulting project/china/data")


library(haven)
data <- read_dta("validation_data.dta")

library(dplyr)
data <- data %>%
  filter(leadertype == "m") %>%
  mutate(
    name_id = as.numeric(as.factor(name)),
    college = edu %in% c("本科", "硕士", "博士", "博士后"),
    minor = ethnicity != "汉族",
    agesq = age^2,
    agecb = age^3,
    firstage = min(age, na.rm = TRUE),  # Assuming by 'name' needs special handling
    agepct3 = ntile(firstage, 3),  # Assuming 'firstage' is pre-calculated for each 'name'
    agepct4 = ntile(firstage, 4)
  )

no_values <- c("1", "_op")
for (i in no_values) {
  data[[paste0("conn2currenttop", i)]] <- data[[paste0("conn2currentsc", i)]] + data[[paste0("conn2currentpb", i)]]
  data[[paste0("bin_conn2currenttop", i)]] <- (data[[paste0("conn2currentsc", i)]] > 0) | (data[[paste0("conn2currentpb", i)]] > 0)
}

# bin_conn2currentsc[i] - "Connected to current PSC members"
# bin_conn2currentpb[i] - "Connected to current PB members"
# bin_conn2currentsc_op - "Connected to current PSC members (work overlap)"
# bin_conn2currentpb_op - "Connected to current PB members (work overlap)"

data <- data %>%
  group_by(name) %>%
  mutate(maxirank = max(rank, na.rm = TRUE)) %>%
  ungroup()

data <- data %>%
  mutate(retire2 = case_when(
    maxirank <= 2 & !is.na(age) ~ as.integer(age > 60),
    maxirank == 2.5 & !is.na(age) ~ as.integer(age > 63),
    maxirank == 3 & !is.na(age) ~ as.integer(age > 65),
    maxirank >= 3.5 & !is.na(age) ~ as.integer(age > 67),
    TRUE ~ NA_integer_
  ))

data <- data %>%
  mutate(rk = as.numeric(factor(rank)))

data <- data %>%
  mutate(female = (sex == "女"))

# retire2 - "Retired"
# female - "Female"
# minor - "Ethnic minority"
# age - "Age"
# agesq - "Age$^2$"
# agecb - "Age$^3$"
# bin_conn2currenttop1 - "Connected to current PSC/PB members(1=yes)"
# firstage - "First age"

data <- data %>%
  filter(age <= 75 | is.na(age))

library(tidyr)
data <- data %>%
  arrange(name_id, year) %>%
  group_by(name) %>%
  mutate(expid = row_number()) %>%
  ungroup()

library(survival)
data$c_samp <- with(data, as.integer(anticorruption == 1))
surv_obj_anticorruption <- with(data, Surv(expid, c_samp))

data$p_samp <- with(data, as.integer(pro2vp_trueprize == 1))
surv_obj_promotion <- with(data, Surv(expid, p_samp))

library(splines)
data <- data %>%
  mutate(nsp_ = ns(expid, df=3))

data <- data %>%
  mutate(firstprov = as.numeric(factor(firstprovince)) - 1)

# age - "Age"
# agesq - "Age\(^2\)"

# bin_conn2currenttop_op - "Patrons became Politburo members (overlap-based measure)"
# bin_conn2currenttop1 - "Patrons became Politburo members (promotion-based measure)"

# bin_conn2currentpb1 - "Patrons became Politburo members (promotion-based)"
# bin_conn2currentsc1 - "Patrons became Standing Committee members (promotion-based)"

# bin_conn2currentpb_op - "Patrons became Politburo members (overlap-based)"
# bin_conn2currentsc_op - "Patrons became Standing Committee members (overlap-based)"

# conn2targets_op - "Patrons became target of investigation (overlap-based)"
# conn2targets - "Patrons became target of investigation (promotion-based)"

############################Analysis#####################################
#######################Table A.1: Promotion##############################

m1 <- glm(pro2vp_trueprize ~ poly(year, 3) + nsp_[, "1"] + nsp_[, "2"] + nsp_[, "3"] + factor(firstprov) + bin_conn2currenttop1,
          data = data, family = binomial())
summary(m1)
# Calculate robust standard errors, clustering by 'name'
library(sandwich)
library(lmtest)
m1_robust_se <- coeftest(m1, vcov = vcovCL(m1, cluster = ~ name))

# Store the model and its robust standard errors together in a list
m1_results <- list(
  model = m1, 
  robust_se = m1_robust_se,
  dura = "✔", 
  pfe = "✔",
  nperson = length(unique(data$name)) 
)
print(m1_results)

m2 <- glm(pro2vp_trueprize ~ poly(year, 3) + nsp_[, "1"] + nsp_[, "2"] + nsp_[, "3"] + factor(firstprov) + 
            minor + female + college + age + I(age^2) + bin_conn2currenttop1, 
          data = data, family = binomial())

m2_robust_se <- coeftest(m2, vcov = vcovCL(m2, cluster = ~ name))

m2_results <- list(
  model = m2, 
  robust_se = m2_robust_se,
  dura = "✔", 
  pfe = "✔",
  nperson = length(unique(data$name)) 
)
print(m2_results)


library(tidyverse)
library(haven)
library(ggplot2)
china_data <- read_dta(file = "fes_china.dta")

# Add in log variables for later use

china_data_logged = china_data %>%
  mutate(ln_share_food = log(food/totalexpenditures),
         ln_share_clothing = log(clothing/totalexpenditures),
         ln_share_housing = log(houserent/totalexpenditures),
         ln_exp = log(totalexpenditures),
         ln_fam_size = log(totalfamilymembers))

china_data_add = china_data %>%
  mutate(ln_food = log(food),
         ln_clothing = log(clothing),
         ln_housing = log(houserent),
         ln_exp = log(totalexpenditures),
         ln_fam_size = log(totalfamilymembers))

# Seperate chinese data into 4 subsets

shanghai = subset(china_data_add, factory==0 | factory==1 | factory==2)

peiping = subset(china_data_add, factory==3)

### Find Elasticities

## shanghai

# food
lm_shanghai_food = lm(ln_food ~ ln_exp+ln_fam_size,shanghai)
plot(ln_food ~ ln_exp, main = "Shanghai Food", data=shanghai)
abline(lm_shanghai_food, col="blue")
summary(lm_shanghai_food)

# clothing
lm_shanghai_clothing = lm(ln_clothing ~ ln_exp+ln_fam_size,shanghai)
plot(ln_clothing ~ ln_exp, main = "Shanghai Clothing", data=shanghai)
abline(lm_shanghai_clothing, col="blue")
summary(lm_shanghai_clothing)

# housing
lm_shanghai_housing = lm(ln_housing ~ ln_exp+ln_fam_size,shanghai)
plot(ln_housing ~ ln_exp, main = "Shanghai Housing", data=shanghai)
abline(lm_shanghai_housing, col="blue")
summary(lm_shanghai_housing)

## peiping

# food
lm_peiping_food = lm(ln_food ~ ln_exp,peiping)
plot(ln_food ~ ln_exp, main = "Peiping Food", data=peiping)
abline(lm_peiping_food, col="blue")
summary(lm_peiping_food)

# clothing
lm_peiping_clothing = lm(ln_clothing ~ ln_exp,peiping)
plot(ln_clothing ~ ln_exp, main = "Peiping Clothing", data=peiping)
abline(lm_peiping_clothing, col="blue")
summary(lm_peiping_clothing)

# housing
lm_peiping_housing = lm(ln_housing ~ ln_exp,peiping)
plot(ln_housing ~ ln_exp, main = "Peiping Housing", data=peiping)
abline(lm_peiping_housing, col="blue")
summary(lm_peiping_housing)



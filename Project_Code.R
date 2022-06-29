POLS 702 - Advanced Techniques of Political Science Research 
Spring 2022 - University of Wisconsin - Milwaukee 

Original study/code is from Potter & Tavits (2015) 

The goal of this project this project was to replicate a published study by 
using different techniques to obtain the same result. 



# Install Packages ---------------------------------------------------------

library(foreign)
library(MASS)
library(dplyr)
library(haven)
library(tidyverse)
#library(readr) if any issues uploading data 
library(arm)
library(car)
library(AER)
library(WDI) # World Data Index; used for GDP data 


# Upload Data -------------------------------------------------------------

dat <- read_dta("data/potter_tavits_data.dta")

vdem <- read_rds("data/V-Dem-CY-Core-v12.rds")


# Drop Statistical Outliers -----------------------------------------------

## Liberia 2012 (113), Albania 2003 (1), and Brazil 2003 (31) 

new_data <- dat %>% slice(-c(1,31,113))


# Create new subsets of democratic years ----------------------------------

Post_1974 <- new_data %>% 
  filter(demin <= 1973)

Pre_1974 <- new_data %>% 
  filter(demin >= 1973)


# OLS model for all democracies (Left side of table 1 in article) --------

## Model fit for all of the countries 
model_1 <- lm(postenp ~ fundparity4 + demyears + fed + pres + log(avemag) + 
                fract + log(avemag):fract, data = new_data)

summary(model_1) ## signs = 0.1*;0.05**;0.01***

display(model_1)


# Table 2: Right side results (p. 83) -------------------------------------

## Model fit for countries following the year 1974

Model_1974 <- lm(postenp ~ fundparity4 + demyears + fed + pres + log(avemag) + 
                   fract + log(avemag):fract, data = Post_1974)	
summary(Model_1974)

display(Model_1974)


# Plot - Fig. #1  (p. 87) --------------------------------------------------

ggplot(new_data, aes(x = preenp, y = fundparity4)) + 
  geom_point() + 
  geom_smooth(se = FALSE, method = lm) + 
  labs(title = "Figure 1", x = "Previous Effective Number of Parties (ENP)", 
       y = "Fund parity")


# Add New Data  -----------------------------------------------------------

tibble(vdem)
range(vdem$year) ## filter 2003 & 2012; to match originally collected data 
## variables select - v2psbars; v2x_polyarchy

vdem_1 <- vdem %>% 
  filter(year == 2003 & 2012) %>% 
  dplyr::select(country_name, v2psbars, v2x_polyarchy) %>%  
  mutate(cnty = country_name, 
         partybar = v2psbars, 
         demindy = v2x_polyarchy) %>% 
  dplyr::select(-c("country_name", "v2psbars", "v2x_polyarchy")) %>% 
  na.omit

tibble(vdem_1)

new_final_dat <- left_join(dat, vdem_1, by = "cnty")

tibble(new_final_dat)

summary(new_final_dat$demindy)

WDIsearch('gdp.*capita.*constant') 
## Using the "GDP per capita (constant 2015 US$)"

wdi_dat <- WDI(indicator = 'NY.GDP.PCAP.KD', country = c('all'), start = 2003, 
               end = 2012)

tibble(wdi_dat)

## Combine the GDP data to all of the new data 

WDI_1 <- wdi_dat %>% 
  filter(year == 2003 & 2012) %>% 
  dplyr::select(country, NY.GDP.PCAP.KD) %>%  
  mutate(cnty = country, 
         GDP = NY.GDP.PCAP.KD) %>% 
  dplyr::select(-c("country", "NY.GDP.PCAP.KD")) %>% 
  na.omit

two_dats <- merge(vdem_1, WDI_1, by.x = 'cnty')

tibble(two_dats)

new_final_dat <- left_join(dat, two_dats, by = "cnty")

tibble(new_final_dat)

tibble(new_final_dat$GDP)

summary(new_final_dat$GDP)


# Creation of New Models with New Variables -------------------------------

## Data being used: new_final_dat 

## Raw model to test OLS assumptions 
raw_model <- lm(postenp ~ fundparity4 + demyears + fed + pres + log(avemag)
                + fract + log(avemag):fract + partybar + demindy + log(GDP),
                data = new_final_dat)

plot(raw_model, which = 4) # Outliers = 1, 90, 113 

new_final_dat[ c(1,90,113), ]

newf_dat <- new_final_dat %>% slice(-c(1,90,113))

newf_dat

## First model is all of the countries 

mod_all <- lm(postenp ~ fundparity4 + demyears + fed + pres + log(avemag)
              + fract + log(avemag):fract + partybar + demindy + log(GDP),
              data = newf_dat)

summary(mod_all)
display(mod_all)

## Second and third model is based on the polity score 

summary(newf_dat$polity) # Average is rounded 
summary(newf_dat$polity >= 8) #Descriptive stats on polity score in data

## Create the subset with the average Polity score 

final_ss <- subset(newf_dat, polity >= 8)

mod_ss_1 <- lm(postenp ~ fundparity4 + demyears + fed + pres + log(avemag)
               + fract + log(avemag):fract + partybar + demindy + log(GDP),
               data = final_ss)

summary(mod_ss_1)
display(mod_ss_1)
















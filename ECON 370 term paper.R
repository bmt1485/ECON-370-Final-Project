rm(list = ls())
cat("\014")
graphics.off()

# Turn off scientific notation
options(scipen = 999)

# Load packages
pacman::p_load(data.table)
library("readxl")
library(gt)
library(dplyr)


# Set seed
set.seed(418518)

# Set working directory 
setwd("C:/Users/ual-laptop/Desktop/Data")

#Extract Data for both questions
data <- read_xlsx("China Data.xlsx", sheet = 1)

library(dplyr)
trimmed_data <- data %>%
  filter( data$GDP > 14005)  ## filtering for high income countries

lm_savings <- lm(Savings ~ GDP + Consumption + HDI + lifeEX + school, data=trimmed_data)
summary(lm_savings)

lm_savings2 <- lm(Savings ~ Consumption, data=trimmed_data)
summary(lm_savings2)


library(ggplot2)
ggplot(trimmed_data, aes(x = Consumption, y = Savings)) +
  geom_point(color = "blue", size = 2) +  # scatter points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # best fit line
  labs(
    title = "Savings vs Consumption",
    subtitle = paste("R² =", round(summary(lm(Savings ~ Consumption, data = trimmed_data))$r.squared, 4)),
    x = "Consumption",
    y = "Savings"
  ) +
  theme_minimal()

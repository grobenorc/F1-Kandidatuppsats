
# Interaktionseffekt gridprop
setwd("C:/Users/claes/OneDrive/Universitet/Statistik Fortsättningskurs/STAH11 Kandidatuppsats/Final_modell")
library(readr)
library(brms)
library(brmsmargins)
library(xtable)
library(kableExtra)
library(tidyverse)
library(knitr)
library(latex2exp)
library(ggthemes)


# Marginal (conditional) effect på startposition
# Hämta den modell som vi väljer att använda, alltså den bästa modellen enligt LOO-results nedan
vilken_modell <- read_rds("fit/loo_results_gridprop.rds")
vilken_modell

fit <- read_rds("fit/fit_circuit_gridprop.rds")

marginal_effect_grid <- plot(conditional_effects(fit)) 
plot1 <- marginal_effect_grid[[1]]
plot_cust <- plot1 + labs(title = "Marginal effect startposition", subtitle = TeX("$\\alpha = 95\\%$"), x = "Slutposition", y = "Startposition") + theme_fivethirtyeight() + theme(axis.title = element_text())
plot_cust
ggsave("img/marginal_effect_grid.png", plot = plot_cust, width = 12, height = 9, bg = "white")


marginal_effect_grid_df <- data_frame(prop_trans = marginal_effect_grid[["gridprop_trans"]][["data"]][["estimate__"]], gridprop_trans = marginal_effect_grid[["gridprop_trans"]][["data"]][["effect1__"]])
h <- 0.001
ave_marginal_effect <- brmsmargins(
  fit,
  add = data.frame(gridprop_trans = c(0, 0 + h )),
  contrasts = cbind("AME Grid" = c(-1 / h, 1 / h)),
  CI = 0.95, CIType = "HDI"
) #https://joshuawiley.com/brmsmargins/articles/fixed-effects-marginaleffects.html#ames-for-logistic-regression

ave_marginal_summary <- ave_marginal_effect$ContrastSummary[, c(1, 2, 3, 4, 7, 8, 11)]
xtable::xtable(ave_marginal_summary, caption = "Genomsnittlig Marginal Effect (M)", label = "tab:ave_marginal_effect")
write_rds(ave_marginal_summary, "fit/ave_marginal_effect.rds")


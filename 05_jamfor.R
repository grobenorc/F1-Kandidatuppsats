setwd("C:/Users/claes/OneDrive/Universitet/Statistik Fortsättningskurs/STAH11 Kandidatuppsats/Final_modell")

library(readr)
library(brms)
library(xtable)
library(bayesplot)
library(ggplot2)
library(Matrix)
library(tidyverse) # Måste ladda Matrix då detta paket annars strular

# Vilken av modellerna är bäst? Detta jämförs med loo enligt nedan (utan gridprop)
fit_basic           <- fit_basic %>% add_criterion("loo")
fit_circuit         <- fit_circuit %>% add_criterion("loo")
fit_vader           <- fit_vader %>% add_criterion("loo")
fit_circuit_vader   <- fit_circuit_vader %>% add_criterion("loo")

loo_results <- loo_compare(
  fit_basic,
  fit_circuit,
  fit_vader,
  fit_circuit_vader,
  model_names = c("Basic", "Circuit", "Weather", "Circuit + Weather")
)

loo_results
write_rds(loo_results, "fit/loo_results.rds")
xtable::xtable(loo_results)

#                    elpd_diff se_diff
# Basic              0.0       0.0
# Circuit           -1.0       0.5
# Weather           -1.2       1.3
# Circuit + Weather -1.9       1.4



# Vilken av modellerna är bäst? Detta jämförs med loo enligt nedan (med gridprop)
fit_basic_gridprop          <- fit_basic_gridprop %>% add_criterion("loo")
fit_circuit_gridprop        <- fit_circuit_gridprop %>% add_criterion("loo")
fit_vader_gridprop          <- fit_vader_gridprop %>% add_criterion("loo")
fit_circuit_vader_gridprop  <- fit_circuit_vader_gridprop %>% add_criterion("loo")

loo_results_gridprop <- loo_compare(
  fit_basic_gridprop,
  fit_circuit_gridprop,
  fit_vader_gridprop,
  fit_circuit_vader_gridprop,
  model_names = c("Basic", "Bantyp", "Väder", "Bantyp + Väder")
)

loo_results_gridprop
write_rds(loo_results_gridprop, "fit/loo_results_gridprop.rds")
xtable::xtable(loo_results_gridprop)


##### Bayes_plot gridar av medelvärde och varians på det två bästa modellerna (utan gridprop) -----

### Utan gridprop
read_rds("fit/loo_results.rds")
#                    elpd_diff se_diff
# Circuit + Weather  0.0       0.0   
# Circuit           -4.3       3.8   
# Weather           -4.4       3.3   
# Basic             -5.8       4.7

fit_vader_circuit <- read_rds("fit/fit_vader_circuit.rds")
fit_circuit       <- read_rds("fit/fit_circuit.rds")


plot_utan_intercept_mean <- bayesplot_grid(
  pp_check(fit_vader_circuit, type = 'stat', stat = 'mean'),
  pp_check(fit_circuit, type = 'stat', stat = 'mean')
  )



plot_utan_intercept_var  <- bayesplot_grid(
  pp_check(fit_vader_circuit, type = 'stat', stat = 'var'),
  pp_check(fit_circuit, type = 'stat', stat = 'var')
)



### Med gridprop
read_rds("fit/loo_results_gridprop.rds")

#                 elpd_diff se_diff
# Bantyp          0.0       0.0   
# Väder          -0.4       5.9   
# Väder + Bantyp -1.7       5.9 

fit_vader_gridprop   <- read_rds("fit/fit_vader_circuit_gridprop.rds")
fit_circuit_gridprop <- read_rds("fit/fit_circuit_gridprop.rds")

win.graph()
plot_med_gridprop_mean <- bayesplot_grid(
  pp_check(fit_vader_gridprop, type = 'stat', stat = 'mean'),
  pp_check(fit_circuit_gridprop, type = 'stat', stat = 'mean')
  )

posterior <- as.matrix(fit_circuit_gridprop)
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 95% intervals")

mc_areas_griprop <- mcmc_areas(posterior,
           prob = 0.95) + plot_title

plot_med_intercept_var <- bayesplot_grid(
  pp_check(fit_basic_gridprop, type = 'stat', stat = 'var'),
  pp_check(fit_circuit_gridprop, type = 'stat', stat = 'var')
  )

# Vi kan alltså se att den modellen med gridprop_trans är den bästa (jämför plottarna mean)


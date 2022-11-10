
setwd("C:/Users/claes/OneDrive/Universitet/Statistik Fortsättningskurs/STAH11 Kandidatuppsats/Test_egen_kval_och_grid")
library(tidyverse)
library(brms)
library(xtable)

options(mc.cores = 12)

##### Inklusive grid men osäker ifall vi verkligen kan ta med den? #####
# which model is best? Compare using LOO (Leave-one-out corss validation, kolla https://mc-stan.org/loo/articles/online-only/faq.html)
fit_basic   <-              read_rds("fit/fit_basic.rds")
fit_circuit <-              read_rds("fit/fit_circuit.rds")
fit_vader <-                read_rds("fit/fit_vader.rds")
fit_vader_circuit <-        read_rds("fit/fit_vader_circuit.rds")
fit_vader_circuit_grid <-   read_rds("fit/fit_vader_circuit_grid.rds")



# Found 103 observations with a pareto_k > 0.7 in model '.'. It is recommended to set 'moment_match = TRUE' in order to perform moment matching for problematic observations. 
# Kolla länk: https://discourse.mc-stan.org/t/improve-model-with-some-observations-pareto-0-7/17500
 

#Den med grid är överlägset bäst men kan vi verkligen ta med den?
loo_results <- loo_compare(
  fit_basic,
  fit_circuit,
  fit_vader,
  fit_vader_circuit,
  fit_vader_circuit_grid,
  model_names = c("Basic", "Circuit", "Väder", "Väder + Circuit", "Väder + Circuit + Grid")
)

#####------

##### Här tar vi inte med modellen inklusive grid #####
fit_basic   <-              read_rds("fit/fit_basic.rds") %>% add_criterion("loo")
fit_circuit <-              read_rds("fit/fit_circuit.rds") %>% add_criterion("loo")
fit_vader <-                read_rds("fit/fit_vader.rds") %>% add_criterion("loo")
fit_vader_circuit <-        read_rds("fit/fit_vader_circuit.rds") %>% add_criterion("loo")

loo_results <- loo_compare(
  fit_basic,
  fit_circuit,
  fit_vader,
  fit_vader_circuit,
  model_names = c("Basic", "Bantyp", "Väder", "Väder + Bantyp")
)
loo_results


write_rds(loo_results, "fit/loo_results.rds")



xtable::xtable(loo_results)

#             elpd_diff     se_diff
#Weather            0.0       0.0   
#Basic             -0.1       2.1   
#Circuit + Weather -0.4       1.2   
#Circuit           -1.9       2.5   
# weather works best

# comparing basic to next best model
bayes_factor(fit_basic, fit_circuit)

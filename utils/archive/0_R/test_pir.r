library(tidyverse)
df_test = readRDS("test/y2025_w28_database_new.RData")

source("utils/fun_calc_pir.R")



df_test = df_test |>
  filter(zone == "NW")

mle = PooledInfRate::pIR(test_code ~ total, data = df_test, pt.method = "firth")
mle$P # 0.0035

mle = PooledInfRate::pIR(test_code ~ total, data = df_test)
mle$P #0.0035

df_tar = df_test |>
  filter(zone == "NW") |>
  filter(spp == "Tarsalis")

mle = PooledInfRate::pIR(test_code ~ total, data = df_tar, pt.method = "firth")
mle$P
# tar = 0.0089

# USING the function
pir_spp = calc_pir(df_test, rm_zone = c("NE", "SE", "SW", "LV", "BE", "BC"))
pir_spp$pir
#pip = 0
#tar = 0.0.009

pir_fun_all = calc_pir(df_test, grp_var = c("zone"), rm_zone = c("NE", "SE", "SW", "LV", "BE", "BC"))

pir_fun_all$pir #0.0035

#the 
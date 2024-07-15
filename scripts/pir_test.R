#run pir_VIR first

dummy_df = data.frame(
  pool = c(1,1:50),
  test_code = c(0, rep(1,50))
) %>%
  nrow()

pooledBin(dummy_df$test_code, dummy_df$pool)


df = data_list %>%
  filter(zone == "BE")

pIR(test_code ~ total, data = df, pt.method = "firth")

df = data.frame(P = mle$P)

plot(df$P)


library(PooledInfRate)
## basic example code
x <- c(1,0)
m <- c(49,1)

pooledBin(x,m)

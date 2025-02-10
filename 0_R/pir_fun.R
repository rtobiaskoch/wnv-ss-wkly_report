

#create a grouping variable for mle
pir_fun = function(df, vars) {
  
  df %>%
  arrange(across(all_of(vars))) %>% #dont split by method because PIR includes gravid traps
  mutate(grp = paste(vars, sep ="-"))

#run pIR
mle = pIR(test_code ~ total|grp, data = df, pt.method = "firth")


#create pIR dataframe
df_pir0 = as.data.frame(mle) %>%
  separate(grp,
           into = vars,
           sep = "-") %>%
  transmute(year = as.integer(year),
            week = as.integer(week),
            zone = zone,
            spp = spp,
            pir = round(P,4),
            pir_lci = round(Lower,4),
            pir_uci = round(Upper,4)
  )
}

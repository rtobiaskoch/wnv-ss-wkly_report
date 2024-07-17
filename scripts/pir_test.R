#run pir_VIR first

#example of NA ERRROR
dummy_df = data.frame(
  grp = c(1,2,3,3,4,5,6,6),
  total = c(1,1,1,1,2,50,2,50),
  test_code = c(0,1,0,1,1,1,1,1)
)


#group
result = pIR(test_code ~ total|grp, data = dummy_df, pt.method = "firth")
df_result = data.frame(
  grp = result$grp,
  p = result$P
)
df_result



#comparison of diff pool sizes with same # of mosquitoes
pool_0 = data.frame(
  grp = 1:49,
  total = 1:49, 
  test_code = 0
)

pool_1 = data.frame(
  grp = 1:49,
  total = 49:1,
  test_code = 1
)

diff_pool = rbind(pool_0, pool_1) %>%
  arrange(grp) %>%
  group_by(grp) %>%
  mutate(p_frac = sum(test_code)/(sum(total))) 

pir_diff_pool = pIR(test_code ~ total|grp, data = diff_pool, pt.method = "firth")

pir_diff_pool = data.frame(
  grp = pir_diff_pool$grp,
  P = pir_diff_pool$P
)

diff_pool = diff_pool %>%
  left_join(pir_diff_pool) %>%
  filter(test_code == 1) %>%
  pivot_longer(cols = c(P, p_frac) , names_to = "y", values_to = "p")

p_2_50 = ggplot(diff_pool, aes(x = total, y = p, color = y)) +
  geom_line() +
  ggtitle("`X` Mosquitoes in (+) Pool in 2 Pools of 50") +
  xlab("Total # Mozzy in Positive Pool") +
  theme_classic()



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Iteratively checking PIR via pool sizes
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#comparison of diff pool sizes with same # of mosquitoes
pool_0 = data.frame(
  grp = 1:49,
  total = 1:1, 
  test_code = 0
)

pool_1 = data.frame(
  grp = 1:49,
  total = 1:49,
  test_code = 1
)

diff_pool0 = rbind(pool_0, pool_1) %>%
  arrange(grp) %>%
  group_by(grp) %>%
  mutate(p_frac = sum(test_code)/(sum(total))) %>%
  filter(grp %in% c(1:2,9:49)) #function breaks between sizes 3 and 8

pir_diff_pool = pIR(test_code ~ total|grp, data = diff_pool0, pt.method = "firth")

pir_diff_pool = data.frame(
  grp = pir_diff_pool$grp,
  P = pir_diff_pool$P
)

diff_pool = diff_pool0 %>%
  left_join(pir_diff_pool) %>%
  pivot_longer(cols = c(P, p_frac) , names_to = "y", values_to = "p")

p_pos = diff_pool %>% 
  filter(test_code == 1) %>%
  ggplot(aes(x = total, y = p, color = y)) +
  geom_line() +
  ggtitle("Increasing (+) Pool size `X` w/ (-) Pool of size 1") +
  xlab("# Mosquitoes in Positive Pools") +
  theme_classic()

p_pos




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#equal increasing pool sizes
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


pool_0 = data.frame(
  grp = 1:50,
  total = 1:50,   
  test_code = 0
)

pool_1 = data.frame(
  grp = 1:50,
  total = 1:50,
  test_code = 1
)


diff_pool0 = rbind(pool_0, pool_1) %>%
  arrange(grp) %>%
  group_by(grp) %>%
  mutate(p_frac = sum(test_code)/(sum(total)))

pir_diff_pool = pIR(test_code ~ total|grp, data = diff_pool0, pt.method = "firth")

pir_diff_pool = data.frame(
  grp = pir_diff_pool$grp,
  P = pir_diff_pool$P
)

diff_pool = diff_pool0 %>%
  left_join(pir_diff_pool) %>%

  pivot_longer(cols = c(P, p_frac) , names_to = "y", values_to = "p")

p_pool_equal = diff_pool %>%
  filter(test_code == 1) %>%
  ggplot(aes(x = total, y = p, color = y)) +
  geom_line() +
  ggtitle("Equal Pool Size `X` (+) and (-) Pools") +
  xlab("# Mosquitoes in Either Pool") +
  theme_classic()

p_pool_equal

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#increasing number of 50 mozzy pools 1 positive pool
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

pools = rep(1:50, times = 1:50)

diff_pool0 = data.frame(
  grp = pools,
  total = rep(50, length(pools))) %>%
  group_by(grp) %>%
  group_by(grp) %>%
  mutate(test_code = ifelse(row_number() == 1 & grp != 1, 1, 0)) %>%
  mutate(p_frac = sum(test_code)/(sum(total)))

pir_diff_pool = pIR(test_code ~ total|grp, data = diff_pool0, pt.method = "firth")

pir_diff_pool = data.frame(
  grp = pir_diff_pool$grp,
  P = pir_diff_pool$P
)

diff_pool = diff_pool0 %>%
  left_join(pir_diff_pool) %>%
  pivot_longer(cols = c(P, p_frac) , names_to = "y", values_to = "p")

p_pool_increase50 = diff_pool %>%
  filter(test_code == 1) %>%
  ggplot(aes(x = grp, y = p, color = y)) +
  geom_line() +
  ggtitle("1 Pos Pool in `X` Pools of 50") +
  xlab("# Pools") +
  theme_classic()

p_pool_increase


pool_test = (p_2_50 + p_pos + p_pool_equal)/p_pool_increase
pool_test

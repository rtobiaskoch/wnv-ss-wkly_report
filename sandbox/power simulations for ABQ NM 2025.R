rm(list=ls())

library(tidyr)
library(MMWRweek)
library(sf)
library(tidyverse)
library(sfheaders)
library(sf)
library(spaMM)
library(raster)
library(plotrix)
library(fitdistrplus)
library(stringr)
library(osmdata)
library(ape)
library(glmmTMB)
library(sjPlot)
library(GLMMmisc)
library(plotrix)
library(ggeffects)
library(MASS)
library(parallel)

##### ABQ data ###########
setwd("D:/aegypti/")
ABQ <- as_tibble(read.csv("ABQ Ae aegypti abundance data_2018-2022.csv")) %>%
  mutate(Date.2 = as.Date(Date, "%m/%d/%Y")) %>%
  mutate(Epi.wk = MMWRweek(Date.2)[,2]) %>%
  mutate(Epi.yr = MMWRweek(Date.2)[,1])

ggplot(ABQ, aes(as.factor(Epi.wk), Abundance))+
  geom_boxplot()+
  facet_grid(Epi.yr~.)+
  scale_y_log10()+
  xlab("CDC Epi week")

#### estimating the distribution of case counts across the entire data set
hist(ABQ$Abundance, breaks=100)
abq.dst1 <- fitdist(ABQ$Abundance, "nbinom")
plot(abq.dst1)
summary(abq.dst1)
### mu = 13.06
### size = 0.64
#### visually perfect neg-binom fit...

## what about site-level variance
null <- glmmTMB(Abundance ~ (1|Location)+(1|Epi.wk)+(1|Epi.yr),
                data=ABQ, family=nbinom2,
                ziformula=~1)
summary(null)
## some variation yr-2-yr & site-2-site
## obvi lots of variation within a season

##### visualizing random effects
re.1 <- plot_model(null, type="re")[[1]] ### lots of sites - not all are sampled regularly
re.2 <- plot_model(null, type="re")[[2]] ### need to standardize when EXPs would take place
re.3 <- plot_model(null, type="re")[[3]] ### increasing number captured per yer since 2018

### some with above/below collections
re.1.tb <- as_tibble(get_model_data(null, type="re")[[1]])
re.1.hgh <- re.1.tb %>% filter(conf.low > 1)
re.1.lw <- re.1.tb %>% filter(conf.high < 1)
re1.rmn <- rbind(re.1.hgh, re.1.lw)
rm.trm <- as.vector(c(re1.rmn$term))


#### standardize weeks + sites (week 23 - 44 contains 1 or 0 pending re transformation)
ABQ2 <- ABQ %>% filter(Epi.wk > 22) %>% filter(Epi.wk < 45) %>%
  filter(!(Location %in% rm.trm)) 

#### re-do distribution fit
abq.dst2 <- fitdist(ABQ2$Abundance, "nbinom")
plot(abq.dst2)
summary(abq.dst2)
### mu = 11.8
### size = 0.68
### still pretty good

#### re-estimate variability
null2 <- glmmTMB(Abundance ~ (1|Location)+(1|Epi.wk)+(1|Epi.yr),
                 data=ABQ2, family=nbinom2,
                 ziformula=~1)
summary(null2)

#zero-inflation is significant
#var location = 0.39, SD = 0.62
#var wk = 0.29, SD = 0.54
#var yr = 0.724, SD = 0.85

##### check no RE is wonky
plot_model(null2, type="re")[[1]] ### lots of sites - not all are sampled regularly
plot_model(null2, type="re")[[2]] ### need to standardize when EXPs would take place
plot_model(null2, type="re")[[3]] ### increasing number captured per yer since 2018

ggplot(ABQ2, aes(as.factor(Epi.wk), Abundance))+
  geom_boxplot()+
  facet_grid(Epi.yr~.)+
  scale_y_log10()+
  xlab("CDC Epi week")

##### should probably remove invasion years? (2018 and 2019)
##### also, maybe 2023 since it looks like its just one trap/week?

ABQ3 <- ABQ2 %>% filter(Epi.yr>2019) %>%
  filter(Epi.yr != 2023)

#### re-do distribution fit
abq.dst3 <- fitdist(ABQ3$Abundance, "nbinom")
plot(abq.dst3)
summary(abq.dst3)
### mu = 13.4
### size = 0.68
### still pretty good

#### re-estimate variability
null3 <- glmmTMB(Abundance ~ (1|Location)+(1|Epi.wk)+(1|Epi.yr),
                 data=ABQ3, family=nbinom2,
                 ziformula=~1)
summary(null3)

#zero-inflation is NOT significant
#var location = 0.39, SD = 0.62
#var wk = 0.28, SD = 0.53
#var yr = 0.61, SD = 0.78

##### check no RE is wonky
plot_model(null3, type="re")[[1]] ### lots of sites - not all are sampled regularly
plot_model(null3, type="re")[[2]] ### need to standardize when EXPs would take place
plot_model(null3, type="re")[[3]] ### increasing number captured per yer since 2018

ggplot(ABQ3, aes(as.factor(Epi.wk), Abundance))+
  geom_boxplot()+
  facet_grid(Epi.yr~.)+
  scale_y_log10()+
  xlab("CDC Epi week")

####### simulating WALS design in a single site with a Treated and untreated area

####### creating data frame shell for 8 sites per block sampled over 8 weeks
dat <- expand.grid(Collection = c("TRT","UTRT"), 
                   Space = 1:8,
                   Week = 1:8)

### need to make sure traps are unique to treated and untreated blocks
dat$Space <- paste(dat$Collection, dat$Space, sep=".")

#### simulating collection data

#### some "truth" - minimum expectation of reduced populations
Effect.size <- 0.35
dff <- 1-Effect.size

# theta= 1/size
abq.sim <- function(...) {
  sim.glmm(design.data=dat,
           fixed.eff = list(intercept=log(median(ABQ2$Abundance)), 
                            Collection = log(c(UTRT = 1.0, 
                                               TRT  = dff))),
           rand.V = c(Space = 0.39, Week = 0.29), 
           distribution = "negbinomial", theta = 1/0.68)
}

par(mfrow=c(3,3))
hist(abq.sim()[,4], breaks=50)
hist(abq.sim()[,4], breaks=50)
hist(abq.sim()[,4], breaks=50)
hist(abq.sim()[,4], breaks=50)
hist(abq.sim()[,4], breaks=50)
hist(abq.sim()[,4], breaks=50)
hist(abq.sim()[,4], breaks=50)
hist(abq.sim()[,4], breaks=50)
hist(abq.sim()[,4], breaks=50)

# theta = size
abq.sim <- function(...) {
  sim.glmm(design.data=dat,
           fixed.eff = list(intercept=log(median(ABQ2$Abundance)), 
                            Collection = log(c(UTRT = 1.0, 
                                               TRT  = dff))),
           rand.V = c(Space = 0.39, Week = 0.29), 
           distribution = "negbinomial", theta = 0.68)
}

par(mfrow=c(3,3))
hist(abq.sim()[,4], breaks=50)
hist(abq.sim()[,4], breaks=50)
hist(abq.sim()[,4], breaks=50)
hist(abq.sim()[,4], breaks=50)
hist(abq.sim()[,4], breaks=50)
hist(abq.sim()[,4], breaks=50)
hist(abq.sim()[,4], breaks=50)
hist(abq.sim()[,4], breaks=50)
hist(abq.sim()[,4], breaks=50)

##### both are fairly similar
##### theta = size allows for a few more "extreme" values
####' *TB: I'm not sure I understand theta vs 1/theta*

###### parallel runs
numCores <- detectCores()
cl <- makeCluster(numCores/2)

#### gotta load everything cluster will need to simulate studd
clusterEvalQ(cl, {
  
  ### packages
  library(GLMMmisc)
  library(glmmTMB)
  
  ### data shell and effect size
  dat <- expand.grid(Collection = c("TRT","UTRT"), 
                     Space = 1:8,
                     Week = 1:12)
  dat$Space <- paste(dat$Collection, dat$Space, sep=".")
  dat$Collection <- relevel(dat$Collection, ref="UTRT")
  
  Effect.size <- 0.35
  dff <- 1-Effect.size
  
  Avg.c <- c(2, 6, 16)
  
  #data simulator - using theta=size
  abq.sim <- function(...) {
    sim.glmm(design.data=dat,
             fixed.eff = list(intercept=log(Avg.c[2]), 
                              Collection = log(c(UTRT = 1.0, 
                                                 TRT  = dff))),
             rand.V = c(Space = 0.39, Week = 0.28), 
             distribution = "negbinomial", theta = 1/0.68)
  }
  
})

#### simulating the statistical test
sim.nb.TMB <- function(...) {
  fit <- glmmTMB(response ~ Collection + 
                   (1|Space)+(1|Week), data=abq.sim(),
                 family=nbinom2)
  p.fit <- coef(summary(fit))$cond[2,4]
  return(p.fit)
}

#### parallel run through all the clusters
px <- parSapply(cl, 1:1000, sim.nb.TMB)

#### calculate proportion of tests with p < 0.05
tab <- table(px < 0.05)
prop.test(tab[2],sum(tab))

g.plt <- cbind.data.frame(Avg.c =  c(6,    6,    6,    6),
                          Theta =  c(0.68, 0.68, 0.68, 0.68),
                          Traps =  c(8,    10,   12,   14),
                          Weeks =  c(8,    8,    8,    8),
                          Eff =    c(0.35, 0.35, 0.35, 0.35),
                          PWR =    c(0.27, 0.25, 0.31, 0.32),
                          PWR.LL = c(0.24, 0.23, 0.28, 0.30),
                          PWR.UL = c(0.29, 0.28, 0.34, 0.35))




#### need to figure out how to geo-locate some of these addresses

library(tidygeocoder)

locs = read.csv('SiteLocs/SiteLocs.csv')


left_join(ABQ,locs, by="Location") %>%
  dplyr::select(Location, Species, Abundance, Date.2, Epi.wk, Epi.yr, latitude, longitude) %>%
  mutate(longitude = na_if(longitude, '?'),
         latitude = na_if(latitude, '?'),
         Location = str_remove(Location, "^Canyon Gate\\s+")) -> ABQ_locs

ABQ_locs %>% na.omit() -> ABQ_locsKnown

ABQ_locs %>% filter(is.na(latitude) | is.na(longitude)) %>%
  filter(str_detect(Location, "&") | str_detect(Location, "^\\d")) -> ABQ_locsSearchable

ABQ_locs %>% filter(is.na(latitude) | is.na(longitude)) %>%
  filter(!str_detect(Location, "&") & !str_detect(Location, "^\\d")) -> ABQ_locsFollowUp
ABQ_locsFollowUp = unique(ABQ_locsFollowUp$Location)


sites <- as.data.frame(unique(ABQ_locsSearchable$Location))
colnames(sites) <- "Location"
sites$addr <- paste(sites$Location, "Albuquerque, NM", sep=", ")

lat_longs <- sites %>%
  geocode(addr, method = 'arcgis', lat = latitude , long = longitude)

#####
some_addresses <- tibble::tribble(~addr, "915 Coal, Albuquerque, NM" ,
                                  "912 Lead, Albuquerque, NM",
                                  "1012 Pacific, Albuquerque, NM",
                                  "906 Cromwell, Albuquerque, NM",
                                  "2703 Truman, Albuquerque, NM",
                                  "3103 Monroe, Albuquerque, NM",
                                  "2905 Ortiz, Albuquerque, NM" ,
                                  "2717 Palomas, Albuquerque, NM",
                                  "7304 Arroy Del Oso NE, Albuquerque, NM" ,
                                  "7909 Sierra Azul , Albuquerque, NM" ,
                                  "7200 Bellrose Ave NE, Albuquerque, NM",
                                  "3100 Aztec Rd NE, Albuquerque, NM",              
                                  "2904 Ortiz Dr NE, Albuquerque, NM",              
                                  "7512 Morrow Ave NE, Albuquerque, NM",            
                                  "7025 Prairie Ct NE, Albuquerque, NM",
                                  "7316 Arvada Ave NE, Albuquerque, NM",            
                                  "8015 Hannett Ave NE, Albuquerque, NM",           
                                  "3603 Cherokee, Albuquerque, NM",                 
                                  "7013 Kiowa NE, Albuquerque, NM",                 
                                  "3308 Delamar NE, Albuquerque, NM",               
                                  "3817 Delamar NE, Albuquerque, NM",               
                                  "7508 Madera Rd NE, Albuquerque, NM",             
                                  "8504 Prestina Pl NE, Albuquerque, NM",           
                                  "8604 Avenales Ave NE, Albuquerque, NM",          
                                  "2508 Wisconsin St NE, Albuquerque, NM",          
                                  "2812 Utah St NE, Albuquerque, NM",               
                                  "8822 Corona NE, Albuquerque, NM",
                                  "2629 Quincy St NE, Albuquerque, NM",             
                                  "2838 Quincy St NE, Albuquerque, NM",             
                                  "2923 Quincy St NE, Albuquerque, NM" ,            
                                  "2946 Truman St NE, Albuquerque, NM",             
                                  "2717 Truman St NE, Albuquerque, NM",             
                                  "2817 La Veta Dr NE, Albuquerque, NM",            
                                  "2829 Palomas Dr NE, Albuquerque, NM" ,           
                                  "6205 Los Arbolas NE, Albuquerque, NM" ,          
                                  "2533 Madera NE, Albuquerque, NM",                
                                  "2508 Palomas Dr NE, Albuquerque, NM",            
                                  "2513 Valencia Dr NE, Albuquerque, NM",           
                                  "2529 Cagua Dr NE, Albuquerque, NM",              
                                  "2417 Cardenas, Albuquerque, NM",                 
                                  "2446 Palomas NE, Albuquerque, NM",               
                                  "3115 Truman NE, Albuquerque, NM",
                                  "3022 Manzano NE, Albuquerque, NM",               
                                  "3042 Quincy NE, Albuquerque, NM",                
                                  "3018 Monroe NE, Albuquerque, NM",                
                                  "10712 Nelle NE, Albuquerque, NM",
                                  "11609 Baldwin NE, Albuquerque, NM" ,             
                                  "10405 Prospect NE, Albuquerque, NM",             
                                  "3225 Reina Dr NE, Albuquerque, NM",              
                                  "3204 Britt NE, Albuquerque, NM",                 
                                  "3915 Fox Sparrow NW, Albuquerque, NM",           
                                  "5423 Hummingbird NW, Albuquerque, NM",           
                                  "3113 Ortiz Dr NE, Albuquerque, NM" ,
                                  "7505 McNerney Dr NE, Albuquerque, NM" ,          
                                  "3024 Espanola NE, Albuquerque, NM" ,
                                  "2609 Wisconsin St NE, Albuquerque, NM" ,         
                                  "2637 Valencia Dr NE, Albuquerque, NM",
                                  "2637 Cardenas NE, Albuquerque, NM" ,             
                                  "2705 Cardenas NE, Albuquerque, NM" ,
                                  "2709 Virginia St NE, Albuquerque, NM" ,          
                                  "3401 Inman Ct NE, Albuquerque, NM" ,             
                                  "7736 Hermanson Pl NE, Albuquerque, NM",
                                  "3217 Manitoba, Albuquerque, NM"       ,
                                  "3916 Rock Dove , Albuquerque, NM" ,              
                                  "7425 Sherwood, Albuquerque, NM" ,
                                  "616 18th St, Albuquerque, NM" ,                  
                                  "1043 Pacaya, Albuquerque, NM" ,                  
                                  "404 Judith, Albuquerque, NM" ,
                                  "310 Solano, Albuquerque, NM",                    
                                  "2701 Cardenas, Albuquerque, NM",
                                  "8100 Southern, Albuquerque, NM" ,
                                  "8104 Southern, Albuquerque, NM",
                                  "7200 Rayo de Sol, Albuquerque, NM",
                                  "419 Adams St NE, Albuquerque, NM",
                                  "9915 Barriston NE, Albuquerque, NM",             
                                  "2520 Viola Rd SW, Albuquerque, NM",              
                                  "9715 Cassidy Pl NW, Albuquerque, NM" ,           
                                  "5838 El Prado Rd NW, Albuquerque, NM",           
                                  "6401 Signal Ave NE, Albuquerque, NM",            
                                  "1311 Constitution Ct NE, Albuquerque, NM",
                                  "2304 Don Andres Rd SW, Albuquerque, NM",         
                                  "1707 Shady Side Dr SW, Albuquerque, NM",         
                                  "6320 Child's Pl SW, Albuquerque, NM",            
                                  "562 Atrisco Dr SW, Albuquerque, NM",             
                                  "1650 Gabaldon Dr NW, Albuquerque, NM",
                                  "1225 Sasebo Dr NE, Albuquerque, NM",             
                                  "2630 Matthew Pl NW, Albuquerque, NM",            
                                  "11909 Caribou Ave, Albuquerque, NM",             
                                  "1401 Roma Ave NE, Albuquerque, NM",
                                  "2220 Vista Larga NE, Albuquerque, NM",
                                  "2837 Alvarado Dr NE, Albuquerque, NM")

#####
#'*TB: removed duplicated '2837 Alvarado Dr NE' from above*
#'*TB: removed duplicated "2220 Vista Larga NE, Albuquerque, NM"*
#'*TB: removed duplicated "2837 Alvarado NE' from above (there were originally 3 of this one)*
#'*TB: removed duplicated "7736 Hermanson Pl NE, Albuquerque, NM"*
#'*TB: quiets errors later*

#intersections <- filter(sites, str_detect(sites$addr, '&')) %>% dplyr::select(addr)
#'*TB: create intersections list*

#some_addresses = rbind(some_addresses, intersections)
#'*TB: combine addresses*

# geocode the addresses - only 92 with "queriable" addresses 
#'*TB: now 91. Was treating "2837 Alvarado Dr NE" and "2837 Alvarado NE" as unique queries*
#'*TB: 114 after adding street intersections*

lat_longs <- some_addresses %>%
  geocode(addr, method = 'arcgis', lat = latitude , long = longitude)
#'*TB: census service returns more (and faster)*
#'*TB: arcgis returns all*'
#### 73 return an address...
#'*TB: returns 114/114*

#lat_longs2 <- 
#  .omit(lat_longs)
#Location2 <- str_split_fixed(lat_longs2$addr, ",", 3)
#lat_longs2$Location <- Location2[,1]

ABQ_locsSearchable <- ABQ_locsSearchable %>%
  dplyr::select(-latitude, -longitude) %>%  # remove old lat/lon
  left_join(
    lat_longs %>%
      dplyr::select(Location, latitude, longitude),  # drop addr by excluding it
    by = "Location"
  )

ABQ.full <- ABQ_locsKnown %>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude)) %>%
           bind_rows(ABQ_locsSearchable) %>%
  dplyr::select(Location, Species, Abundance, Date.2, Epi.wk, Epi.yr, latitude, longitude) %>%
  na.omit() %>%
  st_as_sf(coords=c("longitude","latitude"), crs=4326)


library(osmdata)
big_streets.abq <- getbb("Albuquerque New Mexico")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf() 

med_streets.abq <- getbb("Albuquerque New Mexico")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()

small_streets.abq <- getbb("Albuquerque New Mexico")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

abq.bb <- st_bbox(ABQ.full)

xlim1 <- c(abq.bb[1] - 0.01, abq.bb[3] + 0.01)
ylim1 <- c(abq.bb[2] - 0.01, abq.bb[4] + 0.01)




ggplot() +  
  geom_sf(data = small_streets.abq$osm_lines,
          inherit.aes = FALSE,
          color = "white")+
  geom_sf(data = med_streets.abq$osm_lines,
          inherit.aes = FALSE,
          color = "darkgrey")+
  geom_sf(data = big_streets.abq$osm_lines,
          inherit.aes = FALSE,
          color = "black")+
  geom_sf(data=ABQ3, 
          aes(size=log1p(Abundance),
              color=log1p(Abundance)), alpha=1/2)+
  scale_color_gradient(low="blue", high="red")+
  coord_sf(xlim=xlim1, ylim=ylim1)+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=90))

##### spatial autocorrelation??
library(ape)

ABQ2 <- ABQ.full %>% filter(Epi.wk > 22) %>% filter(Epi.wk < 45) %>%
  filter(!(Location %in% rm.trm)) 

ABQ4 <- ABQ2 %>%
  dplyr::select(Location, Species, Abundance, Date.2, Epi.wk, Epi.yr, geometry) %>%
  na.omit() %>%
  group_by(Location, geometry) %>%
  summarise(Mean = mean(Abundance, na.rm=T),
            Total = sum(Abundance, na.rm=T),
            Median = median(Abundance, na.rm=T))

# aeg.dists <- as.matrix(dist(cbind(ABQ4$longitude, 
#                                   ABQ4$latitude)))

library(units)
aeg.dists <- st_distance(ABQ4)
aeg.dists.inv <- 1/aeg.dists
aeg.dists.inv <- drop_units(aeg.dists.inv)
aeg.dists.inv[aeg.dists.inv == Inf] <- 0
Moran.I(ABQ4$Mean, aeg.dists.inv)

### nothing significant but missing like 75% locations

##### variability within a treatment block...
ABQ5 <- left_join(ABQ,lat_longs2, by="Location") %>%
  dplyr::select(Location, Species, Abundance, Date.2, Epi.wk, Epi.yr, latitude, longitude) %>%
  na.omit() %>%
  filter(latitude>34.96) %>%
  filter(latitude<35.193) %>%
  filter(longitude< -106.5) %>%
  filter(longitude > -106.75)


library(fitdistrplus)
abq.dst.lnrm <- fitdist(ABQ5$Abundance, "lnorm")
abq.dst.wbl <- fitdist(ABQ5$Abundance, "weibull")
abq.dst.psn <- fitdist(ABQ5$Abundance, "pois")
abq.dst.nbnm <- fitdist(ABQ5$Abundance, "nbinom")

gofstat(list(abq.dst.lnrm,abq.dst.wbl, abq.dst.psn, abq.dst.nbnm),
        fitnames=c("log-normal","weibull","Poisson","Nbinom"))

gofstat(list(abq.dst.psn, abq.dst.nbnm),
        fitnames=c("Poisson","Nbinom"))

##### still nbinom
plot(abq.dst.nbnm)
summary(abq.dst.nbnm)
#mu: 15.8
#'*mu: 12.7*
#size = 2.14
#'*size = 0.84*

## what about site-level variance
library(glmmTMB)
null <- glmmTMB(Abundance ~ (1|Location) + (1|Epi.wk)+(1|Epi.yr),
                data=ABQ5, 
                family=nbinom2,
                ziformula=~0)
summary(null)
library(gridExtra)
grid.arrange(plot_model(null, type="re")[[1]] +ggtitle("Variance in Spatail Locations"), 
             plot_model(null, type="re")[[2]]+ggtitle("Variance in Weekly Collections"), nrow=1)
### site = 0.35 (SD 0.6)
#'* site = 0.47 (SD 0.69)*
### wk = 0.18 (SD 0.43)
#'* wk = 0.076(SD 0.28)*

dat <- expand.grid(Block = c("One","Two","Three"), 
                   Collection = c("TRT","UTRT"), 
                   Space = 1:8,
                   Week = 1:8)
### Set the reference
dat$Collection <- relevel(dat$Collection, ref="UTRT")

### need to nest space into block
dat$Space2 <- paste(dat$Block, dat$Space, sep=".")

# simulate data based on a predicted 30% reduction in aegypti 
#   treated vs. untreated areas
library(GLMMmisc)
abq.sim <- function(...) {
  spc <- rnorm(1, 0.21, 0.46) #'*TB: This is site level variance. From above??*
  for (i in 1:1) {
    if (spc[i] < 0) {
      spc[i] = 0.01 }
    else {
      spc[i] = spc[i] }
  }
  
  wk <- rnorm(1, 0.06, 0.24) #'*TB: This is week level variance. From above??*
  for (i in 1:1)
    if (wk[i] < 0) {
      wk[i] = 0.01 }
  else {
    wk[i] = wk[i] }
  
  sim.glmm(design.data=dat,
           fixed.eff = list(intercept=log(median(ABQ5$Abundance)), 
                            Collection = log(c(UTRT = 1.0, 
                                               TRT  = 0.65)),
                            Block = log(c(One = 1.0,
                                          Two = 1.0,
                                          Three=1.0))),
           rand.V = c(Space2 = spc, Week = wk), 
           distribution = "negbinomial", theta = 1/2.30)
}

sim.nb.TMB <- function(...) {
  fit <- glmmTMB(response ~ Collection + Block + 
                   (1|Space2)+(1|Week), data=abq.sim(),
                 family=nbinom2)
  p.coef <- c(coef(summary(fit))$cond[2,1],coef(summary(fit))$cond[2,4]) 
  p.pred <- as.data.frame(ggpredict(fit, terms="Collection", bias_correction = TRUE)) 
  return(c(p.coef, c(p.pred[1,2], p.pred[2,2])))
}

nsim=1000
p <- sapply(1:nsim, sim.nb.TMB)

### PWR 
tab1 <- table(p[2,] < 0.05) #p-values
prop.test(tab1[2],sum(tab1)) #PWR p-values

### PRED percent difference in TRT VS UTRT
pct.dff <- rep(NA, ncol(p))

for (i in 1:length(pct.dff)) {
  pct.dff[i] <- (p[4,i]-p[3,i])/(mean(p[4,i]+p[3,i]))
}

c(mean(pct.dff),std.error(pct.dff))

###### there are actually Space*Block*Collection spatial effects
###### so, 8*3*2 = 48 unique trap collections...
###### time impacts everyone

Traps <-        c(8,     8,     8,     8,     8,     8,     8)
Week <-         c(8,     8,     8,     8,     8,     8,     8)
Site.var <-     c(0.21,  0.21,  0.21,  0.10,  0.5,   0.21,  0.21)
Week.var <-     c(0.06,  0.06,  0.06,  0.06,  0.06,  0.3,   0.5)
Expected.EFF <- c(0.3,   0.4,   0.35,  0.35,  0.35,  0.35,  0.35)
Obs.PWR <-      c(0.53,  0.88,  0.77,  0.8,   0.71,  0.76,  0.71)
Obs.PWR.LL <-   c(0.43,  0.8,   0.67,  0.71,  0.61,  0.66,  0.61)
Obs.PWR.UL <-   c(0.63,  0.93,  0.85,  0.87,  0.79,  0.84,  0.79)
Obs.EFF <-      c(0.17,  0.25,  0.22,  0.22,  0.20,  0.22,  0.21)
Obs.EFF.SE <-   c(0.007, 0.007, 0.008, 0.008, 0.009, 0.007, 0.008)

df <- as_tibble(cbind.data.frame(Traps, Week, Site.var, Week.var,
                                 Expected.EFF, Obs.PWR, Obs.PWR.LL, Obs.PWR.UL,
                                 Obs.EFF, Obs.EFF.SE))

p1 <- df %>% filter(Expected.EFF == 0.35) %>%
  ggplot(., aes(Site.var, Obs.PWR))+
  theme_bw()+
  geom_col()+
  geom_errorbar(aes(ymin=Obs.PWR.LL, ymax=Obs.PWR.UL))+
  geom_hline(yintercept=0.8, col="red", linetype="dashed")+
  facet_grid(. ~ Week.var)+
  ggtitle("Expected reduction in adults in TRT vs UTRT: 35%; 8 traps per sites; 8 weeks of collections")+
  xlab("Variance among trap collections")+
  ylab("Predicted Power (% models with TRT collections < UTRT @ p-value < 0.05)")


p2 <- df %>% filter(Expected.EFF == 0.35) %>%
  ggplot(., aes(Site.var, Obs.EFF))+
  theme_bw()+
  geom_col()+
  geom_errorbar(aes(ymin=Obs.EFF-Obs.EFF.SE, ymax=Obs.EFF+Obs.EFF.SE))+
  geom_hline(yintercept=0.35, col="red", linetype="dashed")+
  facet_grid(. ~ Week.var)+
  ggtitle("Expected reduction in adults in TRT vs UTRT: 35%; 8 traps per sites; 8 weeks of collections")+
  xlab("Variance among trap collections")+
  ylab("Predicted average reduction in adults in TRT vs UTRT")

library(gridExtra)
grid.arrange(p1, p2, nrow=1)

##### let space/time random effects vary all over the place with each simulation
#####   TRUE UNCERTAINTY
Nsim <-         c(100,   1000)
Traps <-        c(8,     8)
Week <-         c(8,     8)
Expected.EFF <- c(0.35,  0.35)
Obs.PWR <-      c(0.78,  0.74)
Obs.PWR.LL <-   c(0.68,  0.71)
Obs.PWR.UL <-   c(0.85,  0.77)
Obs.EFF <-      c(0.21,  0.21)
Obs.EFF.SE <-   c(0.008, 0.003)


##### some sort of spatial prediction (?)
library(spaMM)
null.spc <- fitme(Abundance ~ AR1(1|Epi.wk) + 
                    Matern(1 | longitude + latitude), 
                  data = ABQ5, 
                  family = negbin(link="log")) 
summary(null.spc)
rst.null <- map_ranef(null.spc, return.="raster")

#### some sort of spatial correlation value (?)
dd <- dist(ABQ5[,c("longitude","latitude")])
mm <- MaternCorr(dd, nu = 0.295, rho = 7.17)
df <- as.data.frame(cbind(as.numeric(dd), as.numeric(mm)))
df$km <- df$V1*10.1
ggplot(df, aes(x=km, y=V2)) + theme_bw()+
  geom_line()+
  xlab("Estimated Distance between Traps (units=km)")+
  ylab("Estimated Correlation between Ae. aegypti trap collections")

##### based on the coordinates I have, its all pretty correlated even up to ~ 3 km (r = 0.2)

#### tyring different things
library(ncf)

x<- expand.grid(1:12, 1:12)[,1]
y<- expand.grid(1:12, 1:12)[,2]
z<- exp(rmvn.spa(x=x, y=y, p=13, method="exp"))
z2 <- matrix(z, nrow=12, ncol=12)

library(spatstat)
win <- owin(xrange=c(0,12),yrange=c(0,12))
sim_pattern <- rpoispp(lambda=10, window=win)
plot(sim_pattern)


###### using ABQ3 which is all data with a coordinate
###### using ABQ4 which has the coordinate as a df

ABQ3 <- ABQ2 %>% filter(Epi.yr>2019) %>%
  filter(Epi.yr != 2023)

ABQ3x <- ABQ3 %>% st_transform(.,3578)
bx <- st_bbox(ABQ3x)
xdim <- (2981612-2960549)/1000
ydim <- (1566272-1549392)/1000

library(terra)
abq.rst <- rast(xmin=bx[1], xmax=bx[3], ymin=bx[2], ymax=bx[4],
                ncols=42, nrow=34, vals=median(ABQ3$Abundance))
plot(abq.rst)

# simulate data using a Gaussian field
library(prioritizr)
x <- simulate_data(abq.rst, n = 1, scale = 0.01, intensity=9, 
                   sd=5)
x[x<0] <- 0
# plot simulated data
plot(round(x), main = "simulated data")



###### comparing auto-correlation of the rasters
df <- as.data.frame(round(raster(x)), xy=TRUE)

aeg.dists <- as.matrix(dist(cbind(df$x, 
                                  df$y)))
aeg.dists.inv <- 1/aeg.dists
aeg.dists.inv[aeg.dists.inv == Inf] <- 0
Moran.I(df$layer, aeg.dists.inv)





## End(Not run)
### ok, use ABQ5

ABQ6 <- ABQ5 %>% st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_transform(., 3578)
bx2 <- st_bbox(ABQ6)
2974978-2973662
1557825-1556787

#### roughly 1.3 x 1 km

sd(ABQ5$Abundance)

# simulate data using a Gaussian field
library(prioritizr)
library(reshape)
data.space <- function(...) {
  
  ### create raster
  abq.rst <- rast(xmin=2960549+350, xmax=2974978-350, ymin=-1557825+200, ymax=-1556787-200,
                  ncols=6, nrow=6, vals=14)
  ### simulate spatial data
  x.utrt <- simulate_data(abq.rst, n = 8, scale = 0.1, intensity=10, 
                          sd=10)
  x.utrt[x.utrt<0] <- 0
  x.utrt$ID <- 1:ncell(x.utrt)
  
  x.trt <- simulate_data(abq.rst, n = 8, scale = 0.1, intensity=7, 
                         sd=10)
  x.trt[x.trt<0] <- 0
  x.trt$ID <- 1:ncell(x.trt)
  
  #### aggregate to df
  utrt <- terra::as.array(x.utrt)
  u.df <- melt(utrt)
  ID1 <- u.df[289:324,]$value
  u.df2 <- cbind.data.frame(u.df$X3, u.df$value,rep(ID1,9))
  u.df3 <- as_tibble(u.df2[1:288,])
  colnames(u.df3) <- c("Week","Collection","Grid.ID")
  smpl1 <- sample(1:36, size=8, replace=FALSE)
  
  u.df4 <- u.df3 %>%
    filter(Grid.ID == smpl1[1] | 
             Grid.ID == smpl1[2] |
             Grid.ID == smpl1[3] |
             Grid.ID == smpl1[4] |
             Grid.ID == smpl1[5] |
             Grid.ID == smpl1[6] |
             Grid.ID == smpl1[7] |
             Grid.ID == smpl1[8]) %>%
    mutate(Collection2 = round(Collection, digits=0))%>%
    mutate(Block = "UTRT")
  
  trt <- terra::as.array(x.trt)
  trt.df <- melt(trt)
  ID <- trt.df[289:324,]$value
  trt.df2 <- cbind.data.frame(trt.df$X3, trt.df$value,rep(ID,9))
  trt.df3 <- as_tibble(trt.df2[1:288,])
  colnames(trt.df3) <- c("Week","Collection","Grid.ID")
  smpl <- sample(1:36, size=8, replace=FALSE)
  
  trt.df4 <- trt.df3 %>%
    filter(Grid.ID == smpl[1] | 
             Grid.ID == smpl[2] |
             Grid.ID == smpl[3] |
             Grid.ID == smpl[4] |
             Grid.ID == smpl[5] |
             Grid.ID == smpl[6] |
             Grid.ID == smpl[7] |
             Grid.ID == smpl[8]) %>%
    mutate(Collection2 = round(Collection, digits=0)) %>%
    mutate(Block = "TRT")
  
  trt.tst <- rbind(trt.df4, u.df4)
  trt.tst$Block <- relevel(as.factor(trt.tst$Block), ref="UTRT")
  
  return(trt.tst) 
}

data.test <- function(...) {
  mod <- glmmTMB(Collection2 ~ Block + 
                   (1|Week) + (1|Grid.ID),
                 data=data.space(),
                 family=nbinom2,
                 ziformula=~0)
  summary(mod)
  p.coef <- c(coef(summary(mod))$cond[2,1],coef(summary(mod))$cond[2,4]) 
  p.pred <- as.data.frame(ggpredict(mod, terms="Block", bias_correction = TRUE)) 
  return(c(p.coef, c(p.pred[1,2], p.pred[2,2])))
}

##### random effect variance is pretty small in most simulations using this approach
#####  is space needed if this is the case?

library(parallel)
numCores <- detectCores()
cl <- makeCluster(numCores/2)
clusterEvalQ(cl, {
  library(reshape)
  library(prioritizr)
  library(terra)
  library(tidyverse)
  library(glmmTMB)
  library(ggeffects)
  data.space <- function(...) {
    
    #### generate sampling raster 
    abq.rst <- rast(xmin=2960549+350, xmax=2974978-350, ymin=-1557825+200, ymax=-1556787-200,
                    ncols=6, nrow=6, vals=14)
    
    ### simulate spatial data - 8 seperate grids
    x.utrt <- simulate_data(abq.rst, n = 8, scale = 0.9, intensity=10, 
                            sd=7.5)
    x.utrt[x.utrt<0] <- 0
    x.utrt$ID <- 1:ncell(x.utrt)
    
    x.trt <- simulate_data(abq.rst, n = 8, scale = 0.9, intensity=7, 
                           sd=7.5)
    x.trt[x.trt<0] <- 0
    x.trt$ID <- 1:ncell(x.trt)
    
    #### aggregate to df
    utrt <- terra::as.array(x.utrt)
    u.df <- melt(utrt)
    ID1 <- u.df[289:324,]$value
    u.df2 <- cbind.data.frame(u.df$X3, u.df$value,rep(ID1,9))
    u.df3 <- as_tibble(u.df2[1:288,])
    colnames(u.df3) <- c("Week","Collection","Grid.ID")
    smpl1 <- sample(1:36, size=12, replace=FALSE)
    
    u.df4 <- u.df3 %>%
      filter(Grid.ID == smpl1[1] | 
               Grid.ID == smpl1[2] |
               Grid.ID == smpl1[3] |
               Grid.ID == smpl1[4] |
               Grid.ID == smpl1[5] |
               Grid.ID == smpl1[6] |
               Grid.ID == smpl1[7] |
               Grid.ID == smpl1[8] |
               Grid.ID == smpl1[9] |
               Grid.ID == smpl1[10] |
               Grid.ID == smpl1[11] |
               Grid.ID == smpl1[12]) %>%
      mutate(Collection2 = round(Collection, digits=0))%>%
      mutate(Block = "UTRT")
    
    trt <- terra::as.array(x.trt)
    trt.df <- melt(trt)
    ID <- trt.df[289:324,]$value
    trt.df2 <- cbind.data.frame(trt.df$X3, trt.df$value,rep(ID,9))
    trt.df3 <- as_tibble(trt.df2[1:288,])
    colnames(trt.df3) <- c("Week","Collection","Grid.ID")
    smpl <- sample(1:36, size=12, replace=FALSE)
    
    trt.df4 <- trt.df3 %>%
      filter(Grid.ID == smpl[1] | 
               Grid.ID == smpl[2] |
               Grid.ID == smpl[3] |
               Grid.ID == smpl[4] |
               Grid.ID == smpl[5] |
               Grid.ID == smpl[6] |
               Grid.ID == smpl[7] |
               Grid.ID == smpl[8] |
               Grid.ID == smpl[9] |
               Grid.ID == smpl[10] |
               Grid.ID == smpl[11] |
               Grid.ID == smpl[12]) %>%
      mutate(Collection2 = round(Collection, digits=0)) %>%
      mutate(Block = "TRT")
    
    trt.tst <- rbind(trt.df4, u.df4)
    trt.tst$Block <- relevel(as.factor(trt.tst$Block), ref="UTRT")
    
    return(trt.tst) 
  }
})

data.test <- function(...) {
  mod <- glmmTMB(Collection2 ~ Block + 
                   (1|Week) + (1|Grid.ID),
                 data=data.space(),
                 family=nbinom2,
                 ziformula=~0)
  summary(mod)
  p.coef <- c(coef(summary(mod))$cond[2,1],coef(summary(mod))$cond[2,4]) 
  p.pred <- as.data.frame(ggpredict(mod, terms="Block", bias_correction = TRUE)) 
  return(c(p.coef, c(p.pred[1,2], p.pred[2,2])))
}
px <- parSapply(cl, 1:100, data.test)

### PWR 
tab1 <- table(px[2,] < 0.05) #p-values
prop.test(tab1[2],nsim) #PWR p-values

#library(plotrix)
### PRED percent difference in TRT VS UTRT
pct.dff <- rep(NA, ncol(px))

for (i in 1:length(pct.dff)) {
  pct.dff[i] <- (px[4,i]-px[3,i])/(mean(px[4,i]+px[3,i]))
}

c(mean(pct.dff),std.error(pct.dff))

stopCluster(cl)
#### some notes, high spatial autocorrelation and 40% effect size, pretty well powered
#### reducing autocorrelation reduces power but not a whole lot
#### study is under-powered when effect size is 30%
#*****************************************************************
# copied from hammer.R from dropbox/prodev/AAG/Boston           #
# original created: March 30, 2017                              #
# new creation: September 20, 2017                              #
# last modified: 11.3.17                                       #
# saved: ("NHGIS/current projects/urban/hammerMethod.R)         #
# data from NHGIS                                               #
# read in location: (""NHGIS/current projects/)                 #
# Author: Taylor                                                #
#****************************************************************

library(tidyverse)
library(magrittr)
GAbg <- read.csv("dropbox/boston/nhgis0024_csv/nhgis0024_ds215_20155_2015_blck_grp.csv")
GAbg <- as_tibble(GAbg)
head(GAbg)

USarea <- read.csv("desktop/tempNHGIS/current projects/urban/areas.txt")
USarea <- as_tibble(USarea)
head(USarea)
area2 <- USarea[,-(c(7:9))]

atl <- tibble(county = c(51,57,63,67,89,97,113,121,135,151,247))

ATLbg <- GAbg %>%
  filter(COUNTYA %in% atl$county)

ATLbg %>%
  count(COUNTYA)
# county 51 is chatham county - savannah
# reference to Matt Hauer's paper
atlnh <- tibble(county = c("G1300510","G1300570","G1300630","G1300670","G1300890","G1300970","G1301130","G1301210","G1301350","G1301510","G1302470"))

#cherokee <- GAbg[GAbg$COUNTYA==57,]
#clayton <- GAbg[GAbg$COUNTYA==63,]
#cobb <- GAbg[GAbg$COUNTYA==67,]
#dekalb <- GAbg[GAbg$COUNTYA==89,]
#douglas <- GAbg[GAbg$COUNTYA==97,]
#fayette <- GAbg[GAbg$COUNTYA==113,]
#fulton <- GAbg[GAbg$COUNTYA==121,]
#gwinnett <- GAbg[GAbg$COUNTYA==135,]
#henry <- GAbg[GAbg$COUNTYA==151,]
#rockdale <- GAbg[GAbg$COUNTYA==247,]

#read in housing unit by year Census
hu1940 <- read.csv("dropbox/boston//nhgis0022_csv/nhgis0022_ds78_1940_county.csv")
hu1940 <- as_tibble(hu1940)
atl1940 <- hu1940[hu1940$GISJOIN %in% atlnh$county,]
atl1940$COUNTYA <- atl1940$COUNTYA/10

hu1970 <- read_csv("dropbox/boston//nhgis0025_csv/nhgis0025_ds94_1970_county.csv")
atl1970 <- hu1970[hu1970$GISJOIN %in% atlnh$county,]
str(atl1970)

hu1980 <- read_csv("dropbox/boston//nhgis0025_csv/nhgis0025_ds104_1980_county.csv")
atl1980 <- hu1980[hu1980$GISJOIN %in% atlnh$county,]

hu1990 <- read_csv("dropbox/boston//nhgis0025_csv/nhgis0025_ds120_1990_county.csv")
atl1990 <- hu1990[hu1990$GISJOIN %in% atlnh$county,]

hu2000 <- read_csv("dropbox/boston//nhgis0025_csv/nhgis0025_ds146_2000_county.csv")
atl2000 <- hu2000[hu2000$GISJOIN %in% atlnh$county,]

hu2010 <- read.csv("dropbox/boston//nhgis0026_csv/nhgis0026_ds172_2010_county.csv")
hu2010 <- as_tibble(hu2010)
str(hu2010)
atl2010 <- hu2010[hu2010$GISJOIN %in% atlnh$county,]

# for all counties, won't need to delete huXXXX, and won't need to create
# atlXXXX.
rm(hu1940,hu1970,hu1980,hu1990,hu2000,hu2010)

# modify by Year Census
atl1940 <- atl1940[,-(c(1:5,9))]
atl1970 <- atl1970[,-(1:5)]
atl1980 <- atl1980[,-(c(1:7,10))]
atl1990 <- atl1990[,-(c(1,2,5:16))]
atl2000 <- atl2000[,-(c(1:6,9:12))]
atl2010 <- atl2010[,-(c(2:6,9:39))]

str(ATLbg)

ATLbg <- ATLbg[,-(c(3,4,9,10,13:36))]
test1 <- left_join(ATLbg,atl1940, by = "COUNTYA")
test1 <- left_join(test1,atl1970, by = "COUNTYA")
test1 <- left_join(test1,atl1980, by = "COUNTYA")
test1 <- left_join(test1,atl1990, by = "COUNTYA")
test1 <- left_join(test1,atl2000, by = "COUNTYA")
test1 <- left_join(test1,atl2010, by = "COUNTYA")

test1 <- test1 %>%
  rename(hu40 = BXR001, hu70 = CBV001, hu80 = C8Y001, hu90 = ESA001, hu00 =FKI001, hu10 = IFC001)

test1 <- test1[,-(c(33,35,37,39,41,43,44))]

# repeat county operations
# create a function for this, some day

# cherokee county - merge all years
cherokee <- cherokee[,-(c(3,4,9,10,13:36))]
cherokee <- merge(atl1940,cherokee,by="COUNTYA")
cherokee <- merge(atl1970,cherokee,by="COUNTYA")
cherokee <- merge(atl1980,cherokee,by="COUNTYA")
cherokee <- merge(atl1990,cherokee,by="COUNTYA")
cherokee <- merge(atl2000,cherokee,by="COUNTYA")
cherokee <- merge(atl2010,cherokee,by="COUNTYA")

# calculate adjusted housing units

cherokee$adj1940 <- ((cherokee$ADQSE011/sum(cherokee$ADQSE011))*cherokee$BXR001)
cherokee$adj1970 <- (((cherokee$adj1940+cherokee$ADQSE010+cherokee$ADQSE009+cherokee$ADQSE008)/(sum(cherokee$adj1940)+
                                                                                        sum(cherokee$ADQSE010)+sum(cherokee$ADQSE009)+sum(cherokee$ADQSE008)))*cherokee$CBV001)
cherokee$adj1980 <- (((cherokee$adj1970+cherokee$ADQSE007)/(sum(cherokee$adj1970)+sum(cherokee$ADQSE007))*cherokee$C8Y001))
cherokee$adj1990 <- (((cherokee$adj1980+cherokee$ADQSE006)/(sum(cherokee$adj1980)+sum(cherokee$ADQSE006))*cherokee$ESA001))
cherokee$adj2000 <- (((cherokee$adj1990+cherokee$ADQSE005)/(sum(cherokee$adj1990)+sum(cherokee$ADQSE005))*cherokee$FKI001))
cherokee$adj2010 <- (((cherokee$adj2000+cherokee$ADQSE004)/(sum(cherokee$adj2000)+sum(cherokee$ADQSE005)))*cherokee$IFC001)


adjhu <- function(species, data){
  ... #do stuff
}

test1 %>%
  group_by(COUNTYA) %>%
  mutate(
  adj40 = (ADQSE011/sum(ADQSE011)*hu40),
  adj40a = (hu40/sum(ADQSE011)*ADQSE011))

test1 %>%
  group_by(COUNTYA) %>%
  summarise(
    sum(ADQSE011)
  )

test1a <- test1 %>%
  group_by(COUNTYA) %>%
  mutate(
    j70 = sum(ADQSE008, ADQSE009, ADQSE010, ADQSE011),
    j80 = sum(ADQSE007,ADQSE008, ADQSE009, ADQSE010, ADQSE011),
    j90 = sum(ADQSE006,ADQSE007,ADQSE008, ADQSE009, ADQSE010, ADQSE011),
    j00 = sum(ADQSE005,ADQSE006,ADQSE007,ADQSE008, ADQSE009, ADQSE010, ADQSE011),
    j10 = sum(ADQSE004,ADQSE005,ADQSE006,ADQSE007,ADQSE008, ADQSE009, ADQSE010, ADQSE011),
    i70 = (ADQSE008 + ADQSE009 + ADQSE010 + ADQSE011),
    i80 = (ADQSE007 + ADQSE008 + ADQSE009 + ADQSE010 + ADQSE011),
    i90 = (ADQSE006 + ADQSE007 + ADQSE008 + ADQSE009 + ADQSE010 + ADQSE011),
    i00 = (ADQSE005 + ADQSE006 + ADQSE007 + ADQSE008 + ADQSE009 + ADQSE010 + ADQSE011),
    i10 = (ADQSE004 + ADQSE005 + ADQSE006 + ADQSE007 + ADQSE008 + ADQSE009 + ADQSE010 + ADQSE011),
    adj40a = (hu40/sum(ADQSE011)*ADQSE011),
    adj70a = hu70/j70 * i70,
    adj80a = hu80/j80 * i80,
    adj90a = hu90/j90 * i90,
    adj00a = hu00/j00 * i00,
    adj10a = hu10/j10 * i10
)


#b <- test1a %>%
#  filter(COUNTYA == 057)
#head(b)

(4932/1353)*18
(18/1353)*4932

test1a$COUNTYA <- as.factor(test1a$COUNTYA)

test1a %>% 
  ggplot((mapping = aes(x=COUNTYA, y = adj40a, group = COUNTYA)))+
  geom_boxplot() +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = FALSE)

test1a %>% 
  filter(adj40a < 500) %>%
  ggplot((mapping = aes(x=COUNTYA, y = adj10a, group = COUNTYA)))+
  geom_boxplot() +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = FALSE)

test1a %>% 
  filter(adj40a >10 & adj40a < 400) %>%
  ggplot((mapping = aes(x=ADQSE001, y = adj40a, color = as.factor(COUNTYA))))+
  geom_point()

test1a %>% 
  filter(adj40a >10 & adj40a < 500) %>%
  ggplot((mapping = aes(x = adj40a)))+
  geom_histogram(binwidth = 50) +
  facet_wrap('COUNTYA')

test1a %>% 
  ggplot((mapping = aes(x=COUNTYA, y = adj40a, group = COUNTYA)))+
  geom_boxplot()

test1a %>% 
  ggplot((mapping = aes(x=ADQSE001, y = adj40a, color = as.factor(COUNTYA))))+
  geom_point()

test1a %>%
  group_by(COUNTYA) %>%
  summarize(
    sum(ADQSE011)
  )

area2atl <- area2 %>%
  filter(STATEFP == '13' & COUNTYFP %in% atl$county)

test1b <- left_join(x = test1a, y = area2atl, by = c("GISJOIN.x"="GISJOIN"))

# ham = housing units/square mile
test1c <- test1b %>%
  group_by(COUNTYA) %>%
  mutate(
    ham40 = adj40a/area,
    ham70 = adj70a/area,
    ham80 = adj80a/area,
    ham90 = adj90a/area,
    ham00 = adj00a/area,
    ham10 = adj10a/area
  )

test1c <- ungroup(test1c)

# write_csv(test1c,"desktop/tempNHGIS/current projects/urban/atltest.csv")

test1c %>% 
  filter(ham40 >10 & ham40 < 500) %>%
  ggplot((mapping = aes(x=COUNTYA, y = ham40, group = COUNTYA)))+
  geom_boxplot()

test1c %>% 
  filter(ham40 >10 & ham40 < 500) %>%
  ggplot((mapping = aes(x=COUNTYA, y = ham40, group = COUNTYA)))+
  geom_boxplot()

test1c %>% 
  filter(ham10 > 10 & ham10 < 10000) %>%
  ggplot((mapping = aes(x=ham10, y = adj10a, color = as.factor(COUNTYA))))+
  geom_point()

test1c %>% 
  filter(ham90 >10 & ham90 < 800) %>%
  ggplot((mapping = aes(x = ham90)))+
  geom_histogram() +
  facet_wrap('COUNTYA')

test1c$urb40 <- ''
test1c[,'urb40'] <- sapply(test1c[,'ham40'],function(x)ifelse(x > 200,1,0))
test1c$urb40 <- as.factor(test1c$urb40)

test1c$urb70 <- ''
test1c$urb80 <- ''
test1c$urb90 <- ''
test1c$urb00 <- ''
test1c$urb10 <- ''

test1c[,'urb70'] <- ifelse((test1c$ham70 > 200 | test1c$urb40 == 1),1,0)
test1c[,'urb80'] <- ifelse((test1c$ham80 > 200 | test1c$urb70 == 1),1,0)
test1c[,'urb90'] <- ifelse((test1c$ham90 > 200 | test1c$urb80 == 1),1,0)
test1c[,'urb00'] <- ifelse((test1c$ham00 > 200 | test1c$urb90 == 1),1,0)
test1c[,'urb10'] <- ifelse((test1c$ham10 > 200 | test1c$urb00 == 1),1,0)

test1c$urb70 <- as.factor(test1c$urb70)
test1c$urb80 <- as.factor(test1c$urb80)
test1c$urb90 <- as.factor(test1c$urb90)
test1c$urb00 <- as.factor(test1c$urb00)
test1c$urb10 <- as.factor(test1c$urb10)

ggplot(test1c, mapping = aes(x = urb40)) +
  geom_histogram(stat = 'count')

ggplot(test1c, mapping = aes(x = urb40)) +
  geom_histogram(stat = 'count')

test1c %>%
  group_by(COUNTYA) %>%
  ggplot(mapping = aes(x = urb40)) +
  geom_histogram(stat = 'count')

test1c %>%
  count(urb40)

test1c %>% 
  ggplot((mapping = aes(x=COUNTYA, y = urb40, group = COUNTYA)))+
  geom_boxplot()

# gather all urb40:urb10 to compare urban/suburban over time ?

tidy1c <- test1c %>%
  gather("urb40","urb70","urb80","urb90","urb00","urb10",key = 'urban',na.rm = TRUE, value = "cases") %>%
  select(urban,cases,COUNTYA,ham40,ham70,ham80,ham90,ham00,ham10,YEAR)

test1c %>%
  count(YEAR)

# i need to add year and make it a variable to make this data tidy
# where do I delete 'YEAR' ?
# need to find that!

# 'YEAR' is a *variable* and what year it is is a *value*
# look at tidy census. how is it different from other census data you use?
# combining years, think : full_join, not left join

# spatial data is full of left joins because of 'block groups' is what we are consolidating
# everything to, and then modifying the variables in such a way that we can link them.



### tangent alert: this is ths problem with the weights data. it wasn't tidy!
# i needed to gather the collection of years, to make the dataset longer!
### quadwt is tidy

tidy1c %>%
  count(urban,ham40)

class(test1c$urb10)

test1c %>%
  group_by(urb10,COUNTYA) %>%
  summarize(
    tot = sum(ham40)
  )


tidy1c %>%
  filter(urban == 'urb10') %>%
  group_by(urban, COUNTYA) %>%
  summarize(
    tot = sum(ham40)
  )

tidy1c %>%
  filter(urban == 'urb10') %>%
  group_by(YEAR, urban, COUNTYA) %>%
  count()


str(tidy1c)
tidy1c$urban <- as.factor(tidy1c$urban)

tidy1c %>% 
  group_by(urban) %>%
  ggplot((mapping = aes(x=cases)))+
  geom_histogram(stat = 'count') +
  facet_wrap(~urban)

tidy1c %>%
  count(urban,cases)

tidy1c %>% 
  group_by(urban) %>%
  filter(ham90 < 2000) %>%
  ggplot((mapping = aes(x = urban, y=ham90, fill = ham90)))+
  geom_boxplot() 



###### the above graph is handy

# i'm just messing around iin lines after 260. none of this produces anything meaningful
# from the tidy1c data. I think tidy1c is the best way to look at how urbanization changes
# over time, i.e. compare 1940-2010, but I haven't figured how to create visualizations 
# from it yet. 
# I also have no idea why countya is attached to any // %>% count() ?


test1c %>% 
  filter(ham90 >10 & ham90 < 800) %>%
  ggplot((mapping = aes(x = ham90)))+
  geom_histogram() +
  facet_wrap('COUNTYA')

test1c %>% 
  ggplot((mapping = aes(x=COUNTYA, y = ham40, group = COUNTYA)))+
  geom_boxplot()


# this is the 'end'

# need to spend some time looking at hamXX, huXX, adjXXa values through ggplot to make
# sure data makes sense. Need to think about 'water'
# also need to join and map.

# then, scale it up to all metros/block groups in US



# clayton county - merge all years
clayton <- clayton[,-(c(3,4,9,10,13:36))]
clayton <- merge(atl1940,clayton,by="COUNTYA")
clayton <- merge(atl1970,clayton,by="COUNTYA")
clayton <- merge(atl1980,clayton,by="COUNTYA")
clayton <- merge(atl1990,clayton,by="COUNTYA")
clayton <- merge(atl2000,clayton,by="COUNTYA")
clayton <- merge(atl2010,clayton,by="COUNTYA")

# calculate adjusted housing units - Clayton

clayton$adj1940 <- ((clayton$ADQSE011/sum(clayton$ADQSE011))*clayton$BXR001)
clayton$adj1970 <- (((clayton$adj1940+clayton$ADQSE010+clayton$ADQSE009+clayton$ADQSE008)/(sum(clayton$adj1940)+
                                                                                                  sum(clayton$ADQSE010)+sum(clayton$ADQSE009)+sum(clayton$ADQSE008)))*clayton$CBV001)
clayton$adj1980 <- (((clayton$adj1970+clayton$ADQSE007)/(sum(clayton$adj1970)+sum(clayton$ADQSE007))*clayton$C8Y001))
clayton$adj1990 <- (((clayton$adj1980+clayton$ADQSE006)/(sum(clayton$adj1980)+sum(clayton$ADQSE006))*clayton$ESA001))
clayton$adj2000 <- (((clayton$adj1990+clayton$ADQSE005)/(sum(clayton$adj1990)+sum(clayton$ADQSE005))*clayton$FKI001))
clayton$adj2010 <- (((clayton$adj2000+clayton$ADQSE004)/(sum(clayton$adj2000)+sum(clayton$ADQSE005)))*clayton$IFC001)




# cobb county - merge all years
cobb <- cobb[,-(c(3,4,9,10,13:36))]
cobb <- merge(atl1940,cobb,by="COUNTYA")
cobb <- merge(atl1970,cobb,by="COUNTYA")
cobb <- merge(atl1980,cobb,by="COUNTYA")
cobb <- merge(atl1990,cobb,by="COUNTYA")
cobb <- merge(atl2000,cobb,by="COUNTYA")
cobb <- merge(atl2010,cobb,by="COUNTYA")

# calculate adjusted housing units - cobb

cobb$adj1940 <- ((cobb$ADQSE011/sum(cobb$ADQSE011))*cobb$BXR001)
cobb$adj1970 <- (((cobb$adj1940+cobb$ADQSE010+cobb$ADQSE009+cobb$ADQSE008)/(sum(cobb$adj1940)+
                                                                                             sum(cobb$ADQSE010)+sum(cobb$ADQSE009)+sum(cobb$ADQSE008)))*cobb$CBV001)
cobb$adj1980 <- (((cobb$adj1970+cobb$ADQSE007)/(sum(cobb$adj1970)+sum(cobb$ADQSE007))*cobb$C8Y001))
cobb$adj1990 <- (((cobb$adj1980+cobb$ADQSE006)/(sum(cobb$adj1980)+sum(cobb$ADQSE006))*cobb$ESA001))
cobb$adj2000 <- (((cobb$adj1990+cobb$ADQSE005)/(sum(cobb$adj1990)+sum(cobb$ADQSE005))*cobb$FKI001))
cobb$adj2010 <- (((cobb$adj2000+cobb$ADQSE004)/(sum(cobb$adj2000)+sum(cobb$ADQSE005)))*cobb$IFC001)



# dekalb county - merge all years
dekalb <- dekalb[,-(c(3,4,9,10,13:36))]
dekalb <- merge(atl1940,dekalb,by="COUNTYA")
dekalb <- merge(atl1970,dekalb,by="COUNTYA")
dekalb <- merge(atl1980,dekalb,by="COUNTYA")
dekalb <- merge(atl1990,dekalb,by="COUNTYA")
dekalb <- merge(atl2000,dekalb,by="COUNTYA")
dekalb <- merge(atl2010,dekalb,by="COUNTYA")

# calculate adjusted housing units - dekalb

dekalb$adj1940 <- ((dekalb$ADQSE011/sum(dekalb$ADQSE011))*dekalb$BXR001)
dekalb$adj1970 <- (((dekalb$adj1940+dekalb$ADQSE010+dekalb$ADQSE009+dekalb$ADQSE008)/(sum(dekalb$adj1940)+
                                                                              sum(dekalb$ADQSE010)+sum(dekalb$ADQSE009)+sum(dekalb$ADQSE008)))*dekalb$CBV001)
dekalb$adj1980 <- (((dekalb$adj1970+dekalb$ADQSE007)/(sum(dekalb$adj1970)+sum(dekalb$ADQSE007))*dekalb$C8Y001))
dekalb$adj1990 <- (((dekalb$adj1980+dekalb$ADQSE006)/(sum(dekalb$adj1980)+sum(dekalb$ADQSE006))*dekalb$ESA001))
dekalb$adj2000 <- (((dekalb$adj1990+dekalb$ADQSE005)/(sum(dekalb$adj1990)+sum(dekalb$ADQSE005))*dekalb$FKI001))
dekalb$adj2010 <- (((dekalb$adj2000+dekalb$ADQSE004)/(sum(dekalb$adj2000)+sum(dekalb$ADQSE005)))*dekalb$IFC001)




# douglas county - merge all years
douglas <- douglas[,-(c(3,4,9,10,13:36))]
douglas <- merge(atl1940,douglas,by="COUNTYA")
douglas <- merge(atl1970,douglas,by="COUNTYA")
douglas <- merge(atl1980,douglas,by="COUNTYA")
douglas <- merge(atl1990,douglas,by="COUNTYA")
douglas <- merge(atl2000,douglas,by="COUNTYA")
douglas <- merge(atl2010,douglas,by="COUNTYA")

# calculate adjusted housing units - douglas

douglas$adj1940 <- ((douglas$ADQSE011/sum(douglas$ADQSE011))*douglas$BXR001)
douglas$adj1970 <- (((douglas$adj1940+douglas$ADQSE010+douglas$ADQSE009+douglas$ADQSE008)/(sum(douglas$adj1940)+
                                                                                        sum(douglas$ADQSE010)+sum(douglas$ADQSE009)+sum(douglas$ADQSE008)))*douglas$CBV001)
douglas$adj1980 <- (((douglas$adj1970+douglas$ADQSE007)/(sum(douglas$adj1970)+sum(douglas$ADQSE007))*douglas$C8Y001))
douglas$adj1990 <- (((douglas$adj1980+douglas$ADQSE006)/(sum(douglas$adj1980)+sum(douglas$ADQSE006))*douglas$ESA001))
douglas$adj2000 <- (((douglas$adj1990+douglas$ADQSE005)/(sum(douglas$adj1990)+sum(douglas$ADQSE005))*douglas$FKI001))
douglas$adj2010 <- (((douglas$adj2000+douglas$ADQSE004)/(sum(douglas$adj2000)+sum(douglas$ADQSE005)))*douglas$IFC001)




# fayette county - merge all years
fayette <- fayette[,-(c(3,4,9,10,13:36))]
fayette <- merge(atl1940,fayette,by="COUNTYA")
fayette <- merge(atl1970,fayette,by="COUNTYA")
fayette <- merge(atl1980,fayette,by="COUNTYA")
fayette <- merge(atl1990,fayette,by="COUNTYA")
fayette <- merge(atl2000,fayette,by="COUNTYA")
fayette <- merge(atl2010,fayette,by="COUNTYA")

# calculate adjusted housing units - fayette

fayette$adj1940 <- ((fayette$ADQSE011/sum(fayette$ADQSE011))*fayette$BXR001)
fayette$adj1970 <- (((fayette$adj1940+fayette$ADQSE010+fayette$ADQSE009+fayette$ADQSE008)/(sum(fayette$adj1940)+
                                                                                        sum(fayette$ADQSE010)+sum(fayette$ADQSE009)+sum(fayette$ADQSE008)))*fayette$CBV001)
fayette$adj1980 <- (((fayette$adj1970+fayette$ADQSE007)/(sum(fayette$adj1970)+sum(fayette$ADQSE007))*fayette$C8Y001))
fayette$adj1990 <- (((fayette$adj1980+fayette$ADQSE006)/(sum(fayette$adj1980)+sum(fayette$ADQSE006))*fayette$ESA001))
fayette$adj2000 <- (((fayette$adj1990+fayette$ADQSE005)/(sum(fayette$adj1990)+sum(fayette$ADQSE005))*fayette$FKI001))
fayette$adj2010 <- (((fayette$adj2000+fayette$ADQSE004)/(sum(fayette$adj2000)+sum(fayette$ADQSE005)))*fayette$IFC001)




# fulton county - merge all years
fulton <- fulton[,-(c(3,4,9,10,13:36))]
fulton <- merge(atl1940,fulton,by="COUNTYA")
fulton <- merge(atl1970,fulton,by="COUNTYA")
fulton <- merge(atl1980,fulton,by="COUNTYA")
fulton <- merge(atl1990,fulton,by="COUNTYA")
fulton <- merge(atl2000,fulton,by="COUNTYA")
fulton <- merge(atl2010,fulton,by="COUNTYA")

# calculate adjusted housing units - fulton

fulton$adj1940 <- ((fulton$ADQSE011/sum(fulton$ADQSE011))*fulton$BXR001)
fulton$adj1970 <- (((fulton$adj1940+fulton$ADQSE010+fulton$ADQSE009+fulton$ADQSE008)/(sum(fulton$adj1940)+
                                                                                             sum(fulton$ADQSE010)+sum(fulton$ADQSE009)+sum(fulton$ADQSE008)))*fulton$CBV001)
fulton$adj1980 <- (((fulton$adj1970+fulton$ADQSE007)/(sum(fulton$adj1970)+sum(fulton$ADQSE007))*fulton$C8Y001))
fulton$adj1990 <- (((fulton$adj1980+fulton$ADQSE006)/(sum(fulton$adj1980)+sum(fulton$ADQSE006))*fulton$ESA001))
fulton$adj2000 <- (((fulton$adj1990+fulton$ADQSE005)/(sum(fulton$adj1990)+sum(fulton$ADQSE005))*fulton$FKI001))
fulton$adj2010 <- (((fulton$adj2000+fulton$ADQSE004)/(sum(fulton$adj2000)+sum(fulton$ADQSE005)))*fulton$IFC001)




# gwinnett county - merge all years
gwinnett <- gwinnett[,-(c(3,4,9,10,13:36))]
gwinnett <- merge(atl1940,gwinnett,by="COUNTYA")
gwinnett <- merge(atl1970,gwinnett,by="COUNTYA")
gwinnett <- merge(atl1980,gwinnett,by="COUNTYA")
gwinnett <- merge(atl1990,gwinnett,by="COUNTYA")
gwinnett <- merge(atl2000,gwinnett,by="COUNTYA")
gwinnett <- merge(atl2010,gwinnett,by="COUNTYA")

# calculate adjusted housing units - gwinnett

gwinnett$adj1940 <- ((gwinnett$ADQSE011/sum(gwinnett$ADQSE011))*gwinnett$BXR001)
gwinnett$adj1970 <- (((gwinnett$adj1940+gwinnett$ADQSE010+gwinnett$ADQSE009+gwinnett$ADQSE008)/(sum(gwinnett$adj1940)+
                                                                                        sum(gwinnett$ADQSE010)+sum(gwinnett$ADQSE009)+sum(gwinnett$ADQSE008)))*gwinnett$CBV001)
gwinnett$adj1980 <- (((gwinnett$adj1970+gwinnett$ADQSE007)/(sum(gwinnett$adj1970)+sum(gwinnett$ADQSE007))*gwinnett$C8Y001))
gwinnett$adj1990 <- (((gwinnett$adj1980+gwinnett$ADQSE006)/(sum(gwinnett$adj1980)+sum(gwinnett$ADQSE006))*gwinnett$ESA001))
gwinnett$adj2000 <- (((gwinnett$adj1990+gwinnett$ADQSE005)/(sum(gwinnett$adj1990)+sum(gwinnett$ADQSE005))*gwinnett$FKI001))
gwinnett$adj2010 <- (((gwinnett$adj2000+gwinnett$ADQSE004)/(sum(gwinnett$adj2000)+sum(gwinnett$ADQSE005)))*gwinnett$IFC001)




# henry county - merge all years
henry <- henry[,-(c(3,4,9,10,13:36))]
henry <- merge(atl1940,henry,by="COUNTYA")
henry <- merge(atl1970,henry,by="COUNTYA")
henry <- merge(atl1980,henry,by="COUNTYA")
henry <- merge(atl1990,henry,by="COUNTYA")
henry <- merge(atl2000,henry,by="COUNTYA")
henry <- merge(atl2010,henry,by="COUNTYA")

# calculate adjusted housing units - henry

henry$adj1940 <- ((henry$ADQSE011/sum(henry$ADQSE011))*henry$BXR001)
henry$adj1970 <- (((henry$adj1940+henry$ADQSE010+henry$ADQSE009+henry$ADQSE008)/(sum(henry$adj1940)+
                                                                                                  sum(henry$ADQSE010)+sum(henry$ADQSE009)+sum(henry$ADQSE008)))*henry$CBV001)
henry$adj1980 <- (((henry$adj1970+henry$ADQSE007)/(sum(henry$adj1970)+sum(henry$ADQSE007))*henry$C8Y001))
henry$adj1990 <- (((henry$adj1980+henry$ADQSE006)/(sum(henry$adj1980)+sum(henry$ADQSE006))*henry$ESA001))
henry$adj2000 <- (((henry$adj1990+henry$ADQSE005)/(sum(henry$adj1990)+sum(henry$ADQSE005))*henry$FKI001))
henry$adj2010 <- (((henry$adj2000+henry$ADQSE004)/(sum(henry$adj2000)+sum(henry$ADQSE005)))*henry$IFC001)



# rockdale county - merge all years
rockdale <- rockdale[,-(c(3,4,9,10,13:36))]
rockdale <- merge(atl1940,rockdale,by="COUNTYA")
rockdale <- merge(atl1970,rockdale,by="COUNTYA")
rockdale <- merge(atl1980,rockdale,by="COUNTYA")
rockdale <- merge(atl1990,rockdale,by="COUNTYA")
rockdale <- merge(atl2000,rockdale,by="COUNTYA")
rockdale <- merge(atl2010,rockdale,by="COUNTYA")

# calculate adjusted housing units - rockdale

rockdale$adj1940 <- ((rockdale$ADQSE011/sum(rockdale$ADQSE011))*rockdale$BXR001)
rockdale$adj1970 <- (((rockdale$adj1940+rockdale$ADQSE010+rockdale$ADQSE009+rockdale$ADQSE008)/(sum(rockdale$adj1940)+
                                                                                   sum(rockdale$ADQSE010)+sum(rockdale$ADQSE009)+sum(rockdale$ADQSE008)))*rockdale$CBV001)
rockdale$adj1980 <- (((rockdale$adj1970+rockdale$ADQSE007)/(sum(rockdale$adj1970)+sum(rockdale$ADQSE007))*rockdale$C8Y001))
rockdale$adj1990 <- (((rockdale$adj1980+rockdale$ADQSE006)/(sum(rockdale$adj1980)+sum(rockdale$ADQSE006))*rockdale$ESA001))
rockdale$adj2000 <- (((rockdale$adj1990+rockdale$ADQSE005)/(sum(rockdale$adj1990)+sum(rockdale$ADQSE005))*rockdale$FKI001))
rockdale$adj2010 <- (((rockdale$adj2000+rockdale$ADQSE004)/(sum(rockdale$adj2000)+sum(rockdale$ADQSE005)))*rockdale$IFC001)

atldf <- rbind(cherokee,clayton,cobb,dekalb,douglas,fayette,fulton,gwinnett,henry,rockdale)




atlshp <- read.csv("~/Dropbox/pro dev/AAG/Boston/atlbg1.csv")
atlshp$sqmi <- atlshp$Shape_area / 2590000

atldf <- merge(x = atldf, y =atlshp, by.x = "GISJOIN.y", by.y = "GISJOIN")
atldf <- atldf[,-(52:65)]

atldf$sqmi40 <- atldf$adj1940/atldf$sqmi
atldf$sqmi70 <- atldf$adj1970/atldf$sqmi
atldf$sqmi80 <- atldf$adj1980/atldf$sqmi
atldf$sqmi90 <- atldf$adj1990/atldf$sqmi
atldf$sqmi00 <- atldf$adj2000/atldf$sqmi
atldf$sqmi10 <- atldf$adj2010/atldf$sqmi
atldf$sqmi15 <- atldf$ADQSE001/atldf$sqmi




write.csv(atldf,"~/Dropbox/pro dev/AAG/Boston/hammerATL.csv")


# trying to account for total urbanized area; completed in GEOG321 in arcMap on Wednesday before AAG
# 1 in 'u40' 'u70,' etc. if urbanized
# after completing in arc, this updated file is read back into 'cyanalysis.R'

atldf2 <- atldf[,-(5:45)]
write.csv(atldf2,"~/Dropbox/pro dev/AAG/Boston/hammerATL2.csv")



# ***************************************************
# the following code is commented out and incorrect or inefficient
# remains only as a precaution at this point. delete in one month if not using
# - TJH 4/12/17



# read in atl block groups with urban classification; doing so in order to trim last character of GEOID to join at CT level
# yes, this is stupid and redundant. there is a much better way to do this, but at this point, considering time and ability,
# this is the best way to approach the problem for now. - TJH 4/217 @ 4:31p


#hu1940 <- read.csv("desktop/nhgis0022_csv/nhgis0022_ds78_1940_county.csv")
#atl1940 <- hu1940[hu1940$GISJOIN %in% atlnh$county,]
#atl1940$COUNTYA <- atl1940$COUNTYA/10

#Mtest <- merge(atl1940,ATLbg,by="COUNTYA")

#Mtest$adj1940 <- ((Mtest$ADQSE011/sum(Mtest$ADQSE011))*Mtest$BXR001)

#hu1970 <- read.csv("desktop/nhgis0025_csv/nhgis0025_ds94_1970_county.csv")
#atl1970 <- hu1970[hu1970$GISJOIN %in% atlnh$county,]
#Mtest2 <- merge(atl1970,Mtest,by="COUNTYA")


#Mtest2$adj1970 <- (((Mtest2$adj1940+Mtest2$ADQSE010+Mtest2$ADQSE009+Mtest2$ADQSE008)/(sum(Mtest2$adj1940)+
#                                                                                        sum(Mtest2$ADQSE010)+sum(Mtest2$ADQSE009)+sum(Mtest2$ADQSE008)))*Mtest2$CBV001)

#Mtest2$adj1940+Mtest2$ADQSE010+Mtest2$ADQSE009+Mtest2$ADQSE008                  
#(sum(Mtest2$adj1940)+sum(Mtest2$ADQSE010)+sum(Mtest2$ADQSE009)+sum(Mtest2$ADQSE008))


#hu1980 <- read.csv("desktop/nhgis0025_csv/nhgis0025_ds104_1980_county.csv")
#atl1980 <- hu1980[hu1980$GISJOIN %in% atlnh$county,]
#Mtest3 <- merge(atl1980,Mtest2,by="COUNTYA")

#Mtest3$adj1980 <- (((Mtest3$adj1970+Mtest3$ADQSE007)/(sum(Mtest3$adj1970)+sum(Mtest3$ADQSE007))*Mtest3$C8Y001))


#hu1990 <- read.csv("desktop/nhgis0025_csv/nhgis0025_ds120_1990_county.csv")
#atl1990 <- hu1990[hu1990$GISJOIN %in% atlnh$county,]
#Mtest4 <- merge(atl1990,Mtest3,by="COUNTYA")

#Mtest4$adj1990 <- (((Mtest4$adj1980+Mtest4$ADQSE006)/(sum(Mtest4$adj1980)+sum(Mtest4$ADQSE006))*Mtest4$ESA001))
#Mtest4a <- Mtest4[,-(54:77)]


#hu2000 <- read.csv("desktop/nhgis0025_csv/nhgis0025_ds146_2000_county.csv")
#atl2000 <- hu2000[hu2000$GISJOIN %in% atlnh$county,]
#Mtest5 <- merge(atl2000,Mtest4a,by="COUNTYA")

#Mtest5$adj2000 <- (((Mtest5$adj1990+Mtest5$ADQSE005)/(sum(Mtest5$adj1990)+sum(Mtest5$ADQSE005))*Mtest5$FKI001))

#hu2010 <- read.csv("desktop/nhgis0026_csv/nhgis0026_ds172_2010_county.csv")
#atl2010 <- hu2010[hu2010$GISJOIN %in% atlnh$county,]
#atl2010 <- atl2010[,-(9:38)]

#Mtest6 <- merge(atl2010,Mtest5,by="COUNTYA")
#Mtest6$adj2010 <- (((Mtest6$adj2000+Mtest6$ADQSE004)/(sum(Mtest6$adj2000)+sum(Mtest6$ADQSE005)))*Mtest6$IFC001)
#Mtest6a <- Mtest6[,-(29:36)]

#write.csv(Mtest6a,"~/Dropbox/pro dev/AAG/hammerATL.csv")


### retry
#*************************

#ATLbg <- merge(atl1940,ATLbg,by="COUNTYA")

#ATLbg$adj1940 <- ((ATLbg$ADQSE011/sum(ATLbg$ADQSE011))*ATLbg$BXR001)
#ATLbg <- ATLbg[,-(17:44)]

#hu1970 <- read.csv("desktop/nhgis0025_csv/nhgis0025_ds94_1970_county.csv")
#atl1970 <- hu1970[hu1970$GISJOIN %in% atlnh$county,]
#ATLbg <- merge(atl1970,ATLbg,by="COUNTYA")


#ATLbg$adj1970 <- (((ATLbg$adj1940+ATLbg$ADQSE010+ATLbg$ADQSE009+ATLbg$ADQSE008)/(sum(ATLbg$adj1940)+
 #                                                                                       sum(ATLbg$ADQSE010)+sum(ATLbg$ADQSE009)+sum(ATLbg$ADQSE008)))*ATLbg$CBV001)
#sum(ATLbg$adj1940+ATLbg$ADQSE010)
#sum(ATLbg$ADQSE009)
#sum(ATLbg$ADQSE008)



#hu1980 <- read.csv("desktop/nhgis0025_csv/nhgis0025_ds104_1980_county.csv")
#atl1980 <- hu1980[hu1980$GISJOIN %in% atlnh$county,]
#ATLbg <- merge(atl1980,ATLbg,by="COUNTYA")

#ATLbg$adj1980 <- (((ATLbg$adj1970+ATLbg$ADQSE007)/(sum(ATLbg$adj1970)+sum(ATLbg$ADQSE007))*ATLbg$C8Y001))


#hu1990 <- read.csv("desktop/nhgis0025_csv/nhgis0025_ds120_1990_county.csv")
#atl1990 <- hu1990[hu1990$GISJOIN %in% atlnh$county,]
#ATLbg <- merge(atl1990,ATLbg,by="COUNTYA")

#ATLbg$adj1990 <- (((ATLbg$adj1980+ATLbg$ADQSE006)/(sum(ATLbg$adj1980+ATLbg$ADQSE006))*ATLbg$ESA001))


#hu2000 <- read.csv("desktop/nhgis0025_csv/nhgis0025_ds146_2000_county.csv")
#atl2000 <- hu2000[hu2000$GISJOIN %in% atlnh$county,]
#ATLbg <- merge(atl2000,ATLbg,by="COUNTYA")

#ATLbg$adj2000 <- (((ATLbg$adj1990+ATLbg$ADQSE005)/(sum(ATLbg$adj1990)+sum(ATLbg$ADQSE005))*ATLbg$FKI001))

#hu2010 <- read.csv("desktop/nhgis0026_csv/nhgis0026_ds172_2010_county.csv")
#atl2010 <- hu2010[hu2010$GISJOIN %in% atlnh$county,]
#atl2010 <- atl2010[,-(9:38)]

#ATLbg <- merge(atl2010,ATLbg,by="COUNTYA")
#ATLbg$adj2010 <- (((ATLbg$adj2000+ATLbg$ADQSE004)/(sum(ATLbg$adj2000)+sum(ATLbg$ADQSE005)))*ATLbg$IFC001)
#ATLbga <- ATLbg[,-(29:36)]

#write.csv(ATLbg,"~/desktop/hammerATL.csv")



# keep these in mind. 
by(awp$black, awp$middleschool,summary)
by(awp$black, awp$middleschool,sd, na.rm = TRUE)

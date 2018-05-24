# Hammer US CT
library(tidyverse)
library(stringr)

USct <- read.csv("desktop/tempNHGIS/current projects/urban/nhgis0054_csv/nhgis0054_ds215_20155_2015_tract.csv")
USct <- as_tibble(USct)
head(USct)

USarea <- read.csv("desktop/tempNHGIS/current projects/urban/areasct.txt")
USarea <- as_tibble(USarea)
head(USarea)
USarea <- USarea[,-(c(7:9))]

#read in housing unit by year Census
hu1940 <- read.csv("dropbox/boston//nhgis0022_csv/nhgis0022_ds78_1940_county.csv")
hu1940 <- as_tibble(hu1940)
hu1940$COUNTYA <- as.integer(hu1940$COUNTYA/10)
str(hu1940)

hu1970 <- read_csv("dropbox/boston//nhgis0025_csv/nhgis0025_ds94_1970_county.csv")
hu1980 <- read_csv("dropbox/boston//nhgis0025_csv/nhgis0025_ds104_1980_county.csv")
hu1990 <- read_csv("dropbox/boston//nhgis0025_csv/nhgis0025_ds120_1990_county.csv")
hu2000 <- read_csv("dropbox/boston//nhgis0025_csv/nhgis0025_ds146_2000_county.csv")
hu2010 <- read.csv("dropbox/boston//nhgis0026_csv/nhgis0026_ds172_2010_county.csv")
hu2010 <- as_tibble(hu2010)
str(hu2010)

USct$COUNTYJ <- str_sub(USct$GISJOIN, 1, 8)


hu1940 <- hu1940[,-(c(3:5,9))]
hu1970 <- hu1970[,-(3:6)]
hu1980 <- hu1980[,-(c(3:7,10))]
hu1990 <- hu1990[,-(c(5:16))]
hu2000 <- hu2000[,-(c(3:6,9:12))]
hu2010 <- hu2010[,-(c(3:6,9:39))]

USct <- USct[,-(c(3,4,9,10,12:37))]
library(magrittr)

hu1940 %<>%
  rename(COUNTYJ = GISJOIN)
hu1970 %<>%
  rename(COUNTYJ = GISJOIN)
hu1980 %<>%
  rename(COUNTYJ = GISJOIN)
hu1990 %<>%
  rename(COUNTYJ = GISJOIN)
hu2000 %<>%
  rename(COUNTYJ = GISJOIN)
hu2010 %<>%
  rename(COUNTYJ = GISJOIN)

hu1940$COUNTYJ <- as.factor(hu1940$COUNTYJ)
USct$COUNTYJ <- as.factor(USct$COUNTYJ)
str(USct)

test1 <- left_join(USct,hu1940, by = ("COUNTYJ"))
test1 <- left_join(test1,hu1970, by = "COUNTYJ")
test1 <- left_join(test1,hu1980, by = "COUNTYJ")
test1 <- left_join(test1,hu1990, by = "COUNTYJ")
test1 <- left_join(test1,hu2000, by = "COUNTYJ")
test1 <- left_join(test1,hu2010, by = "COUNTYJ")

test1 <- test1 %>%
  rename(hu40 = BXR001, hu70 = CBV001, hu80 = C8Y001, hu90 = ESA001, hu00 =FKI001, hu10 = IFC001)

# test1 <- test1[,-(c(33,35,37,39,41,43,44))]

test1a <- test1 %>%
  group_by(COUNTYJ) %>%
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


test1a$COUNTYJ <- as.factor(test1a$COUNTYJ)

test1a %>%
  group_by(COUNTYJ) %>%
  summarize(
    sum(ADQSE011)
  )

# join area earlier (at beginning of script. Also, reduce datset further)
test1b <- left_join(x = test1a, y = USarea, by = "GISJOIN")

# ham = housing units/square mile
test1c <- test1b %>%
  group_by(COUNTYJ) %>%
  mutate(
    ham40 = adj40a/areaCT,
    ham70 = adj70a/areaCT,
    ham80 = adj80a/areaCT,
    ham90 = adj90a/areaCT,
    ham00 = adj00a/areaCT,
    ham10 = adj10a/areaCT
  )

test1c <- ungroup(test1c)


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

write.csv(test1c,"desktop/tempNHGIS/current projects/urban/usctham.csv")

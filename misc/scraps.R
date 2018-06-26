# old_code

#core_based_statistical_areas
#core_based_statistical_areas(cb = FALSE, resolution = "500k", year = NULL,
#                             ...)

#load_variables(year = 2015, dataset = "acs5") %>% View
#load_variables(year = 2010, dataset = "acs5") %>% View



USbg <- USbg[,-(c(3,4,9,10,13:37))]


USbg %>%
  mutate(COUNTYJ = str_sub(GISJOIN, 1, 8))

USarea <- USarea[,-(c(7:9))]

USbg2 %>%
  rename(geoid = GEOID) -> USbg2
hu1940 <- as_tibble(hu1940)
hu1940$COUNTYA <- as.integer(hu1940$COUNTYA/10)



hu1940 %<>%
  rename(COUNTYJ = GISJOIN)
hu1970 %<>%
  rename(COUNTYJ = GISJOIN)
hu1980 %>%
  rename(COUNTYJ = GISJOIN) -> hu1980
hu1990 %<>%
  rename(COUNTYJ = GISJOIN)
hu2000 %<>%
  rename(COUNTYJ = GISJOIN)
hu2010 %<>%
  rename(COUNTYJ = GISJOIN)

# modify by Year Census
hu1940 <- hu1940 %>%
  select(-c(3:7,9)) %>%
  rename(COUNTYJ = GISJOIN,
         h_units = BXR001)

hu1970 <- hu1970 %>%
  select(-c(3:7)) %>%
  rename(COUNTYJ = GISJOIN, 
         h_units = CBV001)

hu1980 <- hu1980 %>%
  select(-c(3:10))%>%
  rename(COUNTYJ = GISJOIN,
         h_units = C8Y001)

hu1990 <- hu1990 %>%
  select(-c(3:16))%>%
  rename(COUNTYJ = GISJOIN,
         h_units = ESA001)

hu2000 <- hu2000 %>%
  select(-c(3:12))%>%
  rename(COUNTYJ = GISJOIN,
         h_units = FKI001)

hu2010 <- hu2010 %>%
  select(-c(3:39))%>%
  rename(COUNTYJ = GISJOIN,
         h_units = IFC001)


h1 <- old_hu2 %>%
  rename(h_units = BXR001) %>%
  select(GISJOIN, YEAR, h_units)

h1 %>%
  spread(YEAR, h_units)

h1$YEAR <- as.character(h1$YEAR)

#mutate(GEOID = str_sub(FIPS, 3,2 ) <- '0')
#old_hu2 <- old_hu2 %>%
#  rename(hu40 = BXR001, hu70 = CBV001, hu80 = C8Y001, hu90 = ESA001, hu00 =FKI001, hu10 = IFC001) %>%
#  select(-c(2,4,6,8,10,12))

old_hu2 %>%
  rename(h_units = BXR001) %>%
  select(GISJOIN, YEAR, h_units)

old_hu2 <- old_hu2 %>%
  rename(hu40 = BXR001, hu70 = CBV001, hu80 = C8Y001, hu90 = ESA001, hu00 =FKI001, hu10 = IFC001) %>%
  select(-c(2,4,6,8,10,12))

old_hu2 <- old_hu2 %>%
  rename(hu40 = BXR001, hu70 = CBV001, hu80 = C8Y001, hu90 = ESA001, hu00 =FKI001, hu10 = IFC001) %>%
  gather()
old_hu2
h1

h1$YEAR <- as.factor(h1$YEAR)
h1 %>%
  spread(YEAR, h_units)

# join historic census data with block group data
US_bg <- reduce(list(USbg2,hu1940,hu1970,hu1980,hu1990,hu2000,hu2010), left_join, by = "GISJOIN")

# rename variables for historic census housing units
US_bg <- US_bg %>%
  rename(hu40 = BXR001, hu70 = CBV001, hu80 = C8Y001, hu90 = ESA001, hu00 =FKI001, hu10 = IFC001)


# this is the 'end'

# data on # of homeowners in Fulton County
df_acs15 <- get_acs(
  geography = "block group",
  county = c("Fulton","DeKalb","Gwinnett","Cherokee",
             "Cobb","Douglas","Fayette","Clayton",
             "Henry","Rockdale"),
  state = "GA",
  variables = c('B25003_001','B25003_002','B25003_003'), 
  year = 2015,
  key = th_api_acs,
  geometry = TRUE,
  output = 'wide'
) %>% clean_names() %>%
  rename(B25003_001_15 = b25003_001e, B25003_002_15 = b25003_002e, B25003_003_15 = b25003_003e) %>%
  mutate(hopct15 = B25003_002_15/B25003_001_15*100)
class(df_acs15)

# data on # of poor people in ARC
df_acs15 <- get_acs(
  geography = "tract",
  county = c("Fulton","DeKalb","Gwinnett","Cherokee",
             "Cobb","Douglas","Fayette","Clayton",
             "Henry","Rockdale"),
  state = "GA",
  variables = c('B17001_002E','B17001_002M','B01003_001E', 'B01003_001M'), 
  year = 2015,
  key = th_api_acs,
  geometry = TRUE,
  output = 'wide'
) %>% clean_names() %>%
  mutate(pov_pct = b17001_002e/b01003_001e*100,
         pov_moe = b17001_002m/b01003_001m*100)
class(df_acs15)



# data on # of poor people in ARC
library(janitor)

df_acs15 <- get_acs(
  geography = "block group",
  county = c("Clark"), 
  state = "OH",
  variables = c('B17001_002E','B17001_002M','B01003_001E', 'B01003_001M','B25003_001','B25003_002','B25003_003'), 
  year = 2015,
  key = th_api_acs,
  geometry = TRUE,
  output = 'wide'
) %>% clean_names() %>%
  rename(B25003_001_15 = b25003_001e, B25003_002_15 = b25003_002e, B25003_003_15 = b25003_003e) %>%
  mutate(hopct15 = B25003_002_15/B25003_001_15*100,
         pov_pct = b17001_002e/b01003_001e*100,
         pov_moe = b17001_002m/b01003_001m*100)
class(df_acs15)

test1c$area %>% b2 
# now, Join 2010 "tbl_df" to 2015 "sf"

gaham <- US_bg2 %>%
  filter(STATEA == 13)

library(sp)
acsjoin <- merge(x = df_acs15, y = gaham, by. = 'geoid', by.y = 'GEOID')
acsjoin <- merge(x = df_acs15, y = test1c, by.x = 'geoid', by.y = 'GEOID')

sprfldj <- merge(x = df_acs15, y = test1c, by.x = 'geoid', by.y = 'GEOID')

# df_acs15 is poverty rates for Census Tracts in ARC
sprfldj %>% View


#acs1015 %<>%
#  mutate(chg10_15 = (hopct15 - hopct10)) %>%
#  mutate(tdiff10_15 = B25003_002_15 - B25003_002_10)

# map the data
sprfldj %>% 
  ggplot()+
  geom_sf(aes(fill = hopct15))+
  scale_fill_viridis("urban classification")+
  coord_sf(datum = NA)+
  theme_void(base_family =  "mono")+
  theme(legend.position = c(.15, .15))+
  labs(title = "Atlanta 2010 urbanized \nboundaryby block group\n",
       subtitle = "ACS 2015, via tidycensus",
       caption = "taylor.hafley@uga.edu", 
       x = NULL, y = NULL)

summary(df_acs15$pov_pct)

sprfldj %>% 
  ggplot()+
  geom_sf(aes(fill = urb40))+
  scale_fill_viridis_d("urban classification")+
  coord_sf(datum = NA)+
  theme_void(base_family =  "mono")+
  theme(legend.position = c(.15, .15))+
  labs(title = "Atlanta 1940 urbanized \nboundaryby block group\n",
       subtitle = "ACS 2015, via tidycensus",
       caption = "taylor.hafley@uga.edu", 
       x = NULL, y = NULL)

# tmap

tm_shape(df_acs15) +
  tm_polygons("pov_pct")

tm_shape(df_acs15) +
  tm_polygons("b17001_002e")

tmap_mode("view")

tm_shape(df_acs15) +
  tm_polygons(c("pov_pct", "b17001_002e"), 
              style=c("pretty", "pretty"),
              palette=list("RdYlGn", "Purples"),
              auto.palette.mapping=FALSE,
              title=c("Percent", "Total")) +
  tm_style_grey()

#tmap_mode("plot")

tm_shape(df_acs15) +
  tm_bubbles(size=c("pov_pct", "b17001_002e"), title.size="Homeownership") +
  tm_facets(free.scales=FALSE) +
  tm_layout(panel.labels=c("1940", "2010"))

tm_shape(df_acs15) +
  tm_bubbles(size=c("pov_pct"), title.size="Percent Poverty") +
  tm_facets(free.scales=FALSE) +
  tm_layout(panel.labels=c("1940"))





# tmap

tm_shape(sprfldj) +
  tm_polygons("hopct15")

tm_shape(sprfldj) +
  tm_polygons("adj10a")

tmap_mode("view")

tm_shape(df_acs15) +
  tm_polygons(c("pov_pct", "b17001_002e"), 
              style=c("pretty", "pretty"),
              palette=list("RdYlGn", "Purples"),
              auto.palette.mapping=FALSE,
              title=c("Percent", "Total")) +
  tm_style_grey()

#tmap_mode("plot")

tm_shape(df_acs15) +
  tm_bubbles(size=c("pov_pct", "b17001_002e"), title.size="Homeownership") +
  tm_facets(free.scales=FALSE) +
  tm_layout(panel.labels=c("1940", "2010"))

tm_shape(df_acs15) +
  tm_bubbles(size=c("pov_pct"), title.size="Percent Poverty") +
  tm_facets(free.scales=FALSE) +
  tm_layout(panel.labels=c("1940"))






# these don't work for now (1/19/18)
# stupid example, but this is how you put dot density map

acsjoin %>% View()

tm_shape(acsjoin) +
  tm_borders() +
  tm_bubbles("ham10", "blue", border.col = "black", border.lwd=1, 
             size.lim = c(0, 44000), sizes.legend = c(100, 500, 1000, 4000), 
             title.size="Metropolitan Population") +
  tm_layout()

summary(acsjoin$ham10)

tm_shape(acsjoin) +
  tm_polygons("hopct10", title = "H.O difference") +
  tm_facets("COUNTYJ", free.coords = FALSE) +
  tm_style_grey()

tm_shape(acsjoin) +
  tm_polygons("hopct10", title = "H.O difference") +
  tm_facets("COUNTYJ") +
  tm_style_grey()



# compare number of urban block groups in 1940 w/ 2010
#ggplot(test1c, mapping = aes(x = urb40)) +
#  geom_histogram(stat = 'count')

#ggplot(test1c, mapping = aes(x = urb10)) +
#  geom_histogram(stat = 'count')

#US_bg2 %>%
#  group_by(COUNTYJ) %>%
#  ggplot(mapping = aes(x = urb40)) +
#  geom_histogram(stat = 'count')




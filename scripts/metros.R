
# function to retrieve block groups in metro on demand
#https://walkerke.github.io/2017/05/tigris-metros/

metro_block_groups <- function(metro_name) {
  
  # First, identify which states intersect the metro area using the
  # `states` function in tigris
  st <- states(cb = TRUE)
  cb <- core_based_statistical_areas(cb = TRUE)
  metro <- filter(cb, grepl(metro_name, NAME))
  
  stcodes <- st[metro,]$STATEFP
  
  # Then, fetch the tracts, using rbind_tigris if there is more
  # than one state
  if (length(stcodes) > 1) {
    tr <- rbind_tigris(
      map(stcodes, function(x) {
        block_groups(x, cb = TRUE)
      })
    )
  } else {
    tr <- block_groups(x, cb = TRUE)
  }
  
  # Now, find out which tracts are within the metro area
  within <- st_within(tr, metro)
  
  within_lgl <- map_lgl(within, function(x) {
    if (length(x) == 1) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  
  # Finally, subset and return the output
  output <- tr[within_lgl,]
  
  return(output)
  
}
# select metro of interest, (e.g. below)
chi <- metro_block_groups("Chicago")
atl <- metro_block_groups("Atlanta")
norl <- metro_block_groups("New Orleans")
bos <- metro_block_groups("Boston")
nyc <- metro_block_groups("New York")

# test function
ggplot(chi) + geom_sf()
ggplot(atl) + geom_sf()

# join block group to US_bg2 (created in bg_hammer.R)
chi2 <- left_join(x = chi, y = US_bg2, by = c('GEOID' = 'geoid'))
atl2 <- left_join(x = atl, y = US_bg2, by = c('GEOID' = 'geoid'))
nyc2 <- left_join(x = nyc, y = US_bg2, by = c('GEOID' = 'geoid'))
norl2 <- left_join(x = norl, y = US_bg2, by = c('GEOID' = 'geoid'))

# project (a couple options listed; the projections are general for US, 
# just need to change the input/output object)
atl2 <- atl2 %>%
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
               +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
               +units=m +no_defs")

chi2 <- chi2 %>%
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
               +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
               +units=m +no_defs")

nyc2 <- nyc2 %>%
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
               +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
               +units=m +no_defs")

norl2 <- norl2 %>%
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
               +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
               +units=m +no_defs")


# simple map to test Northeast region (NYC)
tm_shape(nyc2) +
  tm_fill("hu10_sqmi", breaks = c(0, 200, 200000),
          palette = leg_col, auto.palette.mapping = FALSE,
          title = 'NYC 2010')

# and example to show missing data in water block groups
tm_shape(norl2) +
  tm_fill("hu10_sqmi", breaks = c(0, 200, 200000),
          palette = leg_col, auto.palette.mapping = FALSE,
          title = 'NOLA 2010')




# the following scripts create maps with more detail (i.e. legend, roads)

leg_col <- c("#E1EFFA","#065AA0")
lbl <- c("< 200 units/sq mi", "> 200 units/sq mi")

rd <- primary_roads()

atl40 <- tm_shape(atl2) + 
  tm_fill('hu40_sqmi', breaks = c(0, 200, 20000), 
          palette = leg_col, auto.palette.mapping = FALSE,
          legend.show = FALSE,
          title = '1940') +
  tm_add_legend(type = c("fill"), labels = lbl, col = leg_col, 
                title = "1940") +
  tm_shape(rd) +
  tm_lines(col = 'black', alpha = 0.6) +
  tm_compass(type = "arrow", size = 4, position = c(0.82, 0.08)) +
  tm_legend(position = c(0.025, 0.05),
            bg.color = "white",
            frame = TRUE,
            legend.text.size = .7,
            legend.title.size = 1.3) + 
  tm_layout(frame = FALSE, 
            outer.margins=c(0,0,0,0), 
            inner.margins=c(0,0,0,0), asp=0)

atl40


chi40 <- tm_shape(chi2) + 
  tm_fill('hu40_sqmi', breaks = c(0, 200, 20000), 
          palette = leg_col, auto.palette.mapping = FALSE,
          legend.show = FALSE,
          title = '1940') +
  tm_add_legend(type = c("fill"), labels = lbl, col = leg_col, 
                title = "1940") +
  tm_shape(rd) +
  tm_lines(col = 'black', alpha = 0.6) +
  tm_compass(type = "arrow", size = 4, position = c(0.82, 0.08)) +
  tm_legend(position = c(0.025, 0.05),
            bg.color = "white",
            frame = TRUE,
            legend.text.size = .7,
            legend.title.size = 1.3) + 
  tm_layout(frame = FALSE, 
            outer.margins=c(0,0,0,0), 
            inner.margins=c(0,0,0,0), asp=0)

chi40

chi10 <- tm_shape(chi2) + 
  tm_fill('hu10_sqmi', breaks = c(0, 200, 20000), 
          palette = leg_col, auto.palette.mapping = FALSE,
          legend.show = FALSE) +
  tm_add_legend(type = c("fill"), labels = lbl, col = leg_col, 
                title = "2010") +
  tm_shape(rd) +
  tm_lines(col = 'black', alpha = 0.6) +
  tm_compass(type = "arrow", size = 4, position = c(0.82, 0.08)) +
  tm_legend(position = c(0.025, 0.05),
            bg.color = "white",
            frame = TRUE,
            legend.text.size = .7,
            legend.title.size = 1.3) + 
  tm_layout(frame = FALSE, 
            outer.margins=c(0,0,0,0), 
            inner.margins=c(0,0,0,0), asp=0)

chi10


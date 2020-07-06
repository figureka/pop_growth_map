#### Packages ####
library(tidyverse)
library(sf)
library(tmap)
library(tigris)
library(urbnmapr)
library(RColorBrewer)

#### Read in population data ####
# Population in 2010 and 2019 for all US counties
county_pops <- read_csv('co-est2019-alldata.csv') %>%
  select(STATE, COUNTY, STNAME, CTYNAME, POPESTIMATE2010:POPESTIMATE2019,
         BIRTHS2010:BIRTHS2019,
         DEATHS2010:DEATHS2019) %>%
  filter(STNAME != CTYNAME)

# Top 25 most populated cities
city_pops <- read_csv('uscities.csv') %>%
  arrange(desc(population)) %>% head(50)

#### Get Shapefiles for states, counties, cities ####
# Counties from urbnmapr
counties.shp <- get_urbn_map(map = 'counties', sf = TRUE)
# States from tigris
states.shp <- get_urbn_map(map = 'states', sf = TRUE)

#### Calculate population growth from 2010-2019 by county ####
county_pops <- county_pops %>%
  mutate(growth_prc = 100*(POPESTIMATE2019-POPESTIMATE2010)/POPESTIMATE2010)

#### Join county shapes and populations ####
names(county_pops)[3] <- 'state_name'
names(county_pops)[4] <- 'county_name'

growth_map <- left_join(counties.shp, county_pops, by = c('state_name', 'county_name'))
growth_map <- growth_map %>%
  mutate(region = factor(ifelse(state_abbv %in% c('CT','ME','MA','NH','RI','VT',
                                           'NJ','NY','PA'),'Northeast',
                         ifelse(state_abbv %in% c('IL','IN','MI','OH','WI',
                                                  'IA','KS','MN','MO','NE',
                                                  'ND','SD'),'Midwest',
                                ifelse(state_abbv %in% c('DE','FL','GA','MD',
                                                         'NC','SC','VA','DC',
                                                         'WV','AL','KY','MS',
                                                         'TN','AR','OK','LA',
                                                         'TX'),'South',
                                       'West')))))

#### Static Cholorpleth ####
growth_map_1<-
tm_shape(growth_map) +
  tm_fill('growth_prc',
          title = 'Population change',
          style = 'cont',
          breaks = c(-30, -20, -10, -5, 5, 10, 20, 30),
          palette = 'RdBu',
          legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), "%"))) +
  tm_borders(col ='grey90') +
tm_shape(states.shp) +
  tm_borders(col = 'grey40') +
tm_layout(inner.margins = c(.05,.125,.1,.05), # bottom left top right
          title = 'Percent population growth from 2010 to 2019, by county',
          title.size = 1.1,
          legend.position = c("left","bottom"),
          legend.title.size = 1,
          legend.text.size = .6,
          frame = FALSE)
tmap_save(growth_map_1, 'figure1.png', units = 'in', dpi = 300, width = 8, height = 4)

growth_map_2<-
  tm_shape(growth_map) +
  tm_fill('growth_prc',
          title = 'Population change',
          midpoint = 5.767813,
          style = 'cont',
          breaks = c(-30, -20, -10, -5, 5, 10, 20, 30),
          palette = 'RdBu',
          legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), "%"))) +
  tm_borders(col ='grey90') +
  tm_shape(states.shp) +
  tm_borders(col = 'grey40') +
  tm_layout(inner.margins = c(.05,.125,.1,.05), # bottom left top right
            title = 'Percent population growth from 2010 to 2019, by county, minus the US average',
            title.size = 1.1,
            legend.position = c("left","bottom"),
            legend.title.size = 1,
            legend.text.size = .6,
            frame = FALSE)

tmap_save(growth_map_2, 'figure2.png', units = 'in', dpi = 300, width = 8, height = 4)


#### Map of regions ####
region_map<-
  tm_shape(growth_map) +
  tm_fill('region', palette = 'Set2', title = 'Region') +
  tm_borders(col = NULL)+
tm_shape(states.shp) +
  tm_borders()+
tm_layout(frame = FALSE,
          legend.position = c('left', 'center'))

tmap_save(region_map, 'figure3.png', units = 'in', dpi = 300, width = 8, height = 6)


#### Regional share of population ####
f4<-
growth_map %>% st_drop_geometry() %>%
  group_by(region) %>%
  summarize(pop10 = sum(POPESTIMATE2010, na.rm = TRUE),
            pop11 = sum(POPESTIMATE2011, na.rm = TRUE),
            pop12 = sum(POPESTIMATE2012, na.rm = TRUE),
            pop13 = sum(POPESTIMATE2013, na.rm = TRUE),
            pop14 = sum(POPESTIMATE2014, na.rm = TRUE),
            pop15 = sum(POPESTIMATE2015, na.rm = TRUE),
            pop16 = sum(POPESTIMATE2016, na.rm = TRUE),
            pop17 = sum(POPESTIMATE2017, na.rm = TRUE),
            pop18 = sum(POPESTIMATE2018, na.rm = TRUE),
            pop19 = sum(POPESTIMATE2019, na.rm = TRUE)) %>%
  mutate(frac_10 = pop10/sum(pop10),
         frac_11 = pop11/sum(pop11),
         frac_12 = pop12/sum(pop12),
         frac_13 = pop13/sum(pop13),
         frac_14 = pop14/sum(pop14),
         frac_15 = pop15/sum(pop15),
         frac_16 = pop16/sum(pop16),
         frac_17 = pop17/sum(pop17),
         frac_18 = pop18/sum(pop18),
         frac_19 = pop19/sum(pop19)) %>%
  select(region, frac_10:frac_19) %>%
  gather(category, frac, frac_10:frac_19) %>%
  mutate(year = rep(2010:2019, each = 4)) %>%
  ggplot(aes(x = (year), y = frac, fill = region)) +
  geom_area(alpha = .7, color = 'black')+
  labs(title = 'Fraction of US population by region',
       x = element_blank(),
       y = 'Fraction of US population',
       fill = 'Region')+
  scale_fill_brewer(palette = 'Set2') +
  scale_x_continuous(breaks = c(seq(2010,2019,1)))

png('figure4.png', units = 'in', width = 8, height = 6, res = 300)
f4
dev.off()

#### Growth table ####
growth_map %>% arrange(growth_prc) %>% head(25) %>%
  select(state_name, county_name, growth_prc) %>% st_drop_geometry()

growth_map %>% arrange(desc(growth_prc)) %>% head(25) %>%
  select(state_name, county_name, growth_prc) %>% st_drop_geometry()

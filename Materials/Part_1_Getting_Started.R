###################################################################
## Code for Applied Machine Learning by Max Kuhn @ DC
## https://github.com/topepo/DC-ML-2018

library(tidymodels)

thm <- theme_bw() + 
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), 
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

library(AmesHousing)
library(leaflet)
ames <- make_ames()

col_key <- c(
  'NAmes',     '#0000FF',
  'CollgCr',   '#FF0000',
  'OldTown',   '#FFFFFF',
  'Edwards',   '#FF00B6',
  'Somerst',   '#FF3030',
  'NridgHt',   '#009FFF',
  'Gilbert',   '#DD00FF',
  'Sawyer',    '#9A4D42',
  'NWAmes',    '#00FFBE',
  'SawyerW',   '#1F9698',
  'Mitchel',   '#FFACFD',
  'BrkSide',   '#720055',
  'Crawfor',   '#F1085C',
  'IDOTRR',    '#FE8F42',
  'Timber',    '#004CFF',
  'NoRidge',   '#ffff00',
  'StoneBr',   '#B1CC71',
  'SWISU',     '#02AD24',
  'ClearCr',   '#FFD300',
  'MeadowV',   '#886C00',
  'BrDale',    '#FFB79F',
  'Blmngtn',   '#858567',
  'Veenker',   '#A10300',
  'NPkVill',   '#00479E',
  'Blueste',   '#DC5E93',
  'Greens',    '#93D4FF',
  'GreenHills', '#e5f2e5', 
  'Landmrk',   '#C8FF00'
) 
col_key <- as.data.frame(matrix(col_key, byrow = TRUE, ncol = 2))
names(col_key) <- c("Neighborhood", "color")
col_key <- col_key %>%
    mutate(
      Neighborhood =
        dplyr::recode(
          Neighborhood,
          "Blmngtn" = "Bloomington_Heights",
          "Bluestem" = "Bluestem",
          "BrDale" = "Briardale",
          "BrkSide" = "Brookside",
          "ClearCr" = "Clear_Creek",
          "CollgCr" = "College_Creek",
          "Crawfor" = "Crawford",
          "Edwards" = "Edwards",
          "Gilbert" = "Gilbert",
          "Greens" = "Greens",
          "GreenHills" = "Green_Hills",
          "IDOTRR" = "Iowa_DOT_and_Rail_Road",
          "Landmrk" = "Landmark",
          "MeadowV" = "Meadow_Village",
          "Mitchel" = "Mitchell",
          "NAmes" = "North_Ames",
          "NoRidge" = "Northridge",
          "NPkVill" = "Northpark_Villa",
          "NridgHt" = "Northridge_Heights",
          "NWAmes" = "Northwest_Ames",
          "OldTown" = "Old_Town",
          "SWISU" = "South_and_West_of_Iowa_State_University",
          "Sawyer" = "Sawyer",
          "SawyerW" = "Sawyer_West",
          "Somerst" = "Somerset",
          "StoneBr" = "Stone_Brook",
          "Timber" = "Timberland",
          "Veenker" = "Veenker"
        ))

lon_rnd <- range(ames$Longitude)
lat_rnd <- range(ames$Latitude)

ia_map <- leaflet(width = "80%") %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels)

for(i in 1:nrow(col_key)) {
  ia_map <- ia_map %>%
    addCircles(
      data = subset(ames, Neighborhood == col_key$Neighborhood[i]),
      lng = ~Longitude, lat = ~Latitude,
      color = col_key$color[i],
      fill = TRUE,
      fillColor = col_key$color[i],
      radius = 6,
      popup = htmlEscape(col_key$Neighborhood[i]),
      opacity = .25)
}
ia_map

###################################################################
## slide 13

## contains("Sepal")
## 
## # instead of
## 
## c("Sepal.Width", "Sepal.Length")

## merged <- inner_join(a, b)
## 
## # is equal to
## 
## merged <- a %>%
##   inner_join(b)

###################################################################
## slide 14

library(tidyverse)

ames <- 
  read_delim("http://bit.ly/2whgsQM", delim = "\t") %>%
  rename_at(vars(contains(' ')), funs(gsub(' ', '_', .))) %>%
  rename(Sale_Price = SalePrice) %>%
  filter(!is.na(Electrical)) %>%
  select(-Order, -PID, -Garage_Yr_Blt)

ames %>%
  group_by(Alley) %>%
  summarize(mean_price = mean(Sale_Price/1000),
            n = sum(!is.na(Sale_Price)))

###################################################################
## slide 15

library(ggplot2)

ggplot(ames, 
       aes(x = Garage_Type,
           y = Sale_Price)) + 
  geom_violin() + 
  coord_trans(y = "log10") + 
  xlab("Garage Type") + 
  ylab("Sale Price") 

###################################################################
## slide 16

library(purrr)

# summarize via purrr::map
by_alley <- split(ames, ames$Alley)
is_list(by_alley)

map(by_alley, nrow)

# or better yet:
map_int(by_alley, nrow)


# works on non-list vectors too
ames %>%
  mutate(Sale_Price = Sale_Price %>%
           map_dbl(function(x) x / 1000)) %>%
  select(Sale_Price, Yr_Sold) %>%
  head(4)


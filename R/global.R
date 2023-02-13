library(mapdeck)
library(tidyverse)

load("data/data_clean.Rdata")

map_data <- map_data %>% 
  map(
    rename,
    "Gini" = "gini",
    "Top 1% share" = "top1", # TODO reveals outlier in NI LAD
    #"Skew" = "skew", # TODO reveals a lot of outliers? 
    "90:10 ratio" = "nine_ten",
    "90:50 ratio" = "nine_five",
    "50:10 ratio" = "five_ten",
    "Average house price" = "avg",
    "Coefficient of variation" = "cv"
  ) %>% 
  map(
    mutate,
    med = round(med)
  )

# remember to delete actual key before pushing to github 
# set API token for mapbox 
set_token(Sys.getenv("mapbox_public_key"))

# spatial level dropdown choices
levs <- c(
  "LSOA" = "lsoa",
  "MSOA" = "msoa",
  "Built-up area" = "bua",
  "Local Authority District" = "lad",
  "Parliamentary Constituency" = "pc",
  "Travel to work area" = "ttwa"
)

# inequality measure choices
# TODO add CV, skew, 90:10, etc
ineq_measures <- c(
  "Gini",
  "Top 1% share", # TODO reveals outlier in NI LAD
  #"Skew" = "skew", # TODO reveals a lot of outliers? 
  "90:10 ratio",
  "90:50 ratio",
  "50:10 ratio",
  "Average house price",
  "Coefficient of variation"
)

# TODO in options add data source: "housing", "income", "wealth"
# TODO switch to changes in inequality
# TODO add 'draw your own area' actionButton which defines bbox and then retrieves and calculates ineq for the area
# TODO add segregation measures?


vars_to_display <- c("name","med","houses")


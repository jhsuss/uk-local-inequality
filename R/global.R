load("data/data_clean.Rdata")

library(mapdeck)
# TODO how would this work on github or shiny server?
# set API token for mapbox 
set_token(Sys.getenv("mapbox_public_key"))

# spatial level dropdown choices
# TODO add LAD, TTWA
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
  "Gini" = "gini",
  "Top 1%" = "top1", # TODO reveals outlier in NI LAD
  #"Skew" = "skew", # TODO reveals a lot of outliers? 
  "90:10 ratio" = "nine_ten",
  "90:50 ratio" = "nine_five",
  "50:10 ratio" = "five_ten",
  "Coefficient of variation" = "cv"
)

# TODO in options add data source: "housing", "income", "wealth"
# TODO switch to changes in inequality
# TODO add 'draw your own area' actionButton which defines bbox and then retrieves and calculates ineq for the area
# TODO add segregation measures?


vars_to_display <- c("name","med","houses")


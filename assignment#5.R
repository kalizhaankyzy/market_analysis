library(tidyverse)

# data importing
nyc_tr_data <- read.csv("../assignment#2/NYC_RE/NYC_TRANSACTION_DATA.csv")
borough <- read.csv("../assignment#2/NYC_RE/BOROUGH.csv")
building_class <- read.csv("../assignment#2/NYC_RE/BUILDING_CLASS.csv")
neighborhood <- read.csv("../assignment#2/NYC_RE/NEIGHBORHOOD.csv")


data <- merge(nyc_tr_data, neighborhood, by = "NEIGHBORHOOD_ID") %>%
  merge(building_class, by.x = "BUILDING_CLASS_FINAL_ROLL", by.y = "BUILDING_CODE_ID")

##exclude properties where gross square feet or sale price is unknown
canarsie_area <- data %>% 
  filter(NEIGHBORHOOD_NAME == 'CANARSIE') %>%
  filter(!is.na(GROSS_SQUARE_FEET) & !is.na(SALE_PRICE) & GROSS_SQUARE_FEET != 0 & SALE_PRICE != 0) %>%
  filter(TYPE == 'COMMERCIAL')

last_year <- max(year(canarsie_area$SALE_DATE))-1

avg_price_per_sqFt <- canarsie_area %>%
  filter (SALE_DATE > last_year) %>%
  summarize(avg_price_per_sqFt = mean(SALE_PRICE / GROSS_SQUARE_FEET, na.rm = TRUE))

avg_price_per_sqFt




library(tidyverse)
library(ggplot2)
library(dplyr)

# data importing
nyc_tr_data <- read.csv("./NYC_RE/NYC_TRANSACTION_DATA.csv")
borough <- read.csv("./NYC_RE/BOROUGH.csv")
building_class <- read.csv("./NYC_RE/BUILDING_CLASS.csv")
neighborhood <- read.csv("./NYC_RE/NEIGHBORHOOD.csv")

# join tables NYC_TRANSACTION_DATA and NEIGHBORHOOD
data <- merge(x = nyc_tr_data, y = neighborhood, by = "NEIGHBORHOOD_ID")

# creating new column SALE_YEAR by extracting year from the SALE_DATE
data <- data %>%
  mutate(SALE_YEAR = year(as.Date(SALE_DATE, format = "%Y-%m-%d")))

# removing fields with 0 for square feet
data <- data %>%
  filter(GROSS_SQUARE_FEET > 0)

# creating new variable that aggregates data by sale year and neighborhood, 
# showing total price and total gross square feet for each
aggregated_data <- data %>%
  group_by(SALE_YEAR, NEIGHBORHOOD_NAME) %>%
  summarize(
    Total_Sale_Price = sum(SALE_PRICE, na.rm = TRUE),
    Total_Gross_SqFt = sum(GROSS_SQUARE_FEET, na.rm = TRUE)
  ) %>%
  ungroup()

# creating new column to show price per square foot for each neighborhood by year
aggregated_data <- aggregated_data %>%
  mutate(Price_per_SqFt = Total_Sale_Price / Total_Gross_SqFt)

tail(aggregated_data)

# visualizing changes in price per 1 sq.ft. in Canarsie by year
ggplot(
  aggregated_data[aggregated_data$NEIGHBORHOOD_NAME == 'CANARSIE', ], 
  aes(x = SALE_YEAR, y = Price_per_SqFt)) +
  geom_line(color = "#CC6666") +
  geom_point(color = "#CC6666") +
  scale_x_continuous(breaks = seq(min(aggregated_data$SALE_YEAR), max(aggregated_data$SALE_YEAR), by = 2)) +
  scale_y_continuous(breaks = seq(0, max(aggregated_data$Price_per_SqFt), by = 40)) +
  xlab('Year')+
  ylab('The average price per square foot') +
  ggtitle('The average price per square foot in Canarsie',
          subtitle = '2003 - 2023') +
  theme_bw() 


# getting borough_id of Canarsie area
canarsie_br_id <- unique(data[data$NEIGHBORHOOD_NAME == 'CANARSIE', ]$BOROUGH_ID)

canarsie_br_id
# selecting 4 random neighborhoods in same borough
random_data <- neighborhood %>%
  filter(BOROUGH_ID == canarsie_br_id & NEIGHBORHOOD_NAME != 'CANARSIE') %>%
  sample_n(4)

random_data$NEIGHBORHOOD_NAME

# Comparing changes in price per 1 sq.ft. within neighborhoods in same borough
ggplot(
  aggregated_data[aggregated_data$NEIGHBORHOOD_NAME %in% c('CANARSIE', random_data$NEIGHBORHOOD_NAME), ], 
  aes(x = SALE_YEAR, y = Price_per_SqFt, color = NEIGHBORHOOD_NAME, size = ifelse(NEIGHBORHOOD_NAME == 'CANARSIE', 'thick', 'thin'))) +
  geom_line(show.legend = FALSE) +
  geom_point(size = 1.5) +
  scale_size_manual(values = c("thick" = 1.3, "thin" = 0.5)) +
  scale_x_continuous(breaks = seq(min(aggregated_data$SALE_YEAR), max(aggregated_data$SALE_YEAR), by = 2)) +
  scale_y_continuous(breaks = seq(0, max(aggregated_data$Price_per_SqFt), by = 40)) +
  scale_color_brewer(palette="Set1") +
  labs(
    title = 'The average price per square foot in Brooklyn neighborhoods',
    subtitle = '2003 - 2023',
    x = "Year",
    y = "Average price per square foot",
    color = "Neighborhood"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

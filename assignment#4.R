library(tidyverse)
library(forecast)
library(dplyr)
library(ggplot2)

# data importing
nyc_tr_data <- read.csv("../assignment#2/NYC_RE/NYC_TRANSACTION_DATA.csv")
borough <- read.csv("../assignment#2/NYC_RE/BOROUGH.csv")
building_class <- read.csv("../assignment#2/NYC_RE/BUILDING_CLASS.csv")
neighborhood <- read.csv("../assignment#2/NYC_RE/NEIGHBORHOOD.csv")

# data preparation
data <- merge(nyc_tr_data, neighborhood, by = "NEIGHBORHOOD_ID") %>%
  merge(building_class, by.x = "BUILDING_CLASS_FINAL_ROLL", by.y = "BUILDING_CODE_ID")

canarsie_area <-  data %>% 
  filter(NEIGHBORHOOD_NAME == 'CANARSIE' &
           year(as.Date(SALE_DATE, format = "%Y-%m-%d")) >= 2009) %>%
  mutate(SALE_YEAR = year(SALE_DATE))


# TASK 1
canarsie_area_qrtr <- canarsie_area %>%
  mutate(QUARTER = quarter(as.Date(SALE_DATE, format = "%Y-%m-%d"))) %>%
  group_by(SALE_YEAR, QUARTER) %>%
  summarize(TOTAL_SALES = sum(SALE_PRICE))

canarsie_area_qrtr
# Plot data
ggplot(canarsie_area_qrtr, aes(x = SALE_YEAR + (QUARTER-1)/4, y = TOTAL_SALES)) +
  geom_line(color = "steelblue") + 
  scale_x_continuous(breaks = seq(min(canarsie_area_qrtr$SALE_YEAR), max(canarsie_area_qrtr$SALE_YEAR), by = 2)) +
  labs (title = "Yearly and Quarterly total sales of Canarsie area",
        x = "Year",
        y = "Total Sales") +
  theme_minimal()

ggplot(canarsie_area_qrtr, aes(x = QUARTER, y = TOTAL_SALES)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs (title = "Quarterly total sales of Canarsie area",
        x = "Year",
        y = "Total Sales") +
  theme_minimal()

# Convert to a time series object
total_sales_ts <- ts(canarsie_area_qrtr$TOTAL_SALES, start = c(2009, 1), frequency = 4)
total_sales_ts

?ets

model_mam <- ets(
  y=total_sales_ts,
  model = "MAM")

model_maa <- ets(
  y=total_sales_ts,
  model = "MAA")

model_mnm <- ets(
  y=total_sales_ts,
  model = "MNM")

model_ana <- ets(
  y=total_sales_ts,
  model = "ANA")

summary(model_mam)
summary(model_maa)
summary(model_mnm)
summary(model_ana)

p <- predict(model_mnm, 8)
p
plot(p)



# Task 2
canarsie_area_qrtr$TIME = seq(1, nrow(canarsie_area_qrtr))

mr_model <- lm(TOTAL_SALES ~ TIME + QUARTER, data = canarsie_area_qrtr)
summary(mr_model)


last_year <- max(canarsie_area_qrtr$SALE_YEAR)
last_quarter <- max(canarsie_area_qrtr$QUARTER[canarsie_area_qrtr$SALE_YEAR == last_year])

future_data <- data.frame(
  SALE_YEAR = integer(),
  QUARTER = integer(),
  TIME = integer()
)
for (i in 1:8) {
  future_quarter <- (last_quarter + i - 1) %% 4 + 1
  future_year <- last_year + (last_quarter + i - 1) %/% 4
  future_time <- (future_year - min(canarsie_area_qrtr$SALE_YEAR)) * 4 + future_quarter
  future_data <- rbind(future_data, data.frame(SALE_YEAR = future_year, QUARTER = future_quarter, TIME = future_time))
}

future_data$SALES_FORECAST <- predict(mr_model, newdata = future_data)
future_data

data_combination <- canarsie_area_qrtr %>%
  rbind(data.frame(SALE_YEAR = future_data$SALE_YEAR,
                   QUARTER = future_data$QUARTER,
                   TOTAL_SALES = future_data$SALES_FORECAST,
                   TIME = future_data$TIME))


head(data_combination)

ggplot(data_combination, 
       aes(x = TIME, y = TOTAL_SALES)) +
  geom_line(color = "blue") +
  geom_point(data = future_data, aes(y = SALES_FORECAST), color = "red") +
  labs(title = "Sales Forecast for the Next 8 Quarters",
       x = "Year and Quarter",
       y = "Sales Amount") +
  theme_minimal() +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x))



# TASK 3
canarsie_area_res <- canarsie_area %>%
  filter(TYPE == "RESIDENTIAL")
head(canarsie_area_res)

model_3 <- 
  lm(SALE_PRICE ~ SALE_YEAR + GROSS_SQUARE_FEET + YEAR_BUILT + RESIDENTIAL_UNITS,
     data = canarsie_area_res)

summary(model_3)


library(car)
vif(model_3)


canarsie_area_res$predicted_sale_price <- predict(model_3, newdata = canarsie_area_res)
canarsie_area_res$price_diff <- canarsie_area_res$SALE_PRICE - canarsie_area_res$predicted_sale_price

canarsie_area_res %>%
  select(RESIDENTIAL_UNITS, SALE_YEAR, GROSS_SQUARE_FEET, YEAR_BUILT, price_diff) %>%
  filter(price_diff < 0) %>%
  arrange(price_diff) %>%  # Sort by the most negative first
  head(10)  # Top 10 bargains

canarsie_area_res %>%
  select(price_diff) %>%
  filter(price_diff > 0) %>%
  arrange(price_diff) %>%  # Sort by the most negative first
  head(10)  # Top 10 overpriced





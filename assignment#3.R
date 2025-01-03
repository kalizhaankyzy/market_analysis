library(tidyverse)
library(factoextra)
library(gridExtra)
library(ggplot2)

# data importing
nyc_tr_data <- read.csv("../assignment#2/NYC_RE/NYC_TRANSACTION_DATA.csv")
borough <- read.csv("../assignment#2/NYC_RE/BOROUGH.csv")
building_class <- read.csv("../assignment#2/NYC_RE/BUILDING_CLASS.csv")
neighborhood <- read.csv("../assignment#2/NYC_RE/NEIGHBORHOOD.csv")



data <- merge(nyc_tr_data, neighborhood, by = "NEIGHBORHOOD_ID") %>%
  merge(building_class, by.x = "BUILDING_CLASS_FINAL_ROLL", by.y = "BUILDING_CODE_ID")
head(data)

canarsie_area <-  data %>% 
  filter(data$NEIGHBORHOOD_NAME == 'CANARSIE' &
           year(as.Date(data$SALE_DATE, format = "%Y-%m-%d")) >= 2009)

## PART-1
# The total number of sales in your neighborhood since 2009. 
canarsie_area %>%
  summarise(TOTAL_SALES = n())

# The mean sale price and gross square footage 
# for residential properties in your neighborhood since 2009.
canarsie_res_area <- canarsie_area %>%
  filter(TYPE == "RESIDENTIAL")

canarsie_res_area %>%
  summarize(MEAN_SALE_PRICE = mean(SALE_PRICE, na.rm = TRUE),
            MEAN_SqFt = mean(GROSS_SQUARE_FEET, na.rm = TRUE)) %>%
  ungroup()

# A five-number summary for both sale price and gross square footage 
# for residential properties in your neighborhood since 2009
canarsie_res_area %>%
  summarize(
    min_price = min(SALE_PRICE, na.rm = TRUE),
    Q1_price = quantile(SALE_PRICE, 0.25, na.rm = TRUE),
    median_price = median(SALE_PRICE, na.rm = TRUE),
    Q3_price = quantile(SALE_PRICE, 0.75, na.rm = TRUE),
    max_price = max(SALE_PRICE, na.rm = TRUE),
    min_another_col = min(GROSS_SQUARE_FEET, na.rm = TRUE),
    Q1_another_col = quantile(GROSS_SQUARE_FEET, 0.25, na.rm = TRUE),
    median_another_col = median(GROSS_SQUARE_FEET, na.rm = TRUE),
    Q3_another_col = quantile(GROSS_SQUARE_FEET, 0.75, na.rm = TRUE),
    max_another_col = max(GROSS_SQUARE_FEET, na.rm = TRUE)
  )

# The proportion of sales of residential, commercial, mixed, 
# and other properties in your neighborhood since 2009
canarsie_area %>%
  count(TYPE) %>%
  mutate(PROPORTION = n / sum(n))

# The standard deviation of sale prices 
# for residential properties in your neighborhood since 2009. 
canarsie_res_area %>%
  summarize(STANDARD_DEVIATION = sd(SALE_PRICE, na.rm = TRUE))

# Correlation between sale price and gross square feet 
cor(canarsie_area$SALE_PRICE, canarsie_area$GROSS_SQUARE_FEET, use = "complete.obs")



### PART-2
residential_data <- data %>%
  filter(TYPE == "RESIDENTIAL" & year(as.Date(data$SALE_DATE, format = "%Y-%m-%d")) >= 2009)

kpi_params <- residential_data %>%
  group_by(NEIGHBORHOOD_ID) %>%
  summarize(
    MEDIAN_SALE_PRICE = median(SALE_PRICE, na.rm = TRUE),
    SALES_NUMBER = n(),
    SD_SALE_PRICE = sd(SALE_PRICE, na.rm = TRUE)
  )

clustering_data <- kpi_params %>%
  as.data.frame()

clustering_data <- na.omit(clustering_data)
clustering_data_scaled <- scale(clustering_data[, c("MEDIAN_SALE_PRICE", "SALES_NUMBER", "SD_SALE_PRICE")])

k2 <- kmeans(clustering_data_scaled, centers = 2, nstart = 25)

# cluster plot
fviz_cluster(k2, data = clustering_data)


k3 <- kmeans(clustering_data_scaled, centers = 3, nstart = 25)
k4 <- kmeans(clustering_data_scaled, centers = 4, nstart = 25)
k5 <- kmeans(clustering_data_scaled, centers = 5, nstart = 25)

# comparing clusters
p1 <- fviz_cluster(k2, geom = "point", data = clustering_data_scaled) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = clustering_data_scaled) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = clustering_data_scaled) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = clustering_data_scaled) + ggtitle("k = 5")

grid.arrange(p1, p2, p3, p4, nrow = 2)



# cluster plot another option using ggplot()
p1_gg <- clustering_data %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster) %>%
  ggplot(aes(MEDIAN_SALE_PRICE, SALES_NUMBER, color = factor(cluster), label = NEIGHBORHOOD_ID)) +
  geom_text() +
  ggtitle("k = 2")
p2_gg <- clustering_data %>%
  as_tibble() %>%
  mutate(cluster = k3$cluster) %>%
  ggplot(aes(MEDIAN_SALE_PRICE, SALES_NUMBER, color = factor(cluster), label = NEIGHBORHOOD_ID)) +
  geom_text() +
  ggtitle("k = 3")
p3_gg <- clustering_data %>%
  as_tibble() %>%
  mutate(cluster = k4$cluster) %>%
  ggplot(aes(MEDIAN_SALE_PRICE, SALES_NUMBER, color = factor(cluster), label = NEIGHBORHOOD_ID)) +
  geom_text() +
  ggtitle("k = 4")
p4_gg <- clustering_data %>%
  as_tibble() %>%
  mutate(cluster = k5$cluster) %>%
  ggplot(aes(MEDIAN_SALE_PRICE, SALES_NUMBER, color = factor(cluster), label = NEIGHBORHOOD_ID)) +
  geom_text() +
  ggtitle("k = 5")

grid.arrange(p1_gg, p2_gg, p3_gg, p4_gg, nrow = 2)




### PART-3
woodside_pr <- data %>% 
  filter(NEIGHBORHOOD_NAME == 'WOODSIDE' &
           year(as.Date(SALE_DATE, format = "%Y-%m-%d")) >= 2009) %>%
  pull(SALE_PRICE)

canarsie_pr <- data %>%
  filter(NEIGHBORHOOD_NAME == 'CANARSIE' &
           year(as.Date(SALE_DATE, format = "%Y-%m-%d")) >= 2009) %>%
  pull(SALE_PRICE)

t_test <- t.test(woodside_pr, canarsie_pr, 
                        alternative = "greater", 
                        var.equal = FALSE)

print(t_test)










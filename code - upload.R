### GREEK BEVERAGE SALES


#### 0 LIBRARIES

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(DiagrammeR)
library(feasts)
library(tsibble)
require(fable)

#### 1 DATA - IMPORT AND CLEAN
# https://www.kaggle.com/veeralakrishna/predict-demand

train <- read.csv('train.csv')
test <- read.csv('test.csv')

# last 1080 rows are actually all NAs because of some error when splitting test set
apply(apply(train, 2, is.na), 2, sum) # No NAs
train <- train[1:(nrow(train)-1080), ]

# still some NAs in lat/long, but we can drop since those are collinear with shop

# also, some empty container/capacity labels
apply(apply(train, 2, function (x){x == ""}), 2, sum) 

# Appears to be a 1-to-1 relationship between capacity and container (collinear)
# Exploit this to fill blanks and drop a column
for (cont in unique(train$container)){
  if (cont != ""){
    print(c(cont, unique(train[train$container == cont,]["capacity"])))
  }
}
for (cap in unique(train$capacity)){
  if (cap != ""){
    print(c(cap, unique(train[train$capacity == cap,]["container"])))
  }
}

fill_container_na <- function(container, capacity){
  if (container != ""){
    container
  }
  else {
    if (capacity == "330ml"){
      "can"
    }
    else if (capacity == "1.5lt"){
      "plastic"
    }
    else {
      "glass"
    }
  }
}

train <- train %>% rowwise() %>% mutate(container = fill_container_na(container, capacity))
train <- train %>% select(-all_of(c("id", "lat", "long", "capacity")))
train$date <- yearmonth(as.Date(train$date, format = "%d/%m/%y"))
train <- as_tsibble(train, key = c(shop, brand, container))

test <- test %>% rowwise() %>% mutate(container = fill_container_na(container, capacity))
test <- test %>% select(-all_of(c("id", "lat", "long", "capacity")))
test$date <- yearmonth(as.Date(test$date, format = "%d/%m/%y"))
test <- as_tsibble(test, key = c(shop, brand, container))


#### 2 DATA - EXPLORATION

## Draw tree
# full tree
node_labels <- c("Total Sales")
counter <- 1
from <- c()
to <- c()

for (s in unique(train$shop)){
  node_labels <- c(node_labels, s)
  counter <- counter + 1
  shop_node_id <- counter
  from <- c(from, 1)
  to <- c(to, shop_node_id)
  
  for (b in pull(unique(train[train$shop == s, ][, "brand"]))){
    node_labels <- c(node_labels, b)
    counter <- counter + 1
    brand_node_id <- counter
    from <- c(from, shop_node_id)
    to <- c(to, brand_node_id)
    
    for (cont in pull(unique(train[train$shop == s & train$brand == b, ][, "container"]))){
      node_labels <- c(node_labels, cont)
      counter <- counter + 1
      cont_node_id <- counter
      from <- c(from, brand_node_id)
      to <- c(to, cont_node_id)
    }
  }
}

tree_nodes <- create_node_df(n = length(node_labels),
                             label = node_labels)

tree_edges <- create_edge_df(from = from,
                             to = to)

tree <- create_graph() %>% add_node_df(tree_nodes) %>% add_edge_df(tree_edges)
render_graph(tree, layout = "tree")

# partial tree
node_labels <- c("Total Sales")
counter <- 1
from <- c()
to <- c()

for (s in unique(train$shop)){
  node_labels <- c(node_labels, s)
  counter <- counter + 1
  shop_node_id <- counter
  from <- c(from, 1)
  to <- c(to, shop_node_id)
}

tree_nodes <- create_node_df(n = length(node_labels),
                             label = node_labels)

tree_edges <- create_edge_df(from = from,
                             to = to)

partial_tree <- create_graph() %>% add_node_df(tree_nodes) %>% add_edge_df(tree_edges)
render_graph(partial_tree, layout = "tree")


## the task is to use the monthly from 2012 to 2017 to predict monthly sales for 2018
range(train$date)
range(test$date)

# we could break sales into 6480/(12*6) = 90 unique time series, 
# where each series is sales for a certain product at a certain shop
nrow(unique(train[,c("shop", "brand", "container")]))

# What would be a logical structure for our time series?
# shop > brand > container seems logical.
# make into an hts style tsibble, keeping xregs price and pop.
train_hts <- train %>% aggregate_key((shop/brand/container), 
                                     price = mean(price), pop = mean(pop), quantity = sum(quantity))

# Plot aggregated time series.
train_hts %>% filter(is_aggregated(shop)) %>%
  autoplot(quantity) + ylab("quantity sold") + xlab("date") +
  ggtitle("Beverage sales") +
  theme(legend.position = "none")

# Some clear seasonality in each shop's sales.. Doesn't appear to be much of a trend.
train_hts %>% filter(!is_aggregated(shop)) %>% filter(is_aggregated(container)) %>% filter(is_aggregated(brand)) %>%
  autoplot(quantity) + ylab("quantity sold") + xlab("date") +
  ggtitle("Beverage sales by shop") +
  facet_wrap(vars(shop), scales = "free_y", ncol = 3) +
  theme(legend.position = "none")

# Dig deeper: each brand at each shop. Some seasonality visible as well, there is also some trend change.
train_hts %>% filter (is_aggregated(container)) %>% filter(!is_aggregated(shop)) %>% filter(!is_aggregated(brand)) %>%
  autoplot(quantity) + ylab("quantity sold") + xlab("date") +
  ggtitle("Beverage sales by shop and brand") +
  facet_wrap(vars(shop, brand), scales = "free_y", ncol = 5) +
  theme(legend.position = "none")

# Dig even deeper: each container for each brand at each shop. Do one shop at a time (otherwise plots too small)
# Again, some seasonality visible, as well as trend change.
train_hts %>% filter(shop == "shop_1") %>% filter(!is_aggregated(shop)) %>% filter(!is_aggregated(brand)) %>%
  filter(!is_aggregated(container)) %>%
  autoplot(quantity) + ylab("quantity sold") + xlab("date") +
  ggtitle("Individual product beverage sales") +
  facet_wrap(vars(shop, brand), scales = "free_y", ncol = 2) +
  theme(legend.position = "none")


### 3 MODEL BUILDING

# As we go deeper in the hierarchy, the hts model takes longer and longer to fit.
# Start from shallow and go deeper until it becomes impractical.

## DEPTH 1 (by shop) 

depth1_data <- train %>% bind_rows(test)%>%
  aggregate_key((shop), price = mean(price), pop = mean(pop), quantity = sum(quantity))

depth1_m <- depth1_data %>% filter(year(date) <= 2017) %>%
  model(base = ARIMA(quantity ~ price + pop)) %>% reconcile(
    bu = bottom_up(base),
    ols = min_trace(base, method = "ols"),
    mint = min_trace(base, method = "mint_shrink"),
  )

depth1_future <- depth1_data %>% filter(year(date) > 2017)

depth1_fcst <- fabletools::forecast(depth1_m, new_data = depth1_future)

# model evaluation

autoplot(depth1_fcst, depth1_data, level = NULL) +
  facet_wrap(vars(shop), scales = "free_y", ncol = 3)

depth1_fcst %>%
  accuracy(data = depth1_data, measures = list(mase = MASE)) %>%
  pivot_wider(names_from = shop, values_from = mase)

# look at the prediction intervals for aggregated forecast
pal <- brewer.pal(3, "Set1")
depth1_fcst %>% filter(is_aggregated(shop)) %>% filter(.model %in% c("base", "ols")) %>%
  autoplot(depth1_future, level = 95) + scale_colour_manual(values = pal)

## DEPTH 2 (by shop and brand)

depth2_data <- train %>% bind_rows(test)%>%
  aggregate_key((shop/brand), price = mean(price), pop = mean(pop), quantity = sum(quantity))

depth2_m <- depth2_data %>% filter(year(date) <= 2017) %>%
  model(base = ARIMA(quantity ~ price + pop)) %>% reconcile(
    bu = bottom_up(base),
    ols = min_trace(base, method = "ols"),
    mint = min_trace(base, method = "mint_shrink"),
  )

depth2_future <- depth2_data %>% filter(year(date) > 2017)

depth2_fcst <- fabletools::forecast(depth2_m, new_data = depth2_future)

# model evaluation

# plot one shop at a time because otherwise crowded
depth2_fcst %>%  filter(shop == "shop_6") %>% autoplot(depth2_data, level = NULL)  +
  facet_wrap(vars(shop, brand), scales = "free_y", ncol = 3)

depth2_fcst %>%  filter(is_aggregated(brand)) %>% autoplot(depth2_data, level = NULL)  +
  facet_wrap(vars(shop, brand), scales = "free_y", ncol = 3)

View(depth2_fcst %>%
  accuracy(data = depth2_data, measures = list(mase = MASE)) %>%
  pivot_wider(names_from = shop, values_from = mase))

# look at the prediction intervals for aggregated forecast
pal <- brewer.pal(3, "Set1")
depth2_fcst %>% filter(is_aggregated(shop)) %>% filter(.model %in% c("base", "ols")) %>%
  autoplot(depth2_future, level = 95) + scale_colour_manual(values = pal)

## variance at depth 2 is about 95% of that at depth 1.
pal <- brewer.pal(3, "Set3")
d1 <- depth1_fcst %>% filter(is_aggregated(shop)) %>% filter(.model %in% c("ols"))
d2 <- depth2_fcst %>% filter(is_aggregated(shop)) %>% filter(.model %in% c("ols"))

d1$quantity
d2$quantity


## DEPTH 3 (by shop, brand, container)

# NOT RUN (would take too long)
# depth3_data <- train %>% bind_rows(test)%>%
#   aggregate_key((shop/brand), price = mean(price), pop = mean(pop), quantity = sum(quantity))
# 
# depth3_m <- depth3_data %>% filter(year(date) <= 2017) %>%
#   model(base = ARIMA(quantity ~ price + pop)) %>% reconcile(
#     bu = bottom_up(base),
#     ols = min_trace(base, method = "ols"),
#     mint = min_trace(base, method = "mint_shrink"),
#   )
# 
# depth3_future <- depth3_data %>% filter(year(date) > 2017)
# 
# depth3_fcst <- fabletools::forecast(depth3_m, new_data = depth3_future)
# 
# # model evaluation
# 
# autoplot(depth3_fcst, depth3_data, level = NULL) +
#   facet_wrap(vars(shop, brand, container), scales = "free_y", ncol = 3)
# 
# depth3_fcst %>%
#   accuracy(data = depth3_data, measures = list(mase = MASE)) %>%
#   pivot_wider(names_from = shop, values_from = mase)

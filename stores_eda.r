rm(list = ls())
gc()
source("./dependencies.R")

stores <- fread(file = "./stores.csv")

train <- fread(file = "./train.csv")

# Sales by Store
store_sales <- train %>% group_by(store_nbr) %>% summarize(ttl_sales = sum(unit_sales))
setDT(store_sales)
store_sales$store_nbr <- factor(store_sales$store_nbr, levels = store_sales$store_nbr[order(store_sales$ttl_sales)])

ggplotly(ggplot(store_sales, aes(x = store_nbr, y = ttl_sales, fill = store_nbr)) +
    geom_bar(stat = "identity") +
    labs(x = "Store", y = "Total Sales (Quantity)", title = "Total Sales by Store") +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 8, angle = 90)
    ))

# Store by City
ggplotly(ggplot(NULL, aes(x = city, fill = city)) +
    geom_bar(data = stores) +
    labs(x = "City", y = "Count", title = "Count of Stores by City") +
    scale_y_continuous(breaks = seq(0, 20, 1)) + 
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 8, angle = 90)
    ))

# Sales by City
city_sales <- stores[train, on = c("store_nbr")]
city_sales <- city_sales %>% group_by(city) %>% summarize(ttl_sales = sum(unit_sales))
setDT(city_sales)
city_sales[, ttl_sales := round(ttl_sales / 1000000)]
ggplotly(ggplot(NULL, aes(x = city, y = ttl_sales, fill = city)) +
    geom_bar(data = city_sales, stat = "identity") +
    scale_y_continuous(breaks = seq(0, 600, 25)) + 
    labs(x = "City", y = "Total Sales (Quantity (M))", title = "Total Sales by City") +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 8, angle = 90)
    ))

# Store by State
ggplotly(ggplot(NULL, aes(x = state, fill = state)) +
    geom_bar(data = stores) +
    labs(x = "State", y = "Count", title = "Count of Stores by State") +
    scale_y_continuous(breaks = seq(0, 20, 1)) +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 8, angle = 90)
    ))

# Sales by State
state_sales <- stores[train, on = c("store_nbr")]
state_sales <- state_sales %>% group_by(state) %>% summarize(ttl_sales = sum(unit_sales))
setDT(state_sales)
state_sales[, ttl_sales := round(ttl_sales / 1000000)]

ggplotly(ggplot(NULL, aes(x = state, y = ttl_sales, fill = state)) +
    geom_bar(data = state_sales, stat = "identity") +
    scale_y_continuous(breaks = seq(0, 600, 25)) + 
    labs(x = "State", y = "Total Sales (Quantity (M))", title = "Total Sales by State")+
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 8, angle = 90)
    ))

# Store by Type
ggplotly(ggplot(NULL, aes(x = type, fill = type)) +
    geom_bar(data = stores) +
    labs(x = "Type", y = "Count", title = "Count of Stores by Type") +
    scale_y_continuous(breaks = seq(0, 20, 1)) +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
    ))

type_sales <- stores[train, on = c("store_nbr")]
type_sales <- type_sales %>% group_by(type) %>% summarize(ttl_sales = sum(unit_sales))
setDT(type_sales)
type_sales[, ttl_sales := round(ttl_sales / 1000000)]

ggplotly(ggplot(NULL, aes(x = type, y = ttl_sales, fill = type)) +
    geom_bar(data = type_sales, stat = "identity") +
    scale_y_continuous(breaks = seq(0, 600, 25)) + 
    labs(x = "Type", y = "Total Sales (Quantity (M))", title = "Total Sales by Type")+
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
    ))

# Store by Cluster
stores[, cluster := as.factor(cluster)]

ggplotly(ggplot(NULL, aes(x = cluster, fill = cluster)) +
    geom_bar(data = stores) +
    labs(x = "Cluster", y = "Count", title = "Count of Stores by Cluster") +
    scale_y_continuous(breaks = seq(0, 10, 1)) +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
    ))

clr_sales <- stores[train, on = c("store_nbr")]
clr_sales <- clr_sales %>% group_by(cluster) %>% summarize(ttl_sales = sum(unit_sales))
setDT(clr_sales)
clr_sales[, ttl_sales := round(ttl_sales / 1000000)]

ggplotly(ggplot(NULL, aes(x = cluster, y = ttl_sales, fill = cluster)) +
    geom_bar(data = clr_sales, stat = "identity") +
    scale_y_continuous(breaks = seq(0, 600, 25)) + 
    labs(x = "Type", y = "Total Sales (Quantity (M))", title = "Total Sales by Type")+
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
    ))

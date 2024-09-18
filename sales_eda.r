rm(list = ls())
gc()
source("./dependencies.R")

train <- fread(file = "./train.csv")
items <- fread(file = "./items.csv")

# Negative Sales
neg_sales <- train[unit_sales < 0]
nrow(neg_sales) / nrow(train) * 100
nrow(neg_sales[unit_sales < -1])
nrow(neg_sales[unit_sales < -100])
nrow(neg_sales[unit_sales < -1000])
nrow(neg_sales[unit_sales < -10000])

neg_sales_mrg <- items[neg_sales, on = c("item_nbr")]
neg_sales_mrg <- neg_sales_mrg[order(neg_sales_mrg$unit_sales, decreasing = FALSE),]
View(head(neg_sales_mrg, 5))

# On Promotion Sales
ggplotly(ggplot(NULL, aes(onpromotion, fill = onpromotion)) +
    geom_bar(data = train) +
    labs(x = "On Promotion Flag", y = "Count", title = "Count by Promotion Sales") +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
    ))

# On Promotion Sales Item Example
train_clean <- train[!is.na(onpromotion)]
train_clean <- train_clean[item_nbr == 323013]

off_promo <- train_clean[onpromotion == FALSE]
off_promo <- off_promo %>% group_by(date) %>% summarize(ttl_sales = sum(unit_sales))

on_promo <- train_clean[onpromotion == TRUE]
on_promo <- on_promo %>% group_by(date) %>% summarize(ttl_sales = sum(unit_sales))

ttl_promo <- train_clean %>% group_by(date) %>% summarize(ttl_sales = sum(unit_sales))

ggplotly(ggplot(NULL, aes(onpromotion, fill = onpromotion)) +
    geom_bar(data = train) +
    labs(x = "On Promotion Flag", y = "Count", title = "Count by Promotion Sales") +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
    ))

ggplotly(ggplot(NULL, aes(x = date, y = ttl_sales)) +
    geom_line(data = ttl_promo, aes(colour = "Total Sales"), size = 1) +
    geom_line(data = off_promo, aes(colour = "Off Promo"), size = 0.5) +
    geom_line(data = on_promo, aes(colour = "On Promo"), size = 0.5) +
    scale_x_date(date_labels = "%b %d", date_breaks = "months") +
    labs(x = "Date (Days)", y = "Total Sales (Quantity)", title = "Total Sales Vs Promo (Item: 323013 | GROCERY I | Class: 1058)") +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 8, angle = 90)
    ))

# Sales Vs Oil
day_train <- train %>% group_by(date) %>% summarize(ttl_sales = sum(unit_sales))
setDT(day_train)

oil <- fread("./oil.csv")
colnames(oil)[2] <- "ttl_sales"

day_train[, ttl_sales := ttl_sales / 10000]

ggplotly(ggplot(NULL, aes(x = date, y = ttl_sales)) +
    geom_line(data = day_train, colour = "skyblue2") +
    geom_line(data = oil, colour = "red") +
    scale_x_date(date_labels = "%b %d", date_breaks = "months") +
    geom_smooth(data = day_train) +
    labs(x = "Date (Days)", y = "Oil Price ($) | Total Sales / 10000 (Quantity)", title = "Total Sales Vs Oil Price") +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 8, angle = 90)
    ))


# Sales Vs Transactions
transactions <- fread(file = "./transactions.csv")
day_trans <- transactions %>% group_by(date) %>% summarize(ttl_sales = sum(transactions))

day_train[, ttl_sales := ttl_sales * 1000]

ggplotly(ggplot(NULL, aes(x = date, y = ttl_sales)) +
    geom_smooth(data = day_train, aes(colour = "Total Sales")) +
    geom_smooth(data = day_trans, aes(colour = "Transactions")) +
    scale_x_date(date_labels = "%b %d", date_breaks = "months") +
    labs(x = "Date (Days)", y = "Total Sales * 10 (Quantity) | Total Txns", title = "Total Sales Vs Transactions") +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 8, angle = 90)
    ))

# Time Frame of Train and Test data
test <- fread(file = "./test.csv")

plot_train <- train %>%
  distinct(date) %>%
  mutate(dset = "train")

plot_test <- test %>%
  distinct(date) %>%
  mutate(dset = "test")

plot_train <- plot_train %>%
  bind_rows(plot_test) %>%
  mutate(year = year(date))
year(plot_train$date) <- 2017

ggplotly(ggplot(plot_train, aes(date, year, color = dset)) +
    geom_point(shape = "|", size = 10) +
    scale_x_date(date_labels = "%b", date_breaks = "months") +
    labs(x = "Month", y = "Year", color = "Data set", title = "Datasets by Time Period") +
    theme( 
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
    ))

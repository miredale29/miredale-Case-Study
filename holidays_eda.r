rm(list = ls())
gc()
source("./dependencies.R")

holidays <- fread(file = "./holidays_events.csv")
holidays <- holidays[transferred == FALSE]
holidays[type == "Transfer", type := "Holiday"]

train <- fread("./train.csv")
#View(head(train, 1000))

stores <- fread(file = "./stores.csv")
#View(head(stores, 1000))

date_sales <- train %>% group_by(date) %>% summarize(ttl_sales = sum(unit_sales))
setDT(date_sales)

# Earthquake Assessment
ggplotly(ggplot(NULL, aes(x = date, y = ttl_sales)) +
    geom_line(data = date_sales, colour = "skyblue2") +
    geom_point(data = date_sales, colour = "skyblue4") +
    geom_smooth(data = date_sales) +
    scale_x_date(date_labels = "%b %d", date_breaks = "months") +
    labs(x = "Date (Days)", y = "Total Sales (Quantity)", title = "Total Sales (Earthquake)") +
    geom_vline(xintercept = as.numeric(as.Date("2016-04-16")), linetype = 4, colour = "black") +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 8, angle = 90)
    ))

# Pay Day Assessment
date_sales[, eom_date := ceiling_date(ymd(date), 'month') - days(1)]
date_sales[, mom_date := as.Date(paste0(year(date), "-", month(date), "-15"))]

date_extract <- list(unique(date_sales$eom_date))
date_extract_temp <- list(unique(date_sales$mom_date))

date_extract <- rbindlist(list(date_extract, date_extract_temp))

pd_sales <- train[date %in% date_extract$V1]
pd_sales <- pd_sales %>% group_by(date) %>% summarize(ttl_sales = sum(unit_sales))

non_pd_sales <- date_sales[!(date %in% date_extract$V1)]

ggplotly(ggplot(NULL, aes(x = date, y = ttl_sales)) +
    geom_line(data = date_sales, colour = "skyblue2") +
    geom_point(data = non_pd_sales, colour = "skyblue4") +
    geom_point(data = pd_sales, colour = "red") +
    scale_x_date(date_labels = "%b %d", date_breaks = "months") +
    labs(x = "Date (Days)", y = "Total Sales (Quantity)", title = "Total Sales (Pay Day)") +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 8, angle = 90)
    ))

# Holidays Assessment
# # All holidays by locale
non_holiday_sales <- holidays[date_sales, on = c("date")]

nat_holidays <- non_holiday_sales[locale == "National"]
reg_holidays <- non_holiday_sales[locale == "Regional"]
loc_holidays <- non_holiday_sales[locale == "Local"]
non_holiday_sales <- non_holiday_sales[is.na(type)]

ggplotly(ggplot(NULL, aes(x = date, y = ttl_sales)) +
    geom_line(data = date_sales, colour = "skyblue2") +
    geom_point(data = nat_holidays, colour = "red", size = 1) +
    geom_point(data = reg_holidays, colour = "green3", size = 1) +
    geom_point(data = loc_holidays, colour = "orange3", size = 1) +
    geom_point(data = non_holiday_sales, colour = "skyblue4", size = 0.5) +
    geom_smooth(data = date_sales) +
    scale_x_date(date_labels = "%b %d", date_breaks = "months") +
    labs(x = "Date (Days)", y = "Total Sales (Quantity)", title = "Total Sales Vs Holidays") +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 8, angle = 90)
    ))

loc_summary <- data.table(type = c("National", "Regional", "Local"),
                            avg = c(round(sum(nat_holidays$ttl_sales) / nrow(nat_holidays), 0),
                                    round(sum(reg_holidays$ttl_sales) / nrow(reg_holidays), 0),
                                    round(sum(loc_holidays$ttl_sales) / nrow(loc_holidays), 0)),
                            count = c(nrow(nat_holidays),
                                    nrow(reg_holidays),
                                    nrow(loc_holidays)))

ggplotly(ggplot(loc_summary, aes(x = type, y = avg, fill = type)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0("Avg Sales: ", avg), y = 600000), color = "black", size = 3.5) +
    geom_text(aes(label = paste0("Count: ", count), y = 575000), color = "white", size = 3.5) +
    labs(x = "Loc (holiday)", y = "Total Sales (Avg)", title = "Average Sales by Holiday Loc") +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
    ))

# # Holidays by type
hol_holidays <- non_holiday_sales[type == "Holiday"]
add_holidays <- non_holiday_sales[type == "Additional"]
brg_holidays <- non_holiday_sales[type == "Bridge"]
wrk_holidays <- non_holiday_sales[type == "Work Day"]
evt_holidays <- non_holiday_sales[type == "Event"]

type_summary <- data.table(type = c("Holiday", "Additional", "Bridge", "Work Day", "Event"),
                            avg = c(round(sum(hol_holidays$ttl_sales) / nrow(hol_holidays), 0),
                                    round(sum(add_holidays$ttl_sales) / nrow(add_holidays), 0),
                                    round(sum(brg_holidays$ttl_sales) / nrow(brg_holidays), 0),
                                    round(sum(wrk_holidays$ttl_sales) / nrow(wrk_holidays), 0),
                                    round(sum(evt_holidays$ttl_sales) / nrow(evt_holidays), 0)),
                            count = c(nrow(hol_holidays),
                                      nrow(add_holidays),
                                      nrow(brg_holidays),
                                      nrow(wrk_holidays),
                                      nrow(evt_holidays)))

ggplotly(ggplot(type_summary, aes(x = type, y = avg, fill = type)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0("Avg Sales: ", avg), y = 600000), color = "black", size = 3) +
    geom_text(aes(label = paste0("Count: ", count), y = 575000), color = "white", size = 3) +
    labs(x = "Type (Holiday)", y = "Total Sales (Avg)", title = "Average Sales by Type") +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
    ))

# # Specific Local Example (Ambato)
loc_stores <- stores[city == "Ambato"]
loc_sales <- train[store_nbr %in% loc_stores$store_nbr]
loc_sales <- loc_sales %>% group_by(date) %>% summarize(ttl_sales = sum(unit_sales))
setDT(loc_sales)

loc_holidays <- holidays[locale_name == "Ambato"]

non_loc_holidays <- loc_holidays[loc_sales, on = c("date")]

loc_holidays <- non_loc_holidays[!is.na(type)]
non_loc_holidays <- non_loc_holidays[is.na(type)]

ggplotly(ggplot(NULL, aes(x = date, y = ttl_sales)) +
    geom_line(data = loc_sales, colour = "skyblue2") +
    geom_point(data = loc_holidays, colour = "red", size = 1) +
    geom_point(data = non_loc_holidays, colour = "skyblue4", size = 0.5) +
    scale_x_date(date_labels = "%b %d", date_breaks = "months") +
    labs(x = "Date (Days)", y = "Total Sales (Quantity)", title = "Total Sales Vs Holidays") +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 8, angle = 90)
    ))

# MONTHLY Sales (for Xmas Period)
holiday_sales <- holidays[date_sales, on = c("date")]
holiday_sales <- holiday_sales[!is.na(type)]
holiday_sales[, year := isoyear(date)]
holiday_sales[, month := month(date)]
holiday_sales[, year_month := paste0(year, "-", month)]
holiday_sales <- holiday_sales %>% group_by(year_month) %>% summarize(ttl_sales = n())   
setDT(holiday_sales)

holiday_sales[, date := paste0(year_month, "-01")]
holiday_sales[, date_fm := as.Date(date, "%Y-%m-%d")]

train[, year := isoyear(date)]
train[, month := month(date)]
train[, year_month := paste0(year, "-", month)]
month_sales <- train %>% group_by(year_month) %>% summarize(ttl_sales = sum(unit_sales))
setDT(month_sales)

month_sales[, date := paste0(year_month, "-01")]
month_sales[, date_fm := as.Date(date, "%Y-%m-%d")]

ggplotly(ggplot(NULL, aes(x = date_fm, y = ttl_sales)) +
    geom_line(data = month_sales, aes(colour = "Sales")) +
    geom_point(data = month_sales, colour = "skyblue4", size = 1) +
    scale_x_date(date_labels = "%y %b", date_breaks = "months") +
    labs(x = "Date (Month)", y = "Total Sales (Quantity)", title = "Total Sales by Month") +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 8, angle = 90)
    ))

month_sales[, ttl_sales := ttl_sales / 1000000]

ggplotly(ggplot(NULL, aes(x = date_fm, y = ttl_sales)) +
    geom_line(data = month_sales, aes(colour = "Sales")) +
    geom_point(data = holiday_sales, aes(colour = "Holidays"), size = 2) +
    geom_point(data = month_sales, colour = "skyblue4", size = 1) +
    scale_x_date(date_labels = "%y %b", date_breaks = "months") +
    labs(x = "Date (Month)", y = "Total Sales * 1M (Quantity) | Holidays (Count)", title = "Total Sales by Month") +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 8, angle = 90)
    ))

# Weekly Sales
week_sales <- train
week_sales[, year := year(date)]
week_sales[, week := week(date)]

#week_sales[, week_start := paste0(year, "-", month, "-", week)]

week_sales_agg <- week_sales %>% group_by(year, week) %>% summarize(ttl_sales = sum(unit_sales), .groups = "drop")
setDT(week_sales_agg)
week_sales_agg[, week_start := ymd(paste0(year, "-01-01")) + weeks(week - 1)]

ggplotly(ggplot(NULL, aes(x = week_start, y = ttl_sales)) +
    geom_line(data = week_sales_agg, colour = "skyblue2") +
    geom_point(data = week_sales_agg, colour = "skyblue4") +
    geom_smooth(data = week_sales_agg) +
    scale_x_date(date_labels = "%y %b", date_breaks = "months") +
    labs(x = "Week", y = "Total Sales (Quantity)", title = "Weekly Total Sales") +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 8, angle = 90)
    ))

# Day of Week Assessment
train[, month := month(date, label = TRUE)]
train[, wday := wday(date, label = TRUE)]

wday_sales <- train %>% group_by(month, wday) %>% summarize(mean_sales = mean(unit_sales))
setDT(wday_sales)

ggplotly(ggplot(NULL, aes(x = month, y = wday, fill = mean_sales)) +
    geom_tile(data = wday_sales) +
    labs(x = "Month", y = "Day of Week", title = "Mean Sales by Week Day") +
    scale_fill_distiller(palette = "Spectral") +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
    ))



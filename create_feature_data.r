rm(list = ls())
gc()
source("./dependencies.R")

train <- fread("./train.csv")
train <- train[date >= max(train$date) - 365] # Reduce datasize (memory limitations)

test <- fread("./test.csv")
test$unit_sales <- 0
setcolorder(test, "unit_sales", before = 5)

# data train and test to get missing ILDs
data <- rbind(train, test)
data$date <- as.Date(data$date)
data$id <- NULL

rm(train, test)
gc()

data <- data %>%
    as_tsibble(index = date, key = c(store_nbr, item_nbr)) %>%
    fill_gaps()

# Merge in stores
stores <- fread(file = "./stores.csv")
data <- stores[data, on = c("store_nbr")]
data$cluster <- NULL

# Oil prep
oil <- fread(file = "./oil.csv")
colnames(oil)[2] <- "oil_price"
oil$date <- as.Date(oil$date)
dates_to_check <- seq(min(data$date), max(data$date), by = "day")

mng_oil_dts <- dates_to_check[which(!dates_to_check %in% oil$date)]
miss_oil <- data.table(date = as.Date(mng_oil_dts), oil_price = as.numeric(NA))
oil <- rbind(oil, miss_oil)

ts_oil <- as_tsibble(oil, index = date)
ts_oil <- ts_oil %>% fill(oil_price, .direction = "downup")

setDT(ts_oil)
saveRDS(ts_oil, "./oil_impute.rds")

oil <- ts_oil
rm(ts_oil)

# Holidays
holidays <- fread(file = "./holidays_events.csv")
## Transferred = true: Holiday was move from original date (type = Transfer)
## Strip out transferred holidays (no longer holiday's date)
holidays <- holidays[transferred == FALSE]
holidays[type == "Transfer", type := "Holiday"] # Replace new holiday's date type to "Holiday"

## National holidays prep
nat_holidays <- holidays[locale == "National"]

if(nrow(nat_holidays) != length(unique(nat_holidays$date))){
    nat_holidays <- nat_holidays[!duplicated(nat_holidays$date)]
}

## Regional holidays prep
reg_holidays <- holidays[locale == "Regional", .(date, locale_name)]
colnames(reg_holidays)[2] <- "state"
reg_holidays[, mrg_flag := TRUE]

## Local holidays prep
loc_holidays <- holidays[locale == "Local", .(date, locale_name)]

colnames(loc_holidays)[2] <- "city"
dup_locs <- loc_holidays %>% group_by(date) %>% summarize(count = n())
setDT(dup_locs)
dup_locs <- dup_locs[count > 1]
dup_locs <- loc_holidays[date %in% dup_locs$date]
loc_holidays <- loc_holidays[!date %in% dup_locs$date]

## Read in other feature data

# Merge in items
items <- fread(file = "./items.csv")
items$family = NULL
items$class = NULL
data <- items[data, on = c("item_nbr")]

# Merge in oil
data <- oil[data, on = c("date")]

# Merge in transactions
txns <- fread("./transactions.csv")
data <- txns[data, on = c("date", "store_nbr")]

# Merge in holidays
data[date %in% nat_holidays$date, holiday := TRUE] ## National

data <- reg_holidays[data, on = c("date", "state")] ## Regional
data[mrg_flag == TRUE, holiday := TRUE]
data$mrg_flag = NULL

## Local
for(rec in 1:nrow(dup_locs)){
    curr_rec <- dup_locs[rec]
    data[date == curr_rec$date & city == curr_rec$city, holiday := TRUE]
}

data <- loc_holidays[data, on = c("date", "city")]
if(!is.null(data$mrg_flag)){
    data[mrg_flag == TRUE, holiday := TRUE]
    data$mrg_flag = NULL
}

data[is.na(holiday), holiday := FALSE]
data$state = NULL
data$city = NULL

for (i in 1:length(colnames(data))){
    na_count <- nrow(data[is.na(data[[i]])])
    print(paste0("NAs Count for: ", colnames(data)[[i]], " = ", na_count))
}

data[is.na(onpromotion), onpromotion := FALSE]
data[is.na(data)] <- 0

data_lim <- data[date >= "2017-01-01"]
saveRDS(data_lim, "./full_feature_filt")
rm(data_lim)

data[unit_sales < 0, unit_sales := 0]

data$date <- as.Date(data$date)
data$weekday <- as.factor(weekdays(data$date))
data$month <- as.factor(format(data$date, "%m"))

# Using randomForest to determine importance of features
rf_data <- data[store_nbr == 44 & date < as.Date("2017-01-01")]

rf_model <- randomForest(unit_sales ~ ., data = rf_data, importance = TRUE)
importance(rf_model)

# Model showed oil_price and holiday proved to decrease model performance
saveRDS(rf_model, "./rf_model_44_all.rds")
rm(rf_data)

# Group by type and save
type_data <- data %>%
  group_by(date, type, item_nbr) %>%
  summarize(
    transactions = sum(transactions, na.rm = TRUE),
    perishable = first(perishable),
    onpromotion = first(onpromotion),
    holiday = first(holiday),
    oil_price = first(oil_price),
    weekday = first(weekday),
    month = first(month),
    unit_sales = sum(unit_sales, na.rm = TRUE)
  ) %>%
  ungroup()

setDT(type_data)

saveRDS(type_data, "./feature_type.rds")
rm(type_data)

# Group by store and save
data$type <- NULL
for(store in unique(data$store_nbr)){
    store_data <- data[store_nbr == store]

    saveRDS(store_data, paste0("./feature_store/", store, ".rds"))
}
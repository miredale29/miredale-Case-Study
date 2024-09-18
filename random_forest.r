rm(list = ls())
gc()
source("./dependencies.R")

ftr_data <- readRDS("./feature_type.rds")

types <- fread("./stores.csv")
types <- unique(types$type)

for(type_id in types){
    train <- ftr_data[type == type_id & date < as.Date("2017-01-01")] # Grab data to train
    act <- ftr_data[type == type_id & date >= as.Date("2017-01-01") & date < as.Date("2017-05-20")] # Seperate actual data
    test <- ftr_data[type == type_id & date >= as.Date("2017-08-16")] # Separate test data
    
    # Run Model
    rf_model <- randomForest(unit_sales ~ ., data = train)

    saveRDS(rf_model, paste0("./", type_id, "/rf_model.rds"))

    # Predict unit sales on act data
    test_act <- act
    test_act$unit_sales <- NULL

    predictions <- predict(rf_model, newdata = test_act)

    # Combine actual and predicted values
    results <- data.table(date = act$date,
                        actual = act$unit_sales, 
                        predicted = predictions,
                        perishable = act$perishable)

    saveRDS(results, paste0("./", type_id, "/results.rds"))

    forecasts <- predict(rf_model, newdata = test)
    
    fcst_results <- data.frame(date = test$date, 
                            type = test$type,
                            item = test$item_nbr,
                            forecast = forecasts)

    saveRDS(fcst_results, paste0("./", type_id, "/forecast.rds"))
    gc()
}

# Combining results for each type in one dataset for both results and forecast
for(type_id in types){
    act <- ftr_data[type == type_id & date >= as.Date("2017-01-01") & date < as.Date("2017-05-20")] # Seperate actual data
    results <- readRDS(paste0("./", type_id, "/results.rds"))
    setDT(results)

    results$date <- act$date

    fcst <- readRDS(paste0("./", type_id, "/forecast.rds"))

    if(type_id != types[1]){
        full_res <- rbind(full_res, results)
        full_fcst <- rbind(full_fcst, fcst)

    }else{
        full_res <- results
        full_fcst <- fcst
    }
}

saveRDS(full_res, "./rf_full_res.rds")
saveRDS(full_fcst, "./rf_full_fcst.rds")

# Assessing model results and forecasts
mae <- mae(full_res$actual, full_res$pred)  # Mean Absolute Error
print(paste0("Type ", type_id, ": MAE = ", mae))

rmse <- rmse(full_res$actual, full_res$pred) # Root Mean Squared Error
print(paste0("Type ", type_id, ": RMSE = ", rmse))

ggplot(full_res, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Actual vs Predicted Sales (RF)", x = "Actual Sales", y = "Predicted Sales") +
      theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
    )

mean_res <- full_res %>% group_by(date) %>% summarize(value = mean(actual), mean_pred = mean(predicted))
setDT(mean_res)

mean_pred <- mean_res[, .(date, mean_pred)]
colnames(mean_pred)[2] <- "value"

ggplotly(ggplot(NULL, aes(x = date, y = value)) +
    geom_line(data = mean_res, aes(colour = "Actual")) +
    geom_line(data = mean_pred, aes(colour = "Pred")) +
    scale_x_date(date_labels = "%b %d", date_breaks = "weeks") +
    labs(x = "Date", y = "Unit Sales (mean)", title = "Predictions Vs Actual (RF)") +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 8, angle = 90)
    ))

mean_fcst <- full_fcst %>% group_by(date) %>% summarize(unit_sales = mean(forecast))

ggplotly(ggplot(mean_fcst, aes(x = date, y = unit_sales)) +
    geom_line(colour = "skyblue3") +
    geom_point(colour = "skyblue4") +
    scale_x_date(date_labels = "%b %d", date_breaks = "day") +
    labs(x = "Date", y = "Unit Sales (mean)", title = "Forecasts (RF)") +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
    ))

full_res <- full_res[actual > 0 & predicted > 0]

# nwrmsle
full_res[perishable == 0, weight := 1]
full_res[perishable == 1, weight := 1.25]

log_errors <- (log(full_res$predicted + 1) - log(full_res$actual + 1))^2
weighted_errors <- log_errors * full_res$weight
nwrmsle <- sqrt(sum(weighted_errors) / sum(full_res$weight))
print(paste0("Type ", type_id, ": NWRMSLE = ", nwrmsle))

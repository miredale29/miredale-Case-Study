rm(list = ls())
gc()
source("./dependencies.R")

ftr_data <- readRDS("./full_feature_filt.rds")
ftr_data$transactions <- NULL

train <- ftr_data[date < as.Date("2017-07-16")]
act <- ftr_data[date >= as.Date("2017-07-16") & date < as.Date("2017-08-16")] # Seperate actual data
test <- ftr_data[date >= as.Date("2017-08-16")] # Separate test data
test$unit_sales <- NULL

print(paste0("Model - START: ", Sys.time()))
model <- lm(unit_sales ~ ., data = train)
print(paste0("Model - FINISH: ", Sys.time()))

#saveRDS(model, "./lm_model.rds")
saveRDS(model, "./lm_model_no_txn.rds")

val <- copy(act)
val$unit_sales <- NULL
val$f_unit_sales <- predict(model, newdata = val)

val <- val[, .(date, store_nbr, item_nbr, f_unit_sales)]

val <- merge(act, val, by = c("date", "store_nbr", "item_nbr"), all.x = TRUE)

#saveRDS(val, "./lm_results.rds")
saveRDS(val, "./lm_results_no_txn.rds")

test$unit_sales <- predict(model, newdata = test)
saveRDS(test, "./lm_forecast_no_txn.rds")

# Assessing model results and forecasts
#res <- readRDS("./lm_results.rds")
res <- readRDS("./lm_results_no_txn.rds")

mae <- mae(res$unit_sales, res$f_unit_sales)  # Mean Absolute Error
print(paste0("MAE = ", mae))

rmse <- rmse(res$unit_sales, res$f_unit_sales) # Root Mean Squared Error
print(paste0("RMSE = ", rmse))

res[unit_sales < 0, unit_sales := 0]
res[f_unit_sales < 0, f_unit_sales := 0]

ggplot(res, aes(x = unit_sales, y = f_unit_sales)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Actual vs Predicted Sales (LR)", x = "Actual Sales", y = "Predicted Sales") +
      theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
    )

mean_res <- res %>% group_by(date) %>% summarize(value = mean(unit_sales), mean_pred = mean(f_unit_sales))
setDT(mean_res)

mean_pred <- mean_res[, .(date, mean_pred)]
colnames(mean_pred)[2] <- "value"

ggplotly(ggplot(NULL, aes(x = date, y = value)) +
    geom_line(data = mean_res, aes(colour = "Actual")) +
    geom_line(data = mean_pred, aes(colour = "Pred")) +
    scale_x_date(date_labels = "%b %d", date_breaks = "weeks") +
    labs(x = "Date", y = "Unit Sales (mean)", title = "Predictions Vs Actual (LR)") +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 8, angle = 90)
    ))

res <- res[unit_sales > 0 & f_unit_sales > 0]
res[perishable == 0, weight := 1]
res[perishable == 1, weight := 1.25]

log_errors <- (log(res$f_unit_sales + 1) - log(res$unit_sales + 1))^2
weighted_errors <- log_errors * res$weight
nwrmsle <- sqrt(sum(weighted_errors) / sum(res$weight))
print(paste0("NWRMSLE = ", nwrmsle))


#fcst <- readRDS("./lm_forecast.rds")
fcst <- readRDS("./lm_forecast_no_txn.rds")
fcst[unit_sales < 0, unit_sales := 0]
mean_fcst <- fcst %>% group_by(date) %>% summarize(unit_sales = mean(unit_sales))

ggplotly(ggplot(mean_fcst, aes(x = date, y = unit_sales)) +
    geom_line(colour = "skyblue3") +
    geom_point(colour = "skyblue4") +
    scale_x_date(date_labels = "%b %d", date_breaks = "day") +
    labs(x = "Date", y = "Unit Sales (mean)", title = "Forecasts (LR)") +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
    ))
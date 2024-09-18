rm(list = ls())
gc()
source("./dependencies.R")


# Selecting relevant features
train <- readRDS("./full_feature_filt.rds")
train <- train[, .(date, store_nbr, item_nbr, perishable, onpromotion, transactions, unit_sales)]

actuals <- train[date >= "2017-08-01" & date < "2017-08-16"] # Actuals for validation and accuracy

test <- train[date >= "2017-08-16"]
test <- test[, .(id, date, store_nbr, item_nbr, perishable, onpromotion, transactions)]

train <- train[date < "2017-08-01"]

# Convert to time series object for ARIMA to ingest
ts_train <- ts(train$unit_sales, frequency = 365)

# Create external regressors prior to modeling
xreg <- cbind(
  train$transactions,
  train$perishable,
  train$on_promotion
)

fit <- auto.arima(ts_train, xreg = xreg)
summary(fit)

saveRDS(fit, "./arima_model.rds")
fit <- readRDS("./arima_model.rds")

validate <- copy(actuals)
validate$unit_sales = NULL

val_xreg <- cbind(
  validate$transactions,
  validate$perishable,
  validate$on_promotion
)

val_result <- forecast(fit, xreg = val_xreg, h = length(seq(min(validate$date), max(validate$date), "day")))
plot(val_result)

val_fcst <- val_result$mean

val_assess <- data.table(
    date = actuals$date,
    perishable = actuals$perishable,
    actual = actuals$unit_sales,
    predicted = val_result$mean
)

ggplot(val_assess, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Actual vs Predicted Sales (ARIMA)", x = "Actual Sales", y = "Predicted Sales") +
      theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
    )

val_mean <- val_assess %>% group_by(date) %>% summarize(value = mean(actual), mean_pred = mean(predicted))
setDT(val_mean)

val_pred <- val_mean[, .(date, mean_pred)]
colnames(val_pred)[2] <- "value"

ggplotly(ggplot(NULL, aes(x = date, y = value)) +
    geom_line(data = val_mean, aes(colour = "Actual")) +
    geom_line(data = val_pred, aes(colour = "Pred")) +
    scale_x_date(date_labels = "%b %d", date_breaks = "weeks") +
    labs(x = "Date", y = "Unit Sales (mean)", title = "Predictions Vs Actual (ARIMA)") +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 8, angle = 90)
    ))

val_pred[, value := value / 10]

ggplotly(ggplot(NULL, aes(x = date, y = value)) +
    geom_line(data = val_mean, aes(colour = "Actual")) +
    geom_line(data = val_pred, aes(colour = "Pred")) +
    scale_x_date(date_labels = "%b %d", date_breaks = "weeks") +
    labs(x = "Date", y = "Unit Sales (mean) | Pred Sales * 10", title = "Predictions Vs Actual (ARIMA)") +
    theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.line = element_line(colour = "gray2", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 8, angle = 90)
    ))

# Assessing model results and forecasts
mae <- mae(val_assess$actual, val_assess$predict)  # Mean Absolute Error
print(paste0("MAE = ", mae))

rmse <- rmse(val_assess$actual, val_assess$predict) # Root Mean Squared Error
print(paste0("RMSE = ", rmse))

# nwrmsle
val_assess[perishable == 0, weight := 1]
val_assess[perishable == 1, weight := 1.25]

log_errors <- (log(val_assess$predict + 1) - log(val_assess$actual + 1))^2
weighted_errors <- log_errors * val_assess$weight
nwrmsle <- sqrt(sum(weighted_errors) / sum(val_assess$weight))
print(paste0("NWRMSLE = ", nwrmsle))

test_xreg <- cbind(
  test$transactions,
  test$perishable,
  test$on_promotion
)

test_result <- forecast(fit, xreg = test_xreg, h = length(seq(min(test$date), max(test$date), "day")))
plot(test_result)

test$unit_sales <- test_result$mean
submission <- test[, .(id, unit_sales)]
fwrite(submission, "./arima_submission.csv")

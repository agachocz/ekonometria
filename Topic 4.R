# prognozowanie

# model może służyć:
# - wyjaśnieniu zależności (nie musi mieć wysokiego R2)
# - extrapolacji (nie jest tak ważne, jakie zmienne w jaki sposób wpływają)


model <- lm(sales ~ temp + rain + beach + other + price + weekend + I(temp*weekend) + exp_m + flavors_m, data)
summary(model)

prediction <- predict(model, newdata = data.frame(
  temp = 20, rain = 0, beach = 500, other = 80, price = 6,
  weekend = 0, exp_m = 4, flavors_m = 9), se.fit = TRUE
)

# czy na 90% będzie przynajmniej 2100 gałek lodów?
st <- (prediction$fit-2100)/prediction$se.fit
pt(st, 390, lower.tail = F)

# przedział ufności 99%

l = prediction$fit - qt(0.995, 390)*prediction$se.fit
u = prediction$fit + qt(0.995, 390)*prediction$se.fit



# podział na zbiór treningowy i testowy
set.seed(333)
train <- data %>% slice_sample(prop = 0.7)
test <- data %>% anti_join(train)

# estymujemy model na zbiorze treningowym
model <- lm(sales ~ temp + rain + beach + other + price + weekend + I(temp*weekend) + exp_m + flavors_m, data)
summary(model)

# tworzymy predykcję dla zbioru testowego
prediction <- predict(model, newdata = test, se.fit = TRUE)

# błędy prognozy
# ex ante
ex_ante <- prediction$se.fit

# ex post
ex_post <- test$sales - prediction$fit

MAE <- mean(abs(ex_post))
RMSE <- sqrt(mean(ex_post^2))
MAPE <- mean(abs((ex_post)/test$sales))

plot(test$sales, prediction$fit)
lines(test$sales, test$sales)

# ggplot
plot_data <- data.frame(Y = test$sales, Prediction = prediction$fit)

plot_data <- data.frame(
  id = 1:nrow(test),
  Prediction = prediction$fit,
  SE = prediction$se.fit
)

library(ggplot2)
plot_data %>% ggplot(aes(x = id, y = Prediction)) + 
  geom_bar(stat = "identity", fill = 'lightblue', col = 'black') +
  geom_errorbar(aes(ymin = Prediction-SE, ymax = Prediction+SE))


# cross-validation (leave-one-out)

e <- vector()

for(i in 1:nrow(data)){
  df <- data[-i,]
  model <- lm(sales ~ temp + rain + beach + other + price + weekend + I(temp*weekend) + exp_m + flavors_m, df)
  prediction <- predict(model, newdata = data[i,], se.fit = F)
  summ <- summary(model)
  e[i] <- data$sales[i] - prediction
  
  if(i == 1){
    SE <- summ$coefficients[,1]
  } else {
    SE <- rbind(SE, summ$coefficients[,1])
  }
}

hist(e)
MAE <- mean(abs(e))
RMSE <- sqrt(mean(e^2))
MAPE <- mean(abs((e)/data$sales))

shapiro.test(e)

# save standard errors and compute empirical p-value
SE <- as.data.frame(SE)
hist(SE$other)

quantile(SE$other, 0.025)
quantile(SE$other, 0.975)

hist(SE$weekend)
sd(SE$weekend)

# why are errors from this method so narrow? is it correct?

hist(SE$flavors_m)
hist(summ$residuals)

library(lmtest)


# Co chcę zrobić na tych zajęciach:
# przedstawić, jakie założenia modelu muszą być spełnione
# pokazać pakiety i testy
# próba napisania własnego testu


# model "wzorowy"

#data$sales <- 2000 + 10*data$temp - 40*data$rain - 0.1*data$beach + 0.05*data$other 
# - 7*data$price +
#  30*data$weekend + 5*data$temp*data$weekend +
#  10*(data$exp-4)^2 + 5*(data$flavors-7)^2 + rnorm(400, 0, 50)

plot(data$exp, data$sales)
plot(data$flavors, data$sales)

data$exp_m = (data$exp - 4)^2
data$flavors_m = (data$flavors - 7)^2

model <- lm(sales ~ temp*weekend + rain + beach + other + price + exp_m + flavors_m, data)
summary(model)

test_summary <- function(model){
  print(vif(model))
  r <- model$residuals
  print(shapiro.test(r))
  #ks.test(r, "pnorm", mean(r), sd(r))
  #dwtest(model) 
  print(bptest(model))
  print(gqtest(model))
  print(reset(model))
  print(runs.test(r))
  
  plot(model$fitted.values, model$residuals)
  plot(model$fitted.values, data$sales)

}

test_summary(model)




# teraz wychodzimy od metody Hellwiga

model <- lm(sales ~ weekend + beach + rain + exp + temp + flavors, data)
summary(model)
r <- model$residuals

library(car)
vif(model)
plot(model)
data[c(61, 74, 70),]


# DIAGNOSTYKA

# normalność reszt
r <- model$residuals
shapiro.test(r)
mean(r)
hist(r, breaks = 20)



# problemy ze składnikiem losowym - na czym to polega,
# że mamy jeden "poprawny" sposób, ale wiele sposobów na niepoprawny składnik losowy


# autokorelacja - pokazać z zastrzeżeniem, że tylko dla przypadku uszeregowanych danych?
dwtest(model)

# heteroskedastyczność
gqtest(model)
bptest(model)

plot(model$fitted.values, model$residuals)

# liniowość - na czym polega ten test
reset(model)

plot(data$sales, model$fitted.values)

# test liczby serii
install.packages("randtests")
library(randtests)

runs.test(r)



# To niekoniecznie
library(dplyr)
library(ggplot2)
data %>% mutate(weekend = as.factor(weekend)) %>%
  ggplot(aes(x = temp, y = sales, col = weekend)) + geom_point()


# test przerw strukturalnych - ćwiczenie dla studentów

model <- lm(sales ~ temp + rain + beach + other + price + weekend + I(temp*weekend) + exp_m + flavors_m, data)
summary(model)

Sc = sum(model$residuals^2)

data_weekend <- data[data$weekend == 1,]

model <- lm(sales ~ temp + rain + beach + other + price + weekend + I(temp*weekend) + exp_m + flavors_m, data_weekend)
S1 = sum(model$residuals^2)

data_weekday <- data[data$weekend == 0,]

model <- lm(sales ~ temp + rain + beach + other + price + weekend + I(temp*weekend) + exp_m + flavors_m, data_weekday)
S2 = sum(model$residuals^2)

stat = ((Sc - S1 - S2)/9)/((S1 + S2)/(nrow(data_weekend)+nrow(data_weekday)-18))

1-pf(stat, 9, (nrow(data_weekend)+nrow(data_weekday)-18))



# metody radzenia sobie z problemami

# odpowiedni dobór zmiennych - już omówiony
# interakcje między zmiennymi lub nieliniowe zależności - już omówione

# model startowy
model <- lm(sales ~ weekend + beach + rain + exp + temp + flavors, data)
summary(model)

library(corrplot)
corrplot(cor(data))

model <- lm(sales ~ weekend + beach + rain + I((exp-median(exp))^2) + temp + flavors + parking, data)
summary(model)

test_summary(model) # po dodaniu zmiennych i zmianie zależności nieliniowej już jest lepiej


# estymatory odporne - zmienia to tyle, że wskazania testów są bardziej poprawne
library(sandwich)

model <- lm(sales ~ weekend + beach + rain + exp + temp + flavors, data)
summary(model)

coeftest(model)
coeftest(model, vcovHC(model))


# regresja ważona - pomaga na heteroskedastyczność, ale może pogorszyć interpretację i inne aspekty
# ciężko znaleźć konkretne wartości wag, trochę błądzenie na ślepo
model <- lm(sales ~ weekend + beach + rain + exp + temp + flavors, data)
summary(model)

r <- model$residuals
data$lr <- r^2

mr <- lm(lr ~ weekend + beach + rain + exp + temp + flavors, data)
summary(mr)
w <- mr$fitted.values

model_w <- lm(sales ~ weekend + beach + rain + exp + temp + flavors, data, weights = 1/w)
summary(model_w)

test_summary(model_w)

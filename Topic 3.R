library(lmtest)

# porównamy dwa modele: z istotnymi zmiennymi i z metody Hellwiga

model <- lm(sales ~ weekend + beach + rain + I(exp-2)^2 + temp*weekend, data)
summary(model)

model <- lm(sales ~ weekend + beach, data)
summary(model)

# DIAGNOSTYKA

# normalność reszt
shapiro.test(model$residuals)
mean(model$residuals)
hist(model$residuals, breaks = 20)

ks.test(model$residuals, "pnorm")

# autokorelacja - czy to ma sens?
dwtest(model, order.by = data$sales) 

# heteroskedastyczność
bptest(model) 
plot(model$fitted.values, model$residuals)

# liniowość

reset(model)

data_weekend <- data[data$weekend == 1,]

model <- lm(sales ~ beach + rain + I(exp-2)^2 + temp, data_weekend)
summary(model)

# model "wzorowy"

#data$sales <- 2000 + 10*data$temp - 40*data$rain - 0.1*data$beach + 0.05*data$other - 7*data$price +
#  30*data$weekend + 5*data$temp*data$weekend +
#  10*(data$exp-4)^2 + 5*(data$flavors-7)^2 + rnorm(400, 0, 50)

data$exp_m = (data$exp - 4)^2
data$flavors_m = (data$flavors - 7)^2

model <- lm(sales ~ temp + rain + beach + other + price + weekend + I(temp*weekend) + exp_m + flavors_m, data)
summary(model)





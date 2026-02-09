# MODELE NIELINIOWE

# z jakimi przypadkami możemy mieć do czynienia
# logistyczna
# nieliniowa linearyzowana - funkcja produkcji
# nielinearyzowana - MNW


# regresja logistyczna

log(0.3/0.7)
exp(1)/(1 + exp(1))
x <- seq(-10, 10, 0.5)
plot(x)
plot(exp(x)/(1 + exp(x)))

titanic <- read.csv("train_titanic.csv", na.strings = "") %>%
  select(-PassengerId, -Name, -Cabin, -Ticket) %>% 
  mutate(across(c("Sex", "Embarked"), as.factor))


titanic <- titanic[complete.cases(titanic),]
summary(titanic)

n <- nrow(titanic)
id <- 1:n
train_id <- sample(id, round(0.8*n))
train <- titanic %>% slice(train_id)
test <- titanic %>% slice(-train_id)

model_logit <- glm(Survived ~ . - Pclass, family = binomial(link = "logit"), data = train)
summary(model_logit)

cor(titanic_train$Pclass, titanic_train$Fare)
model_logit$fitted.values



prediction <- predict(model_logit, test, type = 'response')
prediction <- data.frame(Real = test$Survived, Predicted = if_else(prediction > 0.5, 1, 0))

table <- data.frame(
  Real_0 = c(
    Pred_0 = nrow(prediction[which(prediction$Real == 0 & prediction$Predicted == 0),]),
    Pred_1 = nrow(prediction[which(prediction$Real == 0 & prediction$Predicted == 1),])
  ),
  Real_1 = c(
    Pred_0 = nrow(prediction[which(prediction$Real == 1 & prediction$Predicted == 0),]),
    Pred_1 = nrow(prediction[which(prediction$Real == 1 & prediction$Predicted == 1),])
  )
)

# sensitivity
Sens <- table[2, 2]/sum(table[,2])

# specificity
Spec <- table[1, 1]/sum(table[,1])




# funkcja produkcji - skąd dane?

non_countries <- c("European Union - 28 countries (2013-2020)", "European Union - 27 countries (from 2020)",
               "EU20 without the UK", "AT, BE, CZ, DE, DK, ES, FI, FR, IT, NL, SE and UK", "EU12 without UK",
               "AT, BE, CZ, DE, DK, EE, ES, FI, FR, HU, IT, LT, LU, LV, NL, RO, SE, SI, SK and UK", "European Union - 15 countries (1995-2004)",
               "Euro area - 19 countries  (from 2020)")
data <- read.csv("national accounts.csv") 

data <- data %>% filter(year == 2018) %>% select(geo_name, GO_CP, EMPE, II_CP) %>%
  filter(filter(across(c("GO_CP", "EMPE", "II_CP"), ~. > 0))) %>%
  filter(!(geo_name %in% non_countries))


unique(data$geo_name)


# GO - gross output
# EMPE - nr of employees
# COMP - compensation of employees
# GFCF - capital?

model <- lm(log(GO_CP) ~ log(II_CP) + log(EMPE), data)
summary(model)


# interpretacja parametrów - elastyczność, efekty skali

library(car)
linearHypothesis(model, c("log(EMPE) + log(II_CP) = 1"))


# krańcowa stopa substytucji?

# zadanie - model dynamiczny?


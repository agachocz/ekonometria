library(dplyr)
# generate simple data

set.seed(111)
data <- data.frame(
  family_size = rpois(50, 3)
) %>% mutate(oranges = family_size+1)

hist(data$family_size)
plot(x = data$family_size, y = data$oranges)
abline(a = 1, b = 1, col = "red")
cor(data$family_size, data$oranges)

# ściśle liniowa zależność, możemy precyzyjnie wyliczyć ze wzoru

# teraz wprowadzam trochę losowości
# 0 - nie lubi pomarańczy z prawdopodobieństwem 30%
# 1 - chce 1 pomarańcze z prawdopodobieństwem 50%
# 2 - chce 2 pomarańcze z prawdopodobieństwem 20%

get_preference = function(family_size){
  sapply(family_size, function(x) {
    sum(sample(c(0, 1, 2), size = x, replace = T, prob = c(0.3, 0.5, 0.2)))
    }
    )
}

set.seed(123)
data <- data %>% mutate(oranges = get_preference(family_size+1))

plot(x = data$family_size, y = data$oranges)
abline(a = 1, b = 1, col = "red")

# suma kwadratów reszt
# y = ax + b + e
# sum(e^2) = sum((y - ax - b)^2)

data$y_fit = data$family_size + 1
data$e = data$oranges - data$y_fit

sum(data$e^2)

# czy mogę dopasować lepiej?

model <- lm(oranges ~ family_size, data = data)
summary(model)

sum(model$residuals^2) # niższe

plot(data$family_size, data$oranges)
abline(a = model$coefficients[1], b = model$coefficients[2], col = "red")


# dodanie więcej zmiennych

# wyprzedaż:
# wersja I (liniowa): jeśli jest wyprzedaż, ludzie biorą jedną pomarańczę więcej
# wersja II: interakcyjna - więcej pomarańczy na każdego kupującego

set.seed(456)
data$sale = sample(c(0, 1), 50, replace = T, prob = c(0.7, 0.3))
data$oranges = get_preference(data$family_size+1) + data$sale

model <- lm(oranges ~ family_size + sale, data = data)
summary(model)

model <- lm(oranges ~ family_size + family_size*sale, data = data)
summary(model)
  
data$oranges = get_preference(data$family_size+1)*(1 + data$sale)

model <- lm(oranges ~ family_size + sale, data = data)
summary(model)

sum(model$residuals^2)

model <- lm(oranges ~ family_size*sale, data = data)
summary(model)

sum(model$residuals^2) # niższy błąd

plot(data$family_size, data$oranges)


# dodać zmienną, która nie ma wpływu na y

set.seed(789)
data$temp = round(rnorm(50, 15, 10))
hist(data$temp)

model <- lm(oranges ~ family_size*sale + temp, data = data)
summary(model)

model <- lm(oranges ~ family_size*sale, data = data)
summary(model) # usunięcie nieistotnej zmiennej poprawia skorygowany R2, ale obniża zwykły

# zmienna może wyjść istotna w 5% przypadków
n_sig <- 0

for(i in 1:1000){
  data$temp = round(rnorm(50, 15, 10))
  model <- lm(oranges ~ family_size*sale + temp, data = data)
  summ <- summary(model)
  p_val <- summ$coefficients["temp",4]
  if(p_val < 0.05) n_sig <- n_sig + 1
}

n_sig
# zgadza się


# ćwiczenie: regresja pozorna

# powtarzaj wiele razy symulację (1000)
# generuj zmienne x i y, które są zależne od swojej przeszłości
# estymuj model i sprawdzaj, w ilu przypdkach p-value jest mniejsze od 5%

n_sig <- 0

for(i in 1:1000){
  
  x = vector()
  y = vector()
  x[1] = 0
  y[1] = 0
  
  for(j in 2:100){
    x[j] = x[j-1]*(-0.9) + rnorm(1)
    y[j] = y[j-1]*(-0.9) + rnorm(1)
  }
  
  model <- lm(y ~ x)
  summ <- summary(model)
  p_val <- summ$coefficients["x",4]
  if(p_val < 0.05) n_sig <- n_sig + 1
}

n_sig


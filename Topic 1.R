library(dplyr)
# generate simple data

data <- data.frame(
  family_size = rpois(50, 3)
) %>% mutate(oranges = family_size+1)

hist(data$family_size)
plot(x = data$family_size, y = data$oranges)

# ściśle liniowa zależność, możemy precyzyjnie wyliczyć ze wzoru

# teraz wprowadzam trochę losowości
get_preference = function(family_size){
  sapply(family_size, function(x) {
    sum(sample(c(0, 1, 2), size = x, replace = T, prob = c(0.3, 0.5, 0.2)))
    }
    )
}

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


# dodanie więcej zmiennych

# promocja - wersja I: liniowa, dodaję więcej pomarańczy
# wersja II: interakcyjna - więcej pomarańczy na każdego kupującego

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

# EFEKTY NIEPRZEWIDYWALNE, USTAWIĆ STABILNE ZIARNO

# dodać zmienną, która nie ma wpływu na y

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


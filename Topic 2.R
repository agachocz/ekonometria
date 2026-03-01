# generowanie danych

# zmienne na temat dni
# podstawowe: weekend, temperatura, wiatr
# pochodne: deszcz - większe prawdopodobieństwo, gdy temperatura jest niższa

# zmienne na temat lodziarni
# podstawowe: jak blisko do plaży, od jak dawna istnieje, ile smaków lodów, czy ma parking
# pochodne: jak blisko do innych, cena za gałkę

# sprzedaż: większa w weekend (interakcja z pogodą i bliskością do plaży?),
# większa przy temperaturze, niższa przy deszczu, niezleżna od wiatru
# kwadratowa zależność od wieku i od gałek lodów, niezależna od parkingu
# im bliżej plaży, tym lepiej, ale im bliżej innych, tym gorzej

# w następnym zadaniu dodać jakieś outliery: dni świąteczne, festiwal, sławna lodziarnia

n_days <- 20

set.seed(111)
days <- data.frame(
  day_nr = 1:n_days,
  weekend = sample(c(0, 1), n_days, replace = T, prob = c(5, 2)/7), # czy dzień weekendowy?
  temp = rnorm(n_days, 25, 3), # temperatura
  wind = rlnorm(n_days, 1.5, 1) # prędkość wiatru
)

# deszcz: większe prawdopodobieństwo, gdy temperatura jest niższa
days$rain = sapply(days$temp, function(x) sample(c(0, 1), 1, prob = c(50+x, 50-x)/100)*rpois(1, 5))

set.seed(222)
n_shops = 20

shops = data.frame(
  shop = 1:n_shops,
  exp = rpois(n_shops, 5), # od ilu lat istnieje lodziarnia
  beach = rlnorm(n_shops, 2.5, 1.5)*10, # odległość od plaży
  flavors = rpois(n_shops, 3)+4, # liczba oferowanych smaków
  parking = sample(c(0, 1), n_shops, replace = T) # czy posiada parking
)

# lodziarnie przy plaży nie mają parkingów
shops$parking = ifelse(shops$beach < 200, 0, shops$parking)

shops$price = ifelse(shops$beach < 200, 7, 6) + sample(c(-1, 0, 1), n_shops, replace = T) # cena gałki lodów
shops$other = sapply(shops$beach, function(x) rlnorm(1, log(x/50), 0.5)*10) # odległość od innej lodziarni

data <- merge(days, shops, by = NULL)

# sprzedaż: zależna od temperatury, deszczu, odległości od plaży i konkurencji, ceny, weekendu, renomy i liczby smaków
data$sales <- 2000 + 10*data$temp - 40*data$rain - 0.1*data$beach + 0.05*data$other - 7*data$price +
  30*data$weekend + 5*data$temp*data$weekend +
  10*(data$exp-4)^2 + 5*(data$flavors-7)^2 + rnorm(400, 0, 50)

data <- data[,-c(1, 6)] # usuwam niepotrzebne zmienne




hist(data$beach)  
hist(data$sales)  

pairs(sales ~ temp + rain + beach + other + price + weekend + exp + flavors, data)



# regresja z wieloma zmiennymi

model <- lm(sales ~ temp + rain + beach + other + price + weekend + exp + flavors + parking + wind, data)
model <- lm(sales ~ ., data)
summary(model)

# Tu omówić interpretację zmiennych

cor(data$price, data$beach)

cor(data)

install.packages("corrplot")
library(corrplot)

corrplot(cor(data))

# koincydencja

sign(model$coefficients)
sign(cor(data))[,11]

# nie występuje dla "other" ze względu na wysoką korelację z "beach" - w modelu lepiej mieć tylko jedną z nich

library(car)
vif(model)

model <- lm(sales ~ temp + rain + beach + price + weekend + exp + flavors + parking + wind, data)
summary(model)

# poprawa - nieistotne są zmienne, które nie brały udziału w tworzeniu sales
# odrzucam nieistotne zmienne

model <- lm(sales ~ temp + rain + beach + weekend + flavors + parking, data)
summary(model)

model <- lm(sales ~ temp + rain + beach + weekend + flavors, data)
summary(model)

# zwiększone skorygowane R2 - więc model zyskuje na pomniejszeniu

# pokazuję, że jeśli usunę ważną zmienną, oba wskaźniki spadają
model <- lm(sales ~ rain + beach + weekend + exp + flavors, data)
summary(model)

plot(model)

# metoda Hellwiga

expand.grid(c(1:3), c(1:3), c(1:3))

comb <- expand.grid(rep(list(c(T,F)), 10))
2^10-1

k <- c(1:10)[unlist(comb[100,])]

m <- 10


cor_matrix <- cor(data)
cor_matrix

Ry <- cor_matrix[-11,11]
Rx <- cor_matrix[-11,-11]


comb <- expand.grid(rep(list(c(T,F)), m))
comb

Max <- 0
K_max <- NULL
#duża pętla po wierszach comb
for(i in 1:(nrow(comb)-1)) {
  k <- c(1:m)[unlist(comb[i,])]
  #print(k)
  wynik <- 0
  
  # pętla po zmiennych w kombinacji
  for(n in k){
    #print(wynik)
    wynik <- wynik + Ry[n]^2/sum(abs(Rx[n,k]))
    #print(wynik)
  }
  if(wynik>Max)
  {
    Max <- wynik
    K_max <- k
  }
}

colnames(data)[K_max]

# Model z Hellwiga może mieć mniejsze R^2

model <- lm(sales ~ weekend + beach + rain + exp + temp + flavors, data)
summary(model)

vif(model)

plot(model)

data[c(119, 254, 285),]

summary(data)

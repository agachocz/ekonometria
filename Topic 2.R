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

n_days <- 50

set.seed(123)
days <- data.frame(
  day_nr = 1:n_days,
  weekend = sample(c(0, 1), n_days, replace = T, prob = c(5, 2)/7),
  temp = rnorm(n_days, 25, 3),
  wind = rlnorm(n_days, 1.5, 1)
)

# deszcz: większe prawdopodobieństwo, gdy temperatura jest niższa
days$rain = sapply(days$temp, function(x) sample(c(0, 1), 1, prob = c(50+x, 50-x)/100)*rpois(1, 5))

hist(days$wind)
hist(days$temp)
hist(days$rain)

summary(days)

set.seed(456)
n_shops = 20

shops = data.frame(
  shop = 1:n_shops,
  exp = rpois(n_shops, 5),
  beach = rlnorm(n_shops, 2.5, 1.5)*10,
  flavors = rpois(n_shops, 3)+4,
  parking = sample(c(0, 1), n_shops, replace = T)
)

shops$parking = ifelse(shops$beach < 200, 0, shops$parking)

shops$price = ifelse(shops$beach < 200, 7, 6) + sample(c(-1, 0, 1), n_shops, replace = T)
shops$other = sapply(shops$beach, function(x) rlnorm(1, log(x/50), 0.5)*10)

data <- merge(days, shops, by = NULL)

data$sales <- 1000 + 8*data$temp - 30*data$rain +(2000-data$beach)/2 + 3*data$other/2 - 5*data$price +
  50*data$weekend + 10*data$temp*data$weekend +
  8*(data$exp-4)^2 + 5*(data$flavors-7)^2 + rnorm(1000, 0, 30)

hist(data$beach)  
hist(data$sales)  

pairs(sales ~ temp + rain + beach + other + price + weekend + exp + flavors, data)

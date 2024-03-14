# Wczytanie danych

dane <- read.csv("E:/Studia/Statistical learning/World Happiness Report.csv")
#dane <- read.csv("C:/Users/X/Desktop/SL/2022.csv")
View(dane)
summary(dane)
dim(dane)
summary(dane[, "Happiness.Score"])

#########################################################################################################
# Czyszczenie danych

# usuwamy panstwa
dane_bez_country <- dane[, -1]

# usuwamy ranking
dane_bez_country <- dane_bez_country[, -1]

# usuwamy zmienna Dystopia
dane_bez_country <- dane_bez_country[-8]

# usuwamy ostatnią pustą kolumnę
dane_bez_country <- dane_bez_country[-9]

# usuwamy NA's
dane_bez_country <- na.omit(dane_bez_country)
View(dane_bez_country)

###################################################################################
#Sprawdzamy jak zadziala model drzew regresyjnych

set.seed(21)

library(tree)
train <- sample(1:nrow(dane), 100)
tree.happ <- tree(Happiness.Score ~ ., data = dane_bez_country, subset = train)
summary(tree.happ)
# model zbudowany w oparciu o tylko 4 zmiennych

# narysujmy otrzymane drzewo
plot(tree.happ)
text(tree.happ, pretty = 0)

# sprawdźmy, czy przycinanie drzewa poprawi jego jakosc predykcyjna
cv.happ <- cv.tree(tree.happ)
plot(cv.happ$size, cv.happ$dev, type = "b", xlab = "Size of a tree", ylab = "Deviation")
# cale rozwazane drzewo jest wybierane walidacja krzyzowa

# minimalna wartosc
cv.happ$dev
cv.happ$size[which.min(cv.happ$dev)]
# najlepszy model to ten z 6 wezlami

best.tree <- prune.tree(tree.happ, best = 6)
summary(best.tree)
# najlepszy model wybrany kroswalidacją ma 3 zmiennych

# sprawdzmy blad testowy dla wybranego przez CV drzewa (z 3 zmiennnymi)
yhat <- predict(best.tree, newdata = dane_bez_country[-train, ])
happ.test <- dane_bez_country[-train, "Happiness.Score"]
plot(yhat, happ.test)
abline(0, 1)
# MSE na zbiorze testowym:
mean((yhat - happ.test)^2)
# Model przewiduje średnio z dokładnością 0.41.

# sprawdzmy blad testowy dla wybranego drzewa na poczatku (z 4 zmiennymi)
summary(tree.happ)
yhat <- predict(tree.happ, newdata = dane_bez_country[-train, ])
happ.test <- dane_bez_country[-train, "Happiness.Score"]
plot(yhat, happ.test)
abline(0, 1)
# MSE na zbiorze testowym:
mean((yhat - happ.test)^2)
# Model przewiduje srednio z dokladnoscia 0.401

##############################################################

#Sprawdzamy jak zadziala model drzew klasyfikacyjnych

#Musimy zdyskretyzowac zmienna Happiness score
summary(dane_bez_country$Happiness.Score)

# 1st Qu. = 4.505 wiec wartosci ponizej 4.5 uznamy za niska wartosc szczescia
# 3rd Qu. = 6.101 wiec wartosci powyzej 6.1 uznamy za wysoka wartosc szczescia
High <- factor(ifelse(dane_bez_country$Happiness.Score >= 6.1, "High",ifelse(
  dane_bez_country$Happiness.Score <= 6.1 & dane_bez_country$Happiness.Score >= 4.5, "Medium", "Low")))

dane <- data.frame(dane_bez_country,High)

train <- sample(1:nrow(dane), 100)
dane.test <- dane[-train, ]
High.test <- High[-train]

tree.dane <- tree(High ~ . - Happiness.Score, dane, subset = train)
plot(tree.dane)
text(tree.dane, pretty=0)

tree.pred <- predict(tree.dane, dane.test, type = "class")
table(tree.pred, High.test)
(12 + 5 + 17) / 51
# 0.67

#Sprawdzamy, czy przyciecie drzewa bedzie mialo sens
cv.dane <- cv.tree(tree.dane, FUN = prune.misclass)
cv.dane
#Mozemy sprobowac dla 4 albo 7? 2 to za malo

prune.dane <- prune.misclass(tree.dane, best = 4)
plot(prune.dane)
text(prune.dane, pretty = 0)

tree.pred <- predict(prune.dane, dane.test, type = "class")
table(tree.pred, High.test)
(14 + 10 + 13) / 51 #0.725 dla 4
(12 + 5 + 17) / 46 #0.67 dla 7

############################################################################


# Wyświetlamy wykresy zależności miedzy zmiennymi
library(ggplot2)
zmienne <- colnames(dane_bez_country)
dim(dane_bez_country)
par(mfrow = c(3,2))

for (zmienna in zmienne) {
  print(ggplot(dane_bez_country, aes_string(x = zmienna, y = "Happiness.Score")) +
          geom_point() +
          labs(title = paste("Wykres zależności Happiness.Score od", zmienna),
               x = zmienna,
               y = "Happiness.Score") +
          theme_minimal())
}

par(mfrow = c(1,1))

#########################################################################################################
# Zobaczmy najlepsze podzbiory

library(leaps)

regfit.full <- regsubsets(Happiness.Score~., data = dane_bez_country)
summary(regfit.full)

reg.summary <- summary(regfit.full)
reg.summary$rsq
reg.summary$bic
reg.summary$adjr2

# Znajdujemy i zaznaczamy min (lub max) dla RSS, Adj R^2, Cp, BIC:
par(mfrow = c(2, 2))

plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
which.min(reg.summary$rss)
points(6, reg.summary$rss[6], col = "red", cex = 2, pch = 20)

plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
which.max(reg.summary$adjr2)
points(6, reg.summary$adjr2[6], col = "red", cex = 2, pch = 20)

plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(6, reg.summary$cp[6], col = "red", cex = 2, pch = 20)

plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
which.min(reg.summary$bic)
points(4, reg.summary$bic[4], col = "red", cex = 2, pch = 20)

# znajdźmy zmienne wybrane do modelu z najniższym BIC
par(mfrow = c(1, 1))
plot(regfit.full , scale = "bic")

# Jest to model z 5 zmiennymi, jednak modele z 6, 4 czy 7 zmiennymi dają bardzo zbliżony efekt
coef(regfit.full, 5)

# Tworzymy zbior treningowy i zbior testowy
set.seed(1)
train <- sample(1: nrow(dane_bez_country), nrow(dane_bez_country) / 2)
test <- (-train)
y.test <- dane_bez_country$Happiness.Score[test]

# na zbiorze treningowym szukamy najlepszych podzbiorow
regfit.best <- regsubsets(Happiness.Score ~ ., data = dane_bez_country[train, ])

# Najpierw tworzymy macierz modelu/macierz planu na danych testowych:
test.mat <- model.matrix(Happiness.Score ~ ., data = dane_bez_country[test, ])

dim(dane_bez_country)
val.errors <- rep(NA, 7)
for(i in 1:7){
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[ , names(coefi)] %*% coefi
  val.errors[i] <- mean((dane_bez_country$Happiness.Score[test] - pred)^2)
}

# Szukamy najlepszego modelu:
val.errors
which.min(val.errors)
coef(regfit.best, 6)
# wychodzi niewiele mniejszy blad w porownaniu do 7 zmiennych

# Porownanie miar
c(reg.summary$rsq[7], reg.summary$rsq[6])
c(reg.summary$bic[7], reg.summary$bic[6])
c(reg.summary$adjr2[7], reg.summary$adjr2[6])

###################################################################################################
# regresja grzbietowa

x<-model.matrix(Happiness.Score~., dane_bez_country)
y<-dane_bez_country$Happiness.Score

library(glmnet)

# siatka przeszukiwań parametru lambda
grid <- 10^seq(10, -2, length = 500)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid) # alpha = 0 - regresja grzbietowa
dim(coef(ridge.mod))

# szukamy najlepszej lambdy walidacją krzyżową
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test, ])
mean((ridge.pred - y.test)^2) # MSE
(rsquared <- 1 - mean((ridge.pred - y.test)^2) / var(y.test)) # wspolczynnik r^2

out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:8, ]

#########################################################################################################
# Chcac wybrac pomiedzy modelem regresji liniowej (oraz ilościa zmiennych branych do modelu), a 
# modelem regresji grzbietowej przeprowadzimy walidacje krzyzowa

# walidacja krzyzowa k = 10

# Funkcja potrzebna do predykcji 
predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[ , xvars] %*% coefi
}


k <- 10
n <- nrow(dane_bez_country)
set.seed(1)
folds <- sample(rep(1:k, length = n))

dim(dane_bez_country)

# Tworzymy macierz, w której będziemy przechowywali wyniki:
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))
print(cv.errors)

# Uruchamiamy CV:
for(j in 1:k){
  best.fit <- regsubsets(Happiness.Score ~ ., data = dane_bez_country[folds != j, ], nvmax = 7)
  for(i in 1:7){ # liczymy dla 6 modeli
    pred <- predict(best.fit, dane_bez_country[folds == j, ], id = i)
    cv.errors[j, i] <- mean((dane_bez_country$Happiness.Score[folds == j] - pred)^2)
  }
}

# Liczymy średnie z kolumn:
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors

par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b")
which.min(mean.cv.errors)
mean.cv.errors[6]

reg.best <- regsubsets(Happiness.Score ~ ., data = dane_bez_country, nvmax = 6)

# Współczynniki dla modelu z 6 zmiennymi
coef(reg.best, 6)

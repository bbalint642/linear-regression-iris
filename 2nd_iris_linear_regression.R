# libek ----
library(datasets)
library(psych)

# adatok beolvasása ----
x = iris

# adathalmaz áttekintése ----
str(x)
summary(x)

# valószínűségi mintavétel ----
nonprobabilitic = iris[1:50,]
summary(nonprobabilitic)

# adatok előkészítése a további munkához ----

set.seed(1234)

training = nonprobabilitic[1:35, -5]
testing = nonprobabilitic[36:50, -5]
summary(training)
summary(testing)


# lineáris regressziós modellek létrehozása ----


model = lm(Sepal.Width~., data = training)
summary(model)

# a csészelevél hossza bizonyult a legfontosabbnak


# modell plottolása

p = plot(training$Sepal.Length, training$Sepal.Width)
abline(model, col="red")
cor(iris[1:49,1:4])
pairs.panels(iris[1:49,1:4])



# modell létrehozása a szignifikáns változóhoz
model = lm(Sepal.Width~Sepal.Length, data=training)
summary(model)

# újra plottolás
abline(model, col = "green")

# előrejelzés
summary(training)
(prediction = predict(model, testing[1]))
cbind(actual=testing[,2], prediction)


# előrejelzés adott sorra

check = testing[10,]
check = data.frame(Sepal.Length=5.1, Petal.Length= 1.9, Petal.Width=0.4)
(predicted_value=predict(model, check[1]))


# hibaszázalék és pontosság meghatározása
difference = abs(testing[,2]-prediction)
error_percentage = difference/testing[,2]*100
accuracy_percentage=100-error_percentage
mean(accuracy_percentage)

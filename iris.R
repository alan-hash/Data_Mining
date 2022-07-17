library(datasets)
data(iris)
summary(iris)
names(iris) <- tolower(names(iris))
library(dplyr)

#filtro
virginica <- filter(iris, species == "virginica")
head(virginica) 
sepalLength6 <- filter(iris, species == "virginica", sepal.length > 6)
tail(sepalLength6)

# seleccion
selected <- select(iris, sepal.length, sepal.width, petal.length)

selected2 <- select(iris, sepal.length:petal.length)
head(selected, 3)

# Mutate
newCol <- mutate(iris, greater.half = sepal.width > 0.5 * sepal.length)
tail(newCol)

#visualizacion 
plot(iris)

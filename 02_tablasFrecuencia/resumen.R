## Generamos una matriz con datos ficticios
edad <- round(runif(10, 18, 25))
sexo <- sample(c(rep(1, 7), rep(2, 3)))
sexo <- factor(sexo,
               levels = 1:2,
               labels = c("hombre",
                          "mujer"))

# ansiedad <- sample(c(rep(1,3), rep(2,5), rep(3,2)))
# ansiedad <- factor(ansiedad,
#                levels = 1:3,
#                labels = c("bajo",
#                           "medio",
#                           "alto"))

npalabras <- round(rnorm(10, 15, sd = 2))

df <- data.frame(edad, sexo, npalabras)

## Escribimos la base en un archivo
write.csv2(df, "./data/npalabras.csv", row.names = FALSE)

## Ejemplo memoria con 1000 casos
edad <- round(runif(1000, 15, 80))
sexo <- sample(c(rep(1, 70), rep(2, 30)))
sexo <- factor(sexo,
               levels = 1:2,
               labels = c("hombre",
                          "mujer"))

npalabras <- round(rnorm(1000, 18, sd = 2))
df <- data.frame(edad, sexo, npalabras)

## Escribimos la base en un archivo
write.csv2(df, "./data/npalabras1000.csv", row.names = FALSE)



## n = 5
## Ejemplo memoria con 5 casos
edad <- round(runif(5, 18, 25))
sexo <- sample(c(rep(1, 3), rep(2, 2)))
sexo <- factor(sexo,
               levels = 1:2,
               labels = c("hombre",
                          "mujer"))

npalabras <- round(rnorm(5, 18, sd = 2))
df <- data.frame(edad, sexo, npalabras)

## Escribimos la base en un archivo
write.csv2(df, "./data/npalabras5.csv", row.names = FALSE)




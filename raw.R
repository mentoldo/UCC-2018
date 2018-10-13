library(dplyr)
library(ggplot2)

## Descargamos la base de la EPH
if(!file.exists("./data/usu_individual_t217.txt")){
    fileURL <- "https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_2_Trim_2017_txt.zip"
    download.file(fileURL, destfile = "./data/EPH_usu_2_Trim_2017_txt.zip")

    ## Descomprimimos
    unzip("data/EPH_usu_2_Trim_2017_txt.zip", exdir = "./data/")
    
    ## Borramos el zip
    file.remove("./data/EPH_usu_2_Trim_2017_txt.zip")
}
        
## Cargamos la base
base <- read.csv2("./data/usu_individual_t217.txt")


## Seleccionamos los casos de Córdoba
baseCordoba <- base %>% 
    filter(AGLOMERADO == 13)

nrow(baseCordoba)

## Seleccionamos los mayores a 18 años
## Realizamos una tabla de distribución de frecuencias para la variable CH12
## Cuál es el nivel más alto que cursa o que cursó


freq <- base %>%
    filter(CH12 %in% 1:9, CH06 >= 18) %>% 
    group_by(CH12) %>% 
    summarise(freq = n()) %>% 
    mutate(CH12 = factor(CH12,
                         levels = 1:9,
                         labels = c("Jardín/Preescolar",
                                    "Primario",
                                    "EGB",
                                    "Secundario",
                                    "Polimodal",
                                    "Terciario",
                                    "Universitario",
                                    "Posgrado Universitario",
                                    "Educación Especial")))


## Realizamos un gráfico de barras
freq %>% ggplot(aes(x = CH12, y = freq)) + 
    geom_col()


## Seleccionamos los casos de educación especial
especial <- base %>% 
    filter(CH12 == 9, CH06 >= 18)

unique(especial$CH06)

## Realizamos un histograma para la edad
especial %>% 
    ggplot(aes(CH06)) +
    geom_histogram(binwidth = 5)


## Boxplot para total de ingresos individuales
especial %>% 
    ggplot(aes(x = as.factor(CH04), y = P47T, fill = as.factor(CH04))) +
    geom_boxplot()

## Categoría ocupacional
especial %>% 
    group_by(ESTADO) %>% 
    summarise(freq = n())


especial %>% 
    group_by(CAT_INAC) %>% 
    summarise(freq = n())


base %>%
    filter(CH12 %in% 1:9) %>% 
    group_by(CH12) %>% 
    summarise(freq = n()) 


## Recortamos la base de CEPRAM
cepram <- read.csv2("./data/cuestionario.csv")

# cepram <- cepram[sample(1:nrow(cepram), 500), !grepl("PROFE|CURSO", colnames(cepram))]
# 
# write.csv2(cepram, "./data/cuestionario.csv", row.names = FALSE)

names(freq1) <- c("nivel_ed", "freq", "freq.rel", "cum.freq", "rel.cum.freq", "pos")

pos <- sum(freq1$freq) - (cumsum(freq1$freq) - freq1$freq / 2)
label <- freq1$freq


freq1 %>% 
    ggplot(aes(x = 1, y = freq, fill = nivel_ed)) +
    geom_col() +
    theme(axis.ticks=element_blank(),
          axis.title=element_blank(),
          axis.text.y=element_blank(),
          # panel.grid = element_blank(),
          panel.grid.minor.y = element_blank(),
          # line = element_blank(),
          axis.text.x=element_text(color='black',size=12)) +
    coord_polar(theta = "y") +
    scale_y_continuous(breaks = pos, labels = label)


## Tablas de contingencia
tipoEnsennanza <- data.frame(id = 1 : 100,
                             tipo = sample(c("Democrático", "Autoritario"), 100, replace = TRUE),
                             rendimiento = sample(c("Alto", "Medio", "Bajo"), 100, replace = TRUE))

write.csv2(tipoEnsennanza, file = "./data/tipoEnsennanza.csv", row.names = FALSE)

tipoEnsennanza$rendimiento <- factor(tipoEnsennanza$rendimiento, labels = c("Bajo", "Medio", "Alto"), ordered = TRUE)




mat <- table(tipoEnsennanza$tipo, tipoEnsennanza$rendimiento)
mat <- addmargins(mat)


## Tablas de contingencia 2x2
tipoEnsennanza <- data.frame(id = 1 : 100,
                             tipo = sample(c("Democrático", "Autoritario"), 100, replace = TRUE),
                             rendimiento = sample(c("Alto", "Bajo"), 100, replace = TRUE))

matriz <- matrix(nrow = 3, ncol = 3)
matriz[3, 4] <- 

matriz[1, ] <- c(5, 35, 50, 90)
matriz[2, ] <- c(260, 40, 10, 310)


### EPH
eph <- read.csv2("../data/usu_individual_t117.txt")

quantile(eph$PP04B3_ANO, na.rm = TRUE)

df <- eph %>% 
    filter(!(PP04B3_ANO %in% c(0, 99)), !is.na(PP04B3_ANO), !is.na(PP08D1), PP08D1 != -9)

df
range(df$PP08D1, na.rm = T)

plot(df$PP04B3_ANO, df$PP08D1)


df <- eph %>% 
    filter(NIVEL_ED %in% 1:6, !is.na(PP08D1), PP08D1 != -9)


plot(df$NIVEL_ED, df$PP08D1)



quantile(df$PP11G_ANO)
sum(!is.na(df$PP04B3_ANO))
hist(df$PP04B3_ANO)

df <- eph %>% 
    filter(!PP04B3_MES %in% c(0, 99), !is.na(PP04B3_MES))

quantile(df$PP04B3_MES)
sum(!is.na(df$PP04B3_ANO))
hist(df$PP04B3_MES)


### Correlación, experimento de memoria

edad <- round(rnorm(100, 25, 5))
npalabras <- round(-0.2 * edad + 25+ rnorm(100, sd = 0.5))

df <- data.frame(edad = edad, npalabras = npalabras)

write.csv2(df, "./data/npalabras_edad.csv", row.names = F)

plot(edad, npalabras)

### r de Pearson

x <- rnorm(100, 20, 4)
y <- x + rnorm(100, 0.5)

df <- data.frame(x = x, y = y)
write.csv2(df, "./data/rdfuerte.csv", row.names = FALSE)

plot(x, y)

cor(x, y)

## Directa moderada
x <- rnorm(100, 20, 4)
y <- x + rnorm(100, 0.5)

df <- data.frame(x = x, y = y)
write.csv2(df, "./data/rdfuerte.csv", row.names = FALSE)

plot(x, y)

cor(x, y)



## inversa fuerte

x <- rnorm(100, 20, 4)
y <- rnorm(100, 20, 4)

df <- data.frame(x = x, y = y)
write.csv2(df, "./data/rnula.csv", row.names = FALSE)

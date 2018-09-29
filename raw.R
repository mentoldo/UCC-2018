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

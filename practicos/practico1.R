usuindividual <- read.csv2("../data/usu_individual_t117.txt")

cuestionario <- read.csv2("../data/cuestionario.csv")


## Seleccionamos
head(dmy(usuindividual$CH05))

library(dplyr)
library(lubridate)
usuindividual %>% 
    filter(CH06 %in% 20:30, CH14 == 98) %>% 
    group_by(CH12) %>% 
    summarise(n = n())


usuindividual %>% 
    filter(CH06 %in% 20:30, NIVEL_ED == 7) %>% 
    group_by(ESTADO) %>% 
    summarise(n = n())

## Personas sin nivel de instrucción
## Posadas
usuindividual %>% 
    filter(AGLOMERADO == 13, CH06 > 18, NIVEL_ED == 7) %>% 
    select(CH06, PONDERA) %>% 
    summarise(media = mean(CH06))

## Cordoba
usuindividual %>% 
    filter(AGLOMERADO == 07, CH06 > 18, NIVEL_ED == 7) %>% 
    select(CH06, PONDERA) %>% 
    summarise(media = mean(CH06))

usuindividual %>% 
    filter(AGLOMERADO == 18, CH06 > 18, NIVEL_ED == 7) %>% 
    select(CH06, PONDERA) %>% 
    summarise(media = mean(CH06))

edad <- usuindividual %>% 
            filter(AGLOMERADO == 07, CH06 > 18, NIVEL_ED == 7) %>% 
            select(CH06)
            

table(cut(edad, breaks = seq(30, 85, by = 10)))

## Distribución de frecuencias
tabla <- usuindividual %>% 
            select(CH12) %>% 
            filter(complete.cases(.), CH12 %in% 1:9) %>% 
            mutate(Nivel = factor(CH12, levels = 1:9, labels = c("Preescolar", "Primario", "EGB", 
                                                         "Secundario", "Polimodal", "Terciario",
                                                         "Universitario", "PosgradoUniversitario", "Educación Especial"))) %>% 
            group_by(Nivel) %>% 
            summarise(f = n()) %>% 
            mutate(Fa = cumsum(f), Fr = prop.table(f), Fra=cumsum(prop.table(f)))
print(kable(tabla))            


usuindividual %>% 
    select(sueldo = PP08D1) %>% 
    filter(complete.cases(.), sueldo > 0) %>% 
    mutate(cat = cut(sueldo, breaks = quantile(sueldo,
                                               probs = seq(0, 1, length.out = 10)
                                               ))) %>% 
    group_by(cat) %>% 
    summarise(n())
    
cut(usuindividual$PP08D1, breaks = quantile(usuindividual$PP08D1, probs = seq(0, 1, length.out = 10), na.rm = TRUE))
cut(usuindividual$PP08D1, breaks = quantile(usuindividual$PP08D1, probs = seq(0, 1, length.out = 10), na.rm = TRUE))
    
breaks = quantile(usuindividual$PP08D1, probs = seq(0, 1, length.out = 10), na.rm = TRUE)    
## Histograma para la edad del CEPRAM
library(ggplot2)
g <- cuestionario %>%
    select(Años = AÑOS, grupos = GRUPOTOMA) %>%
    filter(complete.cases(.), Años %in% 1:17) %>%
    mutate(Años = as.numeric(Años)) %>% 
    ggplot(aes(Años, y = ..count..))
g + geom_histogram(fill = "blue", col = "black", center = 1, binwidth = 1) +
    ylab("Frecuencia Absoluta")

## Boxplot edad particionado por sexo
g <- cuestionario %>% 
    select(Edad = EDAD, Sexo = SEXO) %>% 
    filter(complete.cases(.), Edad %in% 40:90, Sexo %in% 1:2) %>% 
    mutate(Sexo = factor(Sexo, levels = 1:2, labels = c("Hombres", "Mujeres"))) %>% 
    ggplot(aes(x = Sexo, y = Edad, fill = Sexo))
g + geom_boxplot()


cuestionario
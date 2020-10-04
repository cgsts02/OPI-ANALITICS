
library(tidyverse)

Sys.setlocale("LC_ALL", "en_US.UTF-8")

data <- read.csv("carpetas-de-investigacion-pgj-cdmx.csv")

# visualización de Datos

names(data)

str(data)


table(data$año_hechos, data$mes_hechos)

delito_categ <- as.data.frame(table(data$delito, data$categoria_delito))

delito_frec <- as.data.frame(tapply(delito_categ$Freq, delito_categ$Var2, sum))

delito_max <- as.data.frame(table(data$delito))

delitos_2019 <- as.data.frame(table(data$año_hechos, data$delito))

names(delitos_2019) = c("Año","Delito", "Núm_Delitos")

delitos_2019 <- delitos_2019 %>%
  select(Año,Delito,Núm_Delitos)%>%
  filter(Año == 2017  | Año == 2018 | Año == 2019)

ggplot(data = a4, aes(x = Año, y = Núm_Delitos, group = Delito, color='Delito')) +
  geom_line(alpha=0.3)


#------------------

d1 <- delitos_2019%>%
  select(Año,Delito,Núm_Delitos)%>%
  filter(Año == 2018)

d2 <- delitos_2019%>%
  select(Año,Delito,Núm_Delitos)%>%
  filter(Año == 2019)

d2$Año <- as.null(b2$Año)
d1$Año <- as.null(b1$Año)

Delitos_union <- cbind(d1,d2)

names(Delitos_union) = c("Delitos_2018","Freq_2018", "Delitos_2019", "Freq_2019")

Delitos_union$Indice <- Delitos_union$Freq_2019 - Delitos_union$Freq_2018

delito_Alc <- as.data.frame(table(data$alcaldia_hechos))

delito_Alc <- delito_Alc %>%
  select(Var1, Freq)%>%
  filter(Var1 %in% c("COYOACAN", "MIGUEL HIDALGO", "LA MAGDALENA CONTRERAS", "TLAHUAC", "AZCAPOTZALCO",
                     "IZTACALCO", "ALVARO OBREGON", "XOCHIMILCO", "VENUSTIANO CARRANZA", "TLALPAN", "CUAJIMALPA DE MORELOS",
                     "CUAUHTEMOC", "IZTAPALAPA", "MILPA ALTA", "BENITO JUAREZ", "GUSTAVO A MADERO"))


#------------------


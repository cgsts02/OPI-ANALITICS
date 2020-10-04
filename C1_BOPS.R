
library(tidyverse)

# Cargar bases de datos

Bm_data <- read.csv("bops_bm.csv")
Online_data <- read.csv("bops_online.csv")

# Exploración de datos

names(Bm_data)
names(Online_data)

summary(Bm_data)
summary(Online_data)
str(Bm_data)
str(Online_data)

                                  #-------- Sales Result TABLE 1


tapply(Bm_data$sales, Bm_data$after, mean)

tapply(Online_data$sales, Online_data$after, mean)

# Transformación de datos B&M para poder trabajar con ggplot

Bm_data$month <- as.factor(Bm_data$month)
Bm_data$id..store. <- as.factor(Bm_data$id..store.)
Bm_data$year <- as.factor(Bm_data$year)

# Cambiar las etiquetas de la base para una mejor visualización y comprensión

Bm_data$month_name <- ordered(Bm_data$month,
                              levels = levels(Bm_data$month)[c(1,2,3,4,5,6,7,8,9,10,11,12)])

levels(Bm_data$month_name) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")


Bm_data$usa[Bm_data$usa == 0] <- "Canada"
Bm_data$usa[Bm_data$usa == 1] <- "E.U"

Bm_data$after[Bm_data$after == 0] <- "SIN_BOPS"
Bm_data$after[Bm_data$after == 1] <- "BOPS"

Bm_data$usa <- as.factor(Bm_data$usa)
Bm_data$after <- as.factor(Bm_data$after)

#---------------------GRÁFICAS B&M

boxplot(sales ~ Bm_data$month_name, main = "Sales Vs Year", ylab = "sales", data = Bm_data)

ggplot(Bm_data) +
  geom_point(mapping = aes(year, sales, color = after)) +
  facet_wrap(~ week)

ggplot(Bm_data) +
  geom_point(mapping = aes(year, sales, color = after)) +
  facet_grid(usa ~ week)



# Transformación de datos BOPS

Online_data$month <- as.factor(Online_data$month)
Online_data$year <- as.factor(Online_data$year)

# Cambiar las etiquetas de la base para una mejor visualización y comprensión


Online_data$month_name <- ordered(Online_data$month,
                                  levels = levels(Online_data$month)[c(1,2,3,4,5,6,7,8,9,10,11,12)])

levels(Online_data$month_name) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")


Online_data$close[Online_data$close == 0] <- "a 50 Millas"
Online_data$close[Online_data$close == 1] <- "other"

Online_data$after[Online_data$after == 0] <- "SIN_BOPS"
Online_data$after[Online_data$after == 1] <- "BOPS"

#---------------------GRÁFICAS BOPS

ggplot(Online_data) +
  geom_point(mapping = aes(year, sales, color = close)) +
  facet_wrap(~ week)

ggplot(Online_data) +
  geom_point(mapping = aes(year, sales, color = after)) +
  facet_grid(close ~ after)


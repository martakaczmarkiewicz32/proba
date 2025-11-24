# im mnniej snu tym wiecej h dziennie
#im wyzsze uzaleznienie tym wiecej konfliktów
library(ggplot2)
install.packages(tidyverse)
gg_miss_fet(x = Academic_Level, y = Daily_Hours_Studied, size = 3) +
  labs(
    title = "Braki danych w zależności od poziomu wykształcenia"
  )
install.packages("ggplot2")
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
gg_miss_fet(x = Academic_Level, y = Daily_Hours_Studied, size = 3) + 
  labs(title = "Braki danych w zależności od poziomu wykształcenia")
install.packages("naniar")
library("naniar")

if (!require(naniar))install.packages("naniar")
library(naniar)

gg_miss_fet(x = Academic_Level, y = Daily_Hours_Studied, size = 3) + 
  labs(title = "Braki danych w zależności od poziomu wykształcenia")
library(ggplot2)
library(naniar)

gg_miss_var(x = Academic_Level, y = Daily_Hours_Studied, size = 3) + 
  labs(title = "Braki danych w zależności od poziomu wykształcenia")
gg_miss_var(Academic_Level) + 
  labs(title = "Braki danych w zależności od poziomu wykształcenia")
dane <- read.csv("Students Social Media Addiction.csv", 
                 header = TRUE, 
                 stringsAsFactors = FALSE)
gg_miss_var(dane$Academic_Level) +
  labs(title = "Braki danych w zależności od poziomu wykształcenia")
gg_miss_var(dane) +
  labs(title = "Braki danych w zależności od poziomu wykształcenia")

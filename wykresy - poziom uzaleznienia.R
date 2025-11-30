
library(tidyverse)
library(ggplot2)
library(corrplot)
library(psych)
library(dplyr)

dane <- read.csv("Students Social Media Addiction.csv", 
                 header = TRUE, 
                 stringsAsFactors = FALSE)

#rozkład poziomu uzależnienia studentów -ogólny
ggplot(dane, aes(x = Addicted_Score)) +
  geom_histogram(binwidth = 1, 
                 fill = "steelblue", 
                 color = "black") +
  labs(title = "Rozkład poziomu uzależnienia studentów",
       x = "Wynik uzależnienia",
       y = "Liczba studentów") +
  theme_minimal(base_size = 14)

#rozkład poziomu uzależnienia według płci
ggplot(dane, aes(x = Addicted_Score, fill = Gender)) +
  geom_histogram(binwidth = 1, color = "black", alpha = 1) +
  facet_wrap(~ Gender) +
  labs(title = "Poziom uzależnienia według płci",
       x = "Wynik uzależnienia",
       y = "Liczba studentów") +
  theme_minimal(base_size = 14)

#rozkład poziomu uzależnienia według wieku
ggplot(dane, aes(x = Addicted_Score, fill = Age)) +
  geom_histogram(binwidth = 1, color = "steelblue", alpha = 1) +
  facet_wrap(~ Age) +
  labs(title = "Poziom uzależnienia według wieku", 
       x = "Wynik Uzależnienia",
       y = "Wiek studentów") + 
  theme_minimal(base_size = 14)

#Porównanie poziomu uzależnienia od mediów społecznościowych z kondycją psychiczną studentów
ggplot(dane, aes(x = Addicted_Score, y = Mental_Health_Score)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Zależność między uzależnieniem a zdrowiem psychicznym",
       x = "Poziom uzależnienia (Addicted Score)",
       y = "Wynik zdrowia psychicznego (Mental Health Score)") +
  theme_minimal(base_size = 14)

#Porównanie poziomu uzależnienia od mediów społecznościowych a ilością przespanych godzin w nocy
ggplot(dane, aes(x = Addicted_Score, y = Sleep_Hours_Per_Night)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Uzależnienie a liczba przespanych godzin",
       x = "Poziom uzależnienia",
       y = "Godziny snu w nocy") +
  theme_minimal(base_size = 14)

#rozkład poziomu uzależnienia według platformy społecznościowej
ggplot(dane, aes(x = Addicted_Score, fill = Most_Used_Platform)) +
  geom_histogram(binwidth = 1, color = "steelblue", alpha = 1) +
  facet_wrap(~ Most_Used_Platform) +
  labs(title = "Poziom uzależnienia według platformy społecznościowej", 
       x = "Wynik Uzależnienia",
       y = "Platforma społecznościowa") + 
  theme_minimal(base_size = 14)

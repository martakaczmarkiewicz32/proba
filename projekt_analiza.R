# Analiza uzaleznienia studentow od mediow spolecznosciowych
# Projekt grupowy - 2025

# Wczytanie bibliotek
library(tidyverse)

# Wczytanie danych
dane <- read_csv("Students Social Media Addiction.csv")

# Podstawowe informacje o danych
glimpse(dane)
head(dane)
summary(dane)

# Sprawdzenie brakujacych wartosci
dane %>% 
  summarise(across(everything(), ~sum(is.na(.))))

# Czyszczenie danych
dane_clean <- dane %>%
  distinct() %>%
  rename(
    ID = Student_ID,
    Wiek = Age,
    Plec = Gender,
    Poziom_akademicki = Academic_Level,
    Kraj = Country,
    Sredni_czas_uzywania_h = Avg_Daily_Usage_Hours,
    Najpopularniejsza_platforma = Most_Used_Platform,
    Wplyw_na_wyniki = Affects_Academic_Performance,
    Godziny_snu = Sleep_Hours_Per_Night,
    Ocena_zdrowia_psychicznego = Mental_Health_Score,
    Status_zwiazku = Relationship_Status,
    Konflikty_media = Conflicts_Over_Social_Media,
    Wynik_uzaleznienia = Addicted_Score
  )

# Statystyki wedlug plci
dane_clean %>%
  group_by(Plec) %>%
  summarise(
    liczba_osob = n(),
    sredni_czas_uzywania = mean(Sredni_czas_uzywania_h, na.rm = TRUE),
    sredni_wynik_uzaleznienia = mean(Wynik_uzaleznienia, na.rm = TRUE),
    srednia_ocena_zdrowia = mean(Ocena_zdrowia_psychicznego, na.rm = TRUE)
  )

# Analiza wedlug poziomu akademickiego
dane_clean %>%
  group_by(Poziom_akademicki) %>%
  summarise(
    liczba_studentow = n(),
    sredni_czas_h = mean(Sredni_czas_uzywania_h, na.rm = TRUE),
    mediana_wynik_uzaleznienia = median(Wynik_uzaleznienia, na.rm = TRUE)
  ) %>%
  arrange(desc(sredni_czas_h))

# Top 5 krajow z najwyzszym srednim czasem uzywania
dane_clean %>%
  group_by(Kraj) %>%
  summarise(
    liczba_studentow = n(),
    sredni_czas_h = mean(Sredni_czas_uzywania_h, na.rm = TRUE)
  ) %>%
  arrange(desc(sredni_czas_h)) %>%
  top_n(5, sredni_czas_h)

# Najpopularniejsze platformy
dane_clean %>%
  count(Najpopularniejsza_platforma, sort = TRUE)

# Studenci z wysokim ryzykiem uzaleznienia
studenci_wysokie_ryzyko <- dane_clean %>%
  filter(Wynik_uzaleznienia >= 7) %>%
  select(ID, Wiek, Plec, Sredni_czas_uzywania_h, Wynik_uzaleznienia, 
         Godziny_snu, Ocena_zdrowia_psychicznego)

# Wplyw na wyniki akademickie
dane_clean %>%
  group_by(Wplyw_na_wyniki) %>%
  summarise(
    liczba = n(),
    sredni_czas_h = mean(Sredni_czas_uzywania_h, na.rm = TRUE),
    sredni_wynik_uzaleznienia = mean(Wynik_uzaleznienia, na.rm = TRUE),
    srednie_godziny_snu = mean(Godziny_snu, na.rm = TRUE)
  )

# Korelacje miedzy zmiennymi
dane_clean %>%
  summarise(
    korelacja_czas_uzaleznienie = cor(Sredni_czas_uzywania_h, Wynik_uzaleznienia, use = "complete.obs"),
    korelacja_sen_uzaleznienie = cor(Godziny_snu, Wynik_uzaleznienia, use = "complete.obs"),
    korelacja_zdrowie_uzaleznienie = cor(Ocena_zdrowia_psychicznego, Wynik_uzaleznienia, use = "complete.obs")
  )

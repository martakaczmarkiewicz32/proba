
# Analiza uzaleznienia studentow od mediow spolecznosciowych
# Projekt grupowy - 2025

# Instalacja pakietu (tylko raz, mozna zakomentowac po instalacji)
# install.packages("tidyverse")

# Wczytanie bibliotek
library(tidyverse)

# Wczytanie danych
dane <- read_csv("c:/Users/biesi/Desktop/studia/analiza danych w R/proba/Students Social Media Addiction.csv")

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

# Tworzenie nowych zmiennych z mutate()
dane_clean <- dane_clean %>%
  mutate(
    # Kategoryzacja czasu uzywania
    kategoria_czasu = case_when(
      Sredni_czas_uzywania_h < 2 ~ "Niskie",
      Sredni_czas_uzywania_h < 4 ~ "Srednie",
      Sredni_czas_uzywania_h < 6 ~ "Wysokie",
      TRUE ~ "Bardzo wysokie"
    ),
    # Kategoryzacja uzaleznienia
    poziom_uzaleznienia = case_when(
      Wynik_uzaleznienia <= 3 ~ "Brak",
      Wynik_uzaleznienia <= 6 ~ "Lekkie",
      Wynik_uzaleznienia <= 8 ~ "Umiarkowane",
      TRUE ~ "Silne"
    ),
    # Czy ma problem ze snem (mniej niz 6h)
    problem_ze_snem = ifelse(Godziny_snu < 6, "Tak", "Nie"),
    # Ocena zdrowia - kategorie
    zdrowie_kategoria = cut(Ocena_zdrowia_psychicznego, 
                            breaks = c(0, 4, 7, 10),
                            labels = c("Slabe", "Srednie", "Dobre"))
  )

# Analiza kategorii czasu uzywania
dane_clean %>%
  group_by(kategoria_czasu) %>%
  summarise(
    liczba = n(),
    sredni_wynik_uzaleznienia = mean(Wynik_uzaleznienia),
    procent_wplywu_na_nauke = mean(Wplyw_na_wyniki == "Yes") * 100
  )

# Tabela krzyzowa - poziom uzaleznienia vs problem ze snem
dane_clean %>%
  count(poziom_uzaleznienia, problem_ze_snem) %>%
  pivot_wider(names_from = problem_ze_snem, values_from = n, values_fill = 0)

# Filtrowanie z wieloma warunkami
dane_clean %>%
  filter(Wiek >= 20, 
         Sredni_czas_uzywania_h > 5,
         Wplyw_na_wyniki == "Yes") %>%
  select(ID, Wiek, Plec, Sredni_czas_uzywania_h, Najpopularniejsza_platforma) %>%
  arrange(desc(Sredni_czas_uzywania_h))

# Grupowanie wedlug wielu zmiennych
dane_clean %>%
  group_by(Plec, poziom_uzaleznienia) %>%
  summarise(
    liczba = n(),
    sredni_wiek = mean(Wiek),
    .groups = "drop"
  ) %>%
  arrange(Plec, poziom_uzaleznienia)

# Zliczanie obserwacji - funkcja count()
dane_clean %>%
  count(Plec, Poziom_akademicki, sort = TRUE)

# Analiza mediany i kwartyli
dane_clean %>%
  group_by(Najpopularniejsza_platforma) %>%
  summarise(
    liczba_uzytkownikow = n(),
    mediana_czasu = median(Sredni_czas_uzywania_h),
    Q1 = quantile(Sredni_czas_uzywania_h, 0.25),
    Q3 = quantile(Sredni_czas_uzywania_h, 0.75)
  ) %>%
  filter(liczba_uzytkownikow > 10) %>%
  arrange(desc(mediana_czasu))

# Tworzenie rankingu krajow
dane_clean %>%
  group_by(Kraj) %>%
  summarise(
    n = n(),
    sredni_wynik = mean(Wynik_uzaleznienia)
  ) %>%
  filter(n >= 5) %>%
  mutate(ranking = rank(desc(sredni_wynik))) %>%
  arrange(ranking)

# Sprawdzenie czy są duplikaty w oryginalnych danych
sum(duplicated(dane))
# Sprawdzenie czy są duplikaty po czyszczeniu
sum(duplicated(dane_clean))


# --- WYKRESY PODSTAWOWYCH I NAJWAŻNIEJSZYCH ZMIENNYCH ---

# Histogram średniego czasu używania mediów społecznościowych
ggplot(dane_clean, aes(x = Sredni_czas_uzywania_h)) +
  geom_histogram(binwidth = 1, fill = "#69b3a2", color = "black") +
  labs(title = "Histogram średniego czasu używania (h)",
    x = "Średni czas używania (h)",
    y = "Liczba studentów")

# Wykres słupkowy najpopularniejszych platform
ggplot(dane_clean, aes(x = Najpopularniejsza_platforma)) +
  geom_bar(fill = "#404080") +
  labs(title = "Najpopularniejsze platformy społecznościowe",
    x = "Platforma",
    y = "Liczba studentów") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Przygotowanie 3 grup wiekowych i 3 poziomów uzależnienia
dane_clean <- dane_clean %>%
  mutate(
    grupa_wiekowa = cut(Wiek, breaks = c(0, 20, 23, 100), labels = c("<=20", "21-23", ">23")),
    poziom_uzaleznienia_3 = case_when(
      Wynik_uzaleznienia <= 4 ~ "Niski",
      Wynik_uzaleznienia <= 7 ~ "Średni",
      TRUE ~ "Wysoki"
    )
  )

# Histogram: poziom uzależnienia (3 kategorie) wg płci i grupy wiekowej
ggplot(dane_clean, aes(x = poziom_uzaleznienia_3, fill = Plec)) +
  geom_bar(position = "dodge", color = "black") +
  facet_grid(. ~ grupa_wiekowa) +
  scale_fill_manual(values = c("Female" = "#ff69b4", "Male" = "#0072B2")) +
  labs(title = "Poziom uzależnienia wg płci i wieku",
       x = "Poziom uzależnienia",
       y = "Liczba studentów",
       fill = "Płeć")
  # --- Wykres zależności platformy od płci i wieku ---

# Wykres słupkowy: najpopularniejsza platforma wg płci i grup wiekowych
ggplot(dane_clean, aes(x = Najpopularniejsza_platforma, fill = Plec)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ grupa_wiekowa) +
  scale_fill_manual(values = c("Female" = "#ff69b4", "Male" = "#0072B2")) +
  labs(title = "Najpopularniejsze platformy wg płci i wieku",
    x = "Platforma",
    y = "Liczba studentów",
    fill = "Płeć") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

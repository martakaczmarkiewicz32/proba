# Analiza podstawowa: Students Social Media Addiction
# Autor: Analiza danych studentów
# Data: 2025-11-03

# Wczytanie potrzebnych bibliotek
library(tidyverse)
library(ggplot2)
library(corrplot)
library(psych)
library(dplyr)

# Wczytanie pliku CSV
dane <- read.csv("Students Social Media Addiction.csv", 
                 header = TRUE, 
                 stringsAsFactors = FALSE)

# Wyświetlenie pierwszych 6 wierszy
cat("\n=== PIERWSZE 6 WIERSZY ===\n")
head(dane)

# Wyświetlenie ostatnich 6 wierszy
cat("\n=== OSTATNIE 6 WIERSZY ===\n")
tail(dane)

# Wymiary zbioru danych (liczba wierszy i kolumn)
cat("\n=== WYMIARY DANYCH ===\n")
cat("Liczba wierszy:", nrow(dane), "\n")
cat("Liczba kolumn:", ncol(dane), "\n")
dim(dane)

# Nazwy kolumn
cat("\n=== NAZWY KOLUMN ===\n")
names(dane)

# Struktura danych
cat("\n=== STRUKTURA DANYCH ===\n")
str(dane)

# Podsumowanie wszystkich zmiennych
cat("\n=== PODSUMOWANIE STATYSTYCZNE ===\n")
summary(dane)

# Statystyki dla zmiennych numerycznych
cat("\n=== STATYSTYKI DLA ZMIENNYCH NUMERYCZNYCH ===\n")

# Wiek
cat("\nWiek:\n")
cat("Średnia:", mean(dane$Age, na.rm = TRUE), "\n")
cat("Mediana:", median(dane$Age, na.rm = TRUE), "\n")
cat("Odchylenie standardowe:", sd(dane$Age, na.rm = TRUE), "\n")
cat("Min:", min(dane$Age, na.rm = TRUE), "\n")
cat("Max:", max(dane$Age, na.rm = TRUE), "\n")

# Średnie dzienne użycie (godziny)
cat("\nŚrednie dzienne użycie mediów społecznościowych (godziny):\n")
cat("Średnia:", mean(dane$Avg_Daily_Usage_Hours, na.rm = TRUE), "\n")
cat("Mediana:", median(dane$Avg_Daily_Usage_Hours, na.rm = TRUE), "\n")
cat("Odchylenie standardowe:", sd(dane$Avg_Daily_Usage_Hours, na.rm = TRUE), "\n")
cat("Min:", min(dane$Avg_Daily_Usage_Hours, na.rm = TRUE), "\n")
cat("Max:", max(dane$Avg_Daily_Usage_Hours, na.rm = TRUE), "\n")

# Godziny snu
cat("\nGodziny snu na noc:\n")
cat("Średnia:", mean(dane$Sleep_Hours_Per_Night, na.rm = TRUE), "\n")
cat("Mediana:", median(dane$Sleep_Hours_Per_Night, na.rm = TRUE), "\n")
cat("Odchylenie standardowe:", sd(dane$Sleep_Hours_Per_Night, na.rm = TRUE), "\n")

# Wynik zdrowia psychicznego
cat("\nWynik zdrowia psychicznego:\n")
cat("Średnia:", mean(dane$Mental_Health_Score, na.rm = TRUE), "\n")
cat("Mediana:", median(dane$Mental_Health_Score, na.rm = TRUE), "\n")
cat("Odchylenie standardowe:", sd(dane$Mental_Health_Score, na.rm = TRUE), "\n")

# Wynik uzależnienia
cat("\nWynik uzależnienia:\n")
cat("Średnia:", mean(dane$Addicted_Score, na.rm = TRUE), "\n")
cat("Mediana:", median(dane$Addicted_Score, na.rm = TRUE), "\n")
cat("Odchylenie standardowe:", sd(dane$Addicted_Score, na.rm = TRUE), "\n")


cat("\n=== ROZKŁAD PŁCI ===\n")
table(dane$Gender)
prop.table(table(dane$Gender)) * 100  # Procenty

cat("\n=== ROZKŁAD POZIOMU AKADEMICKIEGO ===\n")
table(dane$Academic_Level)
prop.table(table(dane$Academic_Level)) * 100

cat("\n=== NAJCZĘŚCIEJ UŻYWANA PLATFORMA ===\n")
table(dane$Most_Used_Platform)
prop.table(table(dane$Most_Used_Platform)) * 100

cat("\n=== CZY WPŁYWA NA WYNIKI AKADEMICKIE ===\n")
table(dane$Affects_Academic_Performance)
prop.table(table(dane$Affects_Academic_Performance)) * 100

cat("\n=== STATUS ZWIĄZKU ===\n")
table(dane$Relationship_Status)
prop.table(table(dane$Relationship_Status)) * 100

cat("\n=== LICZBA BRAKUJĄCYCH WARTOŚCI W KAŻDEJ KOLUMNIE ===\n")
colSums(is.na(dane))

cat("\n=== CAŁKOWITA LICZBA BRAKUJĄCYCH WARTOŚCI ===\n")
sum(is.na(dane))


cat("\n=== MACIERZ KORELACJI ===\n")
zmienne_numeryczne <- dane[, c("Age", "Avg_Daily_Usage_Hours", 
                                "Sleep_Hours_Per_Night", "Mental_Health_Score", 
                                "Conflicts_Over_Social_Media", "Addicted_Score")]
cor_matrix <- cor(zmienne_numeryczne, use = "complete.obs")
print(round(cor_matrix, 3))


# Ustawienie układu wykresów
par(mfrow = c(2, 3))

# Histogram wieku
hist(dane$Age, 
     main = "Rozkład wieku", 
     xlab = "Wiek", 
     ylab = "Częstość",
     col = "lightblue",
     breaks = 10)

# Histogram średniego dziennego użycia
hist(dane$Avg_Daily_Usage_Hours, 
     main = "Średnie dzienne użycie (godz.)", 
     xlab = "Godziny", 
     ylab = "Częstość",
     col = "lightgreen",
     breaks = 15)

# Histogram godzin snu
hist(dane$Sleep_Hours_Per_Night, 
     main = "Godziny snu", 
     xlab = "Godziny", 
     ylab = "Częstość",
     col = "lightyellow",
     breaks = 10)

# Wykres słupkowy dla płci
barplot(table(dane$Gender), 
        main = "Rozkład płci", 
        xlab = "Płeć", 
        ylab = "Liczba",
        col = c("pink", "lightblue"))

# Wykres słupkowy dla platformy
barplot(table(dane$Most_Used_Platform), 
        main = "Najczęściej używana platforma", 
        xlab = "Platforma", 
        ylab = "Liczba",
        col = rainbow(length(unique(dane$Most_Used_Platform))),
        las = 2,
        cex.names = 0.7)

# Wykres pudełkowy dla wyniku uzależnienia
boxplot(dane$Addicted_Score, 
        main = "Wynik uzależnienia", 
        ylab = "Wynik",
        col = "orange")

# Przywrócenie domyślnego układu
par(mfrow = c(1, 1))


cat("\n=== ŚREDNI WYNIK UZALEŻNIENIA WG PŁCI ===\n")
aggregate(Addicted_Score ~ Gender, data = dane, FUN = mean)

cat("\n=== ŚREDNIE UŻYCIE WG POZIOMU AKADEMICKIEGO ===\n")
aggregate(Avg_Daily_Usage_Hours ~ Academic_Level, data = dane, FUN = mean)

cat("\n=== ŚREDNIE ZDROWIE PSYCHICZNE WG STATUSU ZWIĄZKU ===\n")
aggregate(Mental_Health_Score ~ Relationship_Status, data = dane, FUN = mean)


# Zapisanie podsumowania do pliku tekstowego
sink("podsumowanie_analizy.txt")
cat("RAPORT Z ANALIZY PODSTAWOWEJ\n")
cat("Data:", format(Sys.Date(), "%Y-%m-%d"), "\n\n")
cat("Liczba obserwacji:", nrow(dane), "\n")
cat("Liczba zmiennych:", ncol(dane), "\n\n")
summary(dane)
sink()

# Dodatkowe zaawansowane analizy

# Wykres korelacji przy użyciu corrplot
png("korelacje_zaawansowane.png", width = 800, height = 800)
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black", 
         tl.srt = 45,
         title = "Macierz korelacji zmiennych numerycznych")
dev.off()

# Zaawansowane wykresy ggplot2

# 1. Relacja między czasem spędzonym na mediach a zdrowiem psychicznym
ggplot(dane, aes(x = Avg_Daily_Usage_Hours, y = Mental_Health_Score)) +
  geom_point(aes(color = Gender), alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Zależność między czasem użytkowania a zdrowiem psychicznym",
       x = "Średni dzienny czas użytkowania (godz.)",
       y = "Wynik zdrowia psychicznego") +
  theme_minimal()
ggsave("zdrowie_vs_uzytkowanie.png")

# 2. Rozkład wyników uzależnienia według platformy
ggplot(dane, aes(x = Most_Used_Platform, y = Addicted_Score)) +
  geom_boxplot(aes(fill = Most_Used_Platform)) +
  labs(title = "Rozkład wyników uzależnienia według platformy",
       x = "Platforma",
       y = "Wynik uzależnienia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("uzaleznienie_vs_platforma.png")

# 3. Analiza godzin snu względem użytkowania
ggplot(dane, aes(x = Avg_Daily_Usage_Hours, y = Sleep_Hours_Per_Night)) +
  geom_point(aes(color = Academic_Level), alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Wpływ użytkowania mediów na sen",
       x = "Średni dzienny czas użytkowania (godz.)",
       y = "Godziny snu") +
  theme_minimal()
ggsave("sen_vs_uzytkowanie.png")

# Testy statystyczne

# 1. Test różnic w uzależnieniu między płciami
test_plec <- t.test(Addicted_Score ~ Gender, data = dane)
cat("\n=== TEST T DLA RÓŻNIC W UZALEŻNIENIU MIĘDZY PŁCIAMI ===\n")
print(test_plec)

# 2. ANOVA dla różnic w czasie użytkowania między poziomami akademickimi
model_anova <- aov(Avg_Daily_Usage_Hours ~ Academic_Level, data = dane)
cat("\n=== ANOVA DLA RÓŻNIC W CZASIE UŻYTKOWANIA ===\n")
print(summary(model_anova))

# 3. Korelacja między czasem użytkowania a zdrowiem psychicznym
test_cor <- cor.test(dane$Avg_Daily_Usage_Hours, dane$Mental_Health_Score)
cat("\n=== TEST KORELACJI: CZAS UŻYTKOWANIA VS ZDROWIE PSYCHICZNE ===\n")
print(test_cor)

# Podsumowanie końcowe
cat("\n=== GŁÓWNE WNIOSKI ===\n")
cat("1. Średni czas użytkowania mediów społecznościowych:", round(mean(dane$Avg_Daily_Usage_Hours, na.rm = TRUE), 2), "godzin dziennie\n")
cat("2. Najczęściej używana platforma:", names(which.max(table(dane$Most_Used_Platform))), "\n")
cat("3. Średni wynik zdrowia psychicznego:", round(mean(dane$Mental_Health_Score, na.rm = TRUE), 2), "\n")
cat("4. Korelacja między czasem użytkowania a zdrowiem psychicznym:", round(cor(dane$Avg_Daily_Usage_Hours, dane$Mental_Health_Score, use = "complete.obs"), 3), "\n")

cat("\n=== ANALIZA ZAKOŃCZONA ===\n")
cat("Wyniki zapisane do pliku: podsumowanie_analizy.txt\n")
cat("Dodatkowo utworzono wykresy:\n")
cat("- korelacje_zaawansowane.png\n")
cat("- zdrowie_vs_uzytkowanie.png\n")
cat("- uzaleznienie_vs_platforma.png\n")
cat("- sen_vs_uzytkowanie.png\n")

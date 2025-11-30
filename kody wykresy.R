install.packages("ggplot2")
install.packages("tidyverse")
install.packages("scales")
library("scales")
library(tidyverse)
library(ggplot2)

usage_plot <- ggplot(data = Students_Social_Media_Addiction, aes(x = Avg_Daily_Usage_Hours)) +
  
  geom_histogram(
    aes(y = after_stat(density)), 
    bins = 15, 
    fill = "#1B9E77", 
    color = "white", 
    alpha = 0.7
  ) +
  
  geom_density(
    color = "red", 
    linewidth = 1.2
  ) +
  
  labs(
    title = "Rozkład Średniej Dziennej Liczby Godzin Użytkowania",
    x = "Średnia Dzienna Liczba Godzin Użytkowania",
    y = "Gęstość"
  ) +
  
  theme_minimal()

print(usage_plot)


platform_plot <- ggplot(data = Students_Social_Media_Addiction, aes(y = fct_infreq(Most_Used_Platform))) + 
  
  geom_bar(
    fill = "#E7298A", 
    color = "white"
  ) +
  
  geom_text(
    stat = 'count', 
    aes(label = after_stat(count)), 
    hjust = -0.2, 
    size = 4
  ) +
  
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) + 
  
  labs(
    title = "Liczba Studentów według Najczęściej Używanej Platformy",
    x = "Liczba Studentów",
    y = "Najczęściej Używana Platforma"
  ) +
  
  theme_bw()

print(platform_plot)

scatter_plot <- ggplot(
  data = Students_Social_Media_Addiction, 
  aes(x = Avg_Daily_Usage_Hours, y = Addicted_Score)
) +
  
  geom_point(
    aes(
      color = Gender,                      
      shape = Affects_Academic_Performance, 
      size = Mental_Health_Score           
    ),
    alpha = 0.7 
  ) +
  
  geom_smooth(
    method = "lm", 
    se = FALSE, 
    color = "black", 
    linetype = "dashed"
  ) +
  
  labs(
    title = "Związek Między Czasem Użytkowania a Wynikiem Uzależnienia",
    x = "Średnia Dzienna Liczba Godzin Użytkowania",
    y = "Wynik Uzależnienia (0-10)",
    color = "Płeć", 
    shape = "Wpływa na naukę", 
    size = "Wynik Zdrowia Psych."
  ) +
  
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  )

print(scatter_plot)

#Porównanie Poziomów Akademickich
level_gender_plot <- ggplot(data = Students_Social_Media_Addiction, aes(x = Academic_Level)) +
  geom_bar(
    aes(fill = Gender),
    position = "dodge", 
    color = "black"     
  ) +
  labs(
    title = "Liczba Studentów według Poziomu Akademickiego i Płci",
    x = "Poziom Akademicki",
    y = "Liczba Studentów",
    fill = "Płeć" # Etykieta legendy
  ) +
  
  theme_light() +
  scale_x_discrete(limits = c("High School", "Undergraduate", "Graduate"))

print(level_gender_plot)


#Związek Uzależnienia a Godziny Snu
sleep_boxplot <- ggplot(data = Students_Social_Media_Addiction, aes(x = Affects_Academic_Performance, y = Sleep_Hours_Per_Night)) +
  
  geom_boxplot(
    aes(fill = Affects_Academic_Performance), 
    alpha = 0.7,
    outlier.shape = 8 
  ) +
  geom_jitter(
    color = "black", 
    size = 0.5, 
    alpha = 0.5, 
    width = 0.1
  ) +
  
  labs(
    title = "Godziny Snu w zależności od Wpływu na Wyniki Akademickie",
    x = "Wpływ Mediów Społecznościowych na Wyniki Akademickie",
    y = "Godziny Snu na Dobę",
    fill = "Wpływ"
  ) +
  
  theme_classic() +
  theme(legend.position = "none") 

print(sleep_boxplot)


#Wizualizacja Wyniku Uzależnienia w Czasie Użytkowania
heatmap_bin2d <- ggplot(data = Students_Social_Media_Addiction, aes(x = Avg_Daily_Usage_Hours, y = Addicted_Score)) +
  geom_bin2d(
    bins = 10
  ) +
  
  scale_fill_gradient(low = "yellow", high = "red") + 
  labs(
    title = "Gęstość Uzależnienia vs Czas Użytkowania",
    subtitle = "Im ciemniejszy kolor, tym więcej studentów w danym przedziale",
    x = "Średnia Dzienna Liczba Godzin Użytkowania",
    y = "Wynik Uzależnienia (0-10)",
    fill = "Liczba Studentów"
  ) +
  theme_minimal()

print(heatmap_bin2d)

# Loading subsidiary data -------------------------------------------------

regions <- read.csv("Regional.Structure.csv", sep = ";", encoding = "UTF-8-BOM")

region_c <- read.csv("RegionC.csv", sep = ";", encoding = "UTF-8") 

# Selecting necessary data ------------------------------------------------

nps <- main_data %>% 
  select(RecordNo, RegionA, S14A, S10, S11, S12, starts_with("S9M")) %>% 
  mutate(Area = case_when(S14A <= 25 ~ "5-25 Ha",
                          50 >= S14A ~ "25-50 Ha",
                          100 >= S14A ~ "50-100 Ha",
                          250 >= S14A ~ "100-250 Ha",
                          S14A > 250 ~ "Więcej niż 250 Ha")) %>% 
  mutate(Wiek = case_when(year(Sys.Date()) - S10 < 30 ~ "<30",
                          year(Sys.Date()) - S10 < 40 ~ "30-39",
                          year(Sys.Date()) - S10 < 50 ~ "40-49",
                          year(Sys.Date()) - S10 >= 50 ~ "50+",
                          T ~ "???")) %>% 
  mutate(Wykształcenie = case_when(S11 == 101 ~ "Wyższe rolnicze",
                                   S11 == 102 ~ "Wyższe",
                                   S11 == 103 ~ "Średnie techniczne",
                                   S11 == 104 ~ "Średnie ogólne",
                                   S11 == 994 ~ "Inne",
                                   T ~ "???")) %>% 
  mutate(Funkcja = case_when(S12 == 101 ~ "Dyrektor / wicedyrektor",
                             S12 == 102 ~ "Główny agronom",
                             S12 == 103 ~ "Agronom – ekspert",
                             S12 == 104 ~ "Właściciel",
                             S12 == 104 ~ "Pracownik techniczny",
                             S12 == 994 ~ "Inne",
                             S12 == 999 ~ "Nie wiem / brak odpowiedzi",
                             T ~ "???")) %>% 
  left_join(regions) %>%  
  left_join(region_c) %>% 
  rename(Jablka = S9M1,
         Truskawki = S9M2,
         Sliwki = S9M3,
         Gruszki = S9M4,
         Wisnie = S9M5,
         Borowki = S9M6,
         Porzeczki = S9M7,
         Maliny = S9M8,
         Aronia = S9M9,
         Jagody = S9M10) %>% 
  pivot_longer(cols = c(7:16), names_to = "Company", 
               values_to = "Recommendation") %>%
  mutate(Score = case_when(
    Recommendation < 7 ~ "Detractor",
    Recommendation > 8 ~ "Promoter",
    Recommendation == 7 | Recommendation == 8 ~ "Neutral"
  )) %>%
  select(RecordNo, Województwo, Area, Company, Recommendation, Score, 
         Wiek, Wykształcenie, Funkcja) %>% 
  mutate(detractor = if_else(between(Recommendation, 0, 6), 1, 0),
         neutral = if_else(between(Recommendation, 7, 8), 1, 0),
         promoter = if_else(between(Recommendation, 9, 10), 1, 0),
         total = 1)

# Saving results ------------------------------------------------------

write_csv(nps, paste0(getwd(), '\\nps_analysis.csv'))

# Visualization -----------------------------------------------------------

sum <- aggregate(cbind(detractor, neutral, promoter) ~ Company, 
                 data = nps, sum)

sum$nps_score <- 100 * ((sum$promoter - sum$detractor) / 
                          sum(sum$detractor, sum$neutral, 
                              sum$promoter))
sum$nps_score <- round(sum$nps_score, 2)

ggplot(sum, aes(x = Company, y = nps_score)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  scale_y_reverse() +
  geom_text(aes(label = nps_score, vjust = 0.5)) +
  labs(title = "Net Promoter Score in individual companies", 
       x = "Company", y = "NPS Score")

long <- pivot_longer(sum, cols = c("detractor", "neutral", "promoter"), 
                     names_to = "category", values_to = "result")

ggplot(data = long, aes(x = Company, y = result, fill = category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Net Promoter Score (NPS) for companies",
       x = "Company",
       y = "Number of responses") +
  scale_fill_manual(values = c("#d73027", "#fee08b", "#1a9850"),
                    labels = c("Detractor", "Neutral", "Promoter")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))














# Creating new dataset ----------------------------------------------------

brands <- subset(main_data, select = c("RecordNo", "S11", "S12"))
newCols <- paste("Index", 1:20, sep = "")
brands[,newCols] <- NA
brands <- gather(brands, key = "Index", value = "Dummy", 
                 Index1:Index20,factor_key = TRUE)
brands <- mutate(brands, Index = as.numeric(Index))

# Top of mind -------------------------------------------------------------

TopofMind <- ("S1M1")
df1.1 = subset(main_data, select = c("RecordNo", TopofMind))
df1.1$Index = 1

df1.1 <- rename(df1.1, TopofMind = S1M1)

brands <- left_join(brands, df1.1, by = c("RecordNo", "Index"))

# Unaided -----------------------------------------------------------------

unaided <- c("S1M1", "S1M2", "S1M3", "S1M4", "S1M5",
             "S1M6", "S1M7", "S1M8", "S1M9", "S1M10")
df1.2 <- subset(main_data, select = c("RecordNo", unaided))

df1.2 <- gather(df1.2, key = "Index", value = "Unaided", 
              S1M1:S1M10, factor_key = TRUE)
df1.2 <- mutate(df1.2, Index = as.numeric(Index))

brands <- left_join(brands, df1.2, by = c("RecordNo", "Index"))

# Aided -------------------------------------------------------------------

aided <- c("S2M1", "S2M2", "S2M3", "S2M4", "S2M5",
           "S2M6", "S2M7", "S2M8", "S2M9", "S2M10")
df1.3 = subset(main_data, select = c("RecordNo", aided))

df1.3 <- gather(df1.3, key = "Index", value = "Aided", 
                S2M1:S2M10, factor_key = TRUE)
df1.3 <- mutate(df1.3, Index = as.numeric(Index))

brands <- left_join(brands, df1.3, by = c("RecordNo", "Index"))

# Total -------------------------------------------------------------------

total <- c("S1M1", "S1M2", "S1M3", "S1M4", "S1M5", "S1M6", "S1M7", "S1M8",
           "S1M9", "S1M10", "S2M1", "S2M2", "S2M3", "S2M4", "S2M5", "S2M6",
           "S2M7", "S2M8", "S2M9", "S2M10")
df1.4 = subset(main_data, select = c("RecordNo", total))

df1.4 <- gather(df1.4, key = "Index", value = "TotalAwarness", 
                S1M1:S2M10, factor_key = TRUE)
df1.4 <- mutate(df1.4, Index = as.numeric(Index))

brands <- left_join(brands, df1.4, by = c("RecordNo", "Index"))

brands <- mutate(brands, TopofMind = if_else(Index==20, 999999, 
                                             as.numeric(TopofMind)))
# Segmenting ----------------------------------------------------

brands <- brands %>% 
  select(RecordNo, Index, S11, S12, TopofMind, 
         Unaided, Aided, TotalAwarness) %>%
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
  mutate(TopofMind = case_when(TopofMind == 101 ~ "Jabłka",
                               TopofMind == 102 ~ "Truskawki",
                               TopofMind == 103 ~ "Śliwki",
                               TopofMind == 104 ~ "Gruszki",
                               TopofMind == 105 ~ "Wiśnie",
                               TopofMind == 106 ~ "Borówki",
                               TopofMind == 107 ~ "Porzeczki",
                               TopofMind == 108 ~ "Maliny",
                               TopofMind == 109 ~ "Aronia",
                               TopofMind == 110 ~ "Jagody")) %>%
  mutate(Unaided = case_when(Unaided == 101 ~ "Jabłka",
                             Unaided == 102 ~ "Truskawki",
                             Unaided == 103 ~ "Śliwki",
                             Unaided == 104 ~ "Gruszki",
                             Unaided == 105 ~ "Wiśnie",
                             Unaided == 106 ~ "Borówki",
                             Unaided == 107 ~ "Porzeczki",
                             Unaided == 108 ~ "Maliny",
                             Unaided == 109 ~ "Aronia",
                             Unaided == 110 ~ "Jagody")) %>%
  mutate(Aided = case_when(Aided == 101 ~ "Jabłka",
                           Aided == 102 ~ "Truskawki",
                           Aided == 103 ~ "Śliwki",
                           Aided == 104 ~ "Gruszki",
                           Aided == 105 ~ "Wiśnie",
                           Aided == 106 ~ "Borówki",
                           Aided == 107 ~ "Porzeczki",
                           Aided == 108 ~ "Maliny",
                           Aided == 109 ~ "Aronia",
                           Aided == 110 ~ "Jagody")) %>%
  mutate(TotalAwarness = case_when(TotalAwarness == 101 ~ "Jabłka",
                                   TotalAwarness == 102 ~ "Truskawki",
                                   TotalAwarness == 103 ~ "Śliwki",
                                   TotalAwarness == 104 ~ "Gruszki",
                                   TotalAwarness == 105 ~ "Wiśnie",
                                   TotalAwarness == 106 ~ "Borówki",
                                   TotalAwarness == 107 ~ "Porzeczki",
                                   TotalAwarness == 108 ~ "Maliny",
                                   TotalAwarness == 109 ~ "Aronia",
                                   TotalAwarness == 110 ~ "Jagody"))
  
  
brands <- brands[,!names(brands) %in% c("S11", "S12")]
  
# Saving results ----------------------------------------------------------

write_csv(brands, paste0(getwd(), '\\awareness_analysis.csv'))
  
# Visualization -----------------------------------------------------------

topofmind_count <- data.frame(value = na.omit(brands$TopofMind)) %>% 
  group_by(value) %>% 
  summarize(n = n()) %>% 
  mutate(Percent = n / sum(n))

ggplot(topofmind_count, aes(x = value, y = Percent)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Company", title = "Top of Mind") +
  theme(plot.title = element_text(hjust = 0.5))

unaided_count <- data.frame(value = na.omit(brands$Unaided)) %>% 
  group_by(value) %>% 
  summarize(n = n()) %>% 
  mutate(Percent = n / sum(n))

ggplot(unaided_count, aes(x = value, y = Percent)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Company", title = "Unaided") +
  theme(plot.title = element_text(hjust = 0.5))

aided_count <- data.frame(value = na.omit(brands$Aided)) %>% 
  group_by(value) %>% 
  summarize(n = n()) %>% 
  mutate(Percent = n / sum(n))

ggplot(aided_count, aes(x = value, y = Percent)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Company", title = "Aided") +
  theme(plot.title = element_text(hjust = 0.5))

total_count <- data.frame(value = na.omit(brands$TotalAwarness)) %>% 
  group_by(value) %>% 
  summarize(n = n()) %>% 
  mutate(Percent = n / sum(n))

ggplot(total_count, aes(x = value, y = Percent)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Company", title = "Total Awarness") +
  theme(plot.title = element_text(hjust = 0.5))
            
                               
                           




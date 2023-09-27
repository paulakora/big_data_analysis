# Creating new dataset ----------------------------------------------------

df2.1 <- data.frame(
  company = c(101, 102, 103, 104, 105, 106, 107, 108)
)

all <- main_data %>% left_join(df2.1, by = character())

# Awareness ---------------------------------------------------------------

df2.2 <- main_data %>% 
  select(RecordNo, S1M1:S2M10) %>% pivot_longer(
    cols = matches("S[12]M",),
    names_to = c("S","index"),
    names_pattern = "(S1|S2)M(\\d+)") %>%
  mutate(index = as.numeric(index)) %>% 
  pivot_wider(names_from = S, values_from = value)

all <- all %>% left_join(df2.2 %>% select(-S2) %>% rename(company = S1))

all <- mutate(all, Awareness_score = 1) 
all <- mutate(all, Awareness_score = if_else(index == 1, 5, Awareness_score,
                                             missing = Awareness_score))
all <- mutate(all, Awareness_score = if_else(index >= 2, 4, Awareness_score,
                                             missing = Awareness_score)) 
all <- select(all, -index)

all <- all %>% left_join(df2.2 %>% select(-S1) %>% rename(company = S2))

all <- mutate(all, Awareness_score = if_else(!is.na(index), 2, Awareness_score,
                                           missing = Awareness_score))
all <- select(all, -index)   

# Familiarity -------------------------------------------------------------

df2.2 <- main_data %>% 
  select(RecordNo, S3M1:S3M10) %>%
  pivot_longer(S3M1:S3M10, names_to = "company",
               values_to = "Familiarity_score", names_pattern = "S3M(\\d+)") %>%
  mutate(Familiarity_score = as.numeric(Familiarity_score)) %>%
  mutate(company = as.numeric(company)) %>%
  mutate(company = company + 100) %>% 
  mutate(Familiarity_score = if_else(is.na(Familiarity_score), 1,  
                                     Familiarity_score)) 

all <- left_join(all, df2.2)

# Usage Index -------------------------------------------------------------

df2.3 <- main_data %>% 
  select(RecordNo, S4M1:S5M10) %>% 
  pivot_longer(cols = matches("S[45]M",), names_to = c("S","index"),
               names_pattern = "(S4|S5)M(\\d+)") %>%
  mutate(index = as.numeric(index)) %>% 
  pivot_wider(names_from = S, values_from = value)

all <- all %>% 
  left_join(df2.3 %>% select(-S5) %>% 
            rename(company = S4) %>% rename(LastSeason = index))

all <- all %>% 
  left_join(df2.3 %>% select(-S4) %>% 
            rename(company = S5) %>% rename(ThisSeason = index))

all <- all %>% mutate(Usage_score = case_when(
  is.na(LastSeason) & is.na(ThisSeason) ~ 1,
  !is.na(LastSeason) & is.na(ThisSeason) ~ 3,
  is.na(LastSeason) & !is.na(ThisSeason) ~ 4,
  !is.na(LastSeason) & !is.na(ThisSeason) ~ 5
)) %>% select(-LastSeason, -ThisSeason)

# Future Use --------------------------------------------------------------

df2.4 <- main_data %>% 
  select(RecordNo, S6M1:S6M10) %>% 
  pivot_longer(S6M1:S6M10, names_to = "company", 
               values_to = "FutureUsage_score", 
               names_pattern = "S6M(\\d+)") %>% 
  mutate(FutureUsage_score = as.numeric(FutureUsage_score)) %>% 
  mutate(company = as.numeric(company)) %>% 
  mutate(company = company + 100) %>% 
  mutate(FutureUsage_score = if_else(is.na(FutureUsage_score), 1,  
                                     FutureUsage_score)) 
 
all <- left_join(all, df2.4)

# Satisfaction ------------------------------------------------------------

df2.5 <- main_data %>% 
  select(RecordNo, S7M1:S7M10) %>%
  pivot_longer(S7M1:S7M10, names_to = "company", 
               values_to = "Satisfaction_score", 
               names_pattern = "S7M(\\d+)") %>%
  mutate(Satisfaction_score = as.numeric(Satisfaction_score)) %>%
  mutate(company = as.numeric(company)) %>%
  mutate(company = company + 100) %>% 
  mutate(Satisfaction_score = if_else(is.na(Satisfaction_score), 1,  
                                      Satisfaction_score)) 

all <- left_join(all, df2.5)

# Preference --------------------------------------------------------------

df2.6 <- main_data %>% 
  select(RecordNo, S8M1:S8M3) %>% 
  pivot_longer(S8M1:S8M3, names_to = "Preference_score", values_to = "company",
               names_pattern = "S8M(\\d+)") %>%
  mutate(Preference_score = as.numeric(Preference_score)) %>%
  mutate(Satisfaction_score = if_else(Preference_score == 3, 5, 
                                      Preference_score,
                                      missing = Preference_score)) %>% 
  mutate(Satisfaction_score = if_else(Preference_score == 2, 3, 
                                      Preference_score, 
                                      missing = Preference_score))

all <- left_join(all, df2.6)

# Code list ---------------------------------------------------------------

code <- c(101, 102, 103, 104, 105, 106, 107, 108, 109, 110)
company <- c("Jabłka", "Truskawki", "Śliwki", "Gruszki", "Wiśnie", "Borówki", 
           "Porzeczki", "Maliny", "Aronia", "Jagody")
code_list <- data.frame(code, company)

brand_equity <- all %>%
  select(company, Awareness_score, Familiarity_score, Usage_score, 
         FutureUsage_score, Satisfaction_score, Preference_score) %>%
  rename(code = company) %>%
  left_join(code_list, by = "code")

# Saving results ----------------------------------------------------------

write_csv(brand_equity, paste0(getwd(), '\\brand_equity.csv'))

# Visualization -----------------------------------------------------------

awareness_score <- brand_equity %>%
  group_by(company) %>%
  summarise(total_awareness_score = sum(Awareness_score))

ggplot(awareness_score, aes(x = company, y = total_awareness_score)) +
  geom_bar(stat = "identity") +
  labs(x = "Company", y = "Total awareness score", 
       title = "Histogram of total awareness score for each company")
  
familiarity_score <- brand_equity %>%
  group_by(company) %>%
  summarise(total_familiarity_score = sum(Familiarity_score))

ggplot(familiarity_score, aes(x = company, y = total_familiarity_score)) +
  geom_bar(stat = "identity") +
  labs(x = "Company", y = "Total familiarity score", 
       title = "Histogram of total familiarity score for each company")

usage_score <- brand_equity %>%
  group_by(company) %>%
  summarise(total_usage_score = sum(Usage_score))

ggplot(usage_score, aes(x = company, y = total_usage_score)) +
  geom_bar(stat = "identity") +
  labs(x = "Company", y = "Total usage score", 
       title = "Histogram of total usage score for each company")

futureusage_score <- brand_equity %>%
  group_by(company) %>%
  summarise(total_futureusage_score = sum(FutureUsage_score))

ggplot(futureusage_score, aes(x = company, y = total_futureusage_score)) +
  geom_bar(stat = "identity") +
  labs(x = "Company", y = "Total future usage score", 
       title = "Histogram of total future usage score for each company")

satisfaction_score <- brand_equity %>%
  group_by(company) %>%
  summarise(total_satisfaction_score = sum(Satisfaction_score))

ggplot(satisfaction_score, aes(x = company, y = total_satisfaction_score)) +
  geom_bar(stat = "identity") +
  labs(x = "Company", y = "Total satisfaction score", 
       title = "Histogram of total satisfaction score for each company")

preference_score <- (value = na.omit(brand_equity)) %>%
  group_by(company) %>%
  summarise(total_preference_score = sum(Preference_score))

ggplot(preference_score, aes(x = company, y = total_preference_score)) +
  geom_bar(stat = "identity") +
  labs(x = "Company", y = "Total preference score", 
       title = "Histogram of total preference score for each company")








































# Loading data set --------------------------------------------------------

data <-  read.csv2("Grupa.4.csv")

View(data)

# EDA ---------------------------------------------------------------------

head(data)
tail(data)
summary(data)
skim(data)
is.na(data)
duplicated(data)

main_data <- data %>%
  select(RecordNo, AgentId, AgentUserName, starts_with("S1M"), 
         starts_with("S2M"), starts_with("S3M"), starts_with("S4M"), 
         starts_with("S5M"), starts_with("S6M"), starts_with("S7M"), 
         starts_with("S8M"), starts_with("S9M"), S10, S11, S12, S13, S14A, S14B,
         RegionA)

summary(main_data)
skim(data)
rapply(main_data, function(x) length(unique(x)))

main_data$S14A <- as.numeric(main_data$S14A)
main_data$S14B <- as.numeric(main_data$S14B)

main_data[main_data == 0] <- NA
main_data[main_data == 999] <- NA
main_data[main_data == 99] <- NA

# Saving data to CSV ------------------------------------------------------

write_csv(main_data, paste0(getwd(), '\\main_data.csv'))









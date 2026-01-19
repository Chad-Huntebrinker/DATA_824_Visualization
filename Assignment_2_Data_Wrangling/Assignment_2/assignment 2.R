#Chad Huntebrinker
library(dplyr)
library(tidyr)
library(hflights)
library(lubridate)
head(hflights)

#Excersice 1
df1 <- hflights
df2 <- hflights
df3 <- hflights

#Table 1
df1$Cancelled <- as.logical(df1$Cancelled)

df1 %>% summarise(
  Number_of_Flights = n(),
  Percentage_Cancelled = (sum(Cancelled == TRUE) / n()) * 100
)

#Table 2
df2 <- df2 %>% 
  mutate(
    Cancelled = if_else(Cancelled == 1, "Cancelled", "Not cancelled")
  )
df2 %>% 
  count(UniqueCarrier, Cancelled) %>% 
  group_by(UniqueCarrier) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  select(UniqueCarrier, Cancelled, percentage) %>%
  pivot_wider(
    names_from = Cancelled,
    values_from = percentage,
    values_fill = 0
  ) %>%
  ungroup()

#Table 3
df3 <- df3 %>% filter(Cancelled == 1)
df3 %>% 
  count(UniqueCarrier, CancellationCode) %>% 
  pivot_wider(
    names_from = CancellationCode,
    values_from = n,
    values_fill = 0
  ) %>% 
  ungroup()

#Exercise 4
df4 <- hflights
df4 %>%
  mutate(Date = make_date(Year, Month, DayofMonth)) %>% 
  group_by(Date, Origin) %>% 
  summarise(number_of_flights = n())

#Chad Huntebrinker
library(dplyr)

tableA <- read.csv("Table1 (1).csv")
tableB <- read.csv("Table2.csv")

inner_join_data <- inner_join(tableA, tableB, by = "Accession")
left_join_data <- left_join(tableA, tableB, by = "Accession")
right_join_data <- right_join(tableA, tableB, by = "Accession")
full_join_data <- full_join(tableA, tableB, by = "Accession")  
#Chad Huntebrinker
library(ggplot2)
#data(mpg, overwrite = TRUE)

#Exercise 1
#Explore the dataset
head(mpg)
?mpg
str(mpg)
dim(mpg)

#Convert the character
character_columns <- names(mpg)[sapply(mpg, is.character)]
mpg[character_columns] <- lapply(mpg[character_columns], as.factor)
str(mpg)

#Summarize
summary(mpg)

#We're converting cyl to a factor (since the number of cyl is either 4, 6, 8)
mpg$cyl <- lapply(mpg$cyl, as.factor)

#Exercise 2
#Missing values for cty
sum(is.na(mpg$cty))

#Finding the most frequent cty value
table(mpg$cty)
#18 is the most frequent value, but is it the mean or median?
mean(mpg$cty)
median(mpg$cty)
#The mean and median are close to the value of 18

#Find the peak values of hwy
table(mpg$hwy)
#Looks like the peak values are 17 and 26
#The reason why there are multiple values for hwy is because cars with a lower fuel efficiency
#(larger cars like SUVs and trucks would be in this category) are around 17mpg and cars with
#a higher fuel efficiency (like sedans and smaller cars) are around 26mpg

#mean and median of hwy
mean(mpg$hwy)
median(mpg$hwy)
#It does show some useful information, like what the average mpg of all cars are. But it doesn't
#show the full story (like how there are two peaks) like the histogram does

#Create histograms
p1 <- ggplot(mpg, aes(x=cty)) +
  geom_histogram(fill="red") +
  labs(
    title = "Count of City MPG",
    x = "MPG in City",
    y = "Count"
  ) +
  theme_minimal()

p2 <- ggplot(mpg, aes(x=hwy)) +
  geom_histogram(fill="blue") +
  labs(
    title = "Count of Highway MPG",
    x = "MPG on Highway",
    y = "Count"
  ) +
  theme_minimal()

first_row <- cowplot::plot_grid(p1, nrow = 1)
second_row <- cowplot::plot_grid(p2, nrow = 1)

cowplot::plot_grid(p1, p2, ncol = 1, nrow = 2)

#Transform data from wide to long and then creat an area plot of MPG in city and highway
mpg_long <- mpg %>% 
  pivot_longer(cols = c(cty, hwy),
               names_to = "mileage_type",
               values_to = "mpg")


ggplot(mpg_long, aes(x = mpg, fill = mileage_type)) +
  geom_area(stat = "bin", binwidth = 1, alpha = 0.5, position = "identity") +
  labs(
    title = "City and Highway MPG",
    x = "MPG",
    y = "Count",
    fill = "Mileage Type"
  ) +
  theme_minimal()

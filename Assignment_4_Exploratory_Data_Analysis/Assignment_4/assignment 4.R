#Chad Huntebrinker
library(ggplot2)
library(tidyr)

head(mpg)

#Exercise 1
#Scatter plot
character_columns <- names(mpg)[sapply(mpg, is.character)]
mpg[character_columns] <- lapply(mpg[character_columns], as.factor)
str(mpg)

ggplot(data = mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  geom_smooth(method = lm, se = TRUE) +
  labs(
    title = "City vs Highway MPG",
    x = "City MPG",
    y = "Highway MPG"
  )

#rugplot and heatmap
ggplot(data = mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  geom_rug() + 
  labs(
    title = "Rug Map City vs Highway MPG",
    x = "City MPG",
    y = "Highway MPG"
  )

ggplot(data = mpg, aes(x = cty, y = hwy)) +
  geom_bin2d() +
  labs(
    title = "Heat Map of City vs Highway MPG",
    x = "City Highway",
    y = "Highway MPG"
  )
#What's the insight?
#Insight is that there is a linear relationship between city fuel efficiency and
#highway fuel efficiency. As the city MPG increases, so does the highway.

#Exercise 2:
p1 <- ggplot(data = mpg, aes(x = manufacturer, y = hwy)) +
  geom_boxplot(fill = "lightblue") +
  geom_jitter(color = "black") +
  labs(
    title = "Boxplot of Manfacturers' Highway MPG",
    x = "Manufacturers",
    y = "Highway MPG"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_hex() +
  labs(
    title = "Engine Displacement vs. Highway MPG",
    x = "Engine Displacement",
    y = "Highway MPG",
    fill = "Count"
  ) +
  theme_minimal()

p3 <- ggplot(data=mpg, aes(x=class, y=hwy)) +
  geom_violin(trim = FALSE) +
  labs(
    title = "Highway MPG by Car Class",
    x = "Car Class",
    y = "Highway MPG"
  ) +
  theme_minimal()

first_row <- cowplot::plot_grid(p1, p2, ncol = 2, nrow = 1)
second_row <- cowplot::plot_grid(p3, nrow = 1)

cowplot::plot_grid(first_row, second_row, ncol = 1, nrow = 2)


#Insight:
#I looks like Honda is the most fuel efficient company while
#Land Rover is the least. Also looks like there is negative
#relationship between highway MPG and engine displacement.
#As engine displacement increases, the highway mpg decreases.
#Finally it looks like compact cars are the most efficient car
#while pickups and SUVs are the least.

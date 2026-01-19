#Chad Huntebrinker
library(ggplot2)
library(tidyverse)

mpg_factors <- mpg %>%
  mutate(across(where(is.character), as.factor))

#Problem 1
ggplot(mpg_factors, aes(x = cty, y = hwy, color = displ)) +
  geom_point() +
  labs(
    title = "City vs. Highway MPG with Displacement as color",
    x = "City MPG",
    y = "Highway MPG",
    color = "Engine Displacement"
  ) +
  theme_minimal()

#Displacement with size
ggplot(data = mpg, aes(x = cty, y = hwy, size = displ)) +
  geom_point() +
  labs(title = "City vs. Highway MPG with Displacement as Size")

#I prefer the one with size because it seems easier to distinguish
#which displacment via size vs colors that are a little different

#Problem 2
ggplot(mpg, aes(x = cty, y = hwy, color = factor(cyl), shape = drv)) +
  geom_point(size = 3) +
  facet_wrap(~fl) +
  labs(
    title = "City vs. Highway MPG by Cylinders, Drivetrain, and Fuel Type",
    x = "City MPG",
    y = "Highway MPG",
    color = "Cylinders",
    shape = "Drivetrain"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")
#We see a couple of things:
#First, there a lot more cars that have the 'p' and 'r' fuel type than the other ones.
#We also see that as the number cylinders increase, the highway MPG efficiency decreases.
#We also see that cars with a front wheel drive train are the most efficient, followed by 
#4-wheel, and then rear-wheel
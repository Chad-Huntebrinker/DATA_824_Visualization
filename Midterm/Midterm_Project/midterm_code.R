#Chad Huntebriner
library(ggplot2)
library(readr)

#Get data
col_names <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
iris_data <- read_csv("iris.data", col_names = col_names)


#Good Plot
ggplot(iris_data, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point(size = 2) +
  labs(
    title = "Petal Length vs Petal Width for Iris Species",
    x = "Petal Length (cm)",
    y = "Petal Width (cm)",
    color = "Iris Species"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#This graph shows petal length vs petal width. It also color encodes based on the species. The graph title,
#labels, and axes are informative without becoming too overloaded.

#Bad Plot
ggplot(iris_data, aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length, size = Petal.Width)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Iris Measurements",
    x = "Sepal Length",
    y = "Sepal Width",
    color = "Petal Length",
    size = "Petal Width"
  ) +
  theme_minimal()

#This second graph is also showing petal length vs petal width. However, there isn't any grouping by species,
#there are overlapping points that lack transparency, and the color isn't as easy to separate which petals have
#a larger length. Also, the axex don't have a unit and the title of the graph isn't as clear as to what
#it's measuring.
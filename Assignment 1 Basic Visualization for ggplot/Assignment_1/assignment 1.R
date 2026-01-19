## scatter plot practice 
library(ggplot2)
##Load the data 
data("mtcars")

df<-mtcars

#convert cyl as a groupin variable 
df$cyl <- as.factor(df$cyl)

#Figure 1
ggplot(df, aes(x=wt,y=mpg))+
  geom_point(size = 8, position = "jitter", alpha = 1/5, color = "red") +
  ggtitle("Car fuel consumption") +
  xlab("Engine displacement (volume in liters)") +
  ylab("Highway miles per gallon (MPG)")+
  theme(plot.title = element_text(hjust = 0.5))

#Figure 2
ggplot(df, aes(x=wt,y=mpg, color=factor(am)))+
  geom_point(size = 8, position = "jitter", alpha = 0.2) +
  ggtitle("Car fuel consumption") +
  xlab("Engine displacement (volume in liters)") +
  ylab("Highway miles per gallon (MPG)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(
    values = c("0" = "blue", "1" = "red"),
    labels = c("Automatic", "Manual"),
    name = "Transmission"
  )

#Figure 3
ggplot(df, aes(x=cyl,y=mpg, color="red"))+
  geom_point(size = 7, alpha = 1/3) +
  ggtitle("Car fuel consumption") +
  xlab("Number of cylinders") +
  ylab("Highway miles per gallon (MPG)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

#Figure 4
ggplot(mpg, aes(x=class,y=hwy, color="red"))+
  geom_point(size = 7, position = "nudge", alpha = 1/3) +
  ggtitle("Car fuel consumption") +
  xlab("Car type") +
  ylab("Highway miles per gallon (MPG)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

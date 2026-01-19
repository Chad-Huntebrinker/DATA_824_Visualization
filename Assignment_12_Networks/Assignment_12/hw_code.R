#Chad Huntebrinker
library(tidyverse)
library(readxl)
library(igraph)
library(ggraph)
library(network)
library(sna)
library(visNetwork)
library(threejs)
library(networkD3)
library(ndtv)

drug_network_data <- read_xls("Interaction_drug_network.xls")

#Exercise 1
#Node type for shape/color mapping
g <- graph_from_data_frame(drug_network_data, directed = FALSE)

V(g)$type <- ifelse(V(g)$name %in% drug_network_data$BAIT, "BAIT", "PREY")
V(g)$shape <- ifelse(V(g)$type == "BAIT", "square", "circle")
V(g)$color <- ifelse(V(g)$type == "BAIT", "blue", "green")
E(g)$color <- drug_network_data$COLOR
E(g)$weight <- abs(drug_network_data$WEIGHT)

ggraph(g, layout = "circle") +
  geom_edge_link(aes(color = color)) +
  geom_node_point(aes(shape = type, color = color), size = 6) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

#Exercise 2
#Generate some data
drugnet <- data.frame(
  Drug = c("DrugA","DrugA","DrugB","DrugC","DrugC"),
  Target = c("P1","P3","P2","P1","P4"),
  Effect = c("inhibits","activates","inhibits","binds","activates")
)

g2 <- graph_from_data_frame(drugnet, directed = TRUE)

plot(g2, vertex.color="orange", vertex.size=25, edge.arrow.size=0.4, edge.label=drugnet$Effect,
     edge.label.cex=0.5)

#Created a dataset representing a drug–target interaction network.
#The goal is to model the relationships between two different types of entities:
#Drugs (small molecules or compounds)
#Targets (proteins or receptors they act on)
#Each interaction is represented as a directed connection from a drug to the protein it affects

#DrugA → P1 (inhibits)
#DrugA → P3 (activates)
#DrugB → P2 (inhibits)
#DrugC → P1 (binds)
#DrugC → P4 (activates)
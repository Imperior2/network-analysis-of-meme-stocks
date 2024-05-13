#@Author Marvin Kruber
library("igraph")
library("tidyverse")
################################################ Plot for Centralities in Background ######################################
edges <- c(1,2, 3,1, 3,2, 3,4, 3,5, 3,6, 6,7, 7,8, 7,10, 8,9, 9,10)

network <- make_graph(edges , n=10,  directed = FALSE)

#Assign names to the nodes
V(network)$name = c("A","B","C","D","E","F","G","H","I","J")
#Assign edge weights
E(network)$weight = 1/c(1:11)
summary(network)

# 5 x 5 as pdf size
par(mar=c(0,0,0,0)+.1)
plot(network, edge.label = round(E(network)$weight, 2), vertex.label.color=c("white"), edge.label.color = c("#d46d06"),
     vertex.label.font=2,
     vertex.color = "purple3",
     vertex.label.cex=c(1.2),
     margin = c(0,0,0,0))

centralities <- matrix(c(degree(network), strength(network), betweenness(network), closeness(network),
                         page_rank(network)$vector, transitivity(network, type = "local")), nrow=10, ncol = 6,
                       dimnames = list(V(network)$name, c("Degree", "Strength ","Betweenness", "Closeness", "Eigenvector", "Clustering Coefficient")))
#Strength is the weighted degree
print(centralities)

############################### Graph example, adjacency matrix and minimum spanning tree ####################################
data <- read.csv("./res/adjacency_matrix.csv", sep=";", row.names = "X0")
matrix <- as.matrix(data)
graph3 <- graph_from_adjacency_matrix(matrix, mode="undirected", weighted = TRUE)

#Use 5 x 5 as default pdf size
layout <- layout_nicely(graph3)
par(mar=c(0,0,0,0)+.1)
plot(graph3, layout=layout, edge.label=E(graph3)$weight, vertex.label.color=c("white"), edge.label.color = c("#d46d06"),
     vertex.label.font=2,
     vertex.color = "purple3",
     vertex.label.cex=c(1.2),
     margin = c(0,0,0,0))

Tree <- mst(graph3)
plot(Tree, layout=layout, edge.label=E(Tree)$weight, vertex.label.color=c("white"), edge.label.color = c("#d46d06"),
     vertex.label.font=2,
     vertex.color = "purple3",
     vertex.label.cex=c(1.2),
     margin = c(0,0,0,0))

#Closeness is calculated via 1/sum(d_i,j) not (n-1)/sum(d_i,j)

centralities_ex <- matrix(c(degree(graph3), strength(graph3), betweenness(graph3), betweenness(Tree), closeness(graph3), 
                         closeness(Tree), page_rank(graph3, weights = E(graph3)$weight)$vector, transitivity(graph3, type="local"),
                         transitivity(graph3, type ="weighted")), nrow=5, ncol = 9,
                       dimnames = list(V(graph3)$name, c("Degree", "Strength","Betweenness", "B_MST", "Closeness","C_MST", "Eigenvector", "Clustering Coefficient", "CC Weighted")))

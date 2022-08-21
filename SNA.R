library(igraph)
library(statnet)
library(intergraph)

#This reads the node CSV
d1 <- read.csv(file.choose(), header = T)
d1

d2 <- read.csv(file.choose(), header = T)
d2

#1
#This creates an igraph graph
d3 <- graph_from_data_frame(d=d2, directed= TRUE, vertices=d1)
class(d3)
summary(d3)
d3

#This transform d3 to network and stores it in a variable called net1
net1<-asNetwork(d3)
net1

#Getting the attributes
weights <- get.edge.attribute(net1, "weight")
summary(weights)
weights

colors <- c(paste0(rep("grey",80),seq(80,1)))

d3_indeg <- igraph::degree(d3, mode= c("in"))
d3_indeg

#Plotting the network
plot(d3,
     vertex.label.cex = 0.7,
     vertex.label.color="black",
     vertex.size = log(d3_indeg)*2,
     vertex.color = colors[igraph::degree(d3)],
     edge.width = E(d3)$weight,
     edge.arrow.size = 0.3,
     layout=layout.fruchterman.reingold)

#Determining the most prominent militant organization
colors <- c(paste0(rep("grey",80),seq(80,1)))
plot(d3,
     vertex.label.cex = 0.7,
     vertex.label.color="black",
     vertex.size = log(d3_indeg)*2,
     vertex.color = colors[igraph::degree(d3)],
     edge.arrow.size=0.3, 
     edge.width = E(d3)$weight,
     layout=layout.fruchterman.reingold)

#Ego Network
A_ego <- ego.extract(net1, ego = 26 , neighborhood = c("in"))
H_ego <- ego.extract(net1, ego = 3 , neighborhood = c("in"))
AQ_ego <- ego.extract(net1, ego = 51 , neighborhood = c("in"))


gplot(A_ego,
      gmode = "graph",
      mode="fruchtermanreingold",
      displayisolates = F,
      displaylabels=T,
      label.cex=0.8,
      main = "Al-Qaeda's Ego Network")

gplot(H_ego,
      gmode = "graph",
      mode="fruchtermanreingold",
      displayisolates = F,
      displaylabels=T,
      label.cex=0.8,
      main = "Hezbollah's Ego Network")

gplot(AQ_ego,
      gmode = "graph",
      mode="fruchtermanreingold",
      displayisolates = F,
      displaylabels=T,
      label.cex=0.8,
      main = "AQI's Ego Network")


#Community Detection Algorithms
wt <- cluster_walktrap(d3)
modularity(wt)

plot(wt, 
     d3,
     vertex.label.cex = 0.7,
     vertex.label.color = "black",
     vertex.size = 5,
     edge.arrow.size = 0.5,
     edge.color = "gray",
     layout=layout.fruchterman.reingold)

lp <- cluster_label_prop(d3)
modularity(lp)
membership(lp)

plot(lp, 
     d3,
     vertex.label.cex = 0.7,
     vertex.label.color = "black",
     vertex.size = 5,
     edge.arrow.size = 0.5,
     edge.color = "gray",
     layout=layout.fruchterman.reingold)

im <- cluster_infomap(d3)
modularity(im)

plot(im, 
     d3,
     vertex.label.cex = 0.7,
     vertex.label.color = "black",
     vertex.size = 5,
     edge.arrow.size = 0.5,
     edge.color = "gray",
     layout=layout.fruchterman.reingold)

op <- cluster_optimal(d3)
modularity(op)
membership(op)

plot(op, 
     d3,
     vertex.label.cex = 0.7,
     vertex.label.color = "black",
     vertex.size = log(d3_indeg)*2,
     edge.arrow.size = 0.5,
     edge.color = ifelse(E(d3)$weight >= 3, 'blue', 'red'),
     layout=layout.fruchterman.reingold)

library(dplyr)
library(ggplot2)
library(ggraph)
library(ggforce)
library(igraph)
library(reshape2)
cfec <- feather::read_feather("portfolio/data-generated/cfec.feather")
cfec$year <- as.numeric(cfec$year)

# start with 1985
cfec_1985 = filter(cfec, year %in% c(1985,2014)) %>%
  group_by(p_holder, p_fshy, year) %>%
  summarize(b = ifelse(sum(g_earn)>0,1,0))

# convert to wide
aql <- melt(cfec_1985, id.vars = c("p_fshy", "p_holder", "year"))
cfec_1985 <- dcast(aql, p_holder + year ~ p_fshy)
# replace NAs w/0s
for(i in 1:ncol(cfec_1985)) {
  cfec_1985[which(is.na(cfec_1985[,i])),i] = 0
}

# keep those with > 100 people
cfec_1985 = cfec_1985[,which(apply(cfec_1985,2,sum)>=100)]
cfec_1985 = cfec_1985[,-which(names(cfec_1985)%in%c("Q 11A","S 04X", "L 21C"))]

cfec_2014 = cfec_1985[which(cfec_1985$year==2014),-c(1:2)]
cfec_1985 = cfec_1985[which(cfec_1985$year==1985),-c(1:2)]

cfec_1985 = cfec_1985[sample(seq(1,nrow(cfec_1985)), size=200,replace=F),]
cfec_2014 = cfec_2014[sample(seq(1,nrow(cfec_2014)), size=200,replace=F),]

# Plot people as nodes
cfec_1985 = as.matrix(cfec_1985)
# transform into a term-term adjacency matrix
termMatrix <- cfec_1985 %*% t(cfec_1985)
# build a graph from the above matrix
g = graph.adjacency(termMatrix, weighted=T, mode="undirected")
# remove loops
g = simplify(g)
# set labels and degrees of vertices
V(g)$label = ""#V(g)$name
V(g)$degree = degree(g)

g1985 = g
layout1 <- layout.fruchterman.reingold(g)
plot(g1985, layout=layout1,vertex.size=3, vertex.color = "dodgerblue")
pdf("people_as_nodes_1985.pdf")
plot(g1985, layout=layout1,vertex.size=3, vertex.color = "dodgerblue")
dev.off()

# Plot people as nodes
cfec_2014 = as.matrix(cfec_2014)
# transform into a term-term adjacency matrix
termMatrix <- cfec_2014 %*% t(cfec_2014)
# build a graph from the above matrix
g = graph.adjacency(termMatrix, weighted=T, mode="undirected")
# remove loops
g = simplify(g)
# set labels and degrees of vertices
V(g)$label = ""#V(g)$name
V(g)$degree = degree(g)
g2014 = g
layout2 <- layout.fruchterman.reingold(g)
plot(g2014, layout=layout2,vertex.size=3, vertex.color = "dodgerblue")

pdf("people_as_nodes_2014.pdf")
plot(g2014, layout=layout2,vertex.size=3, vertex.color = "dodgerblue")
dev.off()

pdf("network_plot_2.pdf")
par(mfrow = c(1,2), mai=c(0.1,0.1,0.1,0.1))
plot(g1985, layout=layout1,vertex.size=3, vertex.color = "dodgerblue", main="1985")
plot(g2014, layout=layout2,vertex.size=3, vertex.color = "dodgerblue", main="2014")
dev.off()

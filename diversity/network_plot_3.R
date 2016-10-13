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
permits = names(cfec_1985)

# Plot permits as nodes
cfec_1985 = t(cfec_1985)
# transform into a term-term adjacency matrix
termMatrix <- cfec_1985 %*% t(cfec_1985)
colnames(termMatrix) = permits
diag(termMatrix) = 0
termMatrix[which(termMatrix< 10)] = 0
g = graph.adjacency(termMatrix, weighted=T, mode="undirected",diag=FALSE)

# remove loops
#g = simplify(g)
# set labels and degrees of vertices
V(g)$label = permits#V(g)$name
V(g)$degree = degree(g)
V(g)$size = degree(g)
colours <- colorspace::rainbow_hcl(length(unique(substr(permits,1,1))), start = 60, end = 240)
V(g)$color = colours[as.numeric(as.factor(substr(permits,1,1)))]

g1985 = g
pdf("permits_as_nodes_1985.pdf")
edgebundleR::edgebundle(g, tension = 0.1, fontsize = 15,nodesize = c(1,50))
dev.off()

# Plot permits as nodes
cfec_2014 = t(cfec_2014)
# transform into a term-term adjacency matrix
termMatrix <- cfec_2014 %*% t(cfec_2014)
colnames(termMatrix) = permits
diag(termMatrix) = 0
termMatrix[which(termMatrix< 10)] = 0
g = graph.adjacency(termMatrix, weighted=T, mode="undirected",diag=FALSE)

# remove loops
#g = simplify(g)
# set labels and degrees of vertices
V(g)$label = permits#V(g)$name
V(g)$degree = degree(g)
V(g)$size = degree(g)
colours <- colorspace::rainbow_hcl(length(unique(substr(permits,1,1))), start = 60, end = 240)
V(g)$color = colours[as.numeric(as.factor(substr(permits,1,1)))]

g2014 = g

pdf("permits_as_nodes_2014.pdf")
edgebundleR::edgebundle(g, tension = 0.1, fontsize = 15,nodesize = c(1,50))
dev.off()


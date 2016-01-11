
## Transform a group variable into a list of vertex indices
#to_vertex_ids_list <- function(var) {
#  sapply(unique(var), function(i) { (which(var==i)) })
#}


## EXAMPLES

library(igraph)

## Random graph
g <- sample_gnp(14, 0.35)
plot(g)
res <- reso(g)
res
plot_reso(g, res)

## Nolay graph
g2 <- read_graph(file="examples/graphs/nolay_comp1.txt", format="edgelist", directed=FALSE)
V(g2)$name <- as.character(c(30,31,77,32,110,34,90,70,103,109))
plot(g2)
res <- reso(g2)
plot_reso(g2, res)

## Gollac 1988 graph
g3 <- read_graph(file="examples/graphs/gollac_1988.txt", format="edgelist", n = 7, directed=TRUE)
V(g3)$name <- c("Inconnue", "Ouvriers", "Employés", "Interm", "Cadres", "Indep", "Agriculteurs")
plot(g3)
res <- reso(g3)
plot_reso(g3, res)
res <- reso(g3, type="strong")
plot_reso(g3, res)

## Directed graph, strong connexity
g3 <- read_graph(file="examples/graphs/directed_strong.txt", format="edgelist", directed=TRUE)
V(g3)$name <- as.character(c(13,7,1,11,12,9))
plot(g3)
res <- reso(g3, type="strong")
plot_reso(g3, res)




library(reso)
library(igraph)
context("RESO algorithm test")


## DATA -----------------------------------------------------------

## Nolay graph
nolay <- igraph::read_graph(file="graphs/nolay_comp1.txt",
                            format="edgelist", directed=FALSE)
V(nolay)$name <- as.character(c(30,31,77,32,110,34,90,70,103,109))

## Gollac 1988 graph
gollac <- igraph::read_graph(file="graphs/gollac_1988.txt",
                             format="edgelist", n = 7, directed=TRUE)
V(gollac)$name <- c("Inconnue", "Ouvriers", "Employés", "Interm", "Cadres", "Indep", "Agriculteurs")




## TESTS -----------------------------------------------------------

test_that("RESO for undirected gollac graph is correct", {
  expect_equal(reso(gollac, type = "weak"),
               c("WP_1", "WP_1", "1-AP_2", "CC_3", "CC_3", "CC_3", "CCsing_1"
  ))
})

test_that("RESO with 'weak' type for directed nolay graph is correct", {
  expect_equal(reso(nolay, type = "weak"),
               c("CCsing_4", "1-AP_3", "WP_1", "1-AP_3", "WP_1", "WP_2", "WP_1",
                 "WP_1", "WP_1", "WP_1")
               )
})

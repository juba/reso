library(reso)

context("AP, 1-AP and WP tests")

## DATA -----------------------------------------------------------

## Nolay graph
nolay <- igraph::read_graph(file="graphs/nolay_comp1.txt",
                            format="edgelist", directed=FALSE)
V(nolay)$name <- as.character(c(30,31,77,32,110,34,90,70,103,109))

## Social Networks 1994 graph
sn94 <- igraph::read_graph(file="graphs/social_networks_1994_p148.txt",
                           format="edgelist", n = 4, directed = TRUE)
V(sn94)$name <- letters[1:4]



## TESTS ----------------------------------------------------------

test_that("ap_wp with 'weak' type is correct for nolay graph", {
    expect_equal(ap_wp(nolay, type = "weak"),
                 structure(list(ap = c("31", "32", "34"),
                                ap_1 = c("31", "32", "34"),
                                wp = c("77", "110", "103", "109", "90", "70")),
                           .Names = c("ap", "ap_1", "wp")))
})


test_that("ap_wp with 'strong' type is correct for sn94 graph", {
  expect_equal(ap_wp(sn94, type = "strong"),
               structure(list(ap = c("b", "c"),
                              ap_1 = c("b", "c"),
                              wp = "d"),
                         .Names = c("ap", "ap_1", "wp")))
})

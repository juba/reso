#' Apply the RESO algorithm to a graph
#'
#' @param g an igraph graph
#' @param type connectedness type for directed graphs : "weak" or "strong". Ignored for undirected graphs
#'
#' @details
#' See the reference article for details and explanations about the algorithm.
#'
#' @return
#' Character vector with each vertex group. Groups are given in the form XX_YY,
#' where XX is the type of group ("WP" for weak points, "1-AP" for 1-order
#' articulation points, "CC" for connected components, and "CCsing" for
#' singleton connected components), and YY the level of the group (the iteration
#' during which the group has been produced).
#'
#' @references
#' Monique Dalud-Vincent, Michel Forsé, Jean-Paul Auray, "An algorithm for finding the structure of social groups", \emph{Social Networks}, 16 (1994) 137-162.
#' @export
#' @importFrom igraph V
#' @importFrom igraph V<-
#'
#' @examples
#' ## Random graph
#' g <- igraph::sample_gnp(14, 0.35)
#' plot(g)
#' res <- reso(g)
#' plot_reso(g, res)
#'

reso <- function(g, type = "weak") {

  ## Add temporay names if needed
  if (is.null(names(g))) {
    V(g)$name <- as.character(V(g))
  }

  ## Run algorithm
  groups <- reso_decompose(g, 0, NULL, NULL, type)
  names(groups) <- paste(names(groups), seq_along(names(groups)), sep = "_")

  ## Generate group variable
  group <- rep(NA, length(V(g)))
  for (n in names(groups)) {
    group[V(g)$name %in% groups[[n]]] <- n
  }

  group

}


#' RESO algorithm decomposition iteration
#'
#' Not to be called directly
#'
#' @param g an igraph graph
#' @param iter iteration number
#' @param ap1 list of previous non clustered 1-AP
#' @param ap list of previous non clustered ap
#'

reso_decompose <- function(g, iter, ap1, ap, type) {

  groups <- list()
  .group_name <- ""
  ## Iteration, for group naming
  iter <- iter + 1

  ## split the graph into CC
  comp <- igraph::components(g, mode = type)
  ## Create a group with all the CC singletons
  cc_sing_id <- which(comp$csize == 1)
  if (length(cc_sing_id) > 0) {
    cc_sing <- which(comp$membership %in% cc_sing_id)
    .group_name <- paste0(iter, "_CCsing")
    .group <- V(g)[cc_sing]$name
    groups[[.group_name]] <- .group
  }

  ## Compute all CCs not singleton
  cc_nsing_id <- which(comp$csize > 1)
  ccs <- list()
  for (id in cc_nsing_id) {
    cc_vertices <- which(comp$membership == id)
    cc <- igraph::induced_subgraph(g, cc_vertices)
    ccs[[length(ccs) + 1]] <- cc
  }

  ## for each CC not singleton
  for (cc in ccs) {
    group <- list()
    ## Compute AP, 1-AP and WP
    points <- ap_wp(cc, type = type)
    ## Add AP and 1-AP to list of not clustered previous AP and 1-AP
    ap1 <- unique(c(ap1, points$ap_1))
    ap <- unique(c(ap, points$ap))
    ## If there are WP, create a group with them
    if (length(points$wp) > 0) {
      .group <- points$wp
      .group_name <- paste0(iter, "_WP")
      group[[.group_name]] <- .group
    }
    ## Else if there are not clustered 1-AP, create a group with them
    else if (sum(cc_ap1 <- V(cc)$name %in% ap1) > 0) {
      .group <- V(cc)[cc_ap1]$name
      .group_name <- paste0(iter,"_1-AP")
      group[[.group_name]] <- .group
      ap1 <- setdiff(ap1, .group)
    }
    ## Else if there are not clustered AP, create a group with them
    else if (sum(cc_ap <- V(cc)$name %in% ap) > 0) {
      .group <- V(cc)[cc_ap]$name
      .group_name <- paste0(iter, "_AP")
      group[[.group_name]] <- .group
      ap <- setdiff(ap, .group)
    }
    ## Else, create a group with the whole CC
    else {
      .group <- V(cc)$name
      .group_name <- paste0(iter, "_CC")
      group[[.group_name]] <- .group
    }

    ## Add group to groups list
    groups <- c(groups, group)

    ## Apply the algorithm to the graph without the previously clustered vertices
    subcc <- cc - .group
    if (length(V(subcc)) > 0) groups <- c(groups, reso_decompose(subcc, iter, ap1, ap, type))

  }

  return(groups)

}



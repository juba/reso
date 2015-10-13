## Find AP, 1-AP and WP of a connected graph


#' Find Articulation points and weak point of a graph
#'
#' Find the articulation points (AP), the 1-order articulation
#' points (1-AP) and weak points of a graph.
#'
#' @param g a connected igraph graph
#' @param type connectedness type for directed graphs : "weak" or "strong". Ignored for undirected graphs
#'
#' @details
#' Articulation points are determined with the \code{\link[igraph]{articulation_points}} function.
#'
#' A 1-order articulation point is an articulation point which generates a component of cardinal 1
#' when removed.
#'
#' A weak point is a singleton component of a graph when one of its 1-order articulation points is
#' removed.
#'
#' @references
#' Monique Dalud-Vincent, Michel Forsé, Jean-Paul Auray, "An algorithm for finding the structure of social groups", \emph{Social Networks}, 16 (1994) 137-162.
#'
#' @return
#' A list with 3 elements :
#' \itemize{
#' \item \code{ap} is a character vector of articulation points names
#' \item \code{ap_1} is a character vector of 1-order articulation points names
#' \item \code{wp} is a character vector of weak points names
#' }
#' @export
#' @importFrom igraph V


ap_wp <- function(g, type = "weak") {

  if (!igraph::is_connected(g, mode = type)) stop("g must be connected")

  # initialization
  ap <- igraph::articulation_points(g)$name
  ap_1 <- NULL
  wp <- NULL
  ## For each AP
  for (vname in ap) {
    v <- V(g)[vname]
    .tmpg <- igraph::delete_vertices(g, v)
    ## Compute CC without this AP
    .tmpc <- igraph::components(.tmpg, mode = type)
    ## If one of the CC is a singleton
    if (1 %in% .tmpc$csize) {
      ## v is ap_1
      ap_1 <- append(ap_1, vname)
      ## singletons are wp
      .tmpc_sing_id <- which(.tmpc$csize == 1)
      .tmpc_sing <- which(.tmpc$membership %in% .tmpc_sing_id)
      wp <- append(wp, V(.tmpg)[.tmpc_sing]$name)
    }
  }
  list(ap = ap, ap_1 = ap_1, wp = wp)
}

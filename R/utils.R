#' Plot a graph with vertices colored by a group variable
#'
#' @param g an igraph graph
#' @param group vector of vertices group values
#'
#' @export
#'
#' @examples
#' ## Random graph
#' g <- igraph::sample_gnp(26, 0.15)
#' plot(g)
#' res <- reso(g)
#' plot_reso(g, res)
#'
plot_reso <- function(g, group) {
  group <- factor(group)
  plot(g, vertex.color = as.numeric(group))
  legend("topleft", levels(group), col=igraph::categorical_pal(nlevels(group)), pch=16)
}

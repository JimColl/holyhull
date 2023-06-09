#' @title ashape2poly
#' @description gratefully pilfered from alphahull
#' @param ashape PARAM_DESCRIPTION
#' @return pathX pts
#' @details Code gratefully pilfered from alphahull package so that I could expose parameters
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  [graph_from_edgelist][igraph::graph_from_edgelist], [is.connected][igraph::is.connected], [degree][igraph::degree], [clusters][igraph::clusters], [E][igraph::E], [get.shortest.paths][igraph::get.shortest.paths], [V][igraph::V]
#' @rdname ashape2poly
#' @export
#' @importFrom igraph graph_from_edgelist is.connected degree clusters E get.shortest.paths V

ashape2poly <- function(ashape){
  # Convert node numbers into characters
  ashape$edges[,1] <- as.character(ashape$edges[,1])
  ashape_graph <- igraph::graph_from_edgelist(ashape$edges[,1:2], directed = FALSE)
  if (!igraph::is.connected(ashape_graph)) {
    stop("Graph not connected")
  }
  if (any(igraph::degree(ashape_graph) != 2)) {
    stop("Graph not circular")
  }
  if (igraph::clusters(ashape_graph)$no > 1) {
    stop("Graph composed of more than one circle")
  }
  # Delete one edge to create a chain
  cut_graph <- ashape_graph - igraph::E(ashape_graph)[1]
  # Find chain end points
  ends = names(which(igraph::degree(cut_graph) == 1))
  path = igraph::get.shortest.paths(cut_graph, ends[1], ends[2])$vpath[[1]]
  # this is an index into the points
  pathX = as.numeric(igraph::V(ashape_graph)[path]$name)
  # join the ends
  pathX = c(pathX, pathX[1])
  return(pathX)
}

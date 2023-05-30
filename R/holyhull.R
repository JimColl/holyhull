#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param sf_frame PARAM_DESCRIPTION, Default: NULL
#' @param method PARAM_DESCRIPTION, Default: 'test'
#' @param alpha_value PARAM_DESCRIPTION, Default: 0.01
#' @param concavity PARAM_DESCRIPTION, Default: 2
#' @param length_threshold PARAM_DESCRIPTION, Default: 0
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  [Lines][sp::Lines], [SpatialLines][sp::SpatialLines], [Line][sp::Line], [SpatialPolygons][sp::SpatialPolygons], [Polygons][sp::Polygons], [Polygon][sp::Polygon]
#'  [anglesArc][alphahull::anglesArc], [ahull][alphahull::ahull]
#'  [graph_from_edgelist][igraph::graph_from_edgelist], [is.connected][igraph::is.connected], [degree][igraph::degree], [clusters][igraph::clusters], [E][igraph::E], [get.shortest.paths][igraph::get.shortest.paths], [V][igraph::V]
#'  [st_as_sf][sf::st_as_sf], [st_set_crs][sf::st_set_crs], [st_crs][sf::st_crs], [st_convex_hull][sf::st_convex_hull], [st_union][sf::st_union], [st_transform][sf::st_transform], [st_coordinates][sf::st_coordinates]
#'  [concaveman][concaveman::concaveman]
#' @rdname holyhull
#' @export
#' @import magrittr
#' @import data.table
#' @importFrom sp Lines SpatialLines Line SpatialPolygons Polygons Polygon
#' @importFrom alphahull anglesArc ahull
#' @importFrom igraph graph_from_edgelist is.connected degree clusters E get.shortest.paths V
#' @importFrom sf st_as_sf st_set_crs st_crs st_convex_hull st_union st_transform st_coordinates
#' @importFrom concaveman concaveman
holyhull = function(sf_frame=NULL,method='test',alpha_value=0.01, concavity = 2, length_threshold = 0) {
  # sf_frame=end_points
  # method='concave'
  # alpha_value=0.01
  # concavity = 2
  # length_threshold = 0

  fn_ahull2lines <- function(hull) {
    arclist <- hull$arcs
    lines <- list()
    for (i in 1:nrow(arclist)) {
      # Extract the attributes of arc i
      center_i <- arclist[i, 1:2]
      radius_i <- arclist[i, 3]
      vector_i <- arclist[i, 4:5]
      theta_i <- arclist[i, 6]
      # Convert arc i into a Line object
      line_i <- fn_arc2line(center = center_i, r = radius_i, vector = vector_i, theta = theta_i)
      list_length <- length(lines)
      if(list_length > 0){
        # If a line has already been added to the list of lines
        # Define last_line_coords as the coordinates of the last line added to the list before the ith line
        last_line_coords <- lines[[list_length]]@coords
      }
      if(i == 1){
        # Add the first line to the list of lines
        lines[[i]] <- line_i
      } else if(isTRUE(all.equal(line_i@coords[1,], last_line_coords[nrow(last_line_coords),]))){
        # If the first coordinate in the ith line is equal to the last coordinate in the previous line
        # then those lines should be connected
        # Row bind the coordinates for the ith line to the coordinates of the previous line in the list
        lines[[list_length]]@coords <- rbind(last_line_coords, line_i@coords[2:nrow(line_i@coords),])
      } else {
        # If the first coordinate in the ith line does not match the last coordinate in the previous line
        # then the ith line represents a new line
        # Add the ith line to the list as a new element
        lines[[length(lines) + 1]] <- line_i
      }
    }
    # Convert the list of lines to a Line object
    lines <- sp::Lines(lines, ID = 'l')
    # Convert the Line object to a SpatialLines object
    sp_lines <- sp::SpatialLines(list(lines))
    return(sp_lines)
  }
  fn_arc2line <- function(center, r, vector, theta, npoints = 100) {
    # Get the angles at the extremes of the arcs
    angles <- alphahull::anglesArc(vector, theta)
    # Generate sequence of angles along the arc to determine the points
    seqang <- seq(angles[1], angles[2], length = npoints)
    # Generate x coordinates for points along the arc
    x <- center[1] + r * cos(seqang)
    # Generate y coordinates for points along the arc
    y <- center[2] + r * sin(seqang)
    coords.xy <- cbind(x,y)
    line <- sp::Line(coords = coords.xy)
    return(line)
  }
  fn_spLines2poly <- function(sp_lines){
    # Extract the lines slot
    lines_slot <- sp_lines@lines[[1]]
    # Create a list of booleans indicating whether a given Line represents a polygon
    poly_bool <- sapply(lines_slot@Lines, function(x){
      coords <- lines_slot@Lines[[1]]@coords
      # Check if the first coordinate in the line is the same as the last
      all.equal(coords[1,], coords[nrow(coords),])
    })
    # Pull out the lines that form polygons
    poly_lines <- sp_lines[poly_bool]
    poly_lines_slot <- poly_lines@lines
    # Create SpatialPolygons
    sp_polys <- sp::SpatialPolygons(list(sp::Polygons(lapply(poly_lines_slot, function(x) {
      sp::Polygon(slot(slot(x, "Lines")[[1]], "coords"))
    }), ID = "1")))
    return(sp_polys)
  }
  fn_ashape2poly <- function(ashape){
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
  st_alpha_hull <- function(x_in_4326,y_in_4326,alpha_value=0.01){
    ahull = alphahull::ahull(x = x_in_4326,
                             y = y_in_4326,
                             alpha = alpha_value)
    ahull_lines = fn_ahull2lines(ahull)
    ahull_poly = fn_spLines2poly(ahull_lines) %>%
      sf::st_as_sf() %>%
      sf::st_set_crs(sf::st_crs("EPSG:4326"))
    return(ahull_poly)
  }

  if(method=='test') {
    # ahull_poly = concaveman::concaveman(batman_pts, concavity = 2, length_threshold = 0)
    # ahull_poly = st_alpha_hull(x = sf::st_coordinates(batman_pts)[,1],y = sf::st_coordinates(batman_pts)[,2],alpha = alpha_value)
    ahull_poly = sf::st_convex_hull(sf::st_union(batman_pts))
  }

  if(nrow(sf_frame)<3) {
    print("Riddle me this Batman, how do you make a polygon with fewer than 3 co-linear points?")
    return(FALSE)
  }

  IN_CRS <- sf::st_crs(sf_frame)
  if(!(IN_CRS==sf::st_crs("EPSG:4326"))){
    sf_frame <- sf::st_transform(sf_frame,sf::st_crs("EPSG:4326"))
  }

  if(method=='ahull') {
    ahull_poly = tryCatch(
      expr = {st_alpha_hull(x = sf::st_coordinates(sf_frame)[,1],y = sf::st_coordinates(sf_frame)[,2],alpha = alpha_value)},
      error = function(e){
        concaveman::concaveman(sf_frame, concavity = 2, length_threshold = 0)
      }
    )
  } else if(method=='concave') {
    ahull_poly = tryCatch(
      expr = {concaveman::concaveman(sf_frame, concavity = 2, length_threshold = 0)},
      error = function(e){
        sf::st_convex_hull(sf::st_union(sf_frame))
      }
    )
  } else {
    ahull_poly = sf::st_convex_hull(sf::st_union(sf_frame))
  }

  if(!is(sf::st_as_sf(ahull_poly), "sf")) {
    print("Where's the invisible boatmobil again?")
    return(FALSE)
  }

  return(sf::st_as_sf(ahull_poly))
}

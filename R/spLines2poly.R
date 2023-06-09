#' @title spLines2poly
#' @description gratefully pilfered and wrapper-d around alphahull
#' @param sp_lines pass through for hull object
#' @return sp_polys
#' @details Code gratefully pilfered from alphahull package so that I could expose parameters
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  [SpatialPolygons][sp::SpatialPolygons], [Polygons][sp::Polygons], [Polygon][sp::Polygon]
#' @rdname spLines2poly
#' @export
#' @import magrittr
#' @importFrom sp SpatialPolygons Polygons Polygon

spLines2poly <- function(sp_lines){
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

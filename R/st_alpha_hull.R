#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x_in_4326 PARAM_DESCRIPTION
#' @param y_in_4326 PARAM_DESCRIPTION
#' @param alpha_value PARAM_DESCRIPTION, Default: 0.01
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  [ahull][alphahull::ahull]
#'  [st_as_sf][sf::st_as_sf], [st_set_crs][sf::st_set_crs], [st_crs][sf::st_crs], [st_transform][sf::st_transform]
#' @rdname st_alpha_hull
#' @export
#' @importFrom alphahull ahull
#' @importFrom sf st_as_sf st_set_crs st_crs st_transform

st_alpha_hull <- function(x_in_4326,y_in_4326,alpha_value=0.01){
  ahull = alphahull::ahull(x = x_in_4326,
                           y = y_in_4326,
                           alpha = alpha_value)
  ahull_lines = fn_ahull2lines(ahull)
  ahull_poly = fn_spLines2poly(ahull_lines) %>%
    sf::st_as_sf() %>%
    sf::st_set_crs(sf::st_crs("EPSG:4326")) %>%
    sf::st_transform(sf::st_crs("EPSG:6349"))
  return(ahull_poly)
}

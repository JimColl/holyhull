#' @title st_alpha_hull
#' @description gratefully pilfered and wrapper-d around alphahull
#' @param x_in_4326 x_in_4326
#' @param y_in_4326 y_in_4326
#' @param alpha_value alpha_value to pass to alphahull package, Default: 0.01
#' @return hull in 6349 (a compound crs with an explicit vertical datum)
#' @details Code gratefully pilfered from alphahull package so that I could expose parameters, and renamed incorrectly so that I don't have to wonder what to start my hull auto completes with
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
#' @import magrittr
#' @import data.table
#' @importFrom alphahull ahull
#' @importFrom sf st_as_sf st_set_crs st_crs st_transform

st_alpha_hull <- function(x_in_4326,y_in_4326,alpha_value=0.01){
  ahull = alphahull::ahull(x = x_in_4326,
                           y = y_in_4326,
                           alpha = alpha_value)
  ahull_lines = ahull2lines(ahull)
  ahull_poly = spLines2poly(ahull_lines) |>
    sf::st_as_sf() |>
    sf::st_set_crs(sf::st_crs("EPSG:4326")) |>
    sf::st_transform(sf::st_crs("EPSG:6349"))
  return(ahull_poly)
}

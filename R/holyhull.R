#' @title holyhull
#' @description A wrapper around several useful hull creation tools and utilities with a common entry point
#' @param sf_frame sf points in 4236 that you want to wrap, Default: NULL
#' @param method string to control what hull to generate. One of 'test' for a comparison map concave for concaveman ahull for alphahull or, Default: 'test'
#' @param alpha_value Alpha value to pass to alphahull execution, Default: 0.01
#' @param concavity concavity value to pass to concaveman execution, Default: 2
#' @param length_threshold PARAM_DESCRIPTION, Default: 0
#' @return requested hull or st_convex_hull if requested hull errors out
#' @details master wrapper through which data is accessed
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

  if(method=='test') {
    batman_pts <- make_batman_pts()

    concave_poly = concaveman::concaveman(batman_pts, concavity = concavity, length_threshold = length_threshold)
    ahull_poly = st_alpha_hull(x = sf::st_coordinates(batman_pts)[,1],y = sf::st_coordinates(batman_pts)[,2],alpha = alpha_value) |> sf::st_transform(sf::st_crs("EPSG:4326"))
    st_poly = sf::st_convex_hull(sf::st_union(batman_pts))

    batmap <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
      # leaflet::addProviderTiles("OpenStreetMap",group = "OpenStreetMap") |>
      # leaflet::addProviderTiles("Stamen.Toner",group = "Stamen.Toner") |>
      leaflet::addProviderTiles("Stamen.Terrain",group = "Stamen.Terrain") |>
      # leaflet::addProviderTiles("Esri.WorldStreetMap",group = "Esri.WorldStreetMap") |>
      # leaflet::addProviderTiles("Wikimedia",group = "Wikimedia") |>
      # leaflet::addProviderTiles("CartoDB.Positron",group = "CartoDB.Positron") |>
      # leaflet::addProviderTiles("Esri.WorldImagery",group = "Esri.WorldImagery") |>
      leafem::addFeatures(st_poly, color = "red",fillColor = 'red',fillOpacity = 0.1,group = "st_convex") |>
      leafem::addFeatures(ahull_poly, color = "gold",fillColor = 'gold',fillOpacity = 0.1,group = "ahull") |>
      leafem::addFeatures(concave_poly, color = "black",fillColor = 'black',fillOpacity = 0.1,group = "concaveman") |>
      leaflet::addCircleMarkers(lng = unlist(purrr::map(batman_pts$geometry,1)),
                                             lat= unlist(purrr::map(batman_pts$geometry,2)),
        radius = 2,
        color = "yellow",
        stroke = FALSE, fillOpacity = 0.8
      ) |>
      leaflet::addLegend("bottomright",colors = c("yellow","black","gold","red"),
                         labels = c("Points", "concaveman","alphahull","convex hull"),title = "Holy Hull Batman!",opacity = 1)
    mapview::mapshot(batmap,file=file.path("C:/Users/jimma/Desktop/map/RRASSLER_images",glue::glue('temp.png')))

    title <- cowplot::ggdraw() + cowplot::draw_label(glue::glue("XS id: {id} from model: {basename(gfile_path)}"), fontface='bold')
    main_grid <- cowplot::plot_grid(
      g_plot + ggplot2::ggtitle("Parsed from G file") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 1.0)),
      h_plot + ggplot2::ggtitle("DEM values") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 1.0)),

      union_plot + ggplot2::ggtitle("Overlayed data") + ggplot2::theme(legend.position="bottom",plot.title = ggplot2::element_text(hjust = 1.0)),
      ggplot2::ggplot() + ggplot2::ggtitle("Selected XS") + cowplot::draw_image(image = magick::image_read(file.path(outpath,"RRASSLER_images",glue::glue('temp_{id}.png'))),scale = 1.2) + ggplot2::theme_void() + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 1.0)),

      align = "hv",
      axis = 'tblr',
      # label_fontface = "bold",
      # label_fontfamily = "Times New Roman",
      label_size = 8,
      rel_widths = c(1, 1),
      rel_heights = c(1,1),
      ncol = 2,
      nrow = 2,
      hjust = 0.5,
      vjust = 0.5
      # label_x = 0.01
    )
    final_plot <- cowplot::plot_grid(title,
                                     main_grid,
                                     ncol=1,
                                     rel_heights=c(0.1, 1))
    ggplot2::ggsave(
      filename = glue::glue("{id}.png"),
      plot = final_plot,
      device = "png",
      path = file.path(outpath,"RRASSLER_images", fsep=.Platform$file.sep),
      scale = 4,
      width = 600,
      height = 810,
      units = "px",
      dpi = 300,
      bg = "white"
    )

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
        sf::st_convex_hull(sf::st_union(sf_frame))
      }
    )
  } else if(method=='concave') {
    ahull_poly = tryCatch(
      expr = {concaveman::concaveman(sf_frame, concavity = concavity, length_threshold = length_threshold)},
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

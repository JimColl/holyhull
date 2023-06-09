#' @title make_batman_pts
#' @description Returns a toy set of batman themed points which are used in test plotting.

#' @return geojson formatted point type FeatureCollection
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  [geojson_sf][geojsonsf::geojson_sf]
#' @rdname make_batman_pts
#' @export
#' @importFrom geojsonsf geojson_sf

make_batman_pts = function() {
  batman_pts <- '{
  "type": "FeatureCollection",
  "name": "batman",
  "crs": { "type": "name", "properties": { "name": "urn:ogc:def:crs:OGC:1.3:CRS84" } },
  "features": [
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.146147320410094, 40.570953185916579 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.147665150965423, 40.570472790108127 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.149119738580936, 40.569560028570841 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.150890540895503, 40.567926634747003 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.151712699112991, 40.567013838481316 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.152724586149873, 40.565044077805268 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.153230529668306, 40.563650797445426 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.153167286728518, 40.561921167684574 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.144376518095541, 40.560816103049909 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.135269534763538, 40.55432948632312 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.126858223769375, 40.561344614499049 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.118257183955819, 40.562257488082828 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.120470686849032, 40.566965796227727 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.122557703862597, 40.569079622761087 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.126668494949968, 40.570520829844163 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.125530122033467, 40.567734468146732 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.13109550073635, 40.567013838481316 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.133751704208194, 40.567350133290802 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.134573862425668, 40.571049264664417 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.13558574946255, 40.568983541185297 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.136850608258669, 40.568983541185297 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.137799252355748, 40.570664948845355 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.138811139392644, 40.566965796227727 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.148044608604252, 40.568310966292202 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.153167286728518, 40.564131242225002 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.151333241474148, 40.56220944241511 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.148613795062502, 40.562305533716071 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.146653263928542, 40.562065305205032 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.144882461613975, 40.561296568176111 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.142479229901355, 40.561344614499049 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.140392212887775, 40.561296568176111 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.13855816763342, 40.56072000961084 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.136913851198472, 40.559374686980036 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.1361549359208, 40.558317628803252 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.135712235342183, 40.556732010242932 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.14785487978483, 40.569656109318935 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.148171094483871, 40.569127663497255 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.147412179206214, 40.567782509848527 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.144692732794553, 40.567013838481316 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.142668958720776, 40.566677541981988 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.140392212887775, 40.566869711617095 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.138937625272249, 40.567061880700408 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.137988981175155, 40.569223744866122 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.13830519587421, 40.567878593148663 ] } },
  { "type": "Feature", "properties": { "id": null }, "geometry": { "type": "Point", "coordinates": [ -105.136091692981012, 40.569079622761087 ] } }
  ]
  }' |> geojsonsf::geojson_sf()
  return(batman_pts)
}

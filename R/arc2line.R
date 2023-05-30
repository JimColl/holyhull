#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param center PARAM_DESCRIPTION
#' @param r PARAM_DESCRIPTION
#' @param vector PARAM_DESCRIPTION
#' @param theta PARAM_DESCRIPTION
#' @param npoints PARAM_DESCRIPTION, Default: 100
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  [anglesArc][alphahull::anglesArc]
#'  [Line][sp::Line]
#' @rdname arc2line
#' @export
#' @importFrom alphahull anglesArc
#' @importFrom sp Line

arc2line <- function(center, r, vector, theta, npoints = 100) {
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

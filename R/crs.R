# Helpers for geographic coordinate reference systems (CRSes)

#' @export
crs.lonlat = 4326 # https://epsg.io/4326

#' @export
crs.us.atlas = 2163 # https://epsg.io/2163

#' @export
convert.crs = function(d, from, to, sf = F)
  # Given a 2-column data frame or matrix of points in the CRS `from`,
  # return a data table with columns "x" and "y" in the CRS `to`, or
  # an `sf` object if `sf` is true.
   {assert(ncol(d) == 2)
    orig = data.table::as.data.table(d)
    setnames(orig, c("x", "y"))
    orig[, by = .(x, y), index := .GRP]
    result = sf::st_transform(crs = to, sf::st_as_sf(
        unique(orig[, .(x, y)]),
        coords = c("x", "y"), crs = from))
    if (sf)
        result[orig$index,]
    else
        data.table::as.data.table(`colnames<-`(value = c("x", "y"),
            sf::st_coordinates(result)))[orig$index]}

suppressPackageStartupMessages(
   {library(sf)})

grid.as.sf = function(grid.df, x.col, y.col, attr.cols, crs)
  # Given a data frame or data table describing a grid of squares with
  # one centroid per row, return an `sf` object of the corresponding
  # polygons. The grid need not be rectangular.
   {x = `[.data.frame`(grid.df, , x.col)
    y = `[.data.frame`(grid.df, , y.col)
    st_sf(
        `[.data.frame`(grid.df, , attr.cols, drop = F),
        geometry = st_sfc(crs = crs, mapply(SIMPLIFY = F, square.xyd,
            x,
            y,
            median(c(
                diff(sort(unique(x))),
                diff(sort(unique(y))))))))}

square.xyd = function(x, y, d)
  # Create a polygon object representing a square from the x- and
  # y-coordinates of the center and its side length `d`.
   st_polygon(list(matrix(ncol = 2, byrow = T, c(
       x + d/2, y + d/2,    # NE
       x - d/2, y + d/2,    # NW
       x - d/2, y - d/2,    # SW
       x + d/2, y - d/2,    # SE
       x + d/2, y + d/2)))) # NE

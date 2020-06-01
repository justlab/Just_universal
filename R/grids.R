suppressPackageStartupMessages(
   {library(sf)})

#' @export
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

#' @export
patchwork.regionalize = function(x, y, n.regions)
  # Given cells in a square grid, provided by `x` and `y`
  # vectors, split the area into `n.regions` regions that are
  # roughly rectangular, roughly contiguous, and comprising equal
  # numbers of cells (except cells left over after division by
  # `n.regions`). Return a vector of the regions, numbered 1 through
  # `n.regions`.
   {suppressPackageStartupMessages(
       {library(data.table)})

    stopifnot(length(x) == length(y))
    stopifnot(n.regions <= length(x))

    mg = data.table(
        x = 1 + round((x - min(x)) / median(diff(sort(unique(x))))),
        y = 1 + round((y - min(y)) / median(diff(sort(unique(y))))))
    stopifnot(nrow(unique(mg)) == nrow(mg))

    OUT.OF.BOUNDS = -1
    UNASSIGNED = 0

    m = matrix(OUT.OF.BOUNDS, nrow = max(mg$x), ncol = max(mg$y))
    m[mg[, cbind(x, y)]] = UNASSIGNED

    x.max = nrow(m)
    y.max = ncol(m)

    x.start = min(mg$x)
    y.start = mg[x == x.start, min(y)]

    lapply(1 : n.regions, function(region)
       {to.add = nrow(mg) %/% n.regions +
            (region <= nrow(mg) %% n.regions)
        added = 0
        for (d in 0 : max(x.max, y.max))
           {for (dx in -d : d)
               {x = x.start + dx
                if (x < 1 || x > x.max)
                    next
                for (y in y.start +
                         (if (abs(dx) == d) (-d : d) else c(-d, d)))
                    {if (y < 1 || y > y.max)
                         next
                     if (m[x, y] == UNASSIGNED)
                        {if (added == to.add)
                            {x.start <<- x
                             y.start <<- y
                             return()}
                         m[x, y] <<- region
                         added = added + 1}}}}})

    m[mg[, cbind(x, y)]]}

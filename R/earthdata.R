if (!exists("earthdata.urls.cache"))
    earthdata.urls.cache = new.env(parent = emptyenv())

#' @export
get.earthdata = function(root.dir, bbox, products, satellites, tiles, dates)
  # Download the requested Earthdata satellite products (as an HDF
  # file) for the Cartesian product of the specified products, satellites,
  # tiles, and dates. For monthly products, round down dates to the
  # 1st of the month. If a file already exists locally, it isn't
  # re-downloaded.
  #
  # Return a data table with a column `path` that gives the paths of
  # the downloaded files.
  #
  # When no file exists to be downloaded for a given case, an empty
  # file is created as an indicator that we checked for it.

   {# Check and normalize arguments.
    assert(dir.exists(root.dir))
    assert("bbox" %in% class(bbox))
    assert(sf::st_crs(bbox) == sf::st_crs(crs.lonlat))
    assert(products %in% c("MxD13A3_061", "MxD21A1D_061", "MxD21A1N_061"))
    monthly = products == "MxD13A3_061"
    if (monthly)
        dates = lubridate::floor_date(dates, "month")
    assert(all(satellites %in% c("terra", "aqua")))
    for (vname in c("products", "satellites", "tiles", "dates"))
        assign(vname, unique(get(vname)))
    assert(all(str_detect(tiles, "\\Ah[0-9][0-9]v[0-9][0-9]\\z")))

    # Enumerate the files to be downloaded.
    files = data.table::CJ(
        product = factor(products),
        satellite = factor(satellites),
        date = dates,
        tile = factor(tiles))
    files[, real.product := str_replace(product, "x",
        ifelse(as.character(satellite) == "terra", "O",
        ifelse(as.character(satellite) == "aqua", "Y",
        "")))]
    files[, path := file.path(root.dir,
        real.product, year(date), tile, paste0(date, ".hdf"))]

    # Download one file at a time.
    if (!all(file.exists(files$path)))
       {# Get applicable URLs.
        d = files[!file.exists(path)]
        yps = unique(d[, .(the.year = year(date), product = real.product)])
        d = merge(
            d,
            rbindlist(lapply(1 : nrow(yps), \(i)
                earthdata.urls(yps[i, the.year], yps[i, product], bbox))),
            by = c("real.product", "date", "tile"),
            all.x = T)
        # Use only the alphabetically first URL per product-day-tile.
        # This rule is arbitrary, but we gotta pick somehow.
        d = d[order(url), by = .(real.product, date, tile), head(.SD, 1)]

        message(sprintf("Downloading from Earthdata: %s in %s .. %s (n = %s)",
            paste(unique(d$real.product), collapse = ", "),
            min(d$date),
            max(d$date),
            comma(nrow(d))))
        curl.args = c(
            "--silent", "--show-error", "--fail",
            "--retry", 10,
            "--retry-connrefused", "--ipv4",
              # Forcing IPv4 may be necessary to get
              # `--retry-connrefused` to work:
              # https://github.com/curl/curl/issues/5080
            earthdata.creds())
        pbapply::pboptions(type = "timer")
        pbapply::pbwalk(1 : nrow(d), \(di)

           {dir.create(dirname(d[di, path]), showWarnings = F, recursive = T)
            if (!is.na(d[di, url]))
                # Download the file.
                assert(0 == system2("curl", shQuote(c(
                    d[di, url],
                    "--remote-time",
                    "--output", d[di, path],
                    curl.args))))
            else
                # Make a stub.
                cat("", file = d[di, path])})}

    files[, real.product := NULL][]}

earthdata.urls = function(the.year, product, bbox)
# Use the SpatioTemporal Asset Catalog (STAC) interface to get the URL
# of every Earthdata file we want and the corresponding date.
   {bbox = with(as.list(bbox),
        paste(sep = ",", xmin, ymin, xmax, ymax))

    k = paste(the.year, product, bbox)
    if (k %in% names(earthdata.urls.cache))
        return(earthdata.urls.cache[[k]])

    base.url = "https://cmr.earthdata.nasa.gov/stac/LPCLOUD/search"
    max.good.limit = 100

    message("Fetching Earthdata URLs: ", product)
    out = list()
    progress = NULL
    r = httr::GET(base.url, query = list(
        collections = product,
        limit = max.good.limit,
        bbox = I(bbox),
        datetime = I(sprintf("%d-01-01T00:00:00Z/%d-12-31T23:59:59Z",
            the.year, the.year))))
    repeat
       {httr::stop_for_status(r)
        r = jsonlite::fromJSON(httr::content(r, "text"), simplifyVector = F)
        if (is.null(progress))
           {progress = pbapply::startpb(max =
                ceiling(r$context$matched / max.good.limit) - 1)
            progress.i = 0}
        else
            pbapply::setpb(progress, (progress.i <- progress.i + 1))
        out = c(out, lapply(r$features, \(item) list(
            real.product = product,
            date = as.Date(item$properties$datetime),
            tile = factor(str_match(item$id, "h[0-9][0-9]v[0-9][0-9]")),
            url = unlist(lapply(item$assets, \(x)
               if (x$title == "Direct Download")
                   x$href)))))
        if (length(out) >= r$context$matched)
           {close(progress)
            break}
        l = r$links[[length(r$links)]]
        assert(l$rel == "next")
        r = httr::GET(l$href)}

    out = rbindlist(out)
    assert(!anyNA(out))
    earthdata.urls.cache[[k]] = setkey(out, real.product, date, tile, url)}

#' @export
earthdata.creds = function()
   {creds = Sys.getenv(names = F,
        c("EARTHDATA_USERNAME", "EARTHDATA_PASSWORD"))
    if (any(creds == ""))
        stop("You need to set the environment variables EARTHDATA_USERNAME and EARTHDATA_PASSWORD. If you don't have an account, you can get one at https://urs.earthdata.nasa.gov/users/new . You may also need to approve the \"NASA GESDISC DATA ARCHIVE\" and \"SEDAC Website\" applications; see https://disc.gsfc.nasa.gov/earthdata-login for instructions.")
    c("--user", paste0(collapse = ":", creds),
        "--cookie", "",
        "--anyauth",
        "--location-trusted")}

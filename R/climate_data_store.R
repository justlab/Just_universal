# Get a variable from the Climate Data Store and compute the daily
# mean (or the daily min, mean, and max) for each date and (lon, lat)
# pair in a data table. Set `hours` to use only a subset of hours
# (e.g., a single hour) for each date.
#
# Variable names and descriptions for one dataset can be found at
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=overview

#' @export
add.daily.var.from.climate.data.store = function(
        d, vname.in, dataset, vname.out,
        target.tz, area,
        download.dir, download.filename.fmt,
        hours = 0:23,
        monthly = F,
          # When false, downloads are grouped by year, which is
          # probably more efficient than grouping by month, but in
          # some cases is too big for CDS to accept the request.
        progress = F)
   {assert(all(c("lon", "lat", "date") %in% colnames(d)))
    assert(length(vname.out) %in% c(1, 3))
    tri.output = length(vname.out) == 3
      # With `tri.output`, produce the min, mean, and max per day and
      # location. Otherwise, produce only the mean.

    message("Finding time periods")
    get.period = function(dates)
        paste0(year(dates), if (monthly)
            sprintf("-%02d", month(dates)) else
            "")
    periods = sort(unique(get.period(unique(pmin(
        lubridate::today("UTC"),
        c(d$date, min(d$date) - 1, max(d$date) + 1))))))
          # Peek one day ahead and behind in case we need it for
          # time-zone reasons.

    message("Getting rasters")
    rasts = as.environment(sapply(simplify = F, periods, \(p)
       {if (monthly)
           {p = as.integer(strsplit(p, "-")[[1]])
            the.year = p[1]
            the.month = p[2]}
        else
           {the.year = as.integer(p)
            the.month = NULL}
        terra::rast(climate.data.store.file(
            the.year, the.month,
            area, vname.in, dataset,
            download.dir, download.filename.fmt))}))

    message("Finding cells")
    d[, cds.cell.ix := terra::cellFromXY(
        rasts[[names(rasts)[1]]],
        cbind(d$lon, d$lat))]

    message("Getting values")
    if (progress)
       {pbar = pbapply::startpb(max = length(unique(d$date)))
        if (is.null(pbar))
            progress = F}
    d[, by = date, (vname.out) :=
      # The data is grouped into one raster per period (year or month)
      # with one band per UTC hour. For each location, take the mean
      # (or min, mean, and max, with `tri.output`) of all hours on the
      # requested date in `target.tz`.
       {datetimes = lubridate::with_tz(tz = "UTC", lubridate::make_datetime(
            year(date), month(date), mday(date),
            hours, tz = target.tz))
        period.ixs = match(get.period(date), periods) + (-1 : 1)
          # We have to look at the previous and next period for
          # time-zone issues.
        values = do.call(cbind, lapply(period.ixs, function(period.ix)
           {if (period.ix == 0)
                return(NULL)
            r = rasts[[periods[period.ix]]]
            if (is.null(r))
                NULL
            else
               {i = as.integer(na.omit(match(datetimes, terra::time(r))))
                if (length(i))
                    data.table::as.data.table(terra::extract(r[[i]], cds.cell.ix))
                else
                    NULL}}))
        out = (
            if (!is.null(values) && ncol(values) == length(hours))
               (if (tri.output)
                    list(apply(values, 1, min), rowMeans(values), apply(values, 1, max))
                else
                    rowMeans(values))
            else if (tri.output)
                list(NA_real_, NA_real_, NA_real_)
            else
                NA_real_)
        if (progress)
            pbapply::setpb(pbar, .GRP)
        out}]

    d[, cds.cell.ix := NULL]
    if (progress)
        close(pbar)}

#' @export
climate.data.store.file = function(
        the.year, the.month, area, vname.in, dataset,
        download.dir, download.filename.fmt)
   {fname = do.call(sprintf, c(list(download.filename.fmt, the.year),
        if (!is.null(the.month)) the.month))
    fpath = file.path(download.dir, fname)
    if (!file.exists(fpath))
       {message(sprintf("Downloading %s-%s %s from Climate Data Store.",
            the.year,
            (if (is.null(the.month)) "all-months" else the.month),
            vname.in))
        message("    (This may take a while.)")
        reply = jsonlite::fromJSON(system2("curl", stdout = T, shQuote(c(
            paste0("https://cds.climate.copernicus.eu/api/v2/resources/",
                dataset),
            "--fail",
            "--user-agent", "some-program",
            climate.data.store.creds(),
            "-d", jsonlite::toJSON(auto_unbox = T, digits = NA, c(
                list(
                    year = the.year,
                    month = (if (is.null(the.month)) 1 : 12 else the.month),
                    day = 1 : 31,
                    time = sprintf("%02d:00", 0 : 23),
                    format = "netcdf",
                    product_type = "reanalysis",
                    variable = vname.in,
                    area = area),
                if (dataset == "reanalysis-era5-single-levels")
                    list(grid = c(0.125, 0.125))))))))
        while (reply$state %in% c("queued", "running"))
           {Sys.sleep(15)
            reply = jsonlite::fromJSON(system2("curl", stdout = T, shQuote(c(
                paste0("https://cds.climate.copernicus.eu/api/v2/tasks/",
                    reply$request_id),
                "--fail",
                "--user-agent", "some-program",
                climate.data.store.creds()))))}
        stopifnot(is.character(reply$location))
        download.update.meta(
            reply$location,
            download.dir,
            fname,
            curl = climate.data.store.creds())}
    fpath}

climate.data.store.creds = function()
   {cred = Sys.getenv(names = F, "CLIMATE_DATA_STORE_KEY")
    if (cred == "")
        stop("You need to set the environment variable CLIMATE_DATA_STORE_KEY. See https://cds.climate.copernicus.eu/api-how-to#install-the-cds-api-key .")
    c("--user", cred)}

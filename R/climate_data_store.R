# Get a variable from the Climate Data Store and compute the daily
# mean for each date and (lon, lat) pair in a data table. Set `hours`
# to use only a subset of hours (e.g., a single hour) for each date.
#
# Variable names and descriptions for one dataset can be found at
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=overview

#' @export
add.daily.var.from.climate.data.store = function(
        d, vname.in, dataset, vname.out,
        target.tz, area,
        download.dir, download.filename.fmt,
        hours = 0:23, progress = F)
   {assert(all(c("lon", "lat", "date") %in% colnames(d)))
    if (progress)
       {pbar = pbapply::startpb(max = length(unique(d$date)))
        if (is.null(pbar))
            progress = F}

    rasts = as.environment(sapply(simplify = F,
        as.character(
           (min(year(d$date)) - 1) :
           min(lubridate::today("UTC"), max(year(d$date)) + 1)),
        function(y) terra::rast(climate.data.store.file(
            as.integer(y), area, vname.in, dataset,
            download.dir, download.filename.fmt))))

    d[, cds.cell.ix := terra::cellFromXY(
        rasts[[as.character(min(year(d$date)))]],
        cbind(d$lon, d$lat))]

    d[, by = date, (vname.out) :=
      # The data is grouped into one raster by year with one band per
      # UTC hour. For each location, take the mean of all hours on the
      # requested date in `target.tz`.
       {datetimes = lubridate::with_tz(tz = "UTC", lubridate::make_datetime(
            year(date), month(date), mday(date),
            hours, tz = target.tz))
        values = do.call(cbind, lapply(-1 : 1, function(yi)
          # We have to look at the previous and next year for
          # time-zone issues.
             {r = rasts[[as.character(year(date) + yi)]]
              if (is.null(r))
                  NULL
              else
                 {i = as.integer(na.omit(match(datetimes, terra::time(r))))
                  if (length(i))
                      as.data.table(terra::extract(r[[i]], cds.cell.ix))
                  else
                      NULL}}))
        out = (
            if (!is.null(values) && ncol(values) == length(hours))
                rowMeans(values)
            else
                NA_real_)
        if (progress)
            pbapply::setpb(pbar, .GRP)
        out}]

    d[, cds.cell.ix := NULL]
    if (progress)
        close(pbar)}

climate.data.store.file = function(
        the.year, area, vname.in, dataset,
        download.dir, download.filename.fmt)
   {fname = sprintf(download.filename.fmt, the.year)
    fpath = file.path(download.dir, fname)
    if (!file.exists(fpath))
       {message("Downloading ", the.year, " ", vname.in, " from Climate Data Store")
        message("    (This may take a while.)")
        reply = jsonlite::fromJSON(system2("curl", stdout = T, shQuote(c(
            paste0("https://cds.climate.copernicus.eu/api/v2/resources/",
                dataset),
            "--fail",
            "--user-agent", "some-program",
            climate.data.store.creds(),
            "-d", jsonlite::toJSON(auto_unbox = T, digits = NA, list(
                year = the.year,
                month = 1 : 12,
                day = 1 : 31,
                time = sprintf("%02d:00", 0 : 23),
                format = "netcdf",
                product_type = "reanalysis",
                variable = vname.in,
                area = area,
                grid = c(0.125, 0.125)))))))
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

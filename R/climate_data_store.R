# Get a variable from the Climate Data Store and compute the daily
# mean for each date and (lon, lat) pair in a data table.
#
# Variable names and descriptions for one dataset can be found at
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=overview

#' @export
daily.var.from.climate.data.store = function(
        d, vname.in, dataset, vname.out,
        target.tz, area,
        download.dir, download.filename.fmt,
        progress = F)
   {stopifnot(all(c("lon", "lat", "date") %in% colnames(d)))
    if (progress)
       {pbar = pbapply::startpb(max = length(unique(d$date)))
        if (is.null(pbar))
            progress = F}
    d[, by = date, .SDcols = c("lon", "lat"), (vname.out) :=
      # The data is grouped into one raster by UTC day with one band
      # per hour. For each location, take the mean of all hours on the
      # requested date in `target.tz`.
       {if (progress)
            pbapply::setpb(pbar, .GRP)
        times = lubridate::with_tz(tz = "UTC",
            lubridate::make_datetime(tz = target.tz,
                year(date), month(date), mday(date), c(0, 23)))
        times = seq(times[1], times[2], by = "hour")
        dates = as.Date(times, tz = "UTC")
        rowMeans(do.call(cbind, lapply(unique(dates), function(udate)
            raster::extract(y = .SD, x = raster::stack(
                climate.data.store.file(
                    udate, area, vname.in, dataset,
                    download.dir, download.filename.fmt),
                bands = hour(times[dates == udate]) + 1)))))}]
    if (progress)
        close(pbar)}

climate.data.store.file = function(
        date, area, vname.in, dataset,
        download.dir, download.filename.fmt)
   {fname = sprintf(download.filename.fmt,
        year(date), month(date), mday(date))
    fpath = file.path(download.dir, fname)
    if (!file.exists(fpath))
       {while (T)
           {reply = jsonlite::fromJSON(system2("curl", stdout = T, shQuote(c(
                paste0("https://cds.climate.copernicus.eu/api/v2/resources/",
                    dataset),
                "--fail", "--remote-time",
                "--user-agent", "some-program",
                climate.data.store.creds(),
                "-d", jsonlite::toJSON(auto_unbox = T, digits = NA, list(
                    year = year(date), month = month(date), day = mday(date),
                    format = "netcdf",
                    product_type = "reanalysis",
                    variable = vname.in,
                    time = sprintf("%02d:00", 0 : 23),
                    area = area,
                    grid = c(0.125, 0.125)))))))
            if (reply$state == "queued")
                Sys.sleep(1)
            else
                break}
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

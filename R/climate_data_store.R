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
   {stopifnot(all(c("lon", "lat", "date") %in% colnames(d)))
    if (progress)
       {pbar = pbapply::startpb(max = length(unique(year(d$date))))
        if (is.null(pbar))
            progress = F}
    st.data = function(y)
        st_as_sf(rgdal::readGDAL(silent = T,
            climate.data.store.file(
                y, area, vname.in, dataset,
                download.dir, download.filename.fmt)))
    d[, by = .(the.year = year(date)), (vname.out) :=
      # The data is grouped into one raster by year with one band per
      # UTC hour. For each location, take the mean of all hours on the
      # requested date in `target.tz`.
       {coord = as.data.table(st_coordinates(st.data(the.year)))
        # Find which cell of the grid corresponds to each (lon, lat)
        # pair.
        cell.ix = sapply(1 : .N, function(i)
            which.min((lon[i] - coord$X)^2 + (lat[i] - coord$Y)^2))
        cell.ix[
          # Set `cell.ix` to NA when the location isn't actually
          # inside the cell.
            abs(lon - coord[cell.ix, X]) >
                median(diff(sort(unique(coord$X)))) |
            abs(lat - coord[cell.ix, Y]) >
                median(diff(sort(unique(coord$Y))))] = NA_integer_
        # Get the 24 hours before and after this year for time-zone
        # issues.
        dl = function(y)
           {x = as.data.table(st.data(y))[, -"geometry"]
            # CDS has a bug where it sometimes returns extra all-NA
            # bands. Drop them.
            x[, with = F, which(
               sapply(x, function(col) !all(is.na(col))))]}
        prev = dl(the.year - 1)
        grid = as.matrix(cbind(
            prev[, mget(tail(names(prev), 24))],
            dl(the.year),
            if (the.year + 1 <= year(lubridate::today("UTC")))
                dl(the.year + 1)[, 1:24, with = F]))
        grid.datetimes = lubridate::with_tz(tz = target.tz, seq(
            lubridate::make_datetime(the.year - 1, 12, 31),
            lubridate::make_datetime(the.year + 1, 1, 1, 23),
            by = "hour")[1 : ncol(grid)])
        grid.dates = lubridate::as_date(grid.datetimes)
        grid.hours = hour(grid.datetimes)
        out = sapply(1 : .N, function(i) mean(grid[
            cell.ix[i],
            grid.dates == date[i] & grid.hours %in% hours]))
        if (progress)
            pbapply::setpb(pbar, .GRP)
        out}]
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

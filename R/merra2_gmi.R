# Get a variable from the Modern-Era Retrospective analysis for
# Research and Applications, Version 2, Global Modeling Initiative
# (MERRA-2 GMI) and compute the daily mean for each date and (lon,
# lat) pair in a data table.
#
# Variables are described at e.g.
# https://opendap.nccs.nasa.gov/dods/merra2_gmi/tavg1_2d_aer_Nx.info

merra2.max.year.utc = 2019L

#' @export
add.daily.var.from.merra2.gmi = function(
        d, vname.in, dataset, vname.out,
        target.tz, bbox,
        download.dir, download.filename.fmt,
        progress = F)
   {stopifnot(all(c("lon", "lat", "date") %in% names(d)))

    if (all(year(d$date) > merra2.max.year.utc))
        return()

    m.ix = list(
      # The three indices used by the spatiotemporal variables that
      # are accessible through OPeNDAP.
        lons = seq(-180, 179.375, by = .625),
        lats = seq(-90, 90, by = .5),
        times = lubridate::with_tz(tz = target.tz, seq(
            lubridate::make_datetime(1980, min = 30),
            lubridate::make_datetime(merra2.max.year.utc, 12, 31, 23, 30),
            by = "hour")))
    geo.ix = with(as.list(bbox), c(
      # The ranges of the `lons` and `lats` indices we want to use to
      # fetch the study area.
        range(which(xmin <= m.ix$lons & m.ix$lons <= xmax)) + c(-1, 1),
        range(which(ymin <= m.ix$lats & m.ix$lats <= ymax)) + c(-1, 1)))

    if (progress)
       {pbar = pbapply::startpb(max = nrow(unique(
            d[, .(month(date), year(date))])))
        if (is.null(pbar))
            progress = F}

    d[, by = .(the.year = year(date), the.month = month(date)), (vname.out) :=
      # The data source provides one grid of values per UTC hour (at
      # the half-hour mark). For each location, take the mean of all
      # hours on the requested date in `target.tz`.       
           {fetch.times = m.ix$times[
                 year(m.ix$times) == the.year &
                 month(m.ix$times) == the.month]
            e = raster::extract(y = cbind(lon, lat), x = stack(download.update.meta(
                dir = download.dir,
                to = sprintf(download.filename.fmt,
                    the.year, the.month),
                from = function(to) repeat
                  # Use `ncks` to download a netCDF file for the study
                  # region in this month. The download may need to be
                  # retried.
                   {r = system2('ncks', stdout = F, stderr = F, paste(
                        '-O --no_tmp_fl',
                        '-v', shQuote(vname.in),
                        sprintfv('-d lon,%s,%s -d lat,%s,%s', shQuote(
                            geo.ix - 1)),
                        sprintfv('-d time,%s,%s', shQuote(
                            range(which(m.ix$times %in% fetch.times)) - 1)),
                        shQuote(paste0('https://opendap.nccs.nasa.gov/dods/merra2_gmi/',
                            dataset)),
                        shQuote(to)))
                    if (r == 0)
                        return(T)})))
            out = sapply(1 : .N, function(i)
               {time.ix = which(
                    as.Date(fetch.times, tz = target.tz) == date[i])
                if (length(time.ix) < 24)
                  # We don't have a full day of observations because
                  # we're at the end of data availability.
                    NA_real_
                else
                    mean(e[i, time.ix])})
            if (progress)
                pbapply::setpb(pbar, .GRP)
            out}]

    if (progress)
        close(pbar)}

sprintfv = function(fmt, v)
    do.call(sprintf, c(list(fmt), v))

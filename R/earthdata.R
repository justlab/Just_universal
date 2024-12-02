if (!exists("earthdata_web_dir_cache"))
    earthdata_web_dir_cache  = new.env(parent = emptyenv())

#' @export
get.earthdata = function(root.dir, product, satellites, tiles, dates)
  # Download the requested Earthdata satellite product (as an HDF
  # file) for the Cartesian product of the specified satellites,
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
    assert(length(product) == 1)
    assert(product %in% c(
        "MCD19A2.006", "MCD19A2.061",
        "MCD19A3D.061", "MxD13A3.061",
        "MxD21A1D.061", "MxD21A1N.061"))
    monthly = product == "MxD13A3.061"
    if (monthly)
        dates = lubridate::floor_date(dates, "month")
    for (vname in c("satellites", "tiles", "dates"))
        assign(vname, unique(get(vname)))
    assert(all(satellites %in% (if (startsWith(product, "MCD"))
        "terra.and.aqua" else
        c("terra", "aqua"))))
    assert(all(str_detect(tiles, "\\Ah[0-9][0-9]v[0-9][0-9]\\z")))

    # Set the URL format.
    dir.url.f = function(d) with(d, sprintf(
        "https://e4ftl01.cr.usgs.gov/%s/%s/%04d.%02d.%02d",
        (if (startsWith(product, "MCD"))
            "MOTA" else
            paste0("MOL", toupper(substr(satellite, 1, 1)))),
        str_replace(product, "x", switch(paste(satellite),
            terra = "O",
            aqua = "Y",
            "")),
        year(date), month(date), mday(date)))

    # Enumerate the files to be downloaded.
    files = data.table::CJ(
        satellite = factor(satellites),
        date = dates,
        tile = factor(tiles))
    files[, path := file.path(root.dir,
        product, satellite, year(date), tile, paste0(date, ".hdf"))]

    # Download one file at a time.
    if (!all(file.exists(files$path)))
       {message("Downloading ", product, " from Earthdata")
        curl.args = c(
            "--silent", "--show-error", "--fail",
            "--retry", 10,
            "--retry-connrefused", "--ipv4",
              # Forcing IPv4 may be necessary to get
              # `--retry-connrefused` to work:
              # https://github.com/curl/curl/issues/5080
            earthdata.creds())
        pbapply::pboptions(type = "timer")
        pbapply::pblapply(which(!file.exists(files$path)), function(fi) with(files[fi],

           {dir.url = dir.url.f(files[fi])

            # Get the remote filename by scraping the web page that
            # lists it.
            k = as.character(date)
            if (!(k %in% names(earthdata_web_dir_cache)))
               {r = suppressWarnings(system2("curl",
                    stdout = T, stderr = T,
                    shQuote(c(dir.url, curl.args))))
                if (attr(r, "status") == 22L && all(str_detect(r, "404 Not Found")))
                  # This date has no directory, because it has no
                  # data.
                     earthdata_web_dir_cache[[k]] = "nothing"
                else
                    {assert(is.null(attr(r, "status")))
                     earthdata_web_dir_cache[[k]] = str_match_all(
                         paste(r, collapse = ""),
                         '<a href="([^"]+\\.hdf)"')[[1]][,2]}}
            remote.name = (if (identical(earthdata_web_dir_cache[[k]], "nothing"))
                character() else
                str_subset(earthdata_web_dir_cache[[k]],
                    as.character(tile)))

            dir.create(dirname(path), showWarnings = F, recursive = T)
            if (length(remote.name) == 1)
                # Download the file.
                assert(0 == system2("curl", shQuote(c(
                    paste0(dir.url, "/", remote.name),
                    "--remote-time",
                    "--output", path,
                    curl.args))))
            else if (length(remote.name) == 0)
                # Make a stub.
                cat("", file = path)
            else
                stop()
            NULL}))}

    files}

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

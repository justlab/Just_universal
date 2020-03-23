suppressPackageStartupMessages(
   {library(jsonlite)})

download.update.meta = function(data.root, url, to, f, ...)
  # Download a file from `url` and save it to `to.path` =
  # `data.root`/downloads/`to` if there isn't already a file
  # there, then call `f(to.path, ...)`. Then create or
  # update a metadata file that shows where each file was gotten,
  # when it was downloaded, and its hash.
   {to.path = file.path(data.root, "downloads", to)
    dir.create(dirname(to.path), showWarnings = F, recursive = T)

    if (!file.exists(to.path))
       {meta.path = file.path(data.root, "downloads", "meta.json")
        meta = (if (!file.exists(meta.path)) list() else
            fromJSON(simplifyVector = F, meta.path))

        stopifnot(0 == system2("curl", c(
            url, "-o", to.path,
            "--fail", "--remote-time",
            "--user-agent", "some-program")))

        meta[[to]] = list(
            url = url,
            downloaded = format(lubridate::now("UTC"), "%Y-%m-%dT%H:%M:%SZ"),
            sha256 = digest::digest(file = to.path, algo = "sha256"))
        # I use the temporary variable `json` to ensure that the
        # write only starts (and the old file is clobbered) after
        # we've succesfully generated the new JSON.
        json = toJSON(meta[order(names(meta))],
            pretty = T, auto_unbox = T, digits = NA)
        write(file = meta.path, json)}

    f(to.path, ...)}

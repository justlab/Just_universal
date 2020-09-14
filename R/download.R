#' @export
download.update.meta = function(
        url, dir, to = basename(url), curl = character())
  # Download a file from `url` and save it to `to.path` = `dir`/`to`
  # if there isn't already a file there, creating any needed
  # directories. Then create or update a metadata file in `dir` that
  # shows where each file was gotten, when it was downloaded, and its
  # hash. Return `to.path`.
   {to.path = file.path(dir, to)
    dir.create(dirname(to.path), showWarnings = F, recursive = T)

    if (!file.exists(to.path))
       {meta.path = file.path(dir, "meta.json")
        meta = (if (!file.exists(meta.path)) list() else
            jsonlite::fromJSON(simplifyVector = F, meta.path))

        stopifnot(0 == system2("curl", shQuote(c(
            url, "-o", to.path,
            "--fail", "--remote-time",
            "--user-agent", "some-program",
            curl))))

        meta[[to]] = list(
            url = url,
            downloaded = format(lubridate::now("UTC"), "%Y-%m-%dT%H:%M:%SZ"),
            sha256 = digest::digest(file = to.path, algo = "sha256"))
        # I use the temporary variable `json` to ensure that the
        # write only starts (and the old file is clobbered) after
        # we've succesfully generated the new JSON.
        json = jsonlite::toJSON(meta[order(names(meta))],
            pretty = T, auto_unbox = T, digits = NA)
        write(file = meta.path, json)}

    to.path}

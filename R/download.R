#' @export
#' @import DBI
download.update.meta = function(
        from, dir, to = basename(from), curl = character(),
        decompress = NULL)
  # Download a file from the URL `from` and save it to `to.path` =
  # `dir`/`to` if there isn't already a file there, creating any
  # needed directories. Then create or update a database of metadata
  # in `dir` that shows where each file was gotten, when it was
  # downloaded, its size, and its hash. Return `to.path`.
  #
  # If `from` is a function, it's called to download the file instead
  # of invoking `curl`. It's given one argument, `to.path`. It should
  # return a true value to indicate success.
  #
  # If `decompress` is set to a string such as "gz", the file is
  # decompressed before being hashed. Generally `to` should not
  # already have a compression suffix such as ".gz"; this will be
  # temporarily added instead.
   {to.path = file.path(dir, to)
    dir.create(dirname(to.path), showWarnings = F, recursive = T)
    decompression.program = if (!is.null(decompress)) switch(decompress,
        gz = "gunzip",
        bz2 = "bunzip2",
        xz = "unxz",
        stop(paste0("Unknown compression format: ", decompress)))

    if (!file.exists(to.path))
       {temp.to.path = paste0(to.path,
             if (!is.null(decompress)) paste0(".", decompress))
        stopifnot(if (is.function(from)) from(to.path) else 0 ==
            system2("curl", shQuote(c(
                from, "-o", temp.to.path,
                "--fail", "--remote-time",
                "--user-agent", "some-program",
                curl))))

        if (!is.null(decompress))
            stopifnot(0 == system2(decompression.program,
                shQuote(temp.to.path)))

        meta = dbConnect(RSQLite::SQLite(), file.path(dir, "meta.sqlite"))
        on.exit(dbDisconnect(meta))
        if (!as.integer(dbGetQuery(meta,
                "select count(*) from sqlite_master where type = 'table'")))
           {dbExecute(meta, "pragma journal_mode = wal")
            dbExecute(meta, "create table Downloads
               (file             text primary key,
                url              text,
                time_downloaded  integer,
                size             integer not null,
                sha256           blob not null) without rowid")}
        append.or.replace.one.row(meta, "Downloads", list(
            file = to,
            url = (if (is.function(from)) NA_character_ else from),
            time_downloaded = as.integer(lubridate::now("UTC")),
            size = file.size(to.path),
            sha256 = list(digest::digest(file = to.path,
                algo = "sha256", raw = T))))}

    to.path}

append.or.replace.one.row = function(db, table.name, l)
  # We can't just use `dbAppendTable` because one of the columns
  # has to be a list, and data frames can't have lists as columns;
  # furthermore, we want `insert or replace`, not just `insert`.
    dbExecute(db,
         sub("INSERT", "INSERT OR REPLACE",
            sqlAppendTableTemplate(db, table.name, row.names = F,
                `colnames<-`(data.frame(t(rep(1, length(l)))), names(l)))),
        unname(l))

#' @export
download.once = function(
        from, to, curl = character(), decompress = NULL)
  # Download a file from the URL `from` and save it to `to`. If a file
  # already exists at `to`, do nothing. Return `to`.
  #
  # If `from` is a function, it's called to download the file instead
  # of invoking `curl`. It's given one argument, `to` (with any
  # compression file extension from `decompress` added).
  #
  # If `decompress` is set to a string such as "gz", the file is
  # decompressed after downloading. Generally `to` should not
  # already have a compression file extension such as ".gz"; this will be
  # temporarily added instead.
   {dir.create(dirname(to), showWarnings = F, recursive = T)
    decompression.program = if (!is.null(decompress)) switch(decompress,
        gz = "gunzip",
        bz2 = "bunzip2",
        xz = "unxz",
        stop(paste0("Unknown compression format: ", decompress)))

    if (!file.exists(to))
       {temp.to = paste0(to,
             if (!is.null(decompress)) paste0(".", decompress))
        if (is.function(from))
            from(temp.to)
        else
            assert(0 == system2("curl", shQuote(c(
                from, "-o", temp.to,
                "--fail", "--remote-time",
                "--location-trusted", "--anyauth",
                "--user-agent", "Just.universal::download.once",
                curl))))
        if (!is.null(decompress))
            assert(0 == system2(decompression.program,
                shQuote(temp.to)))}

    to}

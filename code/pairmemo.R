suppressPackageStartupMessages(
   {library(jsonlite)
    library(digest)
    library(tools)})

options(fst_threads = 1)  # Avoid a multithreading bug.

if (!exists("pairmemo.cacheenv"))
    pairmemo.cacheenv = new.env(parent = emptyenv())

pairmemo = function(f, directory, mem = F, fst = F)
  # Enable memoization for the named function. It is typically called as
  #   myfun = pairmemo(myfun, "/some/directory")
  # immediately after the definition of `myfun`.
  #
  # `directory` sets the directory to save the paired files of
  # memoized calls and JSON metadata.
  #
  # If `mem` is true, calls will also be saved in memory (in the
  # variable `pairmemo.cacheenv`), and will be retrieved from
  # memory in preference to disk when possible. This means faster
  # cache hits, especially for large values.
  #
  # If `fst` is true, calls are saved as fst files instead of
  # RDS files. This is only suitable for data frames (including
  # data.tables), and uses more disk space, but offers fast I/O.
  # Note that fst files will always be read as data.tables.
   {f.name = deparse(substitute(f))
    force(f)
    # `directory` is assumed to already exist, but we'll create
    # its per-function subdirectory that we're going to use if it
    # doesn't already exist.
    if (!dir.exists(directory))
        stop("The specified directory does not exist: ", directory)
    directory = file.path(directory, f.name)
    if (mem)
      # The memory cache uses environments instead of lists so we
      # get pass-by-reference semantics. 
       {if (!exists(f.name, pairmemo.cacheenv))
           {pairmemo.cacheenv[[f.name]] = new.env(parent = emptyenv())
            pairmemo.cacheenv[[f.name]]$kcache = new.env(parent = emptyenv())
            pairmemo.cacheenv[[f.name]]$vcache = new.env(parent = emptyenv())}
        kcache = pairmemo.cacheenv[[f.name]]$kcache
        vcache = pairmemo.cacheenv[[f.name]]$vcache}
    function(...)
       {key = list(args = list(...))
        hash = paste0("h", digest(key, algo = "xxhash64"))
        if (mem && exists(hash, vcache))
            return(vcache[[hash]])
        path = file.path(directory, hash)
        if (file.exists(paste0(path, ".json")))
            v = pairmemo.read.call(fst, path)
        else
          # It's a total cache miss, so actually call the function.
           {v = do.call(f, key$args)
            dir.create(directory, showWarnings = FALSE)
            if (fst)
                fst::write.fst(v, path)
            else
                saveRDS(v, file = path)
            key = c(
                list(file_format = (if (fst) "fst" else "rds")),
                key)
            write(file = paste0(path, ".json"),
                toJSON(auto_unbox = T, digits = NA, key))}
        if (mem)
           {kcache[[hash]] = key
            vcache[[hash]] = v}
        v}}

pairmemo.path2hash = function(path)
  # Extract the hash alone from the filepath of a JSON metadata file.
    file_path_sans_ext(regmatches(path,
        regexpr("([a-z0-9]+)\\.json$", path)))

pairmemo.list = function(f, filter = function(x) TRUE)
  # Retrieve the JSON metadata of all saved calls of this function.
  # The result is a list with hashes as names.
  #
  # This function is used internally by `pairmemo.get` and `pairmemo.delete`,
  # but is also useful for checking that the filter you intend to use
  # for one of those functions is correct.
   {fe = environment(f)
    paths = list.files(fe$directory, pattern = "\\.json$", full.names = T)
    l =
       {if (fe$mem)
          # Check for any new entries, and load the corresponding
          # JSON files into the memory cache.
           {for (path in paths[!(
                    pairmemo.path2hash(paths) %in% ls(fe$kcache))])
                fe$kcache[[pairmemo.path2hash(path)]] =
                    fromJSON(path, simplifyVector = F)
            as.list(fe$kcache)}
        else
            `names<-`(lapply(paths, fromJSON, simplifyVector = F), pairmemo.path2hash(paths))}
    if (!length(l))
        return(l)
    # Apply the filter manually instead of with `Filter`, so we
    # can treat 0-length results as false and throw an error for
    # results of length greater than 2.
    ix = sapply(l, function(item)
        {y = filter(item)
         if (length(y) == 0)
             F
         else if (length(y) == 1)
             if (is.na(y))
                 F
             else
                 as.logical(y)
         else
             stop("Filter returned vector of length ", length(y))})
    stopifnot(length(ix) == length(l))
    l[which(ix)]}

pairmemo.get = function(f, filter = function(x) TRUE)
  # Get a list of all cached calls. The name of each element is the
  # hash, and each element is a list of two elements, `k` (the JSON
  # metadata) and `v` (the saved value of the function). The `filter`
  # is called only on the key, so filtered-out entries need not
  # even have their call values loaded from disk.
   {keys = pairmemo.list(f, filter)
    fe = environment(f)
    l = lapply(names(keys), function(hash) list(
        k = keys[[hash]],
        v =
           {if (fe$mem && exists(hash, fe$vcache))
                v = fe$vcache[[hash]]
            else
               {path = file.path(fe$directory, hash)
                v = pairmemo.read.call(fe$fst, path)
                if (fe$mem)
                    fe$vcache[[hash]] = v}
            v}))
    `names<-`(l, names(keys))}

pairmemo.delete = function(f, filter = function(x) TRUE, verbose = T)
  # Delete cached calls. Set `filter` to selectively delete calls.
  # The call values won't be loaded in any case.
   {fe = environment(f)
    deleted = 0
    for (hash in names(pairmemo.list(f, filter)))
       {file.remove(paste0(file.path(fe$directory, hash), ".json"))
        file.remove(file.path(fe$directory, hash))
        if (fe$mem)
           {rm(list = hash, pos = fe$kcache)
            if (exists(hash, fe$vcache))
                rm(list = hash, pos = fe$vcache)}
        deleted = deleted + 1}
    if (verbose)
        message("Deleted ", deleted, " cache entr",
            (if (deleted == 1) "y" else "ies"))}

pairmemo.read.call = function(is.fst, path)
   {if (is.fst)
        fst::read.fst(path, as.data.table = T)
    else
        readRDS(path)}

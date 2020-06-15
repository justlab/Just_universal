pairmemo.cacheenv = new.env(parent = emptyenv())

#' @export
pairmemo = function(f, directory, mem = F, fst = F, ap = NULL)
  # Enable memoization for the named function. It is typically called as
  #   pairmemo(directory = "/some/directory",
  #   myfun <- function (...) {...})
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
  # qs files. This is only suitable for data frames (including
  # data.tables). Note that fst files will always be read as
  # data.tables.
  #
  # `ap` ("argument processing") can be set to a named list of
  # unary functions to apply to the provided arguments. For example,
  #     ap = list(foo = as.integer)
  # would cause a `foo` argument, if provided, to be run through
  # `as.integer` before the cache key is created or it's passed to
  # `f`. Thus, the cache (and the function code) won't distinguish
  # between `f(foo = 3)` and `f(foo = 3L)`.
   {f = substitute(f)
    stopifnot(length(f) == 3 && identical(f[[1]], as.symbol("<-")))
    f.name = deparse(f[[2]])
    f = eval(f[[3]], parent.frame())
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
           {pairmemo.cacheenv[[directory]] = new.env(parent = emptyenv())
            pairmemo.cacheenv[[directory]]$kcache = new.env(parent = emptyenv())
            pairmemo.cacheenv[[directory]]$vcache = new.env(parent = emptyenv())}
        kcache = pairmemo.cacheenv[[directory]]$kcache
        vcache = pairmemo.cacheenv[[directory]]$vcache}
    assign(f.name, pos = parent.frame(), function(...)
       {args = sys.call()
        return.kv = F
        if (length(names(args)) && names(args)[2] == "PAIRMEMO.KV")
          # This argument is meant for us rather than for the wrapped
          # function. Remove it and set a flag.
           {return.kv = eval(args[[2]], parent.frame())
            args = args[-2]}
        # Standardize the arguments by using `match.call`.
        args = lapply(match.call(f, args)[-1], eval, envir = parent.frame())
        # Alphabetize the argument names. This should prevent merely
        # reordering a parameter list from changing cache keys.
        if (length(names(args)))
            args = c(
                args[sort(names(args)[names(args) != ""])],
                args[names(args) == ""])
        # Apply any user-provided argument processing in `ap`.
        params = formals(f)
        for (pn in names(ap))
            {stopifnot(pn %in% names(params))
             if (pn %in% names(args))
                 args[[pn]] = ap[[pn]](args[[pn]])}
        # Remove arguments that are set to their default values,
        # unless there's a "..." parameter, in which case arguments
        # might get misassigned if we do this.
        if (!("..." %in% names(params)))
            for (pn in names(params))
                 if (pn %in% names(args) &&
                         !identical(params[[pn]], substitute()) &&
                         identical(params[[pn]], args[[pn]]))
                     args[[pn]] = NULL
        key = list(args = args)
        hash = paste0("h", digest::digest(key, algo = "xxhash64"))
        if (mem && exists(hash, vcache))
            return(vcache[[hash]])
        path = file.path(directory, hash)
        if (file.exists(paste0(path, ".json")))
           {v = pairmemo.read.call(fst, path)
            if (mem || return.kv)
                key = jsonlite::fromJSON(simplifyVector = F,
                    paste0(path, ".json"))}
        else
          # It's a total cache miss, so actually call the function.
           {t1 = proc.time()
            v = do.call(f, key$args)
            t2 = proc.time()
            dir.create(directory, showWarnings = FALSE)
            if (fst)
                fst::write.fst(v, path)
            else
                qs::qsave(v, file = path)
            key = c(
                list(
                    file_format = (if (fst) "fst" else "qs"),
                    time = unname(t2["elapsed"] - t1["elapsed"])),
                key)
            write(file = paste0(path, ".json"),
                jsonlite::toJSON(auto_unbox = T, digits = NA, key))}
        if (mem)
           {kcache[[hash]] = key
            vcache[[hash]] = v}
        if (return.kv)
            list(k = key, v = v)
        else
            v})}

pairmemo.path2hash = function(path)
  # Extract the hash alone from the filepath of a JSON metadata file.
    tools::file_path_sans_ext(regmatches(path,
        regexpr("([a-z0-9]+)\\.json$", path)))

#' @export
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
                    jsonlite::fromJSON(path, simplifyVector = F)
            as.list(fe$kcache)}
        else
            `names<-`(lapply(paths, jsonlite::fromJSON, simplifyVector = F),
                pairmemo.path2hash(paths))}
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

#' @export
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

#' @export
pairmemo.delete = function(f, filter = function(x) TRUE)
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
    c("Cache entries deleted" = deleted)}

pairmemo.read.call = function(is.fst, path)
   {if (is.fst)
        fst::read.fst(path, as.data.table = T)
    else
        qs::qread(path)}

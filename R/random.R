#' @export
set.seed.with.obj = function(x)
    set.seed(make.seed.for.obj(x))

make.seed.for.obj = function(x)
  # https://stackoverflow.com/a/52787092
   {if (is.integer(x) && length(x) == 1)
        return(x)
    x = as.raw(as.hexmode(substring(
        digest::digest(x, algo = "xxhash32"),
        c(1, 3, 5, 7),
        c(2, 4, 6, 8))))
    x = rawConnection(x)
    seed = readBin(x, "int")
    close(x)
    seed}

#' @rawNamespace export(with.temp.seed)
with.temp.seed = function(seeder, expr)
  # Saves the RNG state, seeds the RNG with
  # `set.seed.for.obj(seeder)`, evaluates the expression, restores
  # the old RNG state, and returns the expression's value.
   {if (!exists(".Random.seed"))
        set.seed(NULL)
    old.seed = .Random.seed
    tryCatch(
       {set.seed.with.obj(seeder)
           v = force(expr)},
       finally = {.Random.seed <<- old.seed})
    v}

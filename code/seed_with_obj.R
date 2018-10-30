# https://stackoverflow.com/a/52787092

make.seed.for.obj = function(x)
   {x = as.raw(as.hexmode(substring(
        digest::digest(x, algo = "xxhash32"),
        c(1, 3, 5, 7),
        c(2, 4, 6, 8))))
    x = rawConnection(x)
    seed = readBin(x, "int")
    close(x)
    seed}

set.seed.with.obj = function(x)
    set.seed(make.seed.for.obj(x))

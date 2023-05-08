.datatable.aware = TRUE

#' @export
assert = stopifnot

#' @export
punl = function(...)
  # Like 'list', but uses punning to generate default names, so
  #   punl(a, b, c = x, d)
  # is equivalent to
  #   list(a = a, b = b, c = x, d = d)
  # As a bonus, 'punl()' is equivalent to
  #   `names<-`(list(), character())
  # That is, 'punl()' creates an empty named list.
   {m = match.call(expand.dots = F)
    inp = as.character(m$"...")
    l = list(...)
    names(l) =
        (if (is.null(names(l)))
            inp
         else
            ifelse(names(l) == "", inp, names(l)))
    l}

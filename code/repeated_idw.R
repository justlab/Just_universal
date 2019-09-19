suppressPackageStartupMessages(
   {library(data.table)})

repeated.idw = function(
        locations,
          # A matrix or data frame of points.
        is.source = T,
          # A logical vector of locations that can ever have
          # source data.
        maxdist = NULL,
          # Only consider source points closer than this distance to
          # each query point.
        multiplier = 1,
          # Multiply all squared distances by this number to help avoid
          # floating-point problems due to very small or large weights.
        source.subsetter = function(i) T)
          # A function that should take an index of `locations` and
          # return a logical vector indicating which elements of
          # `locations[is.source]` to use for the given query point.
          # Use `source.subsetter` to hold out source points for
          # cross-validation.
   {locations = as.matrix(locations)
    source = t(locations[is.source,])

    # Set `si` to a vector with as many elements as there are locations.
    # Each element is NA when `is.source` is false, or the corresponding
    # index of `source` when `is.source` is true.
    if (identical(is.source, T))
        si = 1 : nrow(locations)
    else
       {si = ifelse(is.source, 1, NA_real_)
        si[!is.na(si)] = 1 : sum(!is.na(si))}

    minweight = if (is.null(maxdist)) 0 else
        1/(maxdist^2 * multiplier)
    tables = lapply(1 : nrow(locations), function(i)
       {weights = 1 / (colSums((source - locations[i,])^2) * multiplier)
        w = which(weights > minweight & source.subsetter(i))
        cbind(w, weights[w], deparse.level = 0)})
    stopifnot(all(sapply(tables, nrow)))

    function(li, group, outcome)
       {d = data.table(li, group, outcome)
        d[, by = group, prediction :=
           {s = rep(NA_real_, ncol(source))
            lisi = si[li]
            s[lisi[!is.na(lisi)]] = outcome[!is.na(lisi)]
            sapply(li, function(i)
               {v = s[tables[[i]][, 1]]
                sum(v * tables[[i]][, 2], na.rm = T) /
                    sum(tables[[i]][!is.na(v), 2])})}]
        d$prediction}}

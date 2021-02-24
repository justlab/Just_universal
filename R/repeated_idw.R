#' @export
repeated.idw.tables = function(
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
    attr(tables, "si") = si
    attr(tables, "ncol(source)") = ncol(source)
    tables}

#' @export
repeated.idw = function(tables, li, group, outcome,
        make.prediction = T, fallback = NA_real_,
        future = F)
   {d = data.table::data.table(li, group, outcome, make.prediction)
    if (nrow(d[!is.na(outcome), by = .(li, group), if (.N > 1) 1]))
        stop("repeated.idw: Detected more than one non-NA value for a single (li, group) pair")
    si = attr(tables, "si")

    d[, dix := .I]
    f = (if (future)
        function(...) future.apply::future_lapply(
            future.seed = T, ...) else
        lapply)

    d = rbindlist(f(split(by = "group", d), function(chunk)
       chunk[, .(dix, prediction = if (!any(make.prediction)) NA_real_ else
           {s = rep(NA_real_, attr(tables, "ncol(source)"))
            lisi = attr(tables, "si")[li]
            nlisi = !is.na(lisi) & !is.na(outcome)
            s[lisi[nlisi]] = outcome[nlisi]
            preds = rep(NA_real_, .N)
            preds[make.prediction] = sapply(li[make.prediction], function(i)
               {tab = tables[[i]]
                v = s[tab[, 1]]
                nv = !is.na(v)
                weights = tab[nv, 2]
                if (length(weights))
                   {if (Inf %in% weights)
                      # Treat the positively infinite weights as 1 and all
                      # others as 0.
                        mean(v[nv][weights == Inf])
                    else
                        sum(v[nv] * weights) / sum(weights)}
                else
                    fallback})
            preds})]))

    d$prediction[order(d$dix)]}

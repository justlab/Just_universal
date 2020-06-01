tests = function()
   {set.seed(10)

    repeated.idw = Just.universal::repeated.idw
    repeated.idw.tables = Just.universal::repeated.idw.tables

    message("Generating data")
    basegrid = as.data.table(expand.grid(
        x = seq(0, 1, len = 1000),
        y = seq(0, 1, len = 1000)))
    maxdist = .1
    obs.points = sort(sample.int(nrow(basegrid), 3000))
    location.effect = basegrid[, 2*x + y^2]
    times = 1 : 25
    lonely.time = length(times)
    time.effect = rnorm(length(times), sd = 5)
    observations = rbindlist(lapply(times, function(tim) data.table(
        li = sample(obs.points,
          # Provide only observation for one timepoint, but a bunch
          # of observations for all the others.
          if (tim == lonely.time) 1 else
              round(.7 * length(obs.points))),
        time = tim)))
    setkey(observations, time, li)
    observations[, value :=
        location.effect[li] + time.effect[time] + rnorm(.N)]
    observations[, oi := match(li, obs.points)]

    # A test of cross-validation, using `obs.points` only.
    message("\n~~~~~ Cross-validation")
    local(
       {set.seed(400)
        locs = basegrid[obs.points]
        fold = sample(rep(1 : 10, len = nrow(locs)))
        observations = copy(observations)
        fallback = mean(observations$value)

        message("Building repeated.idw tables")
        tables = repeated.idw.tables(
            locations = locs,
            maxdist = maxdist,
            source.subsetter = function(i)
                fold != fold[i])

        message("repeated.idw")
        print(system.time(observations[, pred.our := repeated.idw(
            tables = tables,
            li = oi,
            group = time,
            outcome = value,
            fallback = fallback)]))

        message("gstat::idw")
        suppressPackageStartupMessages(library(gstat))
        observations[, c("x", "y") := locs[oi]]
        observations[, fold := fold[oi]]
        print(system.time({observations[,
            by = .(this.time = time, this.fold = fold),
            pred.gstat :=
               {train = observations[time == this.time & fold != this.fold]
                if (nrow(train)) idw(
                    formula = value ~ 1,
                    locations = ~ x + y,
                    data = train,
                    newdata = .SD,
                    maxdist = maxdist,
                    debug.level = 0)[, "var1.pred"]
                else
                    fallback}]}))

        observations[,
           {message("Greatest absolute difference: ",
                signif(d = 3, max(abs(pred.our - pred.gstat))))
            message("SD: ", round(d = 3, sd(value)))
            message("RMSE: ", round(d = 3, sqrt(mean((value - pred.our)^2))))}]})

    # A test of predicting out to lots of new points.
    message("\n~~~~~ Extrapolation")
    local(
       {set.seed(500)
        locations = basegrid[, .(x, y, bi = .I)][
            bi %in% obs.points | (x <= .1 & y <= .1)]
        fold = rep(-1, nrow(locations))
        fold[locations$bi %in% obs.points] =
            sample(rep(1 : 10, len = length(obs.points)))
        fallback = mean(observations$value)
        observations = rbind(fill = T, observations, fsetdiff(
            rbindlist(lapply(times, function(tim) data.table(
                li = locations$bi,
                time = tim))),
            observations[, .(li, time)]))
        # Don't provide *any* training data at the lonely time.
        # This will test extrapolation with no available training data.
        observations[time == lonely.time, value := NA_real_]
        observations[is.na(value), value.hidden :=
            location.effect[li] + time.effect[time] + rnorm(.N)]
        observations[, li := match(li, locations$bi)]
        observations[, make.prediction := locations[observations$li,
            x <= .1 & y <= .1]]

        message("Building repeated.idw tables")
        tables = repeated.idw.tables(
            locations = locations[, .(x, y)],
            is.source = fold != -1,
            maxdist = maxdist,
            source.subsetter = function(i)
                fold[fold != -1] != fold[i])

        message("repeated.idw")
        print(system.time(observations[, pred.our := repeated.idw(
            tables = tables,
            li = li,
            group = time,
            outcome = value,
            make.prediction = make.prediction,
            fallback = fallback,
            progress = T)]))

        message("gstat::idw")
        suppressPackageStartupMessages(library(gstat))
        observations[, c("x", "y") := locations[li, .(x, y)]]
        observations[, fold := fold[li]]
        print(system.time({observations[
            (make.prediction),
            by = .(this.time = time, this.fold = fold),
            pred.gstat :=
               {train = observations[
                    time == this.time &
                    fold != this.fold &
                    fold != -1 &
                    !is.na(value)]
                if (nrow(train)) idw(
                    formula = value ~ 1,
                    locations = ~ x + y,
                    data = train,
                    newdata = .SD,
                    maxdist = maxdist,
                    debug.level = 0)[, "var1.pred"]
                else
                    fallback}]}))

        observations[!is.na(value.hidden), value := value.hidden]
        observations[(make.prediction),
           {message("Greatest absolute difference: ",
                signif(d = 3, max(abs(pred.our - pred.gstat))))
            message("SD: ", round(d = 3, sd(value)))
            message("RMSE: ", round(d = 3, sqrt(mean((value - pred.our)^2))))}]})}
#' @import stringr
render.rmd.with.notebook = function(
        input, notebook.path, ...)
  # Render the given R Markdown document and replace references
  # to an associated Daylight notebook
  # ( https://github.com/Kodiologist/Daylight ) with actual figures
  # and tables. `notebook.path` should point to Daylight's
  # rendered HTML, not the source document.
   {out.path = rmarkdown::render(input, ...)
    html = readr::read_file(out.path)
    html = str_replace_all(html, regex(dotall = T, "<style.+?</style>"), "")
      # Strip out all the default CSS.
    html = str_replace(html, "<head>",
        "<head><link rel='stylesheet' type='text/css' href='https://arfer.net/daylight.css'>")
    notebook = readr::read_file(notebook.path)

    html = str_replace_all(html, '(fig|tab)–', '\\1--')
       # RMarkdown replaces `--` with en dashes, even if
       # `smart` is off.

    # Insert the appropriate table or figure, extracted from the
    # notebook, above each caption.
    caption.headers = str_match_all(html,
       "<p>(Table|Figure) ((?:notebook\\.html#[^ :,]+[, ]*)+):")[[1]]
    for (i in 1 : nrow(caption.headers))
       {
        referents = sapply(
            str_split(caption.headers[i, 3], ", ")[[1]],
            function(ref)
               {got = str_extract(notebook, regex(dotall = T,
                    sprintf('<(table|figure) id="%s">.+?</\\1>',
                        str_remove(ref, ".+?#"))))
                if (is.na(got))
                    stop(paste("Reference not found:", ref))
                got})
        
        html = str_replace(html,
            fixed(caption.headers[i, 1]),
            paste0(paste(collapse = "\n", referents), "\n", caption.headers[i, 1]))}

    # Number tables and figures.
    counters = c(Table = 0, Figure = 0)
    for (i in 1 : nrow(caption.headers))
       {type = caption.headers[i, 2]
        counters[type] = counters[type] + 1
        refs = str_extract_all(caption.headers[i, 3],
            "notebook\\.html#[^ :,]+")[[1]]
        if (length(refs) == 0)
            stop()
        else if (length(refs) == 1)
            html = str_replace_all(html,
                fixed(paste(type, refs)),
                paste(type, counters[type]))
        else
          # Figures with subfigures get the subfigures identified
          # with letters, like "Figure 2(a)".
           {html = str_replace_all(html,
                fixed(paste(type, caption.headers[i, 3])),
                paste(type, counters[type]))
            for (i in seq_along(refs))
                html = str_replace_all(html,
                    fixed(paste(type, refs[i])),
                    sprintf("%s %d(%s)",
                        type, counters[type], letters[i]))}}

    cat(file = out.path, html)}

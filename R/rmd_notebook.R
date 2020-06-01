suppressPackageStartupMessages(
   {library(stringr)})

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
    cat(file = out.path, str_replace_all(html,
        "<p>(.+?)</p>",
        function(x)
           {refs = str_match_all(x, 
                "<!-- notebook.html#((?:tab|fig|ofig)--\\S+) -->")[[1]][,2]
            refs = sapply(refs, USE.NAMES = F, function(x)
               {p = str_extract(notebook, regex(dotall = T,
                    sprintf('<(table|figure) id="%s">.+?</\\1>', x)))
                if (is.na(p))
                    stop(paste("Reference not found:", x))
                p})
            paste(collapse = "\n", c(x, refs))}))}

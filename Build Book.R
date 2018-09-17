bookdown::render_book("index.Rmd", "bookdown::gitbook")
bookdown::render_book("index.Rmd", "bookdown::pdf_book")

render_book(input, output_format = NULL, ..., clean = TRUE,
            envir = parent.frame(), clean_envir = !interactive(),
            output_dir = NULL, new_session = NA, preview = FALSE,
            encoding = "UTF-8", config_file = "_bookdown.yml")


#Clear "_main.Rmd"
if (file.exists("_main.Rmd")) {
  file.remove("_main.Rmd")
  print("_main.Rmd removed")
}

file.exists("_main.Rmd")

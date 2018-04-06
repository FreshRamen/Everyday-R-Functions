stargazer_out <- function(..., file_name) {
  star.out = file.path(path.alloc_model.input, file_name)
  stargazer(..., type="html", out=star.out)
  htmltools::includeHTML(star.out)
}
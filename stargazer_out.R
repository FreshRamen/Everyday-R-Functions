stargazer_out <- function(..., file) {
  quietly <- capture.output(stargazer(..., type="html", out=file))
  htmltools::includeHTML(file)
}
#' @export
r_to_html <- function(r_file, author = "") {
  #' @export
  
  # Check for the quarto R package, installing if necessary
  if (!requireNamespace("quarto", quietly = TRUE)) {
    message("Installing the 'quarto' R package...")
    install.packages("quarto")
  }
  
  # Check that the Quarto CLI is available on the system
  if (is.null(quarto::quarto_path())) {
    stop(
      "The Quarto CLI was not found on your system.\n",
      "Please install it from https://quarto.org/docs/get-started/ and try again."
    )
  }
  
  # Validate input
  if (!file.exists(r_file)) stop("File not found: ", r_file)
  
  # Derive paths
  r_dir      <- normalizePath(dirname(r_file))
  r_basename <- basename(r_file)
  stem       <- sub("\\.R$", "", r_basename, ignore.case = TRUE)
  qmd_file   <- file.path(r_dir, paste0(stem, ".qmd"))
  today      <- format(Sys.Date(), "%Y-%m-%d")
  
  # Build the .qmd
  header <- paste0(
    "---\n",
    "title: \"", r_basename, "\"\n",
    "date: \"", today, "\"\n",
    "author: \"", author, "\"\n",
    "format:\n",
    "  html:\n",
    "    embed-resources: true\n",
    "---\n\n",
    "```{r}\n"
  )
  
  r_code <- readLines(r_file, warn = FALSE)
  
  writeLines(c(header, r_code, "```"), con = qmd_file)
  
  # Render to HTML, output goes next to the source file
  on.exit(unlink(qmd_file), add = TRUE)
  quarto::quarto_render(
    input      = qmd_file,
    output_format = "html",
    output_file   = paste0(stem, ".html"),
    execute_dir   = r_dir
  )
  
  message("Done: ", file.path(r_dir, paste0(stem, ".html")))
  invisible(file.path(r_dir, paste0(stem, ".html")))
}

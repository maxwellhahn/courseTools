saveCode <- function(last_name){
  #' Saves all code chunks as R and txt files
  #'
  #' @param last_name User's last name
  #'
  #' @return Outputs 1 R file and 1 txt file
  #'
  #' @export

  input_file <- knitr::current_input()
  lines <- readLines(input_file, warn = FALSE)

  chunk_starts <- grep("^```\\{r", lines)
  chunk_ends <- grep("^```$", lines)

  code_lines <- c()
  for (i in seq_along(chunk_starts)) {
    start <- chunk_starts[i] + 1
    end <- chunk_ends[chunk_ends > chunk_starts[i]][1] - 1

    if (!is.na(end) && end >= start) {
      chunk_code <- lines[start:end]
      chunk_code <- chunk_code[!grepl("^#\\|", chunk_code)]

      if (i > 1) {
        code_lines <- c(code_lines, "## -----------------------------------------------------------------------------", "")
      }

      code_lines <- c(code_lines, chunk_code, "")
    }
  }

  base_name <- sub("\\.rmarkdown$", "", input_file)
  output_file_R <- paste0(base_name, "-", last_name, "-code.R")
  output_file_txt <- paste0(base_name, "-", last_name, "-code.txt")

  writeLines(code_lines, output_file_R)
  writeLines(code_lines, output_file_txt)
}

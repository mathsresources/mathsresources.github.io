# Name of the existing R Markdown file
input_file <- "n5prelim.Rmd"

# Name of the revised file that will be created
output_file <- "n5prelimrevision_with_try_another.Rmd"

# Read the original file
text <- readLines(
  input_file,
  warn = FALSE,
  encoding = "UTF-8"
)

# Keep track of the current question number
current_question <- NA_integer_

# Store the revised lines
output <- character()

for (line in text) {

  # Detect a question image such as Q1.png, Q14.png or Q84.png
  question_match <- regexec(
    "images/Q([0-9]{1,2})\\.png",
    line
  )

  question_result <- regmatches(
    line,
    question_match
  )[[1]]

  if (length(question_result) == 2) {
    current_question <- as.integer(question_result[2])
  }

  # Add the original line
  output <- c(output, line)

  # Add the Try Another block immediately after the
  # closing unhide() belonging to a Solution block
  if (
    length(output) >= 4 &&
    line == '`r unhide()`' &&
    grepl(
      "images/A[0-9]{1,2}\\.png",
      output[length(output) - 2]
    )
  ) {

    if (is.na(current_question)) {
      stop("A solution was found before a question number.")
    }

    e_image <- sprintf(
      "E%02d.png",
      current_question
    )

    output <- c(
      output,
      "",
      '`r hide("Try Another")`',
      "",
      paste0("images/", e_image, ""),
      "",
      '`r unhide()`'
    )
  }
}

# Check the inserted image references
expected_references <- sprintf(
  "images/E%02d.png",
  1:84
)

missing_references <- expected_references[
  !expected_references %in% output
]

duplicate_references <- expected_references[
  vapply(
    expected_references,
    function(reference) sum(output == reference) != 1,
    logical(1)
  )
]

if (length(missing_references) > 0) {
  stop(
    "Missing references: ",
    paste(missing_references, collapse = ", ")
  )
}

if (length(duplicate_references) > 0) {
  stop(
    "Some references do not occur exactly once: ",
    paste(duplicate_references, collapse = ", ")
  )
}

# Write the revised R Markdown file
writeLines(
  output,
  output_file,
  useBytes = TRUE
)

message(
  "Finished. The revised file is: ",
  output_file
)

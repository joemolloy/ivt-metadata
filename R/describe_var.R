#' @importFrom stats na.omit setNames
#' @importFrom graphics hist
#' @importFrom dplyr across cur_column where mutate

#' Print dimensions of a dataset
#'
#' @param x Dataframe
#'
#' @return nothing
#' @export
#'
#' @examples
show_dims <- function(x) {
  n <- nrow(x)
  c <- ncol(x)
  fact(paste("Number of cases: ", n))
  cat('\n')
  fact(paste("Variables per record: ", c))
}

#' Print the labels in a summarized form
#'
#' @param x Column vector
#' @param name Column name
#'
#' @return
#' @export
#'
#' @examples
summarize_labels <- function(x, name) {
  invisible(try({
    labels <- attr(x, "labels")

    formatted_labels <- mapply(function(name, value) paste0(name, " = ", value), labels, names(labels))
    x1 <- paste0("Labels: ", paste('\n\n* ', formatted_labels, collapse = " "), '\n\n&nbsp;\n\n')
    cat(x1)
  }))
}

#' Print the name of the variable
#'
#' @param x Column vector
#' @param name Column name
#'
#' @return
#' @export
#'
#' @examples
varname <- function(x, name) {
  label_x <- Hmisc::label(x)
  cat(pander::pandoc.header(paste0("`", name, "`"), 2))
  cat('\n')
  if (!is.na(label_x)) cat(label_x)
  cat('\n\n&nbsp;\n\n')
}

#' Print the inputed text, with appropriate line breaks afterwards
#'
#' @param x text
#'
#' @return
#' @export
#'
#' @examples
fact <- function(x) {
  cat(paste0(x, ". ", '\n', collapse = '\n'))
}

#' Print the datatype of the column
#'
#' @param x Column vector
#' @param name Column name
#'
#' @return
#' @export
#'
#' @examples
data_type <- function(x, name) {
  format <- stringr::str_to_title(setdiff(unique(c(class(x), mode(x))), c('labelled', 'haven_labelled', 'vctrs_vctr' )))[1]
  fact(paste0("Format: ", paste(format, collapse = ", ")))
}

#' Print the summary of the values
#'
#' @param x Column vector
#' @param name Column name
#'
#' @return
#' @export
#'
#' @examples
nice_summary <- function(x, name) {
   pander::pander(summary(x))
}

#' Print the counts of a categorical variable, horizontally
#'
#' @param x Column vector
#' @param name Column name
#'
#' @return
#' @export
#'
#' @examples
count_levels_horizontal <- function(x, name) {
  m <- haven::as_factor(x)
  m1 <- broom::tidy(summary(m, maxsum = 26))
  m2 <- tidyr::pivot_wider(m1, names_from = names, values_from=x)

  rownames(m2)<-"Count"
  pander::pander(m2)
}

#' Print the counts of categorical column in a vertical table
#'
#' @param x Column vector
#' @param name Column name
#'
#' @return
#' @export
#'
#' @examples
count_levels_vertical <- function(x, name) {
  m <- haven::as_factor(x)
  m1 <- broom::tidy(summary(m, maxsum = 26))
  colnames(m1) <- c('Value', 'Count')

  pander::pander(m1)
}

#' Print the histogram of the column
#'
#' @param x Column vector
#' @param name Column name
#'
#' @return
#' @export
#'
#' @examples
show_histogram <- function(x, name) {
  cat('\n\n')
  hist(x, main = paste("Histogram of", name), xlab = name, probability = TRUE)
  cat('\n\n')
}

#' Infer a set of standard display functions, based on the column provided
#'
#' @param x Column vector
#'
#' @return list of display functions
#' @export
#'
#' @examples
detect_standard_functions <- function(c) {
  library(haven)
  col_type = typeof(c)
  is_date_or_time <- length(intersect(c('Date', 'POSIXct', 'POSIXt'), class(c))) > 0
  funs <- c(data_type)
  total_label_length = ifelse(is.null(attr(c, 'labels')),
                              sum(stringr::str_length(unique(c))),
                              sum(stringr::str_length(names(attr(c, 'labels')))))

  if ((col_type == 'character' || col_type == 'double' || col_type == 'integer')
      && length(unique(c)) <= 7
      && !is.na(total_label_length) && total_label_length < 70) {

    funs <- c(funs, count_levels_horizontal)
  } else if  (col_type == 'character' && length(unique(c)) < 26) {
    funs <- c(funs, count_levels_vertical)
    # } else if ('integer' == typeof(c)) { # integer
    #   funs <- c(funs)
  }  else if (!is_date_or_time && (col_type == 'numeric' || col_type == 'double')) {
    funs <- c(funs, show_histogram)
  }# else {
  #  funs <- c(funs, function(x, name){paste(col_type )})
  #}

  if (!is.null(attr(c, 'labels')) &&
      length(attr(c, 'labels') > 0)) {
    funs <- c(summarize_labels, funs)
  }

  as.list(funs)
}


#' Describe a variable in a Rmarkdown document
#'
#' @param data Dataframe
#' @param name Name of the column to describe
#' @param ... List of display functions to use
#'
#' @return nothing
#' @export
#'
#' @examples
describe_var <- function(data, name, ...) {
  column <- data[[name]]

  funs <- if (length(list(...)) == 0) {
    detect_standard_functions(column)
  } else {
    list(...)
  }

  output <- function(f, x, name) {f(x, name)}

  cat(ifelse(knitr::is_latex_output(), "\n\\minipage{\\textwidth}\n", ''))
  cat(output(varname, column, name))
  cat(unlist(lapply(funs, output, x = column, name = name)), sep=' \n')
  cat('\n\n')
  cat(ifelse(knitr::is_latex_output(), "\\endminipage\n", ''))

}


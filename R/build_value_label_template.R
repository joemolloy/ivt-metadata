#' Title
#'
#' @param ...
#' @param exclude_regexes
#' @param output_file
#'
#' @return
#' @export
#'
#' @examples
build_value_label_template <- function(..., exclude_regexes, previous_value_labels, output_file) {
  datasets = list(...)

  dataset_var_names <- sapply(as.list(substitute(list(...)))[-1L], deparse)
  dataset_given_names <- names(datasets)
  dataset_names <- ifelse(is.null(dataset_given_names) | dataset_given_names == '' , dataset_var_names, dataset_given_names)

  value_structure = lapply(datasets, build_value_list, exclude_regexes)
  names(value_structure) = dataset_names

  if (!missing(previous_value_labels)) {
    lapply(names(previous_value_labels), function(d) {
      lapply(names(previous_value_labels[[d]]), function(v) {
        lapply(names(previous_value_labels[[d]][[v]]), function(val) {
          value_structure[[d]][[v]][[val]] <<- previous_value_labels[[d]][[v]][[val]]
        })
      })
    })
  }


  #lapply(datasets, build_list_element)
  #dataset_names
  if (!missing(output_file)) {
    yaml::write_yaml(value_structure, file=output_file)
  }
  value_structure
}

build_value_list <- function(df, exclude_regexes) {
  var_cols = df
  var_names = colnames(var_cols)

  if (!missing(exclude_regexes)) {
    exclude_matches <- sapply(exclude_regexes, function(x) stringr:: str_starts(var_names, x))
    exclude_matches_logical <- ifelse(dim(exclude_matches) == 2, rowSums(exclude_matches), exclude_matches) > 0
    excluded_cols = var_names[exclude_matches_logical]
  } else {
    excluded_cols = c()
  }

  if(length(excluded_cols) > 0) {
    message("Ignoring the following columns:")
    message(paste(excluded_cols, collapse = ', '))
  }

  #filter vars
  filter1 <- mapply(should_label_var, var_names, var_cols, MoreArgs=list(exclude_regexes=exclude_regexes))
  var_names <- var_names[filter1]
  sapply(var_names, function(v) {
    vals = na.omit(unique(df[[v]]))
    res = lapply(seq_len(length(vals)), function(x) {NULL})
    names(res) = vals
    return (res)
  })
}

should_label_var <- function(v, df_col, exclude_regexes) {
  unique_vals <- na.omit(unique(df_col))
  if (any(stringr:: str_starts(v, exclude_regexes))) return (F)
  if (setequal(stringr::str_to_lower(unique_vals), c('yes','no'))) return (F)
  if (setequal(stringr::str_to_lower(unique_vals), c('true','false'))) return (F)
  if (is.logical(v)) return (F)
  if (lubridate::is.Date(v) || lubridate::is.POSIXct(v) || lubridate::is.POSIXt(v)) return (F)
  if (length(unique_vals) > 25) return (F)
  TRUE
}


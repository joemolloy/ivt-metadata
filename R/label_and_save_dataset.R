
#' Add labels to the variables and values in a dataset
#'
#' @param df_base
#' @param data_labels
#' @param value_labels
#' @param output_location
#' @param filename
#' @param spss_output
#' @param write_output
#'
#' @return
#' @export
#'
#' @examples
label_and_save_dataset <- function(df_base, data_labels,
                                   value_labels,
                                   output_location, filename, spss_output=F, write_output=T) {

  #all add the value labels to the dataframe
  df <- df_base %>%
    mutate(across(where(~ is.character(.x) | is.numeric(.x)), function(c) {
      lbs = unlist(value_labels[[filename]][[cur_column()]])
      inverted_labels = names(lbs)

      if (length(lbs) > 0) {
        if (typeof(c) %in% c('double', 'integer')) {
          inverted_labels <- as.numeric(inverted_labels)
        }
        names(inverted_labels) = lbs

        haven::labelled(c,labels=inverted_labels)
      } else {
        c
      }
    }))

  lapply(colnames(df), function(d) {
    if (!is.null(data_labels[[filename]][[d]])) {
      Hmisc:::label(df[[d]]) <<- data_labels[[filename]][[d]]
    }
  })

  df_labels <- unlist(data_labels[[filename]])

  if (anyNA(df_labels)) {

    missing_labels <- colnames(df)[is.na(df_labels)]
    missing_labels_to_display <- missing_labels[1:pmin(5, length(missing_labels))]
    if (length(missing_labels) > 5) {
      missing_labels_to_display <- c(missing_labels_to_display, '...')
    }
    missing_labels_to_display <- paste(missing_labels_to_display, collapse = ', ')
    warning(paste0(length(missing_labels), " labels in \"", filename, "\" have not been defined: ", missing_labels_to_display))
  }

  if (write_output) {
    readr::write_csv(df, file.path(output_location, paste0(filename, '.csv')))
    local({
      assign(filename, df)
      save(list=c(filename), file=file.path(output_location, paste0(filename, '.rda')), compress = T)
    })

    if (spss_output) {
      haven::write_sav(
        janitor::clean_names(df),
        path=file.path(output_location, paste0(filename, '.sav'))
      )
    }
  }
  return (df)
}

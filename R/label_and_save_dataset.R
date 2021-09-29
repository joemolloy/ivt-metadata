
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
    mutate(across(everything(), function(c) {
      lbs = unlist(value_labels[[filename]][[cur_column()]])
      inverted_labels = names(lbs)
      variable_label <- data_labels[[filename]][[cur_column()]]

      if (length(lbs) > 0) {
        if (typeof(c) %in% c('double', 'integer')) {
          inverted_labels <- as.numeric(inverted_labels)
        }
        names(inverted_labels) = str_sub(lbs, end=5)
        haven::labelled(c,label=variable_label, labels=inverted_labels)
        #labelled::val_labels(c) <- inverted_labels
      } else {
        #sjlabelled::set_label(c, label=inverted_labels)
        labelled::var_label(c) <- variable_label
        c
      }
    }))

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
        path=file.path(output_location, paste0(filename, '.sav')),
        compress = T
      )
      
    }
  }
  return (df)
}

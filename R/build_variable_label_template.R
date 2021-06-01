#' Generate the template list of labels for a set of datasets
#'
#' @param ...
#' @param previous_variable_labels list object of previous labels that should be kept
#' @param output_file if given, then the structure will be saved as a yaml for editing in a text editor
#'
#' @return A list of column names for each dataset specified
#' @export
#'
#' @examples
build_variable_label_template <- function(...,
                                     previous_variable_labels = list(),
                                     output_file = NULL) {

  if (!is.list(previous_variable_labels)) {
    stop('Provided variable labels needs to be a list')
  }

  datasets = list(...)
  dataset_var_names <- sapply(as.list(substitute(list(...)))[-1L], deparse)
  dataset_given_names <- names(datasets)
  dataset_names <- as.list(ifelse(is.null(dataset_given_names) | dataset_given_names == '' ,
                         dataset_var_names, dataset_given_names))

  variable_labels = setNames(mapply(function(x, dataset_name) {
    as.list(setNames(sapply(colnames(x), function(x1) {
      previous_variable_labels[[dataset_name]][[x1]]
    }), colnames(x)))
  }, datasets, dataset_names), dataset_names)

  #save variable list
  if (!is.null(output_file)) {
    yaml::write_yaml(variable_labels, output_file)
  }

  return(variable_labels)

}

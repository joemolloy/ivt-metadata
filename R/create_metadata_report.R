render_metadata_pdf <- function(config_file, keep_tex=F, ...) {

  output_options = list(pdf_document=list(keep_tex=T))
  rmarkdown::render(system.file('rmarkdown/templates/archive_notebook.Rmd', package='ivtmetadata'),
                   params = list(config_file=config_file),
                   output_options = output_options,
                    ...
  )

}

create_metadata_config <- function(file_location_list, output_filename=NULL) {

  description = "  Title: title of the codebook
  Creator:
  Subject:
  Description:
  Publisher:
  Contributor:
  Date:
  Type:
  Format:
  Source:
  Language:
  Relation:
  Coverage:
  Rights:
  Version responsibility: "

  file_template = "  file_location: %s
  Title: x
  Contents: >
    This is the description of the file.
    New lines will be compressed into spaces. To have new lines shown,
    use \\| instead of \\> above. A newline is needed before the next row (in this case title)
  Data collection: x
  Unit of analysis: x
  File structure: x
  Variable Settings: "

  file_setup = list()

  lapply(file_location_list, function(f) {
    dataset_name <- stringr::str_remove(basename(f), '.rda$')
    file_setup$dataset_name <<- yaml::yaml.load(sprintf(file_template, f))
  })

  yaml_template <- list("Document description" = yaml::yaml.load(description),
       "Study description" = yaml::yaml.load(description),
       "Files" = file_setup
  )

  if(!is.null(output_filename)) yaml::write_yaml(yaml_template, output_filename)

  yaml_template

}

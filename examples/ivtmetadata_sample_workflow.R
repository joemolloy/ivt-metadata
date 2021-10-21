devtools::install_github('joemolloy/ivtmetadata')

library(ivtmetadata)
library(yaml)
library(readr)

dataset <- read_csv('....')

variable_labels_file = 'xxx'
value_labels_file = 'xxx'

build_variable_label_template_template(dataset, output_file = variable_labels_file)
build_value_label_template(dataset, output_file = value_labels_file)

# to build_value... you can add list of regexes to ignore:
# i.e. exclude_regexes = c("p_commutemode_*", "*day*", "p_occup_1_*"),

# fill in the above generated files as appropriate in a text editor
# if you run the two commands above, the previous files will be overwritten (at the moment)

variable_labels <- read_yaml(variable_labels_file)
value_labels <- read_yaml(value_labels_file)

output_folder <- 'xxx'

#this will generate both csv and rda files. The rda file is also needed for the metadata paper.
label_and_save_dataset(dataset, variable_labels, value_labels, output_folder, 'dataset_name', spss_output=F)

# then click on File -> New -> R Markdown -> Teamplate -> ivtmetadata

# change the 'participants' name to the 'dataset_name'
# fill in the information about the dataset, and the general document information
# to specify what parts to display for different variables, see the examples under participants. Options are:
# .   [datatype, count_levels_vertical, count_levels_horizontal, nice_summary, show_histogram]

# When you are done, click knitr in the bar above the text window to compile and generate a pdf

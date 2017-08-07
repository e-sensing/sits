# R script 2017-06-28, relabel raster statstack and apply post-processing using transition rules
# by Victor Maus

library(sits)
library(raster)

# default tmp partition might be too small, set a tmp folder in a partition with enough space
rasterOptions(tmpdir = "./.tmp")

# Stack raster fiels (assuming that files are ordered by time)
map_files <- dir(system.file("extdata/raster/twdtw_classification", package="sits"),
                 pattern = ".tif$", full.names = TRUE)

map_stack <- stack(map_files)

# Relabel raster stack - Parallel processing
old_v = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
new_v = c(0, 4, 1, 2, 1, 3, 3, 5, 5, 6,  7,  7,  7,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17)
relabeled_map <- sits_stack_relabel(x = map_stack, old_values = old_v, new_values = new_v)

# Read transition matrix. This gives the possible transitions among classes.
# The first line and the first column of the table has the raster class followed
# by the class label, such that <class_number>.<class_label>. The rows are the
# Class(t) and columns Class(t-1), the cobination between a row (Class(t)) and a
# column (Class(t-1)) gives the updated class number.
transition_matrix <-
     readr::read_csv(system.file("extdata/raster/twdtw_classification/transition_matrix.csv", package="sits"))

# Relabel temporal transitions - Parallel processing
post_proc_map <- sits_stack_transition_relabel(x = relabeled_map, A = transition_matrix)




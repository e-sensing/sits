# Program to read TWDTW pattern files in .RData format and convert them to SITS tables

library (sits)
# Set data dir
data_dir <- system.file("extdata/patterns", package="sits")

# configure the WTSS server
sits_configWTSS()

# get the list of RData files
file.names <- dir(data_dir, pattern =".RData")

# process each file
file.names %>%
     map (function (fn) {
          # load the data pattern from the RData file
          pat <- get(load(paste(data_dir, fn, sep = "/")))
          # get the prefix (required for writing a JSON file)
          prefix <- tools::file_path_sans_ext(fn)
          # make the name of the JSON file
          json_file <- paste(data_dir, prefix, ".json", sep = "/")

          # plot the patterns and save them to JSON file
          sits_fromTWDTW(pat) %>%
               sits_plot (type = "patterns") %>%
               sits_toJSON (json_file)

          # test - read the JSON file and plot them
          sits_fromJSON (json_file) %>%
               sits_plot (type = "patterns")
     })












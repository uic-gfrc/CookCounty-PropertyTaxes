# Define the folder where you want to store the _files/ directories
output_dir <- "quarto_supportfiles/"

# Create the directory if it doesn't exist
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# List all folders ending in "_files"
files_folders <- list.dirs(path = ".", full.names = TRUE, recursive = FALSE)
files_folders <- files_folders[grepl("_files$", files_folders)]

# Move each *_files/ folder to the output directory
for (folder in files_folders) {
  new_location <- file.path(output_dir, basename(folder))
  file.rename(folder, new_location)
}

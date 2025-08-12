# Biocomputing Final
# File translation: Get file name/directory, get file names, loop through all files, 
# load text file, write to csv

# Compile files: get directory, get file names [SET], make an empty dataframe, loop through
# files, append days to out growing data frame, add 2 new columns, deal w/user choices around NA's

# Summary of data: get a data frame, subsetting & tabulating
# rowSums(), sum(x$y == "make")

# Analysis- R: Use custom functions!, load compiles files
# Q1: Answer + rationale as comments, figure, regression or two
# Q2: Answer + rationale as comments, figure
################################################################################
# 1. In which country (X or Y) did the disease outbreak likely begin?
# 2. If Country Y develops a vector for the disease in the next week,
# is it likely to work for citizens of Country X?
####################### FUNCTIONS #########################################
# Function 1
######## Converting .txt to comma delimited .csv files
# Name function "converting" and call "og_directory" to call files that you want to convert; your converted ".csv"
# files will go into "out directory"
converting <- function(og_directory, out_directory){
  # This line is scanning "og_directory" and looking for the files with ".txt"
  # "full.names = TRUE" makes sure to return full file paths
  # "recursive = TRUE" searches in subdirectories
  txt_files <- list.files(og_directory, pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)
  # This for loop iterates over each ".txt" file found in "txt_files"
  for (file in txt_files){
    # This lines reads what is in ".txt" file into a data frame
    # "header = TRUE" looks at the first row containing column names
    # "sep = ", assumes the columns are seperated by spaces
    # "fill = TRUE", makes sure that missing values are filled with NAs
    # "stringsAsFactors = FALSE" avoids converting character columns into factors
    country_data <- read.table(file, header = TRUE, sep = " ", fill = TRUE, stringsAsFactors = FALSE)
    # This line creates the output file path for ".csv" files
    # "sub" replaces ".txt" with ".csv" file path
    # The result of this line is a new file path for the ".csv" version of the files
    output <- sub("\\.txt$", ".csv", file)
    # This line writes the data frame "country_data" to a ".csv" files at the "output" path
    # "row.names = FALSE" makes sure that row numbers are not included within CSV file
    write.csv(country_data, output, row.names = FALSE)
  }
}
# Function 2
###### Compile .csv files in directory to one .csv file
# "og_directory" is the directory where the CSV files are located
# "handleNA" is a parameter that deals with NA values in the data
compiling <- function(og_directory, handleNA = c("remove", "warn", "include")) {
  # List all .csv files recursively in "og_directory"
  txt_files <- list.files(og_directory, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  # "list.files" is used to get files in "og_directory" that are ".csv" files
  # "full.names = TRUE" makes sure the full path is included
  # "recursive = TRUE" allows for searching in subdirectories
  
  # Initialize an empty data frame to store compiled data in
  compiled_data <- data.frame()
  
  # This for loop iterates each ".csv" file found
  for (file in txt_files) {
    # Extract parent folder name to be used as 'country'
    folder_name <- basename(dirname(file))  # Parent folder = countryX or countryY
    
    # Extract the filename and split it into parts
    file_name <- basename(file)
    # Get the filename from the full path
    file_parts <- strsplit(file_name, "_")[[1]]
    # Split the filename by underscores "_"
    
    # Extract the day of the year from the filename 
    dayofYear <- as.integer(sub("\\.csv$", "", file_parts[2]))  # Remove '.csv' and convert to integer
    
    # Read the CSV file
    # "read.csv", loads data from the file into "country_data" data frame
    # "stringsAsFactors = FALSE" makes sure that text columns are not automatically converted to factors
    country_data <- read.csv(file, stringsAsFactors = FALSE)
    
    # Add 'country' and 'dayofYear' columns to data frame
    country_data$country <- folder_name # Adds the "country" column
    country_data$dayofYear <- dayofYear # Adds the "dayofYear" column
    
    # Append to data to "compiled data"
    compiled_data <- rbind(compiled_data, country_data)
    # "rbind" binds the current "country_data" to the "compiled_data"
  }
  
  # Handle NA values based on user choice
  if (handleNA == "remove") {
    compiled_data <- na.omit(compiled_data)
    # If "handleNA" is "remove", "na.omit()" removes rows with NA values
  } else if (handleNA == "warn") {
    if (any(is.na(compiled_data))) {
      warning("Warning, NA values are present in the data.")
    }
    # If "handleNA" is "warn", a warning is shown if there are any NA values in data
  }
  
  # Write the compiled data to a CSV file in the original directory
  # "file.path" combines "og_directory" and "compiled_data.csv" to create the output path 
  # "write.csv" writes "compiled_data" data frame to specified file without row names
  output_path <- file.path(og_directory, "compiled_data.csv")
  write.csv(compiled_data, file = output_path, row.names = FALSE)
  
  # Returns the compiled data frame
  return(compiled_data)
}

# Function 3
###### Summarize compiled data
summarizing <- function(compiled_data){
  # Get the numver of screens in the "compiled_data" dataset
  num_screens <- nrow(compiled_data)
  # Calculate the percentage of patients who are infected while ignoring missing values 
  percent_screened <- mean(compiled_data$infected == 1, na.rm = TRUE)
  # Create a frequency table for the "gender" column in the dataset
  gender <- table(compiled_data$gender)
  # Create a frequency table for the "age" column
  # "cut" function divides the age data into age ranges
  age <- table(cut(compiled_data$age, breaks = c(0,18,30,40,50,60,100), right = FALSE))
  # Output the total number of screens 
  cat("Number of screens run:", num_screens,"\n")
  # Output percentage of infected patients
  cat("Percent of infected patients:", percent_screened, "%\n")
  # Output the gender distribution
  cat("Gender identity:\n")
  print(gender)
  # Output the age distribution 
  cat("Age distribution:\n")
  print(age)
  # Create a bar plot visualizing the age distribution of patients
  age_plot <- barplot(age, main = "Age Distribution of Patients", col = "pink", beside = TRUE)
  # Return a list containing the computed values
  return(list(num_screens = num_screens, percent_screened = percent_screened, gender = gender, age = age))
}


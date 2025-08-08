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
# Converting .txt to comma delimited .csv files
converting <- function(og_directory, out_directory){
  txt_files <- list.files(og_directory, pattern = "\\.txt$", full.names = TRUE)
  for (file in txt_files){
    country_data <- read.table(file, header = TRUE, sep = "", fill = TRUE, stringAsFactors = FALSE)
    output <- file.path(out_directory, paste0(basename(file), ".csv"))
    write.csv(country_data, output, row.names = FALSE)
  }
}
# Function 2
# Compile .csv files in directory to one .csv file
compiling <- function(og_directory, out_directory, handleNA = "warn"){
  txt_files <- list.files(og_directory, pattern = "\\.csv$", full.names = TRUE)
  empty_list <- list()
  for (file in txt_files){
    country_data <- read.csv(file, stringsAsFactors = FALSE)
    country <- strsplit(basename(file), "_")[[1]][1]
    dayofYear <- strsplit(basename(file), "_")[[1]][2]
    country_data$country <- country # Creating column "country"
    country_data$dayofYear <- dayofYear# Creating column "dayofYear"
    empty_list[[length(empty_list) + 1]] <- country_data
  }
  compiled_data <- do.call(rbind, empty_list)
  if (handleNA == "remove"){ 
    compiled_data <- na.omit(compiled_data)
  } else if (handleNA == "warn"){
    if (any(is.na(compiled_data))){ # Checks for NA values
      warning("Warning, NA values present") 
    }
  }
  write.csv(compiled_data, out_directory, row.names = FALSE)
}


# Function 3
# Summarize compiled data
summarizing <- function(compiled_data){
  num_screens <- nrow(compiled_data)
  percent_screened <- mean(compiled_data$infected == 1, na.rm = TRUE)
  gender <- table(compiled_data$gender)
  age <- table(cut(compiled_data$age, breaks = c(0,18,30,40,50,60,100), right = FALSE))
  cat("Number of screens run:", num_screens,"\n")
  cat("Percent of infected patients:", percent_screened, "%\n")
  cat("Gender identity:\n")
  print(gender)
  cat("Age distribution:\n")
  print(age)
  age_plot <- barplot(age, main = "Age Distribution of Patients", col = "pink", beside = TRUE)
  return(list(num_screens = num_screens, percent_screened = percent_screened, gender = gender, age = age))
}



#Set working directory to be RProject2024 file, where supportingFunctions.R file and all project contents are
setwd(".")

#Check that working directory is RProject2024
getwd()

#Load functions made in file "supportingFunctions.R" into new R script ("Analysis.R")
source("supportingFunctions.R")

converting("data", "")
compiled_data <- compiling("data", handleNA = TRUE)
summarizing(compiled_data)


#QUESTION 1: In which country did the outbreak begin?

#The outbreak likely began in countryX.

#To find out the disease's country of origin, we performed two linear regressions 
#(one for countryX and one for countryY). The regressions model the average number of genetic
#markers present in each patient for each day of screening. We are assuming that 
#the presence of more genetic markers (more affected proteins) is indicative of disease
#progression. When we plot the data, we can see that countryX has more genetic markers
#present per patient since the screenings started. Additionally, we can see that countryX 
#has a steeper slope that countryY, meaning that their numbers of genetic markers are 
#increasing faster. Both of these point to the disease having originated in countryX and then 
#potentially having spread to countryY. 

#QUESTION 1: Linear Regressions

#Loading in ggplot2 to library to use for model
library(ggplot2)

#Read in compiled data file
data <- read.csv("data/compiled_data.csv")

# Calculate the total markers for each row by summing up the columns that have "marker" in their name
data$total_markers <- rowSums(data[, grep("marker", names(data))])
# Subset data for countryX
country_x <- subset(data, country == "countryX")
# Subset data for countryY
country_y <- subset(data, country == "countryY")

#Two linear regressions (countryX and countryY)
# "total_markers" is the dependent variable
# "dayofYear" is the independent variable
lm_x <- lm(total_markers ~ dayofYear, data = country_x)
lm_y <- lm(total_markers ~ dayofYear, data = country_y)

# Create a new variable to label the countries
# Creates a new column "country_label" that assigns "CountryX" or "CountryY" based on the "country" column 
data$country_label <- ifelse(data$country == "countryX", "Country X", "Country Y")

#Plot shows average number of genetic markers present in each patient on each screening day, separated by country
plot <- ggplot(data, aes(x = dayofYear, y = total_markers, color = country_label)) +
  # Adds scatter plot points with a transparency of 30% or alpha = 0.3
  geom_point(alpha = 0.3) +
  # Adds a linear regression line for each country, with no confidence interval "se = FALSE", and a solid line
  geom_smooth(method = "lm", se = FALSE, aes(group = country_label), linetype = "solid") +
  # Adds titles and labels to the plot
  labs(
    title = "Genetic Markers",
    x = "Day of Year",
    y = "Mean # of Markers",
    color = "Country"
  ) +
  theme_minimal()
# Displays the plot created
print(plot)

#QUESTION 1: Linear Regression Significance

#In order to prove that the linear regressions performed above our significant, we must 
#extract the p-values and R-squared values. Since no other direct instruction is given, we will 
#assume that a p-value < 0.5 is considered statistically significant. Here, we see that 
#the p-values for countryX and countryY are both ~0, proving the validity of this data. 
#Although the R-squared values are a bit low (generally significance is considered as close to 1
#as possible), we take the results to be significant given the field of study and variance of 
#real world data. 

# Extract p-values and R-squared for Country X
# Gives a summary of "lm_x" for country X
summary_x <- summary(lm_x)
# Extract the R-squared value from the summary of the model for Country X
r_squared_x <- summary_x$r.squared
# Extract the p-value 
p_value_x <- summary_x$coefficients[2, 4]

# Extract p-values and R-squared for Country Y
# Gives a summary of "lm_y" for country Y
summary_y <- summary(lm_y)
# Extract the R-squared value from the summary of the model for Country Y
r_squared_y <- summary_y$r.squared
# Extract the p-value
p_value_y <- summary_y$coefficients[2, 4]

# Print the results for Country X
cat("Country X:\n")
cat("R-squared:", r_squared_x, "\n")
cat("P-value (dayofYear):", p_value_x, "\n\n")

# Print the results for Country Y
cat("Country Y:\n")
cat("R-squared:", r_squared_y, "\n")
cat("P-value (dayofYear):", p_value_y, "\n")


#QUESTION 1 FIGURE: Q-Q Residual

#The linear regressions performed in the earlier part of question 1, assume that the 
#data is distributed normally as a bell curve. To prove this, we perform a Q-Q residual.
#Since the figure shows most of the data aligning with the diagonal, we can assume that 
#the linear regressions performed before are valid and satisy the normality assumption.
#However, this alone does not prove significance of the linear reression, which we
#get from the extracted p-values. 

# Perform the linear regression (as in your original code)
lm_x <- lm(total_markers ~ dayofYear, data = country_x)

# Extract residuals
residuals_x <- resid(lm_x)

# Create a data frame for ggplot
residuals_df <- data.frame(residuals = residuals_x)

# Create the Q-Q plot
qq_plot <- ggplot(residuals_df, aes(sample = residuals)) +
  # This line adds the Q-Q plot points, comparing residuals to a theoretical normal distribution
  stat_qq() +
  # Adds a reference line to the plot, if the residuals are normally distributed, the points will align with line
  stat_qq_line() +
  # Labels plot
  labs(
    title = "Q-Q Plot of Residuals (Country X)",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()
# Print Q-Q plot
print(qq_plot)


#QUESTION 2:If Country Y develops a vaccine for the disease in the next week, is 
#it likely to work for citizens of Country X?

#A vaccine developed for countryY is NOT likely to work for countryX, because it will
#not be targeted for the same genetic markers. 

#We know that how the disease progresses/ changes rapidly, and that the 
#presence of different genetic markers determines differences in the bacterial proteins,
# which will affect their response to a potential vaccine. Using this, we create a bar plot
#figure to show the prevalence of each genetic marker across all patients for countryX and compare
#with countryY. From the figure, we can see that the patients tested in countryX are generally
#affected by the first 5 genetic markers, and have very low rates of genetic markers 6-10.
#Contrastingly, patients of countryY have lower rates of genetic markers 1-5, and significantly higher
#rates of genetic markers 6-10. If countryY were to develop a vaccine, it would likely only work to treat 
#proteins present in markers 6-10, and not 1-5. 


#QUESTION 2: Figure (Bar Plot)

# Load required libraries
library(ggplot2)

# Load the data
data <- read.csv("data/compiled_data.csv")

# Calculate marker presence proportions for each country
marker_columns <- grep("marker", names(data), value = TRUE)

# Initialize an empty list to store results
marker_summary <- list()

# Loop through each unique country in the "country" column
for (country in unique(data$country)) {
  # Subset the data for the current country
  country_data <- data[data$country == country, ]
  # Calculate the mean for each marker column
  proportions <- colMeans(country_data[, marker_columns])
  # Store the proportions in a data frame and add it to the "marker_summary" list
  marker_summary[[country]] <- data.frame(
    # Include the marker names as a column
    marker = marker_columns,
    # Include calculated proportions as a column
    proportion = proportions,
    # Include the country name as a column
    country = country
  )
}

# Combine all country specific data frames into one large data frame
marker_summary_df <- do.call(rbind, marker_summary)
# Clean up marker names by replacing "marker" with "Marker" for better readability 
marker_summary_df$marker <- gsub("marker", "Marker ", marker_summary_df$marker)
# Recode the country names
# This changes the name of "countryX" to "Country X" and "countryY" to "Country Y"
marker_summary_df$country <- ifelse(marker_summary_df$country == "countryX", "Country X", "Country Y")

# Create a bar plot using ggplot2
plot <- ggplot(marker_summary_df, aes(x = marker, y = proportion, fill = country)) +
  # Adds bars for each marker with dodge position for side-by-side comparison
  geom_bar(stat = "identity", position = "dodge") +
  # Labeling plot 
  labs(
    title = "Comparison of Genetic Marker Proportions (Country X vs. Country Y)",
    x = "Genetic Marker",
    y = "Proportion of Patients with Marker Present",
    fill = "Country"
  ) +
  theme_minimal() +
  # Rotate x-axis labels for better readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Print plot
print(plot)


#Set working directory to be RProject2024 file, where supportingFunctions.R file and all project contents are
setwd("C:/Users/lisse/Downloads/Rproject2024")

#Check that working directory is RProject2024
getwd()

#Load functions made in file "supportingFunctions.R" into new R script ("Analysis.R")
source("supportingFunctions.R")

converting("data")
compiling(og_directory = "data", out_directory = "C:/Users/lisse/Downloads/Rproject2024/data/compiled_data.csv")
summarizing("data")
summarizing(actual_data)
actual_data <- read.csv("C:/Users/lisse/Downloads/Rproject2024/allData.csv")
#QUESTION 1: In which country did the outbreak begin?
#The outbreak likely began in CountryX.

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
data <- read.csv("/Users/lisse/Downloads/RProject2024/allData.csv")

data$total_markers <- rowSums(data[, grep("marker", names(data))])

country_x <- subset(data, country == "X")
country_y <- subset(data, country == "Y")

#Two linear regressions (countryX and countryY)
#
lm_x <- lm(total_markers ~ dayofYear, data = country_x)
lm_y <- lm(total_markers ~ dayofYear, data = country_y)

data$country_label <- ifelse(data$country == "X", "Country X", "Country Y")

#Plot shows average number of genetic markers present in each patient on each screening day, separated by country
plot <- ggplot(data, aes(x = dayofYear, y = total_markers, color = country_label)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, aes(group = country_label), linetype = "solid") +
  labs(
    title = "Genetic Markers",
    x = "Day of Year",
    y = "Mean # of Markers",
    color = "Country"
  ) +
  theme_minimal()

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
summary_x <- summary(lm_x)
r_squared_x <- summary_x$r.squared
p_value_x <- summary_x$coefficients[2, 4]

# Extract p-values and R-squared for Country Y
summary_y <- summary(lm_y)
r_squared_y <- summary_y$r.squared
p_value_y <- summary_y$coefficients[2, 4]

# Print the results
cat("Country X:\n")
cat("R-squared:", r_squared_x, "\n")
cat("P-value (dayofYear):", p_value_x, "\n\n")

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
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Q-Q Plot of Residuals (Country X)",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

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
data <- read.csv("/Users/lisse/Downloads/RProject2024/allData.csv")

# Calculate marker presence proportions for each country
marker_columns <- grep("marker", names(data), value = TRUE)

# Initialize an empty list to store results
marker_summary <- list()

for (country in unique(data$country)) {
  country_data <- data[data$country == country, ]
  proportions <- colMeans(country_data[, marker_columns])
  marker_summary[[country]] <- data.frame(
    marker = marker_columns,
    proportion = proportions,
    country = country
  )
}

marker_summary_df <- do.call(rbind, marker_summary)

marker_summary_df$marker <- gsub("marker", "Marker ", marker_summary_df$marker)

marker_summary_df$country <- ifelse(marker_summary_df$country == "X", "Country X", "Country Y")

plot <- ggplot(marker_summary_df, aes(x = marker, y = proportion, fill = country)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Comparison of Genetic Marker Proportions (Country X vs. Country Y)",
    x = "Genetic Marker",
    y = "Proportion of Patients with Marker Present",
    fill = "Country"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot)


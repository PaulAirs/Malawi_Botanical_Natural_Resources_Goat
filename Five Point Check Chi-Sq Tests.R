# Five Point Check Chi-Square Tests
install.packages('tidyverse')

# Load libraries
library(dplyr)

setwd("~/Library/CloudStorage/OneDrive-MurrayEdwardsCollege/BotanicalTST_followupSurvey")

# Read your data (adjust path or use read.csv for CSV)
df <- readxl::read_excel("ChiSq_FPC.xlsx", skip = 1)

# Rename columns (adjust if needed)
colnames(df)[1:6] <- c("Question Set", "Question", "TST_Yes", "TST_No", "Control_Yes", "Control_No")

# Clean: remove rows without a question
df <- df %>% filter(!is.na(Question))

# Prepare results container
results <- data.frame(
  Question = character(),
  Test = character(),
  Statistic = numeric(),
  p_value = numeric(),
  df = character(),
  stringsAsFactors = FALSE
)

# Loop through each row
for (i in 1:nrow(df)) {
  row <- df[i, ]
  table <- matrix(c(row$TST_Yes, row$TST_No, row$Control_Yes, row$Control_No),
                  nrow = 2, byrow = TRUE)
  
# Try Chi-square first
  chi <- tryCatch({
    test <- chisq.test(table)
    list(test = "Chi-square",
         stat = test$statistic,
         p = test$p.value,
         df = test$parameter)
  }, error = function(e) {
    NULL
  })
  
# If expected counts too low, use Fisher's
  if (is.null(chi) || any(chisq.test(table)$expected < 5)) {
    fisher <- fisher.test(table)
    results[i, ] <- list(
      Question = row$Question,
      Test = "Fisher's Exact",
      Statistic = fisher$estimate,
      p_value = fisher$p.value,
      df = "NA"
    )
  } else {
    results[i, ] <- list(
      Question = row$Question,
      Test = "Chi-square",
      Statistic = chi$stat,
      p_value = chi$p,
      df = chi$df
    )
  }
}

# Print or save results
print(results)
write.csv(results, "ChiSq_FPC_results.csv", row.names = FALSE)


###############
# Question 41 #
###############

# Confident

# Create the 2x2 matrix
resource_matrix <- matrix(
  c(17, 25,   # TST-study: Yes, No/Blank
    3, 87),   # Controls: Yes, No/Blank
  nrow = 2,
  byrow = TRUE
)

# Label rows and columns
rownames(resource_matrix) <- c("TST-study", "Controls")
colnames(resource_matrix) <- c("Yes", "No_or_Blank")

# View the table
print(resource_matrix)

# Run Chi-squared test
chi_result <- chisq.test(resource_matrix)
print(chi_result)

# Run Fisher's exact test (more accurate with small cell counts)
fisher_result <- fisher.test(resource_matrix)
print(fisher_result)


# Use less resources

# Create the 2x2 contingency table
resource_matrix <- matrix(
  c(19, 23,   # TST-study
    2, 88),   # Controls
  nrow = 2,
  byrow = TRUE
)

# Add labels
rownames(resource_matrix) <- c("TST-study", "Controls")
colnames(resource_matrix) <- c("Yes", "No_or_Blank")

# View the matrix
print(resource_matrix)

# Chi-squared test
chi_result <- chisq.test(resource_matrix)
print(chi_result)

# Fisher's exact test (recommended due to small cell: 2)
fisher_result <- fisher.test(resource_matrix)
print(fisher_result)


# Better tell when sick

# Create the 2x2 contingency table
sick_matrix <- matrix(
  c(39, 3,   # TST-study
    62, 28),   # Controls
  nrow = 2,
  byrow = TRUE
)

# Add labels
rownames(sick_matrix) <- c("TST-study", "Controls")
colnames(sick_matrix) <- c("Yes", "No_or_Blank")

# View the matrix
print(sick_matrix)

# Chi-squared test
chi_result <- chisq.test(sick_matrix)
print(chi_result)

# Keep goats healthy

# Create the 2x2 contingency table
sick_matrix <- matrix(
  c(35, 7,   # TST-study
    47, 43),   # Controls
  nrow = 2,
  byrow = TRUE
)

# Add labels
rownames(sick_matrix) <- c("TST-study", "Controls")
colnames(sick_matrix) <- c("Yes", "No_or_Blank")

# View the matrix
print(sick_matrix)

# Chi-squared test
chi_result <- chisq.test(sick_matrix)
print(chi_result)


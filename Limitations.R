##################
# Limitations ####
##################

# Raw data for each limitation
data <- data.frame(
  Limitation = c("Cash", "Breeding bucks", "Feed", "Labour", "Theft", "Weather", "Disease", "Health", "Dog bites"),
  Plant_TST_Yes = c(22, 5, 7, 2, 12, 1, 5, 1, 15),
  Plant_TST_No  = c(0, 17, 15, 20, 10, 21, 17, 21, 7),
  TST_Yes       = c(18, 6, 7, 5, 11, 0, 13, 6, 12),
  TST_No        = c(2, 14, 13, 15, 9, 20, 7, 14, 8),
  Controls_Yes  = c(79, 16, 11, 5, 46, 10, 41, 10, 57),
  Controls_No   = c(10, 73, 78, 84, 43, 79, 48, 79, 32)
)

# Function to run all tests and return formatted results
run_tests <- function(g1_yes, g1_no, g2_yes, g2_no) {
  matrix <- matrix(c(g1_yes, g1_no, g2_yes, g2_no), nrow = 2, byrow = TRUE)
  
  # Chi-squared
  chi <- chisq.test(matrix)
  chi_result <- paste0(round(chi$statistic, 3), " (", round(chi$p.value, 4), ")")
  
  # Fisher and OR
  fisher <- fisher.test(matrix, alternative = "two.sided")
  or_result <- paste0(round(fisher$estimate, 2), " (", round(fisher$p.value, 4), ")")
  
  return(list(chi = chi_result, or = or_result))
}

# Prepare empty result table
results <- data.frame(
  Limitation = data$Limitation,
  `Plant-TST vs Controls - Chi2 (p)` = NA,
  `Plant-TST vs Controls - OR (p)` = NA,
  `Plant-TST vs TST - Chi2 (p)` = NA,
  `Plant-TST vs TST - OR (p)` = NA,
  `TST vs Controls - Chi2 (p)` = NA,
  `TST vs Controls - OR (p)` = NA,
  stringsAsFactors = FALSE
)

# Loop through each row and fill in the results
for (i in 1:nrow(data)) {
  # Extract row
  row <- data[i, ]
  
  # Run tests
  res_pc <- run_tests(row$Plant_TST_Yes, row$Plant_TST_No, row$Controls_Yes, row$Controls_No)
  res_pt <- run_tests(row$Plant_TST_Yes, row$Plant_TST_No, row$TST_Yes, row$TST_No)
  res_tc <- run_tests(row$TST_Yes, row$TST_No, row$Controls_Yes, row$Controls_No)
  
  # Store results
  results[i, 2] <- res_pc$chi
  results[i, 3] <- res_pc$or
  results[i, 4] <- res_pt$chi
  results[i, 5] <- res_pt$or
  results[i, 6] <- res_tc$chi
  results[i, 7] <- res_tc$or
}

# Show the final formatted table
print(results)

write.csv(results, "LimitationsTests.csv")

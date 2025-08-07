###########################
# Q15 Feed for sick goats #
###########################

# Create the 3x2 matrix
response_matrix <- matrix(
  c(15, 7,
    17, 3,
    49, 41),
  nrow = 3,
  byrow = TRUE,
  dimnames = list(c("Plant-TST", "TST", "Controls"), c("Yes_Sometimes", "No"))
)

# Function to run chi-squared and pairwise two-tailed Fisher's tests
run_tests <- function(table, name) {
  cat("====", name, "====\n")
  
  # Chi-squared test
  chi <- chisq.test(table)
  cat("Chi-squared test:\n")
  print(chi)
  cat("\n")
  
  # Pairwise 2x2 Fisher's tests (two-sided)
  group_names <- rownames(table)
  pairs <- combn(group_names, 2, simplify = FALSE)
  
  for (pair in pairs) {
    sub <- table[pair, ]
    cat("Fisher's test:", pair[1], "vs", pair[2], "\n")
    fisher <- fisher.test(sub, alternative = "two.sided")
    print(fisher)
    cat("\n")
  }
}

# Run the tests
run_tests(response_matrix, "Use of Trees for Sick Goats (Yes/Sometimes vs No)")



###############################
# Beneficial plant harvesting #
###############################

# Data setup
kakhobo <- matrix(c(17, 5, 7, 13, 17, 72), nrow = 3, byrow = TRUE,
                  dimnames = list(c("Plant-TST", "TST", "Controls"), c("Yes", "No")))
mtawa   <- matrix(c(15, 7, 8, 12, 26, 64), nrow = 3, byrow = TRUE,
                  dimnames = list(c("Plant-TST", "TST", "Controls"), c("Yes", "No")))
malayina <- matrix(c(19, 3, 6, 14, 40, 50), nrow = 3, byrow = TRUE,
                   dimnames = list(c("Plant-TST", "TST", "Controls"), c("Yes", "No")))

# Function to run full test for one plant
run_tests <- function(table, name) {
  cat("====", name, "====\n")
  
  # Chi-squared test across all groups
  chi <- chisq.test(table)
  cat("Chi-squared test:\n")
  print(chi)
  cat("\n")
  
  # Pairwise 2x2 Fisher's Exact Tests (two-tailed)
  group_names <- rownames(table)
  pairs <- combn(group_names, 2, simplify = FALSE)
  
  for (pair in pairs) {
    sub <- table[pair, ]
    cat("Fisher's test:", pair[1], "vs", pair[2], "\n")
    fisher <- fisher.test(sub, alternative = "two.sided")
    print(fisher)
    cat("\n")
  }
}

# Run tests for each plant
run_tests(kakhobo, "Kakhobo")
run_tests(mtawa, "Mtawa")
run_tests(malayina, "Malayina")


####################################
# Beneficial plant supplementation #
####################################

# Data setup
kakhobo <- matrix(c(12, 10, 7, 13, 12, 77), nrow = 3, byrow = TRUE,
                  dimnames = list(c("Plant-TST", "TST", "Controls"), c("Yes", "No")))
mtawa   <- matrix(c(10, 12, 8, 12, 18, 72), nrow = 3, byrow = TRUE,
                  dimnames = list(c("Plant-TST", "TST", "Controls"), c("Yes", "No")))
malayina <- matrix(c(16, 6, 5, 15, 23, 67), nrow = 3, byrow = TRUE,
                   dimnames = list(c("Plant-TST", "TST", "Controls"), c("Yes", "No")))

# Function to run full test for one plant
run_tests <- function(table, name) {
  cat("====", name, "====\n")
  
  # Chi-squared test across all groups
  chi <- chisq.test(table)
  cat("Chi-squared test:\n")
  print(chi)
  cat("\n")
  
  # Pairwise 2x2 Fisher's Exact Tests (two-tailed)
  group_names <- rownames(table)
  pairs <- combn(group_names, 2, simplify = FALSE)
  
  for (pair in pairs) {
    sub <- table[pair, ]
    cat("Fisher's test:", pair[1], "vs", pair[2], "\n")
    fisher <- fisher.test(sub, alternative = "two.sided")
    print(fisher)
    cat("\n")
  }
}

# Run tests for each plant
run_tests(kakhobo, "Kakhobo")
run_tests(mtawa, "Mtawa")
run_tests(malayina, "Malayina")


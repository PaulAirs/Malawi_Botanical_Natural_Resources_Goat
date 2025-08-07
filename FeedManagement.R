###################################
# Feed management - dry cut carry #
###################################

# Create the 2x2 contingency table
cutcarry_matrix <- matrix(
  c(17, 25,   # TST-study
    12, 77),   # Controls
  nrow = 2,
  byrow = TRUE
)

# Add labels
rownames(cutcarry_matrix) <- c("TST-study", "Controls")
colnames(cutcarry_matrix) <- c("Yes", "No")

# View the matrix
print(cutcarry_matrix)

# Chi-squared test
chi_result <- chisq.test(cutcarry_matrix)
print(chi_result)
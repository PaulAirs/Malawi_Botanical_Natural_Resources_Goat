################
# Demographics #
################

install.packages("patchwork")

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

##########
# Gender #
##########

# Create the data frame
gender_data <- data.frame(
  Group = c("TST-study", "TST-study", "Controls", "Controls"),
  Gender = c("Male", "Female", "Male", "Female"),
  Percentage = c(46.34146341, 53.65853659, 34.48275862, 65.51724138)
)

# Plot
a <- ggplot(gender_data, aes(x = Gender, y = Percentage, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # add outline
  labs(x = "Gender",
       y = "Percentage (%)") +
  scale_fill_manual(values = c("white", "darkgreen")) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )

# Create the observed counts table
gender_matrix <- matrix(
  c(19, 30, # TST-study
    22, 57), # Controls
  nrow = 2,
  byrow = TRUE
)

# Add labels
colnames(gender_matrix) <- c("Male", "Female")
rownames(gender_matrix) <- c("TST", "Control")

# View the table
print(gender_matrix)

# Run Chi-squared test
chisq.test(gender_matrix)


###############
# AGE #########
###############

# Income as proportions
age_data <- data.frame(
  Age = c("60+","18-29","30-39","40-49","50-59"),
  TST_study = c(41.46341463,
                9.756097561,
                14.63414634,
                21.95121951,
                12.19512195),
  Controls = c(30,
               11.11111111,
               23.33333333,
               21.11111111,
               13.33333333)
)
# Convert to long format for ggplot
age_long <- age_data %>%
  pivot_longer(cols = c("TST_study", "Controls"),
               names_to = "Group",
               values_to = "Percentage")
# Plot
b <- ggplot(age_long, aes(x = Age, y = Percentage, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(x = "Age Category",
       y = "Percentage (%)") +
  scale_fill_manual(values = c("white", "darkgreen")) +  # TST-study and Controls colors
  theme_bw() +
  theme(
    text = element_text(size = 14),             # Base size for all text
    axis.title = element_text(size = 16),       # Axis titles
    axis.text = element_text(size = 14),        # Axis tick labels
    legend.title = element_text(size = 14),     # Legend title
    legend.text = element_text(size = 13),       # Legend items
    axis.text.x = element_text(angle = 45, hjust = 1))


# Create the observed counts table
age_matrix <- matrix(
  c(17,4,6,9,5,  # TST-study
    27,10,21,19,12),  # Controls
  nrow = 2,
  byrow = TRUE
)

# Add labels
colnames(age_matrix) <- c("60+","18-29","30-39","40-49","50-59")
rownames(age_matrix) <- c("TST", "Control")

# View the table
print(age_matrix)

# Run Chi-squared test
chisq.test(age_matrix)
fisher.test(age_matrix, simulate.p.value = TRUE, B = 10000)


#############
# Education #
#############

# Education as proportions
education_data <- data.frame(
  Education = c("JC", "None", "Form 4-5", "Primary", "Prefer not to say"),
  TST_study = c(11.9047619,
                14.28571429,
                11.9047619,
                59.52380952,
                2.380952381),
  Controls = c(8.888888889,
               18.88888889,
               5.555555556,
               58.88888889,
               5.555555556)
)

# Convert to long format for ggplot
education_long <- education_data %>%
  pivot_longer(cols = c("TST_study", "Controls"),
               names_to = "Group",
               values_to = "Percentage")
# Plot
c <- ggplot(education_long, aes(x = Education, y = Percentage, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(x = "Education Category",
       y = "Percentage (%)") +
  scale_fill_manual(values = c("white", "darkgreen")) +  # TST-study and Controls colors
  theme_bw() +
  theme(
    text = element_text(size = 14),             # Base size for all text
    axis.title = element_text(size = 16),       # Axis titles
    axis.text = element_text(size = 14),        # Axis tick labels
    legend.title = element_text(size = 14),     # Legend title
    legend.text = element_text(size = 13),       # Legend items
    axis.text.x = element_text(angle = 45, hjust = 1))


# Create the observed counts table
education_matrix <- matrix(
  c(5,6,5,25,1,   # TST-study
    8,17,5,53,5),  # Controls
  nrow = 2,
  byrow = TRUE
)

# Add labels
colnames(education_matrix) <- c("JC", "None", "Form 4-5", "Primary", "Prefer not to say")
rownames(education_matrix) <- c("TST", "Control")

# View the table
print(education_matrix)

# Run Chi-squared test
chisq.test(education_matrix)
fisher.test(education_matrix, simulate.p.value = TRUE, B = 10000)



##########
# INCOME #
##########

# Income as proportions
income_data <- data.frame(
  Income = c("Employee", "Entrepreneur", "Farmer", "Farmer / entrepreneur",
             "Not working", "Retired", "Self-employed"),
  TST_study = c(2.439, 12.195, 78.049, 7.317, 0, 2.439, 0),
  Controls = c(1.111, 5.556, 71.111, 6.667, 10, 1.111, 3.333)
)
# Convert to long format for ggplot
income_long <- income_data %>%
  pivot_longer(cols = c("TST_study", "Controls"),
               names_to = "Group",
               values_to = "Percentage")
# Plot
d<- ggplot(income_long, aes(x = Income, y = Percentage, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(x = "Income Category",
       y = "Percentage (%)") +
  scale_fill_manual(values = c("white", "darkgreen")) +  # TST-study and Controls colors
  theme_bw() +
  theme(
    text = element_text(size = 14),             # Base size for all text
    axis.title = element_text(size = 16),       # Axis titles
    axis.text = element_text(size = 14),        # Axis tick labels
    legend.title = element_text(size = 14),     # Legend title
    legend.text = element_text(size = 13),       # Legend items
    axis.text.x = element_text(angle = 45, hjust = 1))


# Create the observed counts table
income_matrix <- matrix(
  c(1, 5, 32, 3, 0, 1, 0,   # TST-study
    1, 5, 64, 6, 9, 1, 3),  # Controls
  nrow = 2,
  byrow = TRUE
)

# Add labels
colnames(income_matrix) <- c("Employee", "Entrepreneur", "Farmer",
                             "Farmer_Ent", "Not_working", "Retired", "Self_employed")
rownames(income_matrix) <- c("TST", "Control")

# View the table
print(income_matrix)

# Run Chi-squared test
chisq.test(income_matrix)
fisher.test(income_matrix, simulate.p.value = TRUE, B = 10000)

########################
# Food worry by income #
########################

income_foodworry <- data.frame(
  Income = c("Employee", "Entrepreneur/Self-employed", "Farmer", "Farmer/Entrepreneur", "Not working / Retired"),
  FoodWorryProp = c(50,
                    23.07692308,
                    31.1827957,
                    22.22222222,
                    88.88888889),
  Count = c(2,
            13,
            93,
            9,
            9))

e <- ggplot(data=income_foodworry, aes(x=Income, y=FoodWorryProp)) 

e <- ggplot(income_foodworry, aes(x = Income, y = FoodWorryProp)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Income Category",
       y = "Percentage Food Worry (%)") +
  theme_bw() +
  theme(
    text = element_text(size = 14),             # Base size for all text
    axis.title = element_text(size = 16),       # Axis titles
    axis.text = element_text(size = 14),        # Axis tick labels
    legend.title = element_text(size = 14),     # Legend title
    legend.text = element_text(size = 13),       # Legend items
    axis.text.x = element_text(angle = 45, hjust = 1))
e

###############
# FIGURE ######
###############

library(patchwork)

top_row <- (a + labs(tag = "a")) | (d + labs(tag = "c"))
bottom_row <- (b + labs(tag = "b")) | (c + labs(tag = "d"))

# Combine rows and adjust spacing between a and b with widths
combined_plot <- (top_row + plot_layout(widths = c(0.5, 1))) /
  bottom_row

# Display
combined_plot

ggsave("combined_plot.pdf",
       plot = combined_plot,
       width = 10,     # inches
       height = 8,     # inches
       dpi = 300       # print quality
)

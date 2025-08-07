################
# Livelihoods ##
################

# libraries
library(ggplot2)
library(tidyr)
library(dplyr)

setwd("~/Library/CloudStorage/OneDrive-MurrayEdwardsCollege/BotanicalTST_followupSurvey")



# TST-study vs Controls
Test <- matrix(c(37,49,7,13), nrow = 2, byrow = TRUE)

rownames(Test) <- c("Participate_Yes", "Participate_No")
colnames(Test) <- c("Yes", "No")
print(Test)
fisher.test(Test)
chisq.test(Test)


##################
# Capital Goods ##
##################

# Load the data (replace with your path if needed)
library(readr)
df <- read_csv("CapitalGoods.csv")

# Convert from wide to long format
df_long <- df %>%
  pivot_longer(
    cols = -Item,
    names_to = "Group",
    values_to = "Percentage"
  )

ggplot(df_long, aes(x = Item, y = Percentage, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    x = "Capital Good",
    y = "Percentage (%)"
  ) +
  scale_fill_manual(
    name = "Group",
    values = c(
      "2019 Survey" = "white",
      "2025 Survey" = "darkgreen",
      "TST-study" = "darkseagreen1",
      "Controls" = "darkseagreen"
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(angle = 0, hjust = 1, size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

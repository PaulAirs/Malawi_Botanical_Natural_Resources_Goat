#####################
# FPC Venn Diagrams #
#####################

install.packages('ggVennDiagram')
install.packages('gridExtra')

setwd("~/Library/CloudStorage/OneDrive-MurrayEdwardsCollege/BotanicalTST_followupSurvey")

# Load required libraries
library(readr)
library(dplyr)
library(ggVennDiagram)
library(ggplot2)

# Read the cleaned CSV file
df <- read_csv("FPC_Venn_R.csv")

# Create output folder (optional)
output_dir <- "venn_exports"
if (!dir.exists(output_dir)) dir.create(output_dir)

# Function to create Venn diagram with green fill
create_venn_plot <- function(sub_df, title) {
  sets <- list(
    Know = which(sub_df$Know),
    Use = which(sub_df$Use),
    Useful = which(sub_df$Useful)
  )
  
  ggVennDiagram(sets, label_alpha = 0) +
    scale_fill_gradient(low = "white", high = "darkgreen") +
    ggtitle(title) +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      legend.position = "none"
    )
}

# Group and generate + export plots faceted by Participate and FPC Test
grouped1 <- df %>%
  group_by(Participate, FPC_Test) %>%
  group_split()

for (sub in grouped1) {
  title <- paste("Participate:", unique(sub$Participate),
                 "\nFPC Test:", unique(sub$FPC_Test))
  p <- create_venn_plot(sub, title)
  filename <- paste0(output_dir, "/venn_participate_",
                     unique(sub$Participate), "_",
                     unique(sub$FPC_Test), ".pdf")
  ggsave(filename, plot = p, width = 6, height = 6)
}

# Group and generate + export plots faceted by Group and FPC Test
grouped2 <- df %>%
  group_by(Group, FPC_Test) %>%
  group_split()

for (sub in grouped2) {
  title <- paste("Group:", unique(sub$Group),
                 "\nFPC Test:", unique(sub$FPC_Test))
  p <- create_venn_plot(sub, title)
  filename <- paste0(output_dir, "/venn_group_",
                     gsub("[^a-zA-Z0-9]", "_", unique(sub$Group)), "_",
                     unique(sub$FPC_Test), ".pdf")
  ggsave(filename, plot = p, width = 6, height = 6)
}

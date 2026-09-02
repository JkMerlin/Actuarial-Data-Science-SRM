library(readxl)
library(dplyr)
library(glue)

# Load syllabus
df <- read_excel("parcourt.xlsx")

# Rename columns for clarity
df <- df %>% 
  rename(
    Section = 1,
    Subsection = 2,
    Source = 3,
    Total = 4,
    Completed = 5
  ) %>% 
  mutate(Status = Completed / Total)

# Define SRM exam sections with row ranges + weights
sections <- list(
  "Basics of Statistical Learning" = list(rows = 7:18, weight = 0.10),
  "Linear Models" = list(rows = 19:48, weight = 0.45),
  "Time Series Models" = list(rows = 49:63, weight = 0.125),
  "Decision Trees" = list(rows = 64:74, weight = 0.225),
  "Unsupervised Learning" = list(rows = 75:86, weight = 0.125)
)

# Compute progress
progress <- lapply(names(sections), function(name) {
  rows <- sections[[name]]$rows
  weight <- sections[[name]]$weight
  
  total <- sum(df$Total[rows], na.rm = TRUE)
  done <- sum(df$Completed[rows], na.rm = TRUE)
  
  pct <- done / total
  weighted <- pct * weight
  
  list(
    name = name,
    pct = pct,
    weighted = weighted
  )
})

# Build README block
progress_block <- "## 📊 Automated Progress Summary\n\n"

for (p in progress) {
  progress_block <- paste0(
    progress_block,
    glue("- **{p$name}**: {scales::percent(p$pct, accuracy = 0.1)} complete (weighted: {scales::percent(p$weighted, accuracy = 0.1)})\n")
  )
}

# Read README
readme <- readLines("README.md")

# Replace placeholder
start <- grep("<!-- PROGRESS_BLOCK -->", readme)

readme[start] <- progress_block

# Write updated README
writeLines(readme, "README.md")

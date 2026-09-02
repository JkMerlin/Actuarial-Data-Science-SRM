library(readxl)
library(dplyr)
library(glue)
library(scales)

df <- read_csv("parcourt.csv")

df <- df %>%
  mutate(
    Section = ifelse(!is.na(Section), Section, NA),
    Section = zoo::na.locf(Section)  # fill down section names
  ) %>%
  filter(!is.na(Total)) %>%   # only rows with numeric data
  mutate(Status = Completed / Total)

weights <- c(
  "Basics of Statistical Learning 5-10%" = 0.10,
  "Linear Models 40-50%" = 0.45,
  "Time Series Models 10-15%" = 0.125,
  "Decision Trees 20-25%" = 0.225,
  "Unsupervised Learning Techniques 10-15%" = 0.125
)

progress <- df %>%
  group_by(Section) %>%
  summarise(
    pct = sum(Completed, na.rm = TRUE) / sum(Total, na.rm = TRUE)
  ) %>%
  mutate(weighted = pct * weights[Section])

progress_block <- "## 📊 Automated Progress Summary\n\n"

for (i in 1:nrow(progress)) {
  progress_block <- paste0(
    progress_block,
    glue("- **{progress$Section[i]}**: {percent(progress$pct[i], accuracy = 0.1)} complete (weighted: {percent(progress$weighted[i], accuracy = 0.1)})\n")
  )
}

readme <- readLines("README.md")
start <- grep("<!-- PROGRESS_BLOCK -->", readme)
readme[start] <- progress_block
writeLines(readme, "README.md")

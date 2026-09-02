library(dplyr)
library(glue)
library(scales)

# ============================================================
# SOA SRM Automated Progress Tracker
# ============================================================

# Read study progress data
df <- read.csv(
  "parcourt.csv",
  stringsAsFactors = FALSE
)

# Clean data
df <- df %>%
  mutate(
    Section = trimws(gsub("\\s+", " ", Section)),
    Total = as.numeric(Total),
    Completed = as.numeric(Completed)
  ) %>%
  filter(!is.na(Total), Total > 0)

# ------------------------------------------------------------
# Exam topic weights
# Using midpoint of published ranges
# ------------------------------------------------------------

weights <- c(
  "Introduction and Review 0%" = 0.00,
  "Basics of Statistical Learning 5-10%" = 0.1,
  "Linear Models 40-50%" = 0.45,
  "Time Series Models 10-15%" = 0.125,
  "Decision Trees 20-25%" = 0.225,
  "Unsupervised Learning Techniques 10-15%" = 0.125
)

# ------------------------------------------------------------
# Calculate topic-level progress
# ------------------------------------------------------------

progress <- df %>%
  group_by(Section) %>%
  summarise(
    Total = sum(Total, na.rm = TRUE),
    Completed = sum(Completed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct = Completed / Total,
    weight = unname(weights[Section]),
    weighted = pct * weight
  )

# ------------------------------------------------------------
# Overall progress
# ------------------------------------------------------------

overall_total <- sum(progress$Total)

overall_completed <- sum(progress$Completed)

overall_pct <- overall_completed / overall_total

weighted_progress <- sum(progress$weighted, na.rm = TRUE)

# ============================================================
# Study Priorities
# ============================================================

priority <- progress %>%
  filter(Total > 0, pct < 1) %>%
  arrange(desc(weight), pct) %>%
  slice_head(n = 3)

progress_block <- c(
  progress_block,
  "### 🎯 Current Study Priorities",
  ""
)

for (i in seq_len(nrow(priority))) {
  progress_block <- c(
    progress_block,
    paste0(
      i, ". **", priority$Section[i], "** — ",
      priority$Completed[i], " / ", priority$Total[i],
      " completed (",
      percent(priority$pct[i], accuracy = 0.1),
      ")"
    )
  )
}

progress_block <- c(
  progress_block,
  ""
)

# ------------------------------------------------------------
# Progress bar
# ------------------------------------------------------------

progress_bar <- function(pct, width = 20) {
  
  completed <- round(pct * width)
  
  paste0(
    strrep("█", completed),
    strrep("░", width - completed),
    " ",
    percent(pct, accuracy = 0.1)
  )
}

# ------------------------------------------------------------
# Build README progress block
# ------------------------------------------------------------

progress_block <- c(
    "",
  paste0(
    "**Overall Reading Progress:** ",
    percent(overall_pct, accuracy = 0.1)
  ),
  "",
  paste0(
    "`",
    progress_bar(overall_pct),
    "`"
  ),
  "",
  paste0(
    "**Exam-Weighted Reading Progress:** ",
    percent(weighted_progress, accuracy = 0.1)
  ),
  "",
  "| SRM Topic | Completed | Total | Progress | Exam Weight |",
  "|---|---:|---:|---:|---:|"
)

# ------------------------------------------------------------
# Add topic rows
# ------------------------------------------------------------

for (i in seq_len(nrow(progress))) {
  
  section <- progress$Section[i]
  
  progress_block <- c(
    progress_block,
    paste0(
      "| ",
      section,
      " | ",
      progress$Completed[i],
      " | ",
      progress$Total[i],
      " | ",
      percent(progress$pct[i], accuracy = 0.1),
      " | ",
      percent(progress$weight[i], accuracy = 0.1),
      " |"
    )
  )
}

# ------------------------------------------------------------
# Add visual topic progress
# ------------------------------------------------------------

progress_block <- c(
  progress_block,
  "",
  "### Topic Progress",
  ""
)

for (i in seq_len(nrow(progress))) {
  
  progress_block <- c(
    progress_block,
    paste0(
      "**",
      progress$Section[i],
      "**  ",
      "`",
      progress_bar(progress$pct[i]),
      "`"
    ),
    ""
  )
}

# ------------------------------------------------------------
# Timestamp
# ------------------------------------------------------------

progress_block <- c(
  progress_block,
  paste0(
    "*Last updated: ",
    format(Sys.time(), "%B %d, %Y at %I:%M %p"),
    "*"
  )
)

# ------------------------------------------------------------
# Update README
# ------------------------------------------------------------

readme <- readLines(
  "README.md",
  warn = FALSE
)

start_marker <- "<!-- AUTO_PROGRESS_START -->"
end_marker <- "<!-- AUTO_PROGRESS_END -->"

start <- match(start_marker, readme)
end <- match(end_marker, readme)

if (is.na(start) || is.na(end)) {
  stop(
    paste0(
      "README.md must contain both:\n",
      start_marker,
      "\n",
      end_marker
    )
  )
}

if (start >= end) {
  stop("Invalid README markers.")
}

# Replace content between markers
readme <- c(
  readme[1:start],
  progress_block,
  readme[end:length(readme)]
)

writeLines(
  readme,
  "README.md",
  useBytes = TRUE
)

# ------------------------------------------------------------
# Verify
# ------------------------------------------------------------

cat("\nREADME updated successfully!\n")
cat("Overall reading progress: ",
    percent(overall_pct, accuracy = 0.1), "\n", sep = "")

cat("Exam-weighted progress: ",
    percent(weighted_progress, accuracy = 0.1), "\n", sep = "")

cat("\nREADME progress section:\n")
cat(
  paste(
    readme[start:min(end + 20, length(readme))],
    collapse = "\n"
  )
)
cat("\n")
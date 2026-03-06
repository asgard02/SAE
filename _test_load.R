setwd("C:/Users/Fixer/Documents/CEM/SAE_CEM/.claude/worktrees/dreamy-swartz")
writeLines("=== TEST START ===")
writeLines(paste("R version:", R.version.string))
writeLines(paste("Working dir:", getwd()))

# Test: can we read the CSV?
df_brut <- read.csv("patients_data.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))
writeLines(paste("CSV rows:", nrow(df_brut), "| cols:", ncol(df_brut)))
writeLines(paste("Columns:", paste(names(df_brut), collapse=", ")))

# Test packages installed
pkgs <- c("shiny","bslib","dplyr","ggplot2","DT","lubridate","tidyr","scales",
          "survival","broom","forcats","stringr","shinycssloaders","plotly","shinyjs")
installed <- sapply(pkgs, function(p) requireNamespace(p, quietly=TRUE))
writeLines(paste("Installed:", sum(installed), "/", length(pkgs)))
missing <- pkgs[!installed]
if (length(missing) > 0) writeLines(paste("MISSING:", paste(missing, collapse=", ")))
writeLines("=== TEST END ===")

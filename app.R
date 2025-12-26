
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("DBI")
# install.packages("RMariaDB")
# install.packages("ggpubr")



library(tidyverse)
library(readxl)
library(dplyr)
library(DBI)
library(RMariaDB)

# Set the directory path (replace with your path)
directory <- "C:\\xampp\\htdocs\\TacticalDataDrivenDecisionSystem\\data"

# Get list of all Excel files in the directory
excel_files <- list.files(path = directory, pattern = "\\.xlsx$|\\.xls$", full.names = TRUE)

# Loop through each file and print contents
for (file in excel_files) {
  
  #cat("Read Sheets Begin \n")
  # Read all sheets in the Excel file
  sheets <- excel_sheets(file)
  #cat("Read Sheets End \n")
  
  for (sheet in sheets) {
    
    
    if (sheet=="Hawaii Tourism Data") {
      #cat("Read Excel Begin \n")
      suppressMessages({
        data <- read_excel(file, sheet = sheet)
      })
      #cat("Read Excel End \n")
      
      cat("\nReading file:", basename(file), "\n")
      cat("\nSheet:", sheet, "\n")
      print(data,n=25)# 1. Convert to matrix
      
      # Print column names for debugging
      print("Column names in the dataset:")
      print(colnames(data))
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      # Rename first column and clean up column names
      colnames(data)[1] <- "Group"
      # Remove the ...number pattern if it exists
      colnames(data) <- gsub("...\\d+", "", colnames(data))
      
      # Remove metadata rows at the bottom (assuming rows 20+ are metadata)
      data <- data[1:18, ]
      
      print(data)
      
      # Convert to long format - selecting columns by position instead
      # Assuming first 3 columns are Group, Units, Indicator and rest are dates
      data_long <- data %>%
        pivot_longer(
          cols = 4:ncol(data),  # Select all columns after the first 3
          names_to = "YYYY-MM",
          values_to = "value"
        ) %>%
        filter(!is.na(value)) %>%
        select(Group, Units, Indicator, `YYYY-MM`, value)
      
      # Function to create SQL INSERT statement
      create_insert <- function(row) {
        sprintf("INSERT INTO `htaaccommodationchoices`(`Group`, `Units`, `Indicator`, `YYYY-MM`, `value`) VALUES ('%s','%s','%s','%s','%s')",
                row$Group, 
                row$Units, 
                row$Indicator, 
                row$`YYYY-MM`, 
                row$value)
      }
      
      # Generate and print SQL statements
      sql_statements <- apply(data_long, 1, create_insert)
      cat(sql_statements, sep = "\n")
      
      # Print the reorganized data
      print(data_long)
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
    }
  }
}
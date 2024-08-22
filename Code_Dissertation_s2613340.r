# Install and load required packages
if (!requireNamespace("censReg", quietly = TRUE)) {
  install.packages("censReg")
}

if (!requireNamespace("deaR", quietly = TRUE)) {
  install.packages("deaR")
}
if (!requireNamespace("AER", quietly = TRUE)) {
  install.packages("AER")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(censReg)
library(dplyr)
library(ggplot2)
library(deaR)
library(AER)

# Load the Data
data_all <- read.csv("C:/Users/Nattakamon/OneDrive/UOE/Dissertation/Data_All.csv")

# Display the first few rows of the dataset 
head(data_all)

# Ensure necessary columns exist
required_columns <- c("ID", "Country", "Year", "Total_Assets", "Operating_Expenses", "Interest_Expense", 
                      "Total_Customer_Deposits", "Netloans", "Net_Interest", "GDP", 
                      "Inflation", "Unemployment")
if (!all(required_columns %in% colnames(data_all))) {
  stop("The dataset does not contain all the required columns. Please check your data.")
}

# Prepare data for DEA and calculate efficiency scores
dea_data_all <- make_deadata(datadea = data_all,
                             inputs = c("Total_Assets", "Operating_Expenses", "Interest_Expense", "Total_Customer_Deposits"),
                             outputs = c("Netloans", "Net_Interest"))

dea_result_all <- model_sbmeff(dea_data_all, orientation = "no", rts = "vrs")
efficiency_scores_all <- efficiencies(dea_result_all)
efficiency_scores_all[is.na(efficiency_scores_all)] <- 1
combined_data <- cbind(data_all, Efficiency = efficiency_scores_all)


# calculate slacks
slacks_all <- slacks(dea_result_all)
slacks_all$slack_input[is.na(slacks_all$slack_input)] <- 0
slacks_all$slack_output[is.na(slacks_all$slack_output)] <- 0

# Combine DMU names, Efficiency scores, and slacks with original data
combined_data <- cbind(data_all, 
                       Efficiency = efficiency_scores_all,
                       Slack_Total_Assets = slacks_all$slack_input[, "Total_Assets"],
                       Slack_Operating_Expenses = slacks_all$slack_input[, "Operating_Expenses"],
                       Slack_Interest_Expense = slacks_all$slack_input[, "Interest_Expense"],
                       Slack_Total_Customer_Deposits = slacks_all$slack_input[, "Total_Customer_Deposits"],
                       Slack_Netloans = slacks_all$slack_output[, "Netloans"],
                       Slack_Net_Interest = slacks_all$slack_output[, "Net_Interest"])



# Replace Negative Slacks with Zero
combined_data <- combined_data %>%
  mutate(across(starts_with("Slack_"), ~ ifelse(. < 0, 0, .)))

# Verify that the negative values have been replaced
summary(combined_data %>% select(starts_with("Slack_")))

# Run the Tobit Regression and Slack Adjustment Process

# Prepare a dataframe to collect adjusted results
adjusted_data <- data.frame()

# Process each year individually for Tobit regression and adjustment of slacks
for (year in unique(combined_data$Year)) {
  cat("Processing Tobit and Slack Adjustment for Year:", year, "\n")
  
  # Filter data for the specific year
  data_year <- subset(combined_data, Year == year)
  
  # Check the number of rows for the filtered year to ensure data is being selected
  cat("Number of observations for Year", year, ":", nrow(data_year), "\n")
  
  # Define the formulas for each Tobit regression
  formula1_in <- Slack_Total_Assets ~ GDP + Inflation + Unemployment
  formula2_in <- Slack_Operating_Expenses ~ GDP + Inflation + Unemployment
  formula3_in <- Slack_Interest_Expense ~ GDP + Inflation + Unemployment
  formula4_in <- Slack_Total_Customer_Deposits ~ GDP + Inflation + Unemployment
  
  formula1_out <- Slack_Netloans ~ GDP + Inflation + Unemployment
  formula2_out <- Slack_Net_Interest ~ GDP + Inflation + Unemployment
  
  # Fit the Tobit models using censReg
  tobit_model1_in <- censReg(formula1_in, data = data_year, left = 0)
  tobit_model2_in <- censReg(formula2_in, data = data_year, left = 0)
  tobit_model3_in <- censReg(formula3_in, data = data_year, left = 0)
  tobit_model4_in <- censReg(formula4_in, data = data_year, left = 0)
  
  tobit_model1_out <- censReg(formula1_out, data = data_year, left = 0)
  tobit_model2_out <- censReg(formula2_out, data = data_year, left = 0)
  
  # Compute the predictions and adjust for censoring at zero
  new_data_in <- data.frame(Intercept = 1, GDP = data_year$GDP, Inflation = data_year$Inflation, Unemployment = data_year$Unemployment)
  predicted_slack1_in <- pmax(as.matrix(new_data_in) %*% coef(tobit_model1_in)[1:4], 0)
  predicted_slack2_in <- pmax(as.matrix(new_data_in) %*% coef(tobit_model2_in)[1:4], 0)
  predicted_slack3_in <- pmax(as.matrix(new_data_in) %*% coef(tobit_model3_in)[1:4], 0)
  predicted_slack4_in <- pmax(as.matrix(new_data_in) %*% coef(tobit_model4_in)[1:4], 0)
  
  predicted_slack1_out <- pmin(as.matrix(new_data_in) %*% coef(tobit_model1_out)[1:4], 0)
  predicted_slack2_out <- pmin(as.matrix(new_data_in) %*% coef(tobit_model2_out)[1:4], 0)
  
  # Adjust inputs using the original inputs
  adjusted_inputs_year <- data_year[, c("Total_Assets", "Operating_Expenses", "Interest_Expense", "Total_Customer_Deposits")]
  adjusted_inputs_year$Total_Assets <- data_year$Total_Assets + (max(predicted_slack1_in, na.rm = TRUE) - predicted_slack1_in)
  adjusted_inputs_year$Operating_Expenses <- data_year$Operating_Expenses + (max(predicted_slack2_in, na.rm = TRUE) - predicted_slack2_in)
  adjusted_inputs_year$Interest_Expense <- data_year$Interest_Expense + (max(predicted_slack3_in, na.rm = TRUE) - predicted_slack3_in)
  adjusted_inputs_year$Total_Customer_Deposits <- data_year$Total_Customer_Deposits + (max(predicted_slack4_in, na.rm = TRUE) - predicted_slack4_in)
  
  # Adjust outputs using the original outputs
  adjusted_outputs_year <- data_year[, c("Netloans", "Net_Interest")]
  adjusted_outputs_year$Netloans <- data_year$Netloans + (predicted_slack1_out - min(predicted_slack1_out, na.rm = TRUE))
  adjusted_outputs_year$Net_Interest <- data_year$Net_Interest + (predicted_slack2_out - min(predicted_slack2_out, na.rm = TRUE))
  
  # Combine adjusted data for the year
  adjusted_data_year <- data.frame(data_year[, c("ID", "Year")], adjusted_inputs_year, adjusted_outputs_year)
  
  # Append to the final dataframe
  adjusted_data <- rbind(adjusted_data, adjusted_data_year)
}


# Prepare adjusted aata for final DEA
adjusted_dea_data <- make_deadata(datadea = adjusted_data,
                                  inputs = c("Total_Assets", "Operating_Expenses", "Interest_Expense", "Total_Customer_Deposits"),
                                  outputs = c("Netloans", "Net_Interest"))

# Perform final DEA using the Adjusted Data
adjusted_dea_result <- model_sbmeff(adjusted_dea_data, orientation = "no", rts = "vrs")

# Get adjusted efficiency scores
adjusted_efficiency_scores <- efficiencies(adjusted_dea_result)
adjusted_efficiency_scores[is.na(adjusted_efficiency_scores)] <- 1

# Combine DMU names with original and adjusted efficiency scores
# Extract the ID, Banks, and Efficiency columns from combined_data, then add the Adjusted_Efficiency scores
final_combined_data <- cbind(combined_data[, c("ID", "Banks","Country","Year","Efficiency")], Adjusted_Efficiency = adjusted_efficiency_scores)


# Save the Final Combined Data with Original and Adjusted Efficiency Scores
# write.csv(final_combined_data, file = "C:/Users/Nattakamon/OneDrive/UOE/Dissertation/final_combined_efficiency_scores.csv", row.names = FALSE)

# Display the first few rows of the final combined data to verify
# head(final_combined_data)
options(max.print = 1000000)  # Increase the limit to a large number
print(final_combined_data)
# Credit Modeling; Default Predictor in R

install.packages("pROC")

# Load the package
library("pROC")


## Load data
# Set working directory. This will be the folder under which all files will be accessed.
setwd("~/Downloads")
# Load the data from the data file. The first row is variable names.
# To avoid trouble we do not use "string as factor" option
Data_set <- read.csv("data.csv",
                     header = TRUE)

# transforming the variable default where TRUE (1) if did default and FALSE(0) if didn't default
Data_set$default <- Data_set$default == "Defaulted"

# creating a model with independent variable is fico and dependent variable is default
model1 = glm(default ~ fico, family = "binomial", data = Data_set)
#shows the summary of this model
summary(model1)

# sets a variable predicted_p with all the predicted values from model1
Data_set$predicted_p <- predict(model1, type = "response")


# Calculate the ROC curve.
ROC1 <- roc(default ~ predicted_p, data = Data_set)

# plot the ROC curve
plot(ROC1)
# turn the ROC curve off
dev.off()

# add column of predicted_default from the predicted values from above and anyone who had a
# prediction value above 0.1 is TRUE and otherwise FALSE. Where TRUE is where the loan applicants
# whose predicted probability of default is greater than 10%
Data_set$Predicted_default <- Data_set$predicted_p > 0.1
# We calculate the number of people who actually defaulted.
Num_pos <- sum(Data_set$default == TRUE)
# We calculate the number of people who actually defaulted and is predicted to do so.
Num_true_pos <- sum(Data_set$Predicted_default == TRUE & Data_set$default == TRUE)
# True positive
print(Num_true_pos/Num_pos) # Display the results

# find the number of people who didn't default
Num_neg <- sum(Data_set$default == FALSE)
# find the true negative, the number of people who didn't default but was predicted to so
Num_false_pos <- sum(Data_set$Predicted_default == TRUE & Data_set$default == FALSE)
# display the results
print(Num_false_pos/Num_neg) # Display the results

#conversion of variables of verification_status since we want if non-verified and otherwise
Data_set$verification_status <- Data_set$verification_status == "Not Verified"
#refactors Data_set$grade
Data_set$grade <- factor(Data_set$grade)



#regression 2. richer
model2 <- glm(default ~ fico + loan_amnt + int_rate + grade + revol_bal + verification_status, family = "binomial", data = Data_set)
summary(model2)

#predicts the new model using the new independent variables
Data_set$predicted_p_2 <- predict(model2, type = "response")


# Compute new ROC curve
ROC2 <- roc(default ~ predicted_p_2, data = Data_set)
# Plot the ROC curve
plot(ROC1, col = "red")
# The argument 'add = TURE' makes sure that the curve is added
# to the original figure created by plot above, instead of replacing the old one.
plot(ROC2, add = TRUE, col = "blue")
# shuts off the ROC curve plotted
dev.off()

# finds the area under the curve of ROC1, the first ROC found in part 2
auc(ROC1)
# finds the area under the curve of ROC2, the new ROC curve
auc(ROC2)

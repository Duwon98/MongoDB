# Task 1 
# Read the dataset
setwd('/Users/duwonha/Desktop/University/Second year/T2/Big data/Assignment2')
drugdata <- read.csv('drug200.csv', stringsAsFactors = TRUE)

#Task 2
training_size <- floor(0.8 * nrow(drugdata))
set.seed(101)
train_int <- sample(seq_len(nrow(drugdata)), size = training_size)
trainingSet <- drugdata[train_int, ]
testSet <- drugdata[-train_int, ]
#Task 3
#Importing library
library(ISLR)
data(package = "ISLR")
require(tree)
# Tree Construnction
tree_accuracy <- tree(formula = Drug ~ Age+Sex+BP+Cholesterol+Na_to_K, data = trainingSet)
#Tree Plot
plot(tree_accuracy)
text(tree_accuracy, pretty = 0)
# Accuracy Calculation
tree_pred = predict(tree_accuracy, drugdata[-train_int,], type = 'class')
with(drugdata[-train_int,], table(tree_pred, Drug))

#Task 4
# Cross-validation and Tree Construction
drug.cv = cv.tree(tree_accuracy, FUN = prune.misclass)
drug.cv > plot(drug.cv)
drug.cv
prune.drug = prune.misclass(tree_accuracy, best = 6)
# Tree plot
plot(prune.drug)
text(prune.drug, pretty=0)
# Accuracy Calculation
tree_pred = predict(prune.drug, drugdata[-train_int,], type = "class")
with(drugdata[-train_int,], table(tree_pred, Drug))


Background
==========

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly (classe=A) and incorrectly (classe=B-E). More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with.

Analysis and model generation
=============================

All variables in the weight lifting dataset associated with measurements are used for the predictive model. Only those variables that have data were chosen and no other variables such as name or time were used. This would not provide any meaningful data to the model. The random forest algorithm is used to generate the model. The given training dataset is used to train and test the model and measured for expected accuracy using a confusion matrix table generated. The model was generated and tested and found to have approximatedly a 99.5% accuracy. A smaller set of variables were tested but, the accuracy of the model went down. Using the most variables for random forest did produce a highly accurate model but, a simplier model may be needed for large datasets. Since the runtime was acceptable for the dataset and the accuracy acceptable, simplier models were not used in this write up for the practical machine learning example.

The complete training set is used to generate the final model and tested with the 20 records provided in the testint dataset.


```r
# Load the caret and randomForest libraries
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
library(randomForest)
```

```
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

```r

# Function provided to write out final answer
write_files = function(x) {
    n = length(x)
    for (i in 1:n) {
        filename = paste0("problem_id_", i, ".txt")
        write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
            col.names = FALSE)
    }
}

# Read the training and testing dataset for the weight lifting exercise
# Split into training dataset into seperate training and testing datasets
wle_train <- read.csv("pml-training.csv", header = TRUE, sep = ",")
wle_test <- read.csv("pml-testing.csv", header = TRUE, sep = ",")
trainIdx <- createDataPartition(wle_train$classe, p = 0.66, list = FALSE)
training <- wle_train[trainIdx, ]
testing <- wle_train[-trainIdx, ]

# Create formula of model from all measurement variables fully populated
fom <- classe ~ num_window + roll_belt + pitch_belt + yaw_belt + total_accel_belt + 
    gyros_belt_x + gyros_belt_y + gyros_belt_z + accel_belt_x + accel_belt_y + 
    accel_belt_z + magnet_belt_x + magnet_belt_y + magnet_belt_z + roll_arm + 
    pitch_arm + yaw_arm + total_accel_arm + gyros_arm_x + gyros_arm_y + gyros_arm_z + 
    accel_arm_x + accel_arm_y + accel_arm_z + magnet_arm_x + magnet_arm_y + 
    magnet_arm_z + roll_dumbbell + pitch_dumbbell + yaw_dumbbell + total_accel_dumbbell + 
    gyros_dumbbell_x + gyros_dumbbell_y + gyros_dumbbell_z + accel_dumbbell_x + 
    accel_dumbbell_y + accel_dumbbell_z + magnet_dumbbell_x + magnet_dumbbell_y + 
    magnet_dumbbell_z + roll_forearm + pitch_forearm + yaw_forearm + total_accel_forearm + 
    gyros_forearm_x + gyros_forearm_y + gyros_forearm_z + accel_forearm_x + 
    accel_forearm_y + accel_forearm_z + magnet_forearm_x + magnet_forearm_y + 
    magnet_forearm_z
# Generate a model using a subset of the training data.
model <- randomForest(fom, data = training)
# Find the predictive result.
result <- predict(model, testing, type = "class")
# Produce the confusion matrix.
table(pred = result, true = testing$classe)
```

```
##     true
## pred    A    B    C    D    E
##    A 1897    3    0    0    0
##    B    0 1287    0    0    0
##    C    0    0 1163   10    0
##    D    0    0    0 1082    8
##    E    0    0    0    1 1218
```

```r
# Check the result to determine accuracy (% of correct results): correct
# prediction/number of testing records
chkresult <- testing$classe == result
sum(chkresult)/nrow(testing)
```

```
## [1] 0.9967
```

```r

# Generate the final model Write out the results to the required files
model <- randomForest(fom, data = wle_train)
answers <- predict(model, wle_test, type = "class")
write_files(answers)
```



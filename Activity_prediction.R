######################################################
############     Data Preparation     ################
######################################################


#install packages if they are not installed
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(ggridges)) install.packages("ggridges", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages('e1071', dependencies=TRUE, repos = "http://cran.us.r-project.org")
if(!require(stats)) install.packages("stats", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")
if(!require(tuneRanger)) install.packages("tuneRanger", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages('forecast', dependencies=TRUE, repos = "http://cran.us.r-project.org")
if(!require(doParallel)) install.packages('doParallel', repos = "http://cran.us.r-project.org")


# Load them if not loaded
if(!require(tidyverse)) library("tidyvers")
if(!require(caret)) library("caret")
if(!require(data.table)) library("data.table")
if(!require(knitr)) library("knitr")
if(!require(ggridges)) library("ggridges")
if(!require(ggplot2)) library("ggplot2")
if(!require(e1071)) library("e1071")
if(!require(stats)) library("stats")
if(!require(gridExtra)) library("gridExtra")
if(!require(ranger)) library("stats")
if(!require(tuneRanger)) library("stats")
if(!require(forecast)) library("forecast")
if(!require(doParallel)) library("forecast")

# Download dataset from UCI dataset
# https://archive.ics.uci.edu/ml/datasets/Activity+Recognition+from+Single+Chest-Mounted+Accelerometer
# Data are downloaded as zip. archive.

temp <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00287/Activity%20Recognition%20from%20Single%20Chest-Mounted%20Accelerometer.zip", temp)

# Read files, I didn't figure out how to read them in one code because of the hard exaction of multi directory zip

AR1 <- fread(text = gsub("::", "\t", readLines(unzip(temp, "Activity Recognition from Single Chest-Mounted Accelerometer/1.csv"))),
                 col.names = c("seq", "x", "y", "z", "activity"))
AR2 <- fread(text = gsub("::", "\t", readLines(unzip(temp, "Activity Recognition from Single Chest-Mounted Accelerometer/2.csv"))),
           col.names = c("seq", "x", "y", "z", "activity"))
AR3 <- fread(text = gsub("::", "\t", readLines(unzip(temp, "Activity Recognition from Single Chest-Mounted Accelerometer/3.csv"))),
           col.names = c("seq", "x", "y", "z", "activity"))
AR4 <- fread(text = gsub("::", "\t", readLines(unzip(temp, "Activity Recognition from Single Chest-Mounted Accelerometer/4.csv"))),
           col.names = c("seq", "x", "y", "z", "activity"))
AR5 <- fread(text = gsub("::", "\t", readLines(unzip(temp, "Activity Recognition from Single Chest-Mounted Accelerometer/5.csv"))),
           col.names = c("seq", "x", "y", "z", "activity"))
AR6 <- fread(text = gsub("::", "\t", readLines(unzip(temp, "Activity Recognition from Single Chest-Mounted Accelerometer/6.csv"))),
             col.names = c("seq", "x", "y", "z", "activity"))
AR7 <- fread(text = gsub("::", "\t", readLines(unzip(temp, "Activity Recognition from Single Chest-Mounted Accelerometer/7.csv"))),
           col.names = c("seq", "x", "y", "z", "activity"))
AR8 <- fread(text = gsub("::", "\t", readLines(unzip(temp, "Activity Recognition from Single Chest-Mounted Accelerometer/8.csv"))),
           col.names = c("seq", "x", "y", "z", "activity"))
AR9 <- fread(text = gsub("::", "\t", readLines(unzip(temp, "Activity Recognition from Single Chest-Mounted Accelerometer/9.csv"))),
           col.names = c("seq", "x", "y", "z", "activity"))
AR10 <- fread(text = gsub("::", "\t", readLines(unzip(temp, "Activity Recognition from Single Chest-Mounted Accelerometer/10.csv"))),
           col.names = c("seq", "x", "y", "z", "activity"))
AR11 <- fread(text = gsub("::", "\t", readLines(unzip(temp, "Activity Recognition from Single Chest-Mounted Accelerometer/11.csv"))),
           col.names = c("seq", "x", "y", "z", "activity"))
AR12 <- fread(text = gsub("::", "\t", readLines(unzip(temp, "Activity Recognition from Single Chest-Mounted Accelerometer/12.csv"))),
           col.names = c("seq", "x", "y", "z", "activity"))
AR13 <- fread(text = gsub("::", "\t", readLines(unzip(temp, "Activity Recognition from Single Chest-Mounted Accelerometer/13.csv"))),
           col.names = c("seq", "x", "y", "z", "activity"))
AR14 <- fread(text = gsub("::", "\t", readLines(unzip(temp, "Activity Recognition from Single Chest-Mounted Accelerometer/14.csv"))),
           col.names = c("seq", "x", "y", "z", "activity"))
AR15 <- fread(text = gsub("::", "\t", readLines(unzip(temp, "Activity Recognition from Single Chest-Mounted Accelerometer/15.csv"))),
           col.names = c("seq", "x", "y", "z", "activity"))



# Crating one data frame

AR <- bind_rows(list(AR1, AR2, AR3, AR4, AR5, AR6, AR7, AR8, AR9, AR10, AR11, AR12, AR13, AR14, AR15), .id = "id" )

########### Adjustment of data ############
# Giving meaningful label to activities
AR$activity <- factor(AR$activity,
                      levels = c(1,2,3,4,5,6,7),
                      labels = c("working", "stand_walk_down", "standing", "walking",
                                 "going_up_down", "walk+talk", "stand+talk"))

# Creating normalization function
normalize <- function(x){
  return(x - min(x)) / (max(x) - min(x))
}

## Creating vector magnitude, applying median filter to remove outliners, normalizing data
### Creating sequential number for each activity
## Delete activity = 0, because there wasn't specification about this type
AR <- AR %>% 
  mutate(id = as.numeric(id), 
         vm = sqrt(x^2 + y^2 + z^2),
          x = runmed(x, k = 5),
          y = runmed(y, k = 5),
          z = runmed(z, k = 5)) %>%
         group_by(activity) %>%
         mutate(seq = as.numeric(1:n()),
                x = normalize(x), 
                y = normalize(y), 
                z = normalize(z),
                vm = normalize(vm)) %>%
  subset(activity != 0)




# Divide AR into data (train set) and Validation (Test set for last step controlling)
set.seed(1, sample.kind="Rounding") # using R 3.6 or later
test_index <- createDataPartition(y = AR$x, times = 1, p = 0.3, list = FALSE)
data <- AR[-test_index,]
temp <- AR[test_index,]

# Make sure activity type and source is also in validation set
validation <- temp %>% 
  semi_join(data, by = "id") %>%
  semi_join(data, by = "activity")

# From now we will work only with data and validation set will be used only at the end

# Clean memory
rm(AR1, AR2, AR3, AR4, AR5, AR6, AR7, AR8, AR9, AR10, AR11,
   AR12, AR13, AR14, AR15, temp, test_index, AR, normalize)

##############################################################
#####              Description of dataset                #####
##############################################################

## Parameters of data
str <- data %>% str()
dim <- dim(data)
head <- data %>% head(5)

# Missing values check
naD <- sum(is.na(data)) 
naV <- sum(is.na(validation))

# Summary of data set
summary <- data %>% summary()



####### Signal analysis ##########
# Length of each activity
activity_length <- data %>% 
  ggplot(aes(seq, activity)) +
  geom_density_ridges(alpha = 0.8) +
  ylab("Activity") +
  xlab("Time sequence") 


# Visualization of different activities for different axis
# Filtering shorter time window 
activity_axis_visualization <- data %>%
  filter(seq < 7000) %>%
  ggplot() +
  geom_line(aes(seq, x, color = "x")) +
  geom_line(aes(seq, y, color = "y")) +
  geom_line(aes(seq, z, color = "z")) +
  geom_line(aes(seq, vm, color = "vm")) +
  facet_grid(activity ~ ., scales = "free") +
  scale_x_continuous(labels = scales::comma) +
  scale_color_manual("", values = c("x"="blue","y"="red", "z"="yellow", "vm" = "green"), 
                      label=c("X", "Y", "Z", "VM")) +
  ylab("") +
  xlab("Time sequence") +
  ggtitle("Activities per axis") 



rm(activity_length, activity_axis_visualization)
### Correlation analysis ###
### separate activity by pattern
dynamic <- data %>% filter(activity %in% c( "stand_walk_down",  "walking", "going_up_down", "walk+talk"))
static <- data %>% filter(activity %in% c("working", "standing", "stand+talk"))

### Creating separate plot for each pattern and axis
lagx_d <- gglagplot(dynamic$x, lags = 1, do.lines = FALSE, main = "X dynamic")
lagy_d <- gglagplot(dynamic$y, lags = 1, do.lines = FALSE, main = "Y dynamic") 
lagz_d <- gglagplot(dynamic$z, lags = 1, do.lines = FALSE, main = "Z dynamic") 

lagx_s <- gglagplot(static$x, lags = 1, do.lines = FALSE, main = "X static") 
lagy_s <- gglagplot(static$x, lags = 1, do.lines = FALSE, main = "y static") 
lagz_s <- gglagplot(static$x, lags = 1, do.lines = FALSE, main = "y static")

lag_list <- list(lagx_d, lagx_s, lagy_d, lagy_s, lagz_d, lagz_s)

######### Signal correlation with lag plot##########
## May take few minutes to process
lag_plot <- grid.arrange(grobs = lag_list, top = "STATIC - Lag plot - DYNAMIC", nrow = 2, ncol = 3)


# Memory clean
rm(stand_talk, walk_talk, going_up_down, Walking, standing, stand_walk_down, 
   working, dynamic, static, lagx_d, lagy_d, lagz_d, lagx_s, lagy_s, lagz_s, 
   lag_list, lag_plot)
##############################################################
#####                 Algorithm building                 #####
##############################################################
### Partition of data set ##
set.seed(1, sample.kind="Rounding") # using R 3.6 or later
test_index <- createDataPartition(y = data$x, times = 1, p = 0.4, list = FALSE)
train_set <- data[-test_index,]
temp <- data[test_index,]

# Make sure activity type and source is also in validation set
test_set <- temp %>% 
  semi_join(data, by = "id") %>%
  semi_join(data, by = "activity")

# Cleare memory
rm(temp, test_index)

######## Decision tree #####
# Define cross validation for all upcoming algorithms
control <- trainControl(method = "cv", number = 10, p = .9)

########### Tuning decision tree with cp ####
#### Be careful may take time to complete ##########
### Using doParralel package to use more cores in computer (This is set to 10 because of 12 maximum)
### It's gonna be used even for knn
cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)


train_tree <- caret::train(activity ~ x + y + z + vm, 
                           method = "rpart",
                           data = train_set,
                           tuneGrid = data.frame(cp = seq(0.01, 0.1, 25)),
                           trControl = control)

#### Fitting best value into the tree ####
fit_tree <- caret::train(activity ~ x + y + z + vm, 
                           method = "rpart",
                           data = train_set,
                           tuneGrid = data.frame(cp = train_tree$bestTune),
                           trControl = control)

#### Making prediction inside the confusion matrix ######
acc_tree <- confusionMatrix(caret::predict.train(fit_tree, test_set), 
                            test_set$activity)$overall["Accuracy"]

# Stop doParallel
stopCluster(cl)

# Writing the results into table
acc_results <- tibble(method = "Decision tree", 
                      Accuracy = acc_tree)


# Memory clear
rm(train_tree, fit_tree, acc_tree)

########## k-nearest neighbors algorithm  ####
# We use predefined cross-validation from previous part

########### Tuning kNN with K ####
#### Be careful may take several hours to complete ##########
### Using doParralel from previous chunk
registerDoParallel(cl)
train_knn <- caret::train(activity ~ x + y + z + vm, method = "knn", 
                   data = train_set,  
                   tuneGrid = data.frame(k = seq(1,30,1)),
                   trControl = control)

## Fitting in best K
fit_knn <- caret::train(activity ~ x + y + z + vm, method = "knn", 
                          data = train_set,  
                          tuneGrid = data.frame(k = train_knn$bestTune),
                          trControl = control)

#### Making prediction inside the confusion matrix ######
acc_knn <- confusionMatrix(caret::predict.train(fit_knn, test_set), 
                           test_set$activity)$overall["Accuracy"]

# Stop using doParallel
stopCluster(cl)
# Writing the results into table
acc_results <- bind_rows(acc_results, 
                         tibble(method = "k-nearest neighbors algorithm ", 
                                Accuracy = acc_knn))
# Memory clear
rm(train_knn, fit_knn, acc_knn)

########## Random forest  ####
### Using Ranger package for random forest (Can you all the computational power)
## Preparing our data = clearing
data_rf <- train_set %>% select(-id, -seq) %>% as.data.frame()

## Creating classification task that is needed for tuneRanger
data.task <- makeClassifTask(data = data_rf, target = "activity")

### Auto tuning withe tuneRanger function
tune_rf <- tuneRanger(data.task, measure = list(acc), num.trees = 150,
                        tune.parameters = c("mtry", "min.node.size", "sample.fraction"))

## Fitting in the best values
fit_rf <- ranger(activity ~ x + y + z + vm, train_set, 
                 num.trees = 150,
                 mtry = 2,
                 min.node.size = 2,
                 sample.fraction = 0.6463493)

### Making prediction inside the confusion matrix.
## We have to change class of the objet because of ranger 
acc_rf <- confusionMatrix(as.factor(predict(fit_rf, test_set)$predictions),
                          test_set$activity)$overall["Accuracy"]

## Writing the results
acc_results <- bind_rows(acc_results, 
                         tibble(method = "Random Forest", 
                                Accuracy = acc_rf))



rm(data_rf, data.task, tune_rf, acc_rf)

##############################################################
####    predict validation set with best algorithm       #####
##############################################################
### Define the prediction
predict <- predict(fit_rf, validation)

### Confusion matrix
conf_mat <- confusionMatrix(as.factor(predict$predictions), validation$activity)

## Change of the class to data frame
predictdf <- as.data.frame(predict$predictions) 

# Histograms
predict_hist <- predictdf %>% ggplot() + geom_bar(aes(x = predict$predictions), fill = "blue") + xlab("") + ylab('Predicted activity')
real_hist <- validation %>% ggplot() + geom_bar(aes(x = activity), fill = "red") + xlab("") + ylab("Real activity")

# Plot them together
arrange <- grid.arrange(grobs = list(predict_hist, real_hist), top = "Activity comparison")

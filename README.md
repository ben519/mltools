# mltools
Exploratory and diagnostic machine learning tools for R

About
------

The goal of this package is multifold:

- Speed up data preparation for feeding machine-learning models
- Identify structure and patterns in a dataset
- Evaluating the results of a machine-learning model

Installation
------

```r
install.packages("devtools")

library(devtools)
install_github("ben519/mltools")
```

Demonstration
------

Predict whether or not someone is an alien

```r
library(data.table)

train <- data.table(
  SkinColor=c("green", "white", "brown", "white", "blue", "white", "green", "white"),
  IQScore=c(300, 95, 105, 250, 115, 85, 130, 115),
  Cat1=c("type1", "type1", "type2", "type4", "type2", "type4", "type1", "type1"),
  Cat2=c("type1", "type2", "type6", "type5", "type7", "type5", "type2", "type1"),
  Cat3=c("type4", "type4", "type11", "type2", "type11", "type2", "type4", "type4"),
  IsAlien=c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE)
)

test <- data.table(
  SkinColor=c("white", "green", "brown", "white", "red"),
  IQScore=c(79, 100, 125, 90, 115),
  Cat1=c("type4", "type4", "type3", "type1", "type1"),
  Cat2=c("type5", "type5", "type9", "type8", "type2"),
  Cat3=c("type2", "type2", "type7", "type4", "type4")
)

train
   SkinColor IQScore  Cat1  Cat2   Cat3 IsAlien
1:     green     300 type1 type1  type4    TRUE
2:     white      95 type1 type2  type4   FALSE
3:     brown     105 type2 type6 type11   FALSE
4:     white     250 type4 type5  type2    TRUE
5:      blue     115 type2 type7 type11    TRUE
6:     white      85 type4 type5  type2   FALSE
7:     green     130 type1 type2  type4    TRUE
8:     white     115 type1 type1  type4   FALSE

test
   SkinColor IQScore  Cat1  Cat2  Cat3
1:     white      79 type4 type5 type2
2:     green     100 type4 type5 type2
3:     brown     125 type3 type9 type7
4:     white      90 type1 type8 type4
5:       red     115 type1 type2 type4
```

### Questions about the data:
- Are there any pairs of categorical fields which are highly/perfectly correlated?
- Are there any parent-child related categorical fields?
- How does the target variable change with IQScore?
- What's the cardinality and skewness of each feature?

```r
# Combine train (excluding IsAlien) and test
fulldataset <- rbind(train[, !"IsAlien", with=FALSE], test, fill=TRUE)

#--------------------------------------------------
## Check for correlated and hierarchical fields

gini_impurities(fulldataset, wide=TRUE)  # get weighted conditional gini impurities
        Var1      Cat1      Cat2      Cat3 SkinColor
1:      Cat1 0.0000000 0.3589744 0.0000000 0.4743590
2:      Cat2 0.0000000 0.0000000 0.0000000 0.3461538
3:      Cat3 0.0000000 0.3589744 0.0000000 0.4743590
4: SkinColor 0.4102564 0.5384615 0.4102564 0.0000000

# (Cat1, Cat3) = (Cat3, Cat1) = 0 => Cat1 and Cat3 perfectly correspond to each other
# (Cat1, Cat2) = 0.36 and (Cat2, Cat1) = 0 => Cat1-Cat2 exhibit a parent-child relationship. You can guess Cat1 by knowing Cat2, but not vice-versa.

#--------------------------------------------------
## Check relationship between numeric field and target variable

bin_data(train, col="IQScore", bins=seq(0, 300, by=100))
         Bin LB.closed RB.open N IQScore.mean IsAlien.mean
1:   [0,100)         0     100 2        90.00          0.0
2: [100,200)       100     200 4       116.25          0.5
3: [200,300)       200     300 1       250.00          1.0

#--------------------------------------------------
## Check skewness of fields

skewness(fulldataset)
$SkinColor
   SkinColor Count       Pcnt
1:     white     6 0.46153846
2:     green     3 0.23076923
3:     brown     2 0.15384615
4:      blue     1 0.07692308
5:       red     1 0.07692308

$Cat1
    Cat1 Count       Pcnt
1: type1     6 0.46153846
2: type4     4 0.30769231
3: type2     2 0.15384615
4: type3     1 0.07692308
...
```

### Preparing for ML model
- Cateogrical fields in train and test should be factors with the same levels
- Split the training dataset to do cross validation
- Convert datasets to sparses matrices

```r
set.seed(711)

#--------------------------------------------------
## Set SkinColor as a factor, such that it has the same levels in train and test
## Set low frequency skin colors (1 or fewer occurences) as "_other_"

skincolors <- list(train$SkinColor, test$SkinColor)
skincolors <- set_factor(skincolors, aggregationThreshold=1)
train[, SkinColor := skincolors[[1]] ]  # update train with the new values
test[, SkinColor := skincolors[[2]] ]  # update test with the new values

# Repeat the process above for other categorical fields (without setting low freq. values as "_other_")
for(col in c("Cat1", "Cat2", "Cat3")){
  vals <- list(train[[col]], test[[col]])
  vals <- set_factor(vals)
  set(train, j=col, value=vals[[1]])
  set(test, j=col, value=vals[[2]])
}

#--------------------------------------------------
## Randomly split the training data into 2 equally sized datasets

train <- train[sample(nrow(train), nrow(train))]  # randomly shuffle the data
cvdata <- chunk(train, chunks=2)  # split data into 2 partitions

cvtrain <- cvdata[[1]]
   SkinColor IQScore  Cat1  Cat2   Cat3 IsAlien
1:     white     115 type1 type1  type4   FALSE
2:     brown     105 type2 type6 type11   FALSE
3:     white      95 type1 type2  type4   FALSE
4:   _other_     115 type2 type7 type11    TRUE

cvtest <- cvdata[[2]]
   SkinColor IQScore  Cat1  Cat2  Cat3 IsAlien
1:     white     250 type4 type5 type2    TRUE
2:     green     130 type1 type2 type4    TRUE
3:     green     300 type1 type1 type4    TRUE
4:     white      85 type4 type5 type2   FALSE

#--------------------------------------------------
## Convert cvtrain and cvtest to sparse matrices
## Note that unordered factors are one-hot-encoded

cvtrain.sparse <- sparsify(cvtrain)
4 x 21 sparse Matrix of class "dgCMatrix"
     SkinColor__other_ SkinColor_brown SkinColor_green SkinColor_white IQScore Cat1_type1
[1,]                 .               .               .               1     115          1
[2,]                 .               1               .               .     105          .
[3,]                 .               .               .               1      95          1
[4,]                 1               .               .               .     115          .

cvtest.sparse <- sparsify(cvtest)
4 x 21 sparse Matrix of class "dgCMatrix"
     SkinColor__other_ SkinColor_brown SkinColor_green SkinColor_white IQScore Cat1_type1
[1,]                 1               .               .               .     115          .
[2,]                 .               .               .               1      85          .
[3,]                 .               .               1               .     130          1
[4,]                 .               .               .               1     115          1
```

### Evaluate model
- What was the model's AUC ROC score?
- How well did the model do on each sample?

```r
#--------------------------------------------------
## Naive model that guesses someone is an alien if their IQScore is > 130

cvtest[, Prediction := ifelse(IQScore > 130, TRUE, FALSE)]

#--------------------------------------------------
## Evaluate predictions

# Overall using AUC ROC
auc_roc(preds=cvtest$Prediction, actuals=cvtest$IsAlien)  # 

# Individual scores (see help(roc_scores) for details)
cvtest[, ROCScore := roc_scores(preds=Prediction, actuals=IsAlien)]
cvtest[order(ROCScore)]
   SkinColor IQScore  Cat1  Cat2   Cat3 IsAlien Prediction  ROCScore
1:     white     250 type4 type5  type2    TRUE       TRUE 0.0000000
2:     brown     105 type2 type6 type11   FALSE      FALSE 0.0000000
3:     green     300 type1 type1  type4    TRUE       TRUE 0.0000000
4:   _other_     115 type2 type7 type11    TRUE      FALSE 0.5833333
```

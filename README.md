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

Predict whether or not someone is an alien.

```r
library(mltools)

alien.train
   SkinColor IQScore  Cat1  Cat2   Cat3 IsAlien
1:     green     300 type1 type1  type4    TRUE
2:     white      95 type1 type2  type4   FALSE
3:     brown     105 type2 type6 type11   FALSE
4:     white     250 type4 type5  type2    TRUE
5:      blue     115 type2 type7 type11    TRUE
6:     white      85 type4 type5  type2   FALSE
7:     green     130 type1 type2  type4    TRUE
8:     white     115 type1 type1  type4   FALSE

alien.test
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
# Combine alien.train (excluding IsAlien) and alien.test
alien.all <- rbind(alien.train[, !"IsAlien", with=FALSE], alien.test, fill=TRUE)

#--------------------------------------------------
## Check for correlated and hierarchical fields

gini_impurities(alien.all, wide=TRUE)  #  weighted conditional gini impurities
        Var1      Cat1      Cat2      Cat3 SkinColor
1:      Cat1 0.0000000 0.3589744 0.0000000 0.4743590
2:      Cat2 0.0000000 0.0000000 0.0000000 0.3461538
3:      Cat3 0.0000000 0.3589744 0.0000000 0.4743590
4: SkinColor 0.4102564 0.5384615 0.4102564 0.0000000

# (Cat1, Cat3) = (Cat3, Cat1) = 0 => Cat1 and Cat3 perfectly correspond to each other
# (Cat1, Cat2) > 0 and (Cat2, Cat1) = 0 => Cat1-Cat2 exhibit a parent-child relationship.
# You can guess Cat1 by knowing Cat2, but not vice-versa.

#--------------------------------------------------
## Check relationship between IQScore and IsAlien by binning IQScore into groups

bin_data(alien.train, col="IQScore", bins=seq(0, 300, by=100))
         Bin LB.closed RB.open N IQScore.mean IsAlien.mean
1:   [0,100)         0     100 2        90.00          0.0
2: [100,200)       100     200 4       116.25          0.5
3: [200,300)       200     300 1       250.00          1.0

#--------------------------------------------------
## Check skewness of fields

skewness(alien.all)
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

skincolors <- list(alien.train$SkinColor, alien.test$SkinColor)
skincolors <- set_factor(skincolors, aggregationThreshold=1)
alien.train[, SkinColor := skincolors[[1]] ]  # update train with the new values
alien.test[, SkinColor := skincolors[[2]] ]  # update test with the new values

# Repeat the process above for other categorical fields (without setting low freq. values as "_other_")
for(col in c("Cat1", "Cat2", "Cat3")){
  vals <- list(alien.train[[col]], alien.test[[col]])
  vals <- set_factor(vals)
  set(alien.train, j=col, value=vals[[1]])
  set(alien.test, j=col, value=vals[[2]])
}

#--------------------------------------------------
## Randomly split the training data into 2 equally sized datasets

alien.train <- alien.train[sample(nrow(train), nrow(train))]  # randomly shuffle the data
partition_idxs <- chunk(1:nrow(alien.train), chunks=2)  # split the indices of train into two partitions

cvtrain <- alien.train[partition_idxs[[1]]]
   SkinColor IQScore  Cat1  Cat2   Cat3 IsAlien
1:     brown     105 type2 type6 type11   FALSE
2:     white     115 type1 type1  type4   FALSE
3:     white      95 type1 type2  type4   FALSE
4:     white     250 type4 type5  type2    TRUE

cvtest <- alien.train[partition_idxs[[2]]]
   SkinColor IQScore  Cat1  Cat2   Cat3 IsAlien
1:     white      85 type4 type5  type2   FALSE
2:     green     300 type1 type1  type4    TRUE
3:     green     130 type1 type2  type4    TRUE
4:   _other_     115 type2 type7 type11    TRUE

#--------------------------------------------------
## Convert cvtrain and cvtest to sparse matrices
## Note that unordered factors are one-hot-encoded

library(Matrix)

cvtrain.sparse <- sparsify(cvtrain)
4 x 6 sparse Matrix of class "dgCMatrix"
     SkinColor__other_ SkinColor_brown SkinColor_green SkinColor_white IQScore Cat1_type1 ...
[1,]                 .               1               .               .     105          .
[2,]                 .               .               .               1     115          1
[3,]                 .               .               .               1      95          1
[4,]                 .               .               .               1     250          .

cvtest.sparse <- sparsify(cvtest)
4 x 6 sparse Matrix of class "dgCMatrix"
     SkinColor__other_ SkinColor_brown SkinColor_green SkinColor_white IQScore Cat1_type1 ...
[1,]                 .               .               .               1      85          .
[2,]                 .               .               1               .     300          1
[3,]                 .               .               1               .     130          1
[4,]                 1               .               .               .     115          .
```

### Evaluate model
- What was the model's AUC ROC score?
- How good was the model's predictions for each sample?

```r
#--------------------------------------------------
## Naive model that guesses someone is an alien if their IQScore is > 130

cvtest[, Prediction := ifelse(IQScore > 130, TRUE, FALSE)]

#--------------------------------------------------
## Evaluate predictions

# Area Under the ROC Curve (AUC ROC)
auc_roc(preds=cvtest$Prediction, actuals=cvtest$IsAlien)
0.67

# Individual scores to determine which predictions were good/bad (see help(roc_scores) for details)
cvtest[, ROCScore := roc_scores(preds=Prediction, actuals=IsAlien)]
cvtest[order(ROCScore)]
   SkinColor IQScore  Cat1  Cat2   Cat3 IsAlien Prediction  ROCScore
1:     white      85 type4 type5  type2   FALSE      FALSE 0.0000000
2:     green     300 type1 type1  type4    TRUE       TRUE 0.0000000
3:     green     130 type1 type2  type4    TRUE      FALSE 0.4166667
4:   _other_     115 type2 type7 type11    TRUE      FALSE 0.4166667
```

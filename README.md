# NBA Shot Logs Analysis

## 1. Description

The main purpose of this project was to perform a multivariate analysis on a public dataset obtained from [Kaggle](https://www.kaggle.com/dansbecker/nba-shot-logs). It contains information about over 125 thousand NBA shots, which occurred during 2014-2015 season. Furthermore, two tree-based classification models have been fit in order to be able to predict potential new observations.

This project was initially developed as a project of Multivariate Analysis at the Master of Innovation and Research in Informatics (Polytechnical University of Catalonia).



## 2. Data Pre-processing

Composed by the following phases:
* Data cleansing
* Feature extraction
* Data enrichment
* Missing values handling

### Data cleansing

In this first step, we have explored the data set in order to look for possible wrond values, outliers and missing values. We could 

The __touch_time__'s values should be between 0 and 24 seconds as this is the range of a possession. Negatives values were marked as NA and treated later whereas values higher than 24 (all of them pretty close to this figure) were rounded to 24.

Concerning the __shot_clock__ column, we have detected a spike in the histogram for values close to 24 seconds. After doing some online reasearch, we have realized that these shots belonged to big men playing close to the hoop. So we can assume they are tip-ins or put-backs where a player near the hoop collects an offensive rebound and _shot_clock_ is not reset.

<p align="center">
  <img src="https://github.com/SergioLlana/nba-shot-analysis/blob/master/images/shot_clock.png" width="40%">
  <img src="https://github.com/SergioLlana/nba-shot-analysis/blob/master/images/rebounds.png" style="display: inline-block; vertical-align: top" width="20%">
</p>

Finally, we have taken a look to the interaction between __shot_distance__ and __shot_type__. The following plot shows the density of both 2- and 3-pointer shots. We can appreciate how they are not separated, but we do not know the exact meaning of the shot distance so we cannot make a decision.

<p align="center">
  <img src="https://github.com/SergioLlana/nba-shot-analysis/blob/master/images/density.png" width="55%">
</p>


### Feature extraction

In order to have extra information about each shot, we have created three new features based on the existing ones:
* __shot_difficulty__: based on the distance between the shooter and the defender. Ordered factor with possible values: "Highly Contested", "Contested", "Open" and "Wide Open".
* __shot_cat__: type of shot based on the distance to the basket and the number of dribbles done by the shooter. Factor with values such as "Catch&Shoot", "Cut", "Drive"...
* __clutch__: binary feature which defines whether the shooter was under pressure based on if the shot was done in the last period of the game and if the final result was tight enough.

### Data enrichment

As the data set did not contain too much information about the players, we scrapped data from ESPN's website, extracting player's salaries and position. As we had to use player's names as joining key and could vary slighly, we have used R's packeg _stringdist_ in order to automatize the process.

The idea is to use the salary as a naïve measure of the quality of the player.

### Handling missing values

We realized that most NAs present in __shot_clock__ occurred when the __remaining_seconds__ of the period were lower than 24 seconds and, therefore, the _shot_clock_ had no importance anymore and was turned off. Then, we have decided to impute them with a random numbre from a uniform distribution between 0 and _remaining_seconds_.

<p align="center"><img src="https://github.com/SergioLlana/nba-shot-analysis/blob/master/images/shot_clock_nas.png" width="50%"></p>

Regarding the 285 NAs in __touch_time__, we have used R's _catdes_ function to study whether other categorical variables were related with those missing values it did not work. We have imputed them using KNN (k = 1) and we have supervised that the new values' average was similar to the original one.

### Transforming categorical variables into continuous

As our next step was to apply PCA and this technique only takes into account continuous variables, some of the key features in our data set would have been ignored (e.g. __shooter__, __defender__ or __shot_cat__). We have created a continuous variable for each of them, based on the percentage of successful shots per modality (conditioned to whether the shot is a 2-pointer or a 3-pointer). 

Note that for _defender_, we have not used the percentage of _success_, but the percentage of missed shots by the shooter. The original features have been kept as they will be used normally in the prediction phase.



## 3. Principal Components Analysis

We have applied standardized PCA (from _FactoMineR_) using all continuous variables as active variables and the categorical ones as supplementary. After analylizing the screeplot, we have decided to use 9 principal components as significant. The following figure shows the correlation between each variable and the resulting dimensions.

<p align="center"><img src="https://github.com/SergioLlana/nba-shot-analysis/blob/master/images/corrplot.png" width=35%"></p>

Despite some variables are already highly correlated, with the main principal components, we have applied _varimax_ rotation in order to refine our found latent factors.

<p align="center"><img src="https://github.com/SergioLlana/nba-shot-analysis/blob/master/images/varimax.png" width="65%"></p>

We can clearly see how features like the distance to the basket, the type of shot and the percentages of success of the shooter, the defender... form the first dimensions. On the other hand, the second dimension is formed by the type of action (the number of dribbles and the amount of seconds the shooter had the ball before shooting).

## 4. Clustering for large datasets

We wanted to take advantage of the dimensionality reduction obtained by PCA (which is supposed to remove the noise) to cluster the projected individuals and find groups of shots that are homogeneous and distinct among them. However, due to the high amount of observations we could not apply hierarchical clustering plus k-means directly.

Then, we have performed a commonly used technique for large datasets which requires less computing time with quite good results. It consisted on performing twice k-means (k = 14) and then, we have formed a cross-table of both partitions and compute the centroids of its non-empty cells. Finally, we have performed hierarchical clustering of the computed centroids weighted by the number of individuals per cell.

<p align="center"><img src="https://github.com/SergioLlana/nba-shot-analysis/blob/master/images/hierarchical.png" width="70%"></p>

The plots above show the aggregated distance at each iteration of the hierarchical clustering. We have decided to cut at the second highest jump in height. After the hierarchical clustering and once decided k, we have performed a consolidation operation, which consists of running k-means using the obtained results as initialization of the algorithm.

<p align="center"><img src="https://github.com/SergioLlana/nba-shot-analysis/blob/master/images/clusters.png" width="50%"></p>

After analyzing each cluster with _catdes_ we have extracted the following insights of each of them:
* Blue cluster: mix of 2-pointer shots.
* Green cluster: mix of 3-pointer shots.
* Black cluster: 2-pointer shots from fast actions.
* Red cluster: both 2- and 3-pointer shots from long plays.



## 5. Prediction

The goal of this final step is to fit obtain a model capable to predict the __success__ of a future action, so we are in a binary classification problem. For that task, we have partitioned the data set into train/test sets (with a ratio of 70:30). But there were three alternatives:
* Random partition with balanced response in both sets.
* Temporal partition, where most recent observations are used as test set.
* Partition by players, which reserves the shots from a subset of players as test set.

Although the last two options are more honest, we have chosen the first one. The temporal partition is not useful in this case as last observations belong to NBA playoffs and these games are not comparable to the regular season. Regarding the partition by players, it would be useful if we wanted to compare NBA to observations from other leagues, but we thought that NBA is a bit of an outlier in basketball because of its level and its own rules.

We have chosen to fit a decision tree and a random forests.

### Decision Tree

We have used R's _rpart_ package to build the tree, using 10-fold cross validation as validation protocol in order to obtain an honest estimate of the generalization error of the tree on new unseen data. In addition, we have used this error to compare the possible values of the complexity parameter __cp__, which specifies how the cost of the tree is penalized by the number of leaves.

Once obatained the best _cp_ with value 0.00028, we have pruned the tree and use it to predict the observations of the test set and to obtain the following variable importance barplot.

<p align="center"><img src="https://github.com/SergioLlana/nba-shot-analysis/blob/master/images/varimp_dt.png" width="60%"></p>

### Random Forest

With random forests there is no need of cross validation, as the out-of-bag error yields an honest estimate of the generalization error. Then, we have defined a grid of candidate values for both the number of trees (_ntree_ parameter) and the number of variables taken into acount at each node (_mtry_ parameter) and we have chosen the combination that minimizes the OOB error.

The best random forest model (with _mtry_=3 and _ntree_=1000) was used to predict the observaitons of the test set and to obtain the variable importance too.

<p align="center"><img src="https://github.com/SergioLlana/nba-shot-analysis/blob/master/images/varimp_rf.png" width="60%"></p>

### Results

After predicting the test set observations with both models, we have computed the confusion matrices, which allow us to obtain the following meatrics. Although accuracy is almost the same, random forest has achieved a higher precision, which is more interesting when we want to classify correctly the true successful shots (without focusing on missed ones).

|               | Accuracy | Precision (positive) |
| ------------- |:--------:| --------------------:|
| Decision tree | 61.91 %  | 37.77 %              |
| Random forest | 61.96 %  | 40.07 %              |

In addition, we have also plotted the ROC curves in the same plot. Both are pretty similar, being the random forest a bit better.

<p align="center"><img src="https://github.com/SergioLlana/nba-shot-analysis/blob/master/images/ROC curves.png" width="60%"></p>



## 6. Contact us

For further information, please read the [full report](https://github.com/SergioLlana/nba-shot-analysis/blob/master/docs/NBA%20Shot%20Logs%20Analysis.pdf) or email us:
* sergio.llana@est.fib.upc.edu
* pau.madrero@est.fib.upc.edu

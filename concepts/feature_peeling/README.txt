# Feature Peeling

I'm thinking of a concept for ensembling many xgboost models. I'd like to start off with high subsample (0.9 or so) and a high col_subsample (also 0.9 or so), which will allow the model to pick and choose the best features for every iteration. Still do the same process for training/testing: generate sparse matrices, conduct a cv to determine best # rounds, build model, test.


* What likely happens here is that the algorithm will greedily pick only the very best feature to split on almost every single time. This is why I usually set the col_sample to be much lower (0.3-0.5) whenever I have many features. *


But let's return to the 0.9 / 0.9 model just built. It will likely be dependent on that very best feature. So now let's train the same model, with the same exact parameters, but leave out that one very best feature from the datasets. Then we'll rebuild the model again using the exact same partitions and random seeds and everything. The model will likely find a new best feature to use a s its initial split point in nearly every tree.


If we choose 10 iterations of feature peeling, then we would end up with 10 xgboost models and 10 sets of predictions.


## Model design:


** Dataset setup: **
- Train == half of all records with a non-NA value for logerror (target variable)
- Holdout == the other half of all records with a non-NA value for logerror
- Test == all records with a NA value for logerror (useful for feature scaling and PLB checking)


** Steps **
1) Run every single iteration is trained on the train set and will predict on holdout







# Feature Peeling -- with rotating params


This would be the exact same as the above strategy, except I would pi
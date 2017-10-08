# kaggle_zillow
predicting log error - Kaggle competition


## First steps


**Current methodology**

* Investigate features with "id" in them, what do the numbers represent? should they be categorical? ordinal? are there certain values of the id field that should be set apart from the rest?
* Investigate all other features
	- continuous variables should also have a binned version, etc.
* Create a really terrible baseline model and see how well it tracks with the public leader board
	- most challenging part here will be to figure out the pipeline for getting predictions for each of the dates we're responsible for predicting on. maybe the very first baseline will be just a standard prediction across all dates, then we come up with a better way to spread the predictions across all of the dates?

## Update

* Currently calculating numeric feature interactions
	- Ranged numeric fields between 0-1, multiplied those each by 100, now finding all 2-way interactions between these fields
	- This is very memory intensive, I tried using a 64gb memory AWS EC2 machine, but it still locked up on me
	- Currently calculating 2-way interactions for only 5,000 records at a time, storing in sparse matrix, then caching to disk 
		- Will then write another function to read these files in, concatenate them together, then prepare them for modeling
* Initial baseline: I split into train, test, and holdout
	- local holdout score was almost exactly the same as the public leader board score
	- this is a great sign, what I'm doing is actually reflecting on the PLB
* **Strategies left to try**
	- Finish numeric interactions (maybe explore taking it up to 3/4 way interactions)
	- Make categorical features target variables, then use the probability predictions of each class of the categorical feature as features to the final model
		- starting with `tv_cat_airconditioningtypeid`
	- Find interactions between the predicted probabilities of above bullet point

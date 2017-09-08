# kaggle_zillow
predicting log error - Kaggle competition


## First steps


**Current methodology**

* Investigate features with "id" in them, what do the numbers represent? should they be categorical? ordinal? are there certain values of the id field that should be set apart from the rest?
* Investigate all other features
	- continuous variables should also have a binned version, etc.
* Create a really terrible baseline model and see how well it tracks with the public leader board
	- most challenging part here will be to figure out the pipeline for getting predictions for each of the dates we're responsible for predicting on. maybe the very first baseline will be just a standard prediction across all dates, then we come up with a better way to spread the predictions across all of the dates?



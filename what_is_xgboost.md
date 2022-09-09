What is XGBoost? Well, it's something called gradient boosting. 

The general idea seems to be that instead of building a set of trees in parallel (random forest), we build a set of trees in series. 

Which basically means: 

1. Build a simple tree
1. Calculate the error for that tree
1. Build a tree to fit the error for the previous tree
1. Repeat for a l;

Man it is surprisingly hard to find a decent explanation of what an xgboost model is. Maybe this is something that I could write up? 


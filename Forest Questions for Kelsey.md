* What parameters do you have to tune? Looks like I have mtry, the number of trees, maximum depth, and minimum node size. Some things on the internet were claiming you didn't need to do much tuning for random forests. 
  * Tree depth and number of trees is really important in tuning the model. Kelsey first bailed on the regression, b/c she didn't have enough data. 
* Did you worry about the structure of the problem? Like that two samples taken very close to each other might have similar PFAS concentrations? Or is that not actually true? I've got a similar problem with my data, except it's mostly temporal correlations that I'm worried about
* How did you do training/testing/validating? Did you do any sort of cross-validation? 
* Was the set of input parameters something that you basically set in advance? Or did you do any manual adjustment to add more covariates? 
* Is it helpful to drop un-important parameters from the model? 
  * It's probably good to remove variables that aren't going anything. The VIP table can be used to create a parsimonious model. So if some variables are like 0% contribution, removing them can improve the fit.
* Why did you choose random forests? Did you consider gradient boosted trees? (I don't know much about them and haven't used them, but I'm seen some things say that they have better performance than random forests) 
* Balancing between classes? Cross-validation? 
  * ArcPro has ways for accounting for sparse classes. 



Kelsey has like 20-25 variables. 

You could try putting in distance to roadway and distance to gas stations. You could try also adding in a feature for smoke plumes. 






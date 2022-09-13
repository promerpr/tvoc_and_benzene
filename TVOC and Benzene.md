# TVOC and Benzene

I think I've reached an interesting place on the TVOC and Benzene analysis, where we've got a model that's performing reasonably well:

| .metric | .estimator |  mean |    n | std_err | .config              |
| :------ | :--------- | ----: | ---: | ------: | :------------------- |
| rmse    | standard   | 0.440 |    8 |   0.069 | Preprocessor1_Model1 |
| rsq     | standard   | 0.399 |    7 |   0.102 | Preprocessor1_Model1 |



Those are out-of-sample metrics on a log-scale. So about 95% of the time we're within a factor of e (since rmse is 0.44, assuming the errors are normally distributed). 

What's also interesting is that the single best predictor of high benzene levels isn't the absolute value of tvoc but the standard deviation of the measurements during the gc sample. The second best predictor is the wind speed. While I can come up with some hand-wavy explanations of why that make sense, it's not what I would have predicted at all. 

![image-20220908162055361](C:\Users\promerpr\AppData\Roaming\Typora\typora-user-images\image-20220908162055361.png)



Now we're in an interesting place. I haven't looked at the test data at all, and I'm resisting doing it for a while. I can see a number of different ways where this could go: 

1. Use this as the final model, write up a document about it and send it out to Kristy and Tara (and eventually to the broader OGHIR team). 
1. Connect with Brittany (sp?) on the SPOD cannister measurements and try to get some more data. I'd have to figure out the best way to deal with the  incredible imbalance between cannisters and CAMML deployments - maybe that's a job for the themis package? Maybe just oversampling the cannisters is the way to go? I worry about that leading to overfitting though. 
1. Compare to a linear model with splines for tvoc_sd and WindSpeed. I'm curious have the xgboost model would compare to a more "classical" model. The linear model w/ splines would also be a more interpretable model than the xgboost model.
1. Repeat this process, but looking at either the sum of the VOC concentrations measured by the CAMML/canisters or by the hazard index. Because the natural question is "why would we expect tVOC measurements to be correlated with Benzene, since they're measuring different things".
1. Figure out if I could deploy this model "in production" - that would mean figuring out how to pull tVOC data from the API and like…generating live predictions on shinyapps.io. I think I'd need to figure out some persistent storage feature, which is going to be a pain (and figure out what new data I need to pull, etc.)

3 is easy, 2 should be straightforward (start with an email), 4 would require connecting with Tara about the reference concentrations she uses. 1 should probably wait until I've done those 3. And 5 would need to wait till much later. 

When we look at a linear model with splines for tvoc and WindSpeed, I get the following:

| .metric | .estimator | mean |    n | std_err | .config              |
| :------ | :--------- | ---: | ---: | ------: | :------------------- |
| rmse    | standard   | 0.53 |    8 |    0.07 | Preprocessor1_Model1 |
| rsq     | standard   | 0.36 |    7 |    0.09 | Preprocessor1_Model1 |

So while that's not as good as the xgboost model, it's definitely respectable. The R2 especially is very similar to the xgboost model; the RMSE is notably higher. I wonder if the spline model might actually be better for production, given that it's more understandable. 

# BEST PREDICTING MODEL

Although forecasting models might not have been the best choice for this
kind of problem, we chose to go with auto.arima.
The reason for this was, that we started with this part of the group-work 
before we had the classes on 'ML with R' and our intuition has guided us here.

With this lack of knowledge, we went for a pipeline, that would allow
for model validation via auto-commit over the Kaggle pipeline.
However, this means that our best model does not implement a proper 
train/test split, it does not make use of cross-validation or 
hyper-parameter tuning, and it is probably far from efficient.

After some experimentation with the models from the forecast 
package (naive, ses, holt, arima, tbats, nnetar) we found that auto.arima 
results in the best scores.
When we added additional external regressor variables, the score further 
improved. When we added the variables from the principal component analysis
provided by the professor the score continued to improve, however the 
processing time also increased substantially. 

To decrease processing time, we paralleled the prediction by rows, which
resulted in a an increase of performance. 

The last run of this prediction happened on 8 cores and took about 40h. 
The scores achieved with auto.arima heavily depended on the amount of 
PCA-Variables used as external regressors.

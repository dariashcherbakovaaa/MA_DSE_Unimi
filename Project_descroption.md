There is group work on the R package, which helps to make the research process more efficient by suggesting the use of a particular statistical test based on the uploaded dataset. The potential output could include t-test, Chi-square test, ANOVA, Linear and Logistic regressions. It works only with parametric tests.

Find the summary of the functions included in the package below:

The create_dataframe() asks the user to input basic information regarding the number and type of their dependent and independent variables
Then, sample_dependence () requests the user to report whether the dependent and independent variables are in the same sample.
sample_size() runs through the uploaded data to calculate the sample size.
The check_shapiro() identifies the p-value which should later be copy-pasted in the p_val() function.
p_val() evaluates whether the provided data is normally distributed or not
Finally, test_performance() algorithm combines the information from the previous functions and runs it through the loops to identify which statistical test would be the most suitable given the provided conditions.


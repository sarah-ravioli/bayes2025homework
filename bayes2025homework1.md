# Homework 1: Assigned 15 January 2025

## This homework has two parts:

1. Simulate data a linear regression problem of your choice and show you can return the parameters you set. Your n should be between 50-100. 
Next, add a binary covariate to your dataset that interacts with your other predictor variable. Make the effect size of this interaction HALF the effect size of the first predictor variable, and again test how well you can return the parameter values.
Now, using just the first 25% of your data, see how well you can return the parameters you set.
Make a plot of sampling from your data from 1 to the n your data showing the estimated parameters for each sample size. Compare how well the model does across the different parameters. Which is better or worse at and why?

2. Review the sample datasets and pick one you want to use to go through the complete workflow with (which is the homework for next week). 

Your answers should be BRIEF. We'll review the homework in class on Tuesday. 

### What file types can I submit?
I prefer .R or .Rmd for code, but Quarto works too. If you use R then you can write answers with comments or submit a separate .md or .txt file with your answers.   

### What models should I fit?
You can use basic lm (`lm`) or the lm from rstanarm (`stan_lm`). 

### Due 21 January 2025 before the start of class

### Submit how? If you are enrolled in class, you must:
1. Make a folder (name it your first name, all lowercase letters)
2. Put your analyses and any output there, format the code so it runs from the repo without additional files.
3. Push to GitHub.
If you're auditig you don't have to do this, but I still recommend it. 

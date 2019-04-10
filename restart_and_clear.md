Restarting and Clearing
================
Jose Francisco Endrinal
February 18, 2018

Introduction
------------

I'm trying so hard to get people on the bandwagon of clearing and restarting their R sessions frequently. If there's anything you can do to ensure the integrity of your code, it's restarting the R session, clearing your console screen, and re-running your code.

This practice ensures that you always start from a clean slate when you run your analysis. If you start your analysis with objects in your workspace and your code relies on that workspace, chances are that your analysis will fail to deliver, or you get unexpected (often wrong) results from the code you just run.

Restarting
----------

Restarting begins your R session as if nothing ever happened and deletes all the objects you have in your workspace. Basically, you start with a clean slate.

If you're on Rstudio you can use the `.rs.restartR()` command in the console or use the following Keyboard shortcuts depending on your OS:

`Ctrl+Shift+F10` for Windows and Linux
`Command+Shift+F10` for MacOS

#### Why this is important

R stores all your created R objects (often functions, variables and data) in a workspace environment in your current R session. And chances are, you won't be in your R session if you won't be using and transforming these objects.

What's important is that these objects do not stand alone. Otherwise, it becomes difficult to recreate them and modify them. And for other people who want to check your analysis, it won't be clear how your objects are created if they only exist in the R workspace.

To solve these problems, These objects have to have their origin in *code*. You have to be able to trace every object you create to a script or R markdown file somewhere in your project.

To ensure that your code does what it does, it helps that your console always starts on a clean slate. Doing this ensures two things:
1. It makes sure that your R console only relies on the objects generated or called by your script.
2. It will force you to code all the R objects you will be needing and not solely depend on the workspace.

**Restart your console and restart it often.**

#### An Example

Let's say David wants to make a new analysis on his dataset, `df`. He wants to do it on a new R script.

Thinking that his data is already on his workspace, he decides to only code the analysis and not write the code generating the dataset he wants to work with. As you will expect, he does not restart his console.

This is what his analysis looks like:

``` r
# My regression
rel_lm <- lm(y ~ x, data = df)
```

David has created a new object called `rel_lm` which is a linear model of his dataset `df`. He uses `summary` to check the significance of variable x on his model.

``` r
summary(rel_lm)
```

    ## 
    ## Call:
    ## lm(formula = y ~ x, data = df)
    ## 
    ## Residuals:
    ##     1     2     3     4     5 
    ##  0.50 -0.56 -0.12 -0.08  0.26 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   3.6200     0.2085   17.36 0.000416 ***
    ## x             2.0600     0.1474   13.97 0.000794 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4662 on 3 degrees of freedom
    ## Multiple R-squared:  0.9849, Adjusted R-squared:  0.9798 
    ## F-statistic: 195.3 on 1 and 3 DF,  p-value: 0.0007936

Luckily for David, his regression shows promising results.

He then decides to share his results with a colleague, Jonathan. Jonathan does not believe in the results so he asks for a copy of the code that David made.

But what happens when Jonathan runs the code? Remember, he does not have the workspace that David has. And because David did not restart his console, he did not see that his analysis depended on the R object `df`.

Here's what it looks like when Jonathan ran the same code.

``` r
# My regression
rel_lm <- lm(y ~ x, data = df)
```

    ## Error in terms.formula(formula, data = data): 'data' argument is of the wrong type

The analysis returns an error. Because the object `df` is not on Jonathan's workspace, he cannot run the regression needed to see the results.

Clearing
--------

Clearing your console keeps the distractions away. Basically you start with a clear screen, but you do not delete your environment objects.

You don't want to look at the console and think you still have objects that you can use. Clear your screen to clear away all that clutter.

You can type the command `cat("\014")` on the console or you can use `Ctrl+L`. This works for all operating systems.

#### Why this is important

I wish R started the console without any text and just the cursor. The text in the console for a frequent R user is incredibly distracting. It would be better to clear it out.

Here's how the screen looks like.

![](img/console.png)

And here's how the screen looks like when it's cleared:

![](img/console2.png)

Doesn't this look better?
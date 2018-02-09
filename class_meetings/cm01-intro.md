BAIT 509 Class Meeting 01
================

GitHub
------

Your participation will be assessed in class, as well as your attempts at doing the in-class exercises. Be sure to commit and push to your github repo while doing class exercises. I'll be using Source Tree to communicate between my laptop and GitHub.

-   5%: attempting the in-class exercises, as assessed through your commit history.
    -   Key word here is *attempt* -- we're looking for effort here, not correctness.
-   5%: in-class discussion
    -   You'll be given the opportunity to briefly present your in-class exercises.

Regarding the in-class mini-presentations, don't fret if you or your team can't figure out the answer to a question you are presenting! You won't be penalized *at all* for not knowing, or getting an answer wrong, but at least try. The floor will be turned to the class for an answer, or I will chime in with some insight.

Introduction to class outlines:
-------------------------------

When the topic of discussion is a Machine Learning methodology:

1.  Brief explanation of the main idea underlying a methodology.
2.  Hands-on exploratory work with toy datasets to allow you to build concepts.
3.  Connection to higher-level concepts, including more complex extensions.

In your actual assignments, you'll be working with real data.

Terminology
-----------

In supervised learning, we try to gain information on a variable *Y*, given observations on variables *X*<sub>1</sub>, …, *X*<sub>*p*</sub>.

The variable *Y* is called the **response variable**, and sometimes the *dependent variable*.

The variables *X*<sub>1</sub>, …, *X*<sub>*p*</sub> are called **predictor variables** (or just "predictors"). There are many other terms for these, including *features*, *independent variables*, and *covariates*.

Fundamental Concept: Predictors give us more information about the response
---------------------------------------------------------------------------

### Numeric Example

Take the example where (*X*, *Y*) are bivariate normal; *Y* is also Normal. Using the distribution of *Y* alone, there is a lot of uncertainty as to what a future outcome of the response might be. But if we observe, say, *X* = 2, then the distribution of *Y* is more certain. This is the **conditional distribution** of the response given the predictors.

Almost all supervised learning methods decide to use the **mean** as the prediction.

### Categorical Example

Take the example where *Y* can be one of three categories: *A*, *B*, or *C*.

DGP:

logit(*P*(*Y* = *B*|*X* = *x*)) = 5 + *x*

logit(*P*(*Y* = *C*|*X* = *x*)) = *x*

*X* is Exponential(1).

Show the marginal of Y. Show some conditional distributions.

Almost all supervised learning methods decide to use the **mode** as the prediction.

Fundamental Concept: Evaluating Prediction Goodness
---------------------------------------------------

Suppose we've come up with a model that makes predictions of the response, given knowledge of predictors. How can we evaulate how good (or bad) the predictions are? It depends on whether the response is categorical or numeric.

### Categorical Response

When the response is *categorical*, the **prediction accuracy** measures prediction "goodness" as the proportion of correct predictions:
$$ \\text{Prediction Accuracy} = \\frac{\\text{Number of correct predictions}}{\\text{Total number of predictions}}. $$
 Equivalently, we can measure the prediction "badness" with the **error rate**, which is one minus the prediction accuracy, telling us the proportion of *incorrect* predictions.

This will be the main focus of the course, but there are others, too. A common measure of goodness is the (Shannon) **information** (the equivalent measure of badness is **entropy**). These measurements consider the entire conditional distribution, as opposed to just the mode.

### Numeric Response

When the response is *numeric*, and we're forming predictions using a mean estimate (usually the case), then there are a few measures of goodness. Suppose we've made *N* predictions $\\hat{y}\_1, \\ldots, \\hat{y}\_N$, for which the actual response ended up being *y*<sub>1</sub>, …, *y*<sub>*N*</sub>.

-   The **mean squared error** (MSE) is a universal measure of badness (the larger this number, the worse the model is).
    $$ \\text{MSE} = \\frac{1}{n}\\sum\_{i=1}^n (y\_i - \\hat{y}\_i)^2. $$
-   The **coefficient of determination** (*R*<sup>2</sup>) is a universal measure of goodness.
-   The **likelihood** is a measure of goodness, and is only applicable when we've made distributional assumptions.

There are extensions to these, such as AIC and adjusted *R*<sup>2</sup>, but these have a different meaning. We'll touch on these later.

LAB
---

-   Give an exercise with discrete and continuous *Y* with no predictors. Then introduce a categorical predictor. Then a continuous predictor (but without the categorical).
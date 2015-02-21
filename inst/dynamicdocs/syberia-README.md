## Mungebits

**[Mungebits](http://github.com/robertzk/mungebits)** - The core objects responsible for
ensuring that the same data preparation occurs in training (development) and prediction
(production).

It is a tremendously under-appreciated fact that [data science is largely data janitorial
work](http://www.nytimes.com/2014/08/18/technology/for-big-data-scientists-hurdle-to-insights-is-janitor-work.html).
In other words, it is impossible to get significant insight without rolling up your
sleeves and re-molding and manually fixing your data until it can be passed to a statistical
algorithm. This is difficult enough as it is to do while developing a model.

It is a far harder proposal to achieve the same consistency in data preparation during
prediction. When launching a model in production so that it scores live customers,
the data coming into the trained statistical algorithm should be qualitatively identical
to the data that was used during training / development. That is, we must *replicate*
the data preparation from training during prediction.

Unfortunately, this is not as simple as re-executing the same code. For example, if we
impute a column's missing values with its mean, we obviously cannot perform the 
same procedure on one data point; we must *remember* the mean, and use that cached
information to perform a matching operation. This is a subtle but incredibly important
point: in order to transform static, training data versus live, prediction data, 
it is possible that we must use completely different code to achieve the same mathematical
transformation.

A `mungebit` is an object with two methods, `train` and `predict`, with a special keyword
available. In the `train` method, we can set things like `inputs$mean <<- mean(some_column)`
in order to store (for example) a mean that we will need for live imputation. The `inputs`
keyword is a variable that lives in a parent environment of the `train` method's 
environment, and can be modified using the `<<-` operator for use in the `predict`
method.

An abstract mungebit is usually independent of any data set: the idea of imputing a variable,
dropping a column with many missing values, or performing [sure independence screening](http://onlinelibrary.wiley.com/store/10.1111/j.1467-9868.2008.00674.x/asset/j.1467-9868.2008.00674.x.pdf;jsessionid=978642E589014AA154A21BE2CE854D22.f01t01?v=1&t=i04x8nfw&s=8a5207bd8384e1ebe65fbd845f639d749b02cabc)
are all operations that work on almost any data set. To record the dependence on some data
set, we can wrap a `mungebit` in a `mungepiece`: an object that also has a `train` and
`predict` method, but stores a `mungebit`, `train_args` (training arguments) and 
`predict_args` (predict arguments). For example, if we have a mungebit that aims to
keep some set and only some set of fixed named variables, but we must be careful to
drop the dependent variable during prediction, we can pass the variables we'd like to
preserve separately for training and prediction. In this case, the mungepiece's `mungebit`
would be a `mungebit` that generically preserves all but the given variables, its
`train_args` would be our set of desired variables including the dependent, and `predict_args`
would be this set excluding the dependent.

Finally, one can use the [`munge` function](https://github.com/robertzk/mungebits/blob/master/R/munge.r) to execute a list of mungebits in succession
on some `data.frame`. For a more detailed explanation, see the [interactive
mungebits tutorial](http://en.wikipedia.org/wiki/Vaporware). (**TODO**: Make this.)

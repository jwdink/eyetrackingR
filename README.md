eyetrackingR
=====================================

A collection of R functions (and accompanying guide) for analyzing data collected with an eyetracker.

## Author, Copyright, & Citation

All original code written by and copyright (2013), Brock Ferguson (brockferguson.com).

You can cite this software using:

> Ferguson, B. (2013). eyetrackingR: An R Library for Eyetracking Data Analysis. Retrieved from https://github.com/brockf/eyetrackingR.


## Quick Guide (Placeholder)

Hi, I'm Jacob, and I'm re-writing this library.

Much of this involves re-writing the internals to be faster and more concise, with no difference in the workflow from the user's point of view.

However, I have decided to rework and even eliminate some functions that I felt were too specific and redundant with already existing functions in R (particularly within dplyr).

In what follows, I'll make some quick notes on how a typical workflow might go in this updated version of eyetrackingR, with an emphasis on things that have changed from the attached PDF guide. 

### Set_Data_Options

There's now an 'item_factor' option. Previously 'trial' was used as item. This is obviously problematic in designs where  items are repeated on trials.

### Clean By Factor

This function is no longer present in eyetrackingR. To replace its functionality, use dplyr's "filter."

For example, the clean_by_factor code in the guide can be replaced with:

```
library(dplyr) # make sure dplyr is loaded (only need to do this once)

allowed_levels = c('Response')
data = filter(data, Window %in% allowed_levels))
```

This code takes 'data', and filters it on the condition that the 'Window' factor is in the allowed_levels. (For this case you could use == instead of %in%, but this demonstrates the more general case of multiple levels.)

### Subset By Window

This function is no longer present in eyetrackingR. To replace its functionality, we can again use dplyr's "filter."

For example, to get the subset of data from 367ms to 1500ms (as in the guide, in the plotting example), we can run:

```
data_in_window = filter(data, Time >= 367, Time <= 1500)
```

This code takes the data, and returns the subset where time was greater than 367ms and less than 1500ms.

### Clean By Trackloss

These function has been temporarily removed. The basic functionality will be reinstated at some point, but any new function(s) will probably be limited to summarizing trackloss information, since filtering based on trackloss can be done (as above) using the 'filter' function.

### Verify Dataset

This function now must be run on your dataset before doing further visualization/analysis of it. For example, the 'plot' function will not work on your dataset unless you've verified it first.

So make sure you run

```
data = verify_dataset(data, data_options)
```

Before diving in.

### Plotting

Rather than a separate 'plot_data' function, eyetrackingR now adds a method to the 'plot' function.

As long as you've run 'verify_dataset' (as above), you can run code like this:

```
plot(data_v, data_options, dv = "Target", factor = "Condition") 
```

This will plot proportion of looking to target, across conditions, over the course of the entire trial. 

The output of this function is a ggplot object, and therefore you can modify the look of this graph using the typical ggplot syntax. If you're not familiar with this syntax, that's no problem. Some common things you might want to do are:

```
plot(data_v, data_options, dv = "Target", factor = "Condition") +
coord_cartesian(ylim = c(0,1)) +
xlab('My X-Label') +
ylab('My Y-Label')
```

The above code rescales the y-axis so that it goes from 0 to 1 (since we are plotting proportions).

If you only want to plot a window of time, simply call the plot function on data you've already subsetted. For example

```
data_in_window = filter(data_v, Time >= 367, Time <= 1500)
plot(data_in_window, data_options, dv = "Target", factor = "Condition")
```

### Window Analysis

The function allows you to specify the window of time you're interested in analyzing with the 'window' argument. (Alternatively, you can filter your data as described above.)

The function returns 'weights' instead of 'wts' now. These weights are already inverted, ready for an LMER. They do NOT need to be inverted by you.

```
df_window = window_analysis(data = datav, data_options, window = c(367, 1500))

fit_window = lmer(formula = elog ~ Condition (1|ParticipantName) + (1|Trial), data = df_window, weights = weights)

summary(fit_window) # get a summary

drop1(fit_window, test= "Chisq") # see which factors are significant, get a p-value
```

### Sequential Bins Analysis

This function still gives you an estimate of whether the looking across conditions has diverged for each timepoint.

Now, however, the function is more focused on confidence intervals than p-values. It runs a mixed-effects model for each timepoint, and extracts the confidence intervals. When confidence intervals diverge from zero, this provides evidence that looking across conditions has diverged.

This is a more natural way of getting an estimate for when conditions differ when making so many comparisons (whereas p-values jump around too much).

The result of this function can be plotted. E.g.:

```
data_in_window = filter(datav, Time >= 0, Time <= 2000)

df_seq = sequential_bins_analysis(data = data_in_window, data_options, time_bin_size = 100, dv= 'Target', factors = c('Condition') )

plot(df_seq, data_options, factor="Condition")
```

The resulting plot shows us how our estimate of the 'Condition' parameter changes over the course of the trial. As it diverges from zero, this provides more evidence that the two conditions are different.





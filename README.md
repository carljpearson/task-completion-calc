# Task Completion Calculator
#### Made for Red Hat UXD, but free to use and copy with attribution

[Hosted here live](https://rh-uxd.shinyapps.io/shiny_task/)

# About

This shiny app was made to plot and calculate basic statistics around task completion, so the language refers to tasks throughout the interface. Statistically, this calculator will function around any set of bernoulli variables (success/failure, or anything other single observation of a binary outcome).

A lot of inspiration is derived from [MeasuringU's calculator](https://measuringu.com/wald/). This goal for my instance was to simplify some of the choices and graph the output. Additional, much of the theory and calcuations are derived from Quantifying the User Experience by Suaro and Lewis.

I originally built this in ggplot2 but have made it completely with the plotly package for R, as the interactivity can be nice and it is a touch faster. Plus, download functions are now integrated into the canvas object.

  ### Future
  
This app is not "complete" necessarily. If you think an essential and common feature is missing, please let me know. Also, formatting specifics continue to be a challenge in the shiny framework, so please comment around visual issues but know I may already be trying to resolve them. Finally, if anyone wants to help me run this on an instance of OpenShift, please let me know!!
  
# App description

## Data entry side panel
  
   #### Basic options

First you can specify the amount of tasks you are testing (from 1 to 8), and then the total number of participants you tested across these tasks (from 2 to 50). From here, you can enter the total number of successes/completions you observed across the participants for each task.

  #### Advanced options

The default selection for **confidence levels** is 95%, which uses a z-equivalent of 1.96 is all the subsequent calculations. Other options range from 80% to 99%. This selection is mainy used to calculate the confidence intervals (which are always calculated based on the LaPlace point estimate). 

The default selection for **point estimate** method is LaPlace. This method leads to more generalizable point estimates in small sample sizes, especially when all or no participants succeed, as many researchers find it unpalatable to say "we expect 100% of users to pass"). The formula is as follows to get the point estimate proportion `(successes+1)/(total+2)`. The other option, if you truly want to see the exact observed proportions is to use the Exact option, where the formula is simply `successes/total`. This option changes the eventual plot bars to the exact observed proportions (10 of 10 users succeeding means 100%), but still keeps an overlay of the LaPlace method in the form of a dot.

## Outputs main panel

  #### Plot
  
This plot graphs all of the tasks, which each bar representing a completion rate in percentage. Labels will update based on chosen point estimates and confidence levels.

  ##### Hover text
  
The hover text shows the LaPlace estimate, the actual observed proportion, and then the best conservative estimation (Plausible failure rate). This last piece takes the lowest confidence interval, and then subtracts the percentage from 100 to give a conservative perspective.

  ##### Confidence intervals
  
Confidence intervals are always calculated based around on the LaPLace method point estimate, to maximize statistical generalizability. The intervals themselves are created through the [Adjusted Wald method](http://journals.sagepub.com/doi/abs/10.3102/1076998611411915?journalCode=jebb), which tends to show superior coverage accuracy on discrete variables with samples of less than 100. 

  #### View plot options
  
This switch shows the options for **benchmark line** and **color palette chooser**. The benchmark line puts a consistent horizontal line at 78%, based on a broad set of data from [MeasuringU](https://measuringu.com/task-completion/) that describes a typical completion rate to aim for. The color palette chooser has a default selection using [Red Hat brand colors](https://brand.redhat.com/elements/color/), a color-blind friendly palette from RColorBRewer, and alternating gray scale dual tones. 


  #### View graph descriptions
  
For each task, a verbal paragraph of explanation is given. It reads out the observed successes (exact point estimate), alongside the LaPlace best statistical estimate. Finally, given the chosen confidence interval, it puts the task in terms of the lower confidence interval, estimating the minimum amount of users we could expect to fail. This is based on the idea of interpeting confidence intervals as a measure of 'pluasbility' of that value occurring (Smithson, 2003). 

  #### View data table

This view shows the specific (rounded) values used to create the plot object.

  #### Citations:

Sauro, J., & Lewis, J. R. (2016). Quantifying the user experience: Practical statistics for user research. Morgan Kaufmann.

Smithson, M. (2002). Confidence intervals (Vol. 140). Sage Publications.

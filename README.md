# Tutorial on R functions/control statements and visualisation via ggplot

This tutorial consists on two exercises:
* Implementation of a function to coerce variables in a dataframe given certain conditions. To this end, four different approaches are followed:
    * Base R, for-loop with conditionals.
    * Base R, vectorised `apply` functions.
    * Library `purrr`, with `map` functions, pipes and conditionals.
    * Library `data.table`, with conditionals.
    As shown in the R file, the general pattern is for version 4 to be the most efficient when the number of columns becomes significant, though this is more properly tested via the `microbenchmark` library.

* Analysis and visualisation of synthetic patients' data with hypertension/diabetes. Execution of the R file generates the necessary visualisations and stores them locally. Illustratively, the grid of histograms wrapped by gender, age and condition are shown below.

<img src="https://github.com/AlfaBetaBeta/Tutorial-Functions-Viz/blob/master/img/Grid_of_histograms.png" width=100% height=100%>
# zenodo-locust-datasets-analysis
Analysis of the extracellular recordings from the locust olfactory pathway available on zenodo (https://zenodo.org/record/21589)

This repository contains the description as well as the result of spike sorting applied to the datasets available on zendo.

For now the analysis is done with [R](https://www.r-project.org/) but a `C` version is in preparation. The way the code is applied to the specific datasets can be followed with `html` files that were generated using [Emacs](https://www.gnu.org/software/emacs/tour/) [Org mode](http://orgmode.org/), especially the [Babel](http://orgmode.org/worg/org-contrib/babel/) extension of `Org mode` (the developers of these wonderful tools are warmly thanked! Without their work none of what follows would have been possible--this does not mean that they should be held responsible for my mistakes--). 

The `R_Sorting_Code` sub-directory contains the few `R` functions that were specifically developed for sorting, together with their documentation.

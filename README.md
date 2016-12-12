# zenodo-locust-datasets-analysis
Analysis of the extracellular recordings from the locust olfactory pathway available on zenodo (https://zenodo.org/record/21589)

This repository contains the description as well as the result of spike sorting applied to the datasets available on zendo. There is as associated [GitHub page](https://christophe-pouzat.github.io/zenodo-locust-datasets-analysis/) that should be much easier to browse at start.

## The code
For now the analysis is done with [R](https://www.r-project.org/) but a `C` version is in preparation. The way the code is applied to the specific datasets can be followed with `html` files that were generated using [Emacs](https://www.gnu.org/software/emacs/tour/) [Org mode](http://orgmode.org/), especially the [Babel](http://orgmode.org/worg/org-contrib/babel/) extension of `Org mode` (the developers of these wonderful tools are warmly thanked! Without their work none of what follows would have been possible--this does not mean that they should be held responsible for my mistakes--). 

The `R_Sorting_Code` sub-directory contains the few `R` functions that were specifically developed for sorting, together with their documentation.

## The experiments

There are one or several `html` files (and `org` source files) per experiment--one if a single tetrode was recorded and more if two or more were recorded--. Each `html` file is an "sorting lab book", you will find there every command used to carry out the sorting together with comments and diagnostic plots. Each `HDF5` `group` in the data file is analyzed trial per trial. The templates are allowed to drift to track the units. The diagnostic plots show for each unit in the model:

- the successive templates, 
- the number of events attributed it (as well as the number of unclassified events),
- the observed counting process,
- the estimated ISI density,
- the raster plot (when that makes sense, that is when odors were applied). 

Feel free to critisize the procedure and its results. If you manage to improve it, be cool and let me know!

## Spike trains format

The spike trains in directory `locustXXX_spike_trains` are stored in ASCII format with one spike time (in seconds) per line. They are named `locustXXX_StimID_tetY_uZ.txt`, where `XXX` gives the experiment data and `Y` the tetrode label, `StimID` is a stimulation identifier (more precisely a `group` name in the `HDF5` data file) and `Z` is the unit number. When several trials, like say 25 stimulation with citronelal, were recorded, the successive trials will be found one after the other and time 0 is defined as the start of the acquisition of the first trial.

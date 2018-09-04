# Workflows

Ad-hoc pipeline construction and execution in R. 

Allows:
- constructing analysis and processing plans in R language
- local execution or on an SGE cluster

## Install

Not on CRAN. Install using devtools.

`devtools::install_github('eachanjohnson/workflows')`

R dependencies:

- devtools
- [eachanjohnson/errR](https://github.com/eachanjohnson/errR)
- [dplyr](https://github.com/hadley/dplyr)
- [magrittr](https://github.com/hadley/magrittr)
- [future](https://github.com/HenrikBengtsson/future)

## Usage

`library(workflows)`

`workflow` objects are containers for `pipeline` objects, which keep track of analysis plans, which targets have been built,
and checkpointing of intermediate results.


## See also

Inspired by -- but not even close in functionality to -- the [Dask](https://github.com/dask/dask) project for Python.

Other, more functional pipeline/workflow/multicore/DAG packages for R include but are not limited to:

- [future](https://github.com/HenrikBengtsson/future)
- [batchtools](https://github.com/mllg/batchtools)
- [multidplyr](https://github.com/hadley/errR)
- [drake](https://ropensci.github.io/drake/)
- [flowr](https://github.com/sahilseth/flowr)



# cecb

Library provides methods to run CEC20{13, 17} benchmarks and plot ECDF curves.

## Installation

This package isn't on CRAN repository, so you have to install it via `devtools`.

```r
devtools::install_github("warbarbye/cecb")
```

## Introduction

`cecb` is a tiny library for testing optimization algorithms in a parallel manner. It provides a bunch of methods that simplify workflow with CEC benchmarks. All you have to do is:

* write a benchmark configuration file in YAML
* run benchmark via `cecb::run_benchmark()` function
* do some magic with results.

`cecb` provides *only* ECDF (Empirical Cumulative Distribution Function) curves as a measure of algorithm performance.

## Algorithms API

Mainly package is devoted to population-based optimization algorithms in real space but there is no issue to use it with any type of optimization method. 

There are some restrictions imposed on your algorithm.

## Input

Algorithm has following parameters: 

```r
some_alg = function(par, fn, ..., lower, upper) {
	...
}
```
In the above signature `par` stands for an initial point, `fn` for an evaluation function, and `lower`, `upper` are boundaries for hyperrectangle which sets the feasibility region.


### Output 
It has to return the named list with:

	1. `bestVal` key which contains the best value recorded in **each** iteration
	2. `value` key which contains the best value across **whole** iterations.

in other words, your algorithm function should return a named list like below:

```r
return(list(bestVal, value, ...))
```

## Workflow

The usage of package `{cecb}` is pretty simple. 
To run a benchmark one has to provide a YAML config file and use a proper function from the package. 
When benchmark is done and data is saved one could plot ECDF curves. Workflow in pseudo-R looks like one below:

```r
benchmark:
	yaml-config.yml %>%
	cecb::run_benchmark() 
ECDF plots:
	data = 
		filepath(s)-to-benchmark-results %>%
		cecb::get_dfr(config = list(dimension, function_number, repetitions))
	ecdf_plot = 
		data %>%
		cecb::ecdf_plot()
```


### Benchmark configuration

The benchmark is configured by YAML with structure and example data written below:

```
methods:
     1: 
       algorithm: "cma_es_csa"
       params:
         - "sigma"
         - "lambda"
       values:
         - 0.40
         - 100
     2: 
       algorithm: "cma_es_csa"
       params:
         - "sigma"
         - "lambda"
       values:
         - 1
         - 500
ids:
  - "cma-es-sigma-0.4-pop-100"
  - "cma-es-sigma-1-pop-500"
probnum:
  - 1
  - 2
  - 3
dims:
  - 50
cec: 13
repnum: 1
cpupc: 0.75
source: "data/path/with/algorithms"
dest: "data/path/for/results"
```

where:

* `methods` stands for optim-methods you are going to test
* `1`, `2`, `...` are ordinal numbers of tested functions
* `algorithm` is the name of your method. It has to be identical to the filename with source code
* `params` is a list of names of algorithm parameters
* `values` are values of that parameters 
* `ids` contains literal IDs of benchmark 
* `probnum` is the function number in the benchmark's problem set
* `dims` contains dimensionality of function
* `cpupc` is the percent of used CPU cores
* `source` place where your algorithms live (a filepath)
* `dest` place where results of the benchmark will live (a filepath) 

:heavy_exclamation_mark: **ID is also your algorithm identifier in data frames and on plots** 

## References 

More information about CEC benchmarks and ECDF curves one can find here: 

[1] "Biobjective Performance Assessment with the COCO Platform", _Dimo Brockhoff, Tea Tusar, Dejan Tusar, Tobias Wagner, Nikolaus Hansen, Anne Auger_

[2] "Benchmark Functions for the CEC’2013 Special Session and Competition on Large-Scale Global Optimization", _Xiaodong Li, Ke Tang_, et al.

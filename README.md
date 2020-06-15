# cecb

## Installation

This package isn't on CRAN repository, so you have to install it via `devtools`.

```r
devtools::install_github("warbarbye/CECBench")
```

## Introduction

`cecb` is a tiny framework for testing optimization algorithms in a parallel manner. It provides a bunch of methods that simplify workflow with CEC benchmarks. All you have to do is:

* write a benchmark configuration file in YAML
* run benchmark via `cecb::run_benchmark()` function
* do some magic with results.

`cecb` provides *only* ECDF (Empirical Cumulative Distribution Function) curves as a measure of algorithm performance.

## Algorithms API

Mainly package is devoted to population-based optimization algorithms in real space but there is no issue to use it with any type of optim-method. 
There are two restrictions imposed on your algorithm:

1. it has to return named list with `bestVal` key which contains the best value recorded in each iteration
2. it has to return named list with `value` key which contains the best value across whole iterations.

In other words, your algorithm function should return an object like below:

```r
return(
  list(
        bestVal,
        value,
        ... # other values which your algorithm returns
        )
)
```

## Benchmark

### Configuration

The benchmark is configured by YAML with structure written below:

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
* `algorithm` is the name of your method. It has to be identical to filename with source code
* `params` is a list of names of algorithm parameters
* `values` are values of that parameters 
* `ids` contains literal IDs of benchmark
* `probnum` is function number in benchmark's problem set
* `dims` contains dimensionality of function
* `cpupc` is the percent of used CPU cores
* `source` place where your algorithms live
* `dest` place where results of the benchmark will live 

## ECDF curves

TODO


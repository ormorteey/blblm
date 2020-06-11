# blbglmpar: Generalized Linear Model with Little Bag of Bootstraps and Parallelization

## Introduction

The purpose of this package is to provide an alternative to existing Generalized Linear Model fitting package -`glm` - equipped with both parallelization and bootstrap capabilities. The goal is that one should be able to replace the package  `glm` with `blbglmpar` when there is a need for parallelization and bootstrap capabilities and things will just work. 

## Dependencies

`blbglmpar` uses a couple of packages so as to achieve the goal. The list of packages which serves as dependencies for include:

  * [`furrr`](https://github.com/DavisVaughan/furrr) for parallelization
  * [`tidyverse`](https://www.tidyverse.org/) for efficient data manipulation
  * [`vroom`](https://www.tidyverse.org/blog/2019/05/vroom-1-0-0/) for efficient lazy loading of large files data
  
## Usage

Loading blbglmpar into R-session.

```{r setup}
library(blbglmpar)
```
blbglmpar fits models using the template `blbglm(formula= formula, data = data, m = 10, B = 200, family = gaussian)`

The default behaviour of blbglmpar is that it does not run using parralellization and family is gaussian. Thus it works like the classical lm function but with bootstrap.


Read the vignette [here](/readme.html)
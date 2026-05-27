# extract a row from longitudinal parameter set

take a random sample from longitudinal parameter set given age at
infection, for a list of antibodies

## Usage

``` r
ldpar(age, antigen_isos, nmc, npar, ...)
```

## Arguments

- age:

  age at infection

- antigen_isos:

  antigen isotypes

- nmc:

  mcmc sample to use

- npar:

  number of parameters

- ...:

  passed to `simpar()`

## Value

an array of 7 parameters and initial conditions:
c(y0,b0,mu0,mu1,c1,alpha,shape)

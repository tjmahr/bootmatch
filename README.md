
<!-- README.md is generated from README.Rmd. Please edit that file -->
bootmatch: bootstrap group matching
===================================

[![Travis build status](https://travis-ci.org/tjmahr/bootmatch.svg?branch=master)](https://travis-ci.org/tjmahr/bootmatch)

Installation
------------

You can install bootmatch from github with:

``` r
# install.packages("devtools")
devtools::install_github("tjmahr/bootmatch")
```

Motivation (Origin Story)
-------------------------

One day, I tried to match two groups of participants on a single measurement variable (age, in months). I wanted participants matched into pairs (*A*<sub>*i*</sub>,*B*<sub>*i*</sub>), and I wanted the pairs to not differ in age by more than 2 months, |Age(*A*<sub>*i*</sub>) − Age(*B*<sub>*i*</sub>)| ≤ 2. This kind of constraint is [called a *caliper*](https://rdrr.io/cran/optmatch/man/caliper-methods.html "optmatch::caliper() documentation") in the matching software literature. (When the caliper is 0, then exact matching is performed). I wanted the computer to give me a matching with as many pairs as possible that obeyed this caliper constraint.

I tried four different packages, and they all failed. For some, they couldn't handle missing data. Okay, let's remove the rows with missing data. Some didn't like how they nominal treatment group was larger than the control group. Fine, let's switch the labels. Some provided matches, but did not give provide the matches in pairs. (Thus, one group was larger than the other.) Some failed, I think, because some of the participants were unmatchable. No luck at all. Perhaps, if I have buckled down and studied the documentation and associated articles, I could have made the software work—maybe. But I just wanted the group matches, and I wanted the matching script to be usable for me and future collaborators.

I threw my hands in the air, and I decided to code my own algorithm for matching.

Example
-------

<!-- Here's the idea: -->
<!-- * Given some a dataframe `data`, a grouping expression `y`, a matching covariate `x`, and an optional caliper constraint -->
This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```


<!-- README.md is generated from README.Rmd. Please edit that file -->
bootmatch: bootstrap group matching
===================================

[![Travis build status](https://travis-ci.org/tjmahr/bootmatch.svg?branch=master)](https://travis-ci.org/tjmahr/bootmatch)

Installation
------------

You can install bootmatch from Github with:

``` r
# install.packages("devtools")
devtools::install_github("tjmahr/bootmatch")
```

Motivation (Origin Story)
-------------------------

One day, I tried to match two groups of participants on a single measurement variable (age, in months). I wanted participants matched into pairs (*a*,*b*), and I wanted the pairs to not differ in age by more than 2 months, |Age(*a*) − Age(*b*)| ≤ 2. This kind of constraint is [called a *caliper*](https://rdrr.io/cran/optmatch/man/caliper-methods.html "optmatch::caliper() documentation") in the matching software literature. (When the caliper is 0, then exact matching is performed). I wanted the computer to give me a matching with as many pairs as possible that obeyed this caliper constraint.

I tried four different packages, and they all failed. For some, they couldn't handle missing data. Okay, let's remove the rows with missing data. Some didn't like how the nominal treatment group was larger than the control group. Fine, let's switch the labels. Some provided matches, but did not provide the matches in pairs. (Thus, one group was larger than the other.) Some failed, I think, because some of the participants were unmatchable. No luck at all. Perhaps, if I have buckled down and studied the documentation and associated articles, I could have made the software work—maybe. But I just wanted the group matches, and I wanted the matching script to be usable for me and future collaborators.

I threw my hands in the air, and I decided to code my own algorithm for matching.

### Bootstrap Based Matching

Here is the basic algorithm for a single random matching:

1.  Generate all legal pairings (*a*,*b*) that satisfy the caliper constraint.
2.  Randomly select one of the pairings, say (*a*<sub>*i*</sub>,*b*<sub>*j*</sub>), to keep. All other pairings with *a*<sub>*i*</sub> or *b*<sub>*j*</sub> are no longer legal.
3.  Repeat last two steps until no legal pairings remain.

The bootstrap matching repeats this process many times producing many sets of matches. Of these potential matches, we select:

-   The matching with the largest number of matched pairs.
-   Then break ties by selecting the matching where the *z*-score difference in the matching measure (e.g., *age*) between the two groups is the smallest in size.
-   Then break remaining ties by selecting the first among the biggest-*n*, smallest-*z* matches.

A Small Example
---------------

Here are two groups of 5 each.

``` r
df <- tibble::data_frame(
  Group = c(rep("Treatment", 5), rep("Control", 5)) ,
  ID = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"),
  Age = c(10, 12, 11, 12, NA, 9, 12, 13, 14, 11)
)
df
#> # A tibble: 10 x 3
#>    Group     ID      Age
#>    <chr>     <chr> <dbl>
#>  1 Treatment a     10.0 
#>  2 Treatment b     12.0 
#>  3 Treatment c     11.0 
#>  4 Treatment d     12.0 
#>  5 Treatment e     NA   
#>  6 Control   f      9.00
#>  7 Control   g     12.0 
#>  8 Control   h     13.0 
#>  9 Control   i     14.0 
#> 10 Control   j     11.0
```

Some facts about the data here:

-   2 pairs that can be exactly matched.
-   3 members of the `Treatment` group can be matched to 2 members of the `Control` group
-   1 of the `Age` values is missing.

Exact matching
--------------

`boot_match_univariate()` will match two groups on a single variable. By setting `caliper` to 0, it will return exact matches.

``` r
library(bootmatch)
# for reproducibility
set.seed(20180116)

boot_match_univariate(df, Group, Age, caliper = 0, boot = 10)
#> Z-score difference for 2 pairs: 0
#> # A tibble: 10 x 5
#>    Group     ID      Age Matching     Matching_MatchID
#>    <chr>     <chr> <dbl> <chr>                   <int>
#>  1 Treatment a     10.0  unmatchable                NA
#>  2 Treatment b     12.0  unmatched                  NA
#>  3 Treatment c     11.0  matched                    10
#>  4 Treatment d     12.0  matched                     7
#>  5 Treatment e     NA    missing-data               NA
#>  6 Control   f      9.00 unmatchable                NA
#>  7 Control   g     12.0  matched                     4
#>  8 Control   h     13.0  unmatchable                NA
#>  9 Control   i     14.0  unmatchable                NA
#> 10 Control   j     11.0  matched                     3
```

The original dataframe is returned with two additional columns. `Matching` is the matching status for that row. The possible values are:

-   `matched` - this row was matched to a member in the other group.
-   `unmatched` - this row *could* have been matched to a member of the other. group, but its potential matches were matched up to other rows.
-   `unmatchable` - this row cannot be matched to any row in the other group.
-   `missing-data` - this row has missing data on the matching measure

The other returned column `Matching_MatchID` has the ID of that row's match. By default, the ID is the row number. For example, `"c"` is matched to row 10 which is `"j"`.

For convenience, we can provide the name of a column with ID's that will be used instead of row numbers.

``` r
set.seed(20180116)
boot_match_univariate(df, Group, Age, caliper = 0, boot = 10, id = ID)
#> Z-score difference for 2 pairs: 0
#> # A tibble: 10 x 5
#>    Group     ID      Age Matching     Matching_MatchID
#>    <chr>     <chr> <dbl> <chr>        <chr>           
#>  1 Treatment a     10.0  unmatchable  <NA>            
#>  2 Treatment b     12.0  unmatched    <NA>            
#>  3 Treatment c     11.0  matched      j               
#>  4 Treatment d     12.0  matched      g               
#>  5 Treatment e     NA    missing-data <NA>            
#>  6 Control   f      9.00 unmatchable  <NA>            
#>  7 Control   g     12.0  matched      d               
#>  8 Control   h     13.0  unmatchable  <NA>            
#>  9 Control   i     14.0  unmatchable  <NA>            
#> 10 Control   j     11.0  matched      c
```

### Caliper matching

Let's look at the data again.

``` r
df
#> # A tibble: 10 x 3
#>    Group     ID      Age
#>    <chr>     <chr> <dbl>
#>  1 Treatment a     10.0 
#>  2 Treatment b     12.0 
#>  3 Treatment c     11.0 
#>  4 Treatment d     12.0 
#>  5 Treatment e     NA   
#>  6 Control   f      9.00
#>  7 Control   g     12.0 
#>  8 Control   h     13.0 
#>  9 Control   i     14.0 
#> 10 Control   j     11.0
```

If we allow a little wiggle room, say +/- 1 month, then we can match 4 members of the `Control` group. Setting the `caliper` to 1 will do this.

``` r
boot_match_univariate(df, Group, Age, caliper = 1, boot = 10, id = ID)
#> Z-score difference for 4 pairs: 0
#> # A tibble: 10 x 5
#>    Group     ID      Age Matching     Matching_MatchID
#>    <chr>     <chr> <dbl> <chr>        <chr>           
#>  1 Treatment a     10.0  matched      f               
#>  2 Treatment b     12.0  matched      j               
#>  3 Treatment c     11.0  matched      g               
#>  4 Treatment d     12.0  matched      h               
#>  5 Treatment e     NA    missing-data <NA>            
#>  6 Control   f      9.00 matched      a               
#>  7 Control   g     12.0  matched      c               
#>  8 Control   h     13.0  matched      d               
#>  9 Control   i     14.0  unmatchable  <NA>            
#> 10 Control   j     11.0  matched      b
```

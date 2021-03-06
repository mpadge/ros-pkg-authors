A response to the [call for
help](https://github.com/ropenscilabs/annual-report-help) contributing
to the rOpenSci 2019 Annual Report, via [issue
\#4](https://github.com/ropenscilabs/annual-report-help/issues/4), which
aimed to quantify “Number of authors per package”. From the original
description by [@sckott](https://github.com/sckott):

> Does the number of maintainers per package change through time?

> In the interest of software sustainability, ideally each package would
> have more than one maintainer, but this is relatively rare. We’d like
> to see the number of maintainers per package increase over time, but
> number of maintainers is hard to address without detailed knowledge of
> each repo. As a proxy, we could count up all authors regardless of
> their role (not counting reviewers, funders).

> Question becomes: Does the number of authors per package change
> (increase) through time?

Suggested approaches were based on extracting “official” authors from
package DESCRIPTION files, but numbers of authors in this context are
unavoidably cumulative, and must increase across time because authors
are very generally *not* ever removed once added. Addressing the issue
through official DESCRIPTION files would thus require comparison of
these rates of increase with some kind of neutral, expected value, which
seems impracticable, so alternative approaches are pursued here.

The analyses mostly work via several functions defined in several
`function-*.R` files, loaded here, along with necessary libraries.

``` r
library (jsonlite)
library (dplyr)
library (magrittr)
library (ggplot2)
library (cranlogs)
source ("functions-repos.R")
source ("functions-extract.R")
source ("functions-analyse.R")
```

The functions mostly use the github graphql API to extract the full
commit histories of all rOpenSci package repos, and of RStudio packages
granted the honour of being listed in their [“official” hex sticker
page](https://github.com/rstudio/hex-stickers/tree/master/PNG). The
extraction of commit histories from the github graphql API requires a
client to be established with the following code:

``` r
token <- Sys.getenv("GITHUB_GRAPHQL_TOKEN") # or whatever
gh_cli <- ghql::GraphqlClient$new (
    url = "https://api.github.com/graphql",
    headers = list (Authorization = paste0 ("Bearer ", token))
)
```

## Get commit histories from github

The objects of this analysis are the entire commit histories (for the
default branch of a repository) of rOpenSci and RStudio repositories.
The first functions obtain all associated repositories as a `data.frame`
with columns for both repository name and associated github
organization. The second column is necessary because not all
repositories are directly and respectively hosted on
[`github/ropensci`](https://github.com/ropensci) or
[`github/rstudio`](https://github.com/rstudio). These data are extracted
with the functions, `get_ros_repos()` and `get_rstudio_repos`. Commit
histories can then be extracted by submitting these resultant
`data.frame` objects to the single function, `get_all_commits()`. This
function takes about 20 minutes or so to run for each of rOpenSci and
RStudio, so the resultant data are saved to enable immediate re-loading
in all subsequent analyses.

``` r
repos <- get_ros_repos ()
system.time (
             dat_ros <- get_all_commits (gh_cli, repos)
             )
names (dat_ros) <- repos
saveRDS (dat_ros, "results-ropensci.Rds")

dat <- get_rstudio_repos ()
system.time (
             dat_rst <- get_all_commits (gh_cli, repos)
             )
names (dat_rst) <- repos
saveRDS (dat_rst, "results-rstudio.Rds")
```

## Proportion of contributions from non-primary contributors

All of the following analyses focus on “non-primary contributors”, which
are simply contributors other than the statistically dominant
contributor. The first metric analysed here is the proportion of
non-primary contributions, via the `prop_np_commits()` function, which
by default yields quarterly-aggregates of contributions.

``` r
np_commits_rst <- prop_np_commits (readRDS ("results-rstudio.Rds"))
np_commits_rst$org <- "RStudio"
np_commits_ros <- prop_np_commits (readRDS ("results-ropensci.Rds"))
np_commits_ros$org <- "rOpenSci"
results <- bind_rows (np_commits_ros, np_commits_rst) %>%
    rename (non_primary = n)
    

ggplot (results, aes (date, non_primary)) +
    geom_point (colour = "#9239F6") +
    geom_smooth (colour = "#FF0076", method = "lm", formula = y ~ x) +
    facet_wrap (.~org) +
    theme (axis.title.y = element_text (angle = 90))
```

<img src="prop_np_commits-1.png" width="100%" /> RStudio packages
clearly have a statistically higher proportion of non-primary
contributions:

    #> mean (RStudio) = 0.369; mean (rOpenSci) = 0.264 [T = 7.77, df = 1227.1, p = 0.000000000000017]

The figure also clearly reveals that proportions of non-primary
contributions for RStudio packages have actually decreased between the
years 2010 and 2019 (although this decrease was not significant; T =
-1.8; p = 0.069). There was no significant change for rOpenSci (T =
-0.25; p = 0.8).

## Effect of package prominence

Packages that are more prominent may attract more non-primary
contributions, and so the preceding results may to some extent merely
reflect differences in package prominence. (We use the term “prominence”
here in lieu of “popularity”, with due connotation that prominence is an
attribute that can be actively manipulated; in particular, RStudio has a
commercial budget not available to rOpenSci, and which is able to be
directed towards increasing the prominence of their packages.)
Prominence is quantified here by the total number of package downloads
divided by the time elapsed since a package’s first release. For that,
we use the [`cranlogs` package](https://cranlogs.r-pkg.org/). Note that
not all rOpenSci packages have been released on CRAN, and so prominence
metrics will only exist for those which have. The extraction of
downloads can take quite some time, so we save the results for
subsequent analyses.

``` r
pkgs_rst <- names (readRDS ("results-rstudio.Rds"))
x <- cran_downloads (pkgs_rst, from = "1997-04-01")
saveRDS (x, file = "cran-rstudio.Rds")
pkgs_ros <- names (readRDS ("results-ropensci.Rds"))
x <- cran_downloads (pkgs_ros, from = "1997-04-01")
saveRDS (x, file = "cran-ropensci.Rds")
```

Each of these is a single `data.frame` with columns for `date` (daily
values from the specified `from` date), `count` of daily downloads, and
`package` naming each requested package. The following function then
converts these daily downloads for each package into a single measure of
average daily downloads over the entire lifetime of a package.

``` r
x <- readRDS ("cran-rstudio.Rds")
p_rst <- unlist (lapply (split (x, as.factor (x$package)), function (i) {
                 first <- which (i$count > 0) [1]
                 sum (i$count) / (nrow (i) - first + 1) }))
x <- readRDS ("cran-ropensci.Rds")
p_ros <- unlist (lapply (split (x, as.factor (x$package)), function (i) {
                 first <- which (i$count > 0) [1]
                 sum (i$count) / (nrow (i) - first + 1) }))
```

That of course raises the question of what those “prominence” scores
look like?

``` r
dat_rst <- data.frame (package = names (p_rst),
                       prominence = as.numeric (p_rst),
                       org = "RStudio",
                       stringsAsFactors = FALSE)
dat_ros <- data.frame (package = names (p_ros),
                       prominence = as.numeric (p_ros),
                       org = "rOpenSci",
                       stringsAsFactors = FALSE)
dat <- rbind (dat_rst, dat_ros)
dat$log_prominence <- log10 (dat$prominence)
ggplot (dat, aes (x = org, y = log_prominence, fill = org)) + 
    geom_violin (alpha = 0.7) +
    theme (axis.title.y = element_text (angle = 90))
```

<img src="prominence-1.png" width="100%" />

And perhaps unsurprisingly, RStudio packages are enormously more
prominent that rOpenSci packages (noting that the scale is logarithmic).
Does this prominence affect the proportions of non-primary
contributions? For that we need a single measure of the proportion of
commits aggregated over the entire history of each repo, extracted here
with a `quarterly = FALSE` argument.

``` r
np_commits_rst <- prop_np_commits (readRDS ("results-rstudio.Rds"), quarterly = FALSE)
np_commits_rst$org <- "RStudio"
np_commits_ros <- prop_np_commits (readRDS ("results-ropensci.Rds"), quarterly = FALSE)
np_commits_ros$org <- "rOpenSci"
np_commits <- dplyr::bind_rows (np_commits_ros, np_commits_rst) %>%
    dplyr::rename (package = repo)

dat <- dplyr::left_join (dat, np_commits, by = c ("package", "org")) %>%
    rename (non_primary = n)
ggplot (dat, aes (x = log_prominence, y = non_primary, colour = org)) +
    geom_point () +
    geom_smooth (method = "lm", formula = y ~ x) +
    theme (axis.title.y = element_text (angle = 90))
#> Warning: Removed 58 rows containing non-finite values (stat_smooth).
#> Warning: Removed 58 rows containing missing values (geom_point).
```

<img src="prom-non-primary-1.png" width="100%" />

The two organizations follow categorically different trajectories. More
prominent RStudio packages attract significantly greater proportions of
non-primary contributions (T = 4.9, p = 0.000012), while more prominent
rOpenSci packages tend to attract *lower* proportions of non-primary
contributions, and so become more dominated by singular primary
contributors, although this effect is not significant (T = -1.4, p =
0.18).

## Temporal patterns of non-primary contributions

We now delve into more detailed analyses of the git commit histories,
through analysing both numbers of commits and numbers of lines of code
committed. We quantify numbers of distinct contributors, through
aggregating numbers of both commits and lines of code over a defined
time period – fixed at 3 months throughout all of the following,
although could be easily modified – and grouping by unique contributor.
Contributions from the primary contributor are removed from the
analysis, so as only to count contributions from additional people other
than the primary author. The numbers are then converted to relative
amounts for each time period, sorted in decreasing order, and then
converted to a linear rate of decrease per additional unique
contributor. This property – referred to from hereon as “non-primary
contribution rate” – is strictly negative, but will approach one in the
ideal situation of all contributors to a package having equal
contributions. The more negative the non-primary contribution rate, the
more a package is dominated by a single contributor. This metric is
derived for each package for each quarter in which sufficient data are
available.

``` r
dat <- readRDS ("results-rstudio.Rds")
commits_rst <- stats_commits (dat)
lines_rst <- stats_lines (dat)
np_commits_rst <- prop_np_commits (dat)
dat <- readRDS ("results-ropensci.Rds")
commits_ros <- stats_commits (dat)
lines_ros <- stats_lines (dat)
np_commits_ros <- prop_np_commits (dat)

commits_rst$org <- "RStudio"
lines_rst$org <- "RStudio"
commits_ros$org <- "rOpenSci"
lines_ros$org <- "rOpenSci"

mean (commits_ros$slope, na.rm = TRUE); mean (commits_rst$slope, na.rm = TRUE)
#> [1] -0.3324508
#> [1] -0.1510639
mean (lines_ros$slope, na.rm = TRUE); mean (lines_rst$slope, na.rm = TRUE)
#> [1] -0.5132642
#> [1] -0.2597777

results <- rbind (commits_rst,
                  commits_ros,
                  lines_rst,
                  lines_ros) %>%
    dplyr::filter (!is.na (slope))

ggplot (results, aes (date, slope)) +
    geom_point (colour = "#9239F6") +
    geom_smooth (colour = "#FF0076", method = "lm") +
    facet_wrap (.~var + org) +
    ylab ("Non-primary contribution rate") +
    theme (axis.title.y = element_text (angle = 90))
#> `geom_smooth()` using formula 'y ~ x'
```

<img src="authors-per-time-interval-1.png" width="100%" />

Non-primary contributions in terms of both commits and lines of code
have thus increased over time in both organizations, with values being
clearly higher for RStudio than rOpenSci.

## rOpenSci package categories

We now repeat the above analyses for sub-sets of packages within
categories designed by rOpenSci. These categories are provided in the
`get_ros_repos()`, summarised thus:

``` r
pkgs <- get_ros_repos ()
tab <- table (pkgs$category)
knitr::kable (data.frame (name = names (tab),
                          num_packages = as.integer (tab)))
```

| name               | num\_packages |
| :----------------- | ------------: |
| altmetrics         |             2 |
| data-access        |            79 |
| data-analysis      |             4 |
| data-extraction    |             3 |
| data-publication   |             5 |
| data-tools         |            14 |
| data-visualization |             4 |
| databases          |             4 |
| geospatial         |            20 |
| http-tools         |            11 |
| image-processing   |             5 |
| literature         |            22 |
| scalereprod        |            12 |
| security           |             2 |
| taxonomy           |             7 |

We now repeat the analysis immediately above of relative rates of
non-primary contributions over time for packages within each of these
categories. The baseline for comparison formed from all packages
considered together has a mean non-primary contribution rate of -0.332,
with an increase per year of 0.0169. The equivalent RStudio values are a
mean of -0.151 with an increase per year of 0.0192.

``` r
category_stats <- function (pkgs, commits, category = "data-access")
{
    cat_pkgs <- pkgs$repo [which (pkgs$category %in% category)]
    cat_commits <- commits_ros [commits_ros$repo %in% cat_pkgs, ] %>%
        filter (!is.na (slope))
    slope <- mn <- NA
    if (nrow (cat_commits) > 1)
    {
        mod <- summary (lm (cat_commits$slope ~ cat_commits$date))
        slope <- mod$coefficients [2, 1]
        mn <- mean (cat_commits$slope, na.rm = TRUE)
    }
    c (mean_slope = mn, change = slope)
}
res <- t (vapply (unique (pkgs$category [!is.na (pkgs$category)]), function (i)
                  category_stats (pkgs, commits, i), numeric (2)))
res <- data.frame (category = c ("RStudio-all", "rOpenSci", rownames (res)),
                   npcr = c (npcr_rst, npcr_ros, res [, 1]),
                   change = c (slope_rst, slope_ros, res [, 2]),
                   stringsAsFactors = FALSE) %>%
    filter (!is.na (change))
# Leave Rstudio and rOpenSci at stop, and sort all other rows
index <- c (1, 2, 2 + order (res$npcr [3:nrow (res)], decreasing = TRUE))
knitr::kable (res [index, ], digits = c (0, 3, 3), row.names = FALSE)
```

| category           |    npcr |  change |
| :----------------- | ------: | ------: |
| RStudio-all        | \-0.151 |   0.019 |
| rOpenSci           | \-0.332 |   0.017 |
| image-processing   | \-0.012 |   0.024 |
| http-tools         | \-0.221 | \-0.086 |
| databases          | \-0.223 | \-0.005 |
| data-visualization | \-0.280 | \-0.053 |
| data-publication   | \-0.300 |   0.055 |
| scalereprod        | \-0.317 |   0.011 |
| data-access        | \-0.319 |   0.023 |
| data-extraction    | \-0.333 |   0.000 |
| literature         | \-0.361 |   0.016 |
| geospatial         | \-0.375 |   0.070 |
| taxonomy           | \-0.414 |   0.020 |
| data-tools         | \-0.423 |   0.037 |
| data-analysis      | \-0.594 | \-0.224 |

And the image processing category is the one and only category that
outperforms RStudio in terms both of overall non-primary commits
(through having the lowest non-primary commit rate of all), and in that
tendency increasing more strongly over time (`change` = 0.024). The
following three categories (http-tools, databases, data-visualization)
all have relatively low mean non-primary commit values, yet actually
become more negative over time, indicating *decreasing* degrees of
community engagement in the code of these packages. The next category of
data-publication has the second-highest rate of increase in engagement
over time (`change =` 0.055). The highest rate of increase in engagement
comes from the geospatial category, to which most of my packages belong.
So at least I am potentially part of one small yet positive contribution
to the broader rOpenSci community.

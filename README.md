# Authorial contributions of rOpenSci packages

Mostly works via several functions defined in `function-defs.R`, loaded
here.

``` r
library(crandb)
library(jsonlite)
library(dplyr)
library (magrittr)
source ("function-defs.R")
```

## get authors from description files

Modified from @skcott’s code to grab author lists from package
DESCRIPTION files.

``` r
x <- fromJSON("https://raw.githubusercontent.com/ropensci/roregistry/gh-pages/registry.json")
pkg_names <- filter(x$packages, on_cran) %>% .$name
urls <- filter(x$packages, on_cran) %>% .$github
urls <- gsub ("https://github.com/", "", urls)
orgs <- vapply (urls, function (i) strsplit (i, "/") [[1]] [1], character (1))
repos <- vapply (urls, function (i) strsplit (i, "/") [[1]] [2], character (1))
names (orgs) <- names (repos) <- repos

if (!file.exists ("authors.Rds"))
{
    authors <- get_pkg_authors ()
    saveRDS (authors, file = "authors.Rds")
} else
    authors <- readRDS ("authors.Rds")
```

## get commit history from github and match to those authors

This requires a graphql client to be established with the following
code:

``` r
token <- Sys.getenv("GITHUB_GRAPHQL_TOKEN") # or whatever
gh_cli <- ghql::GraphqlClient$new (
    url = "https://api.github.com/graphql",
    headers = httr::add_headers (Authorization = paste0 ("Bearer ", token))
)
```

Extracting all of the commit histories takes about 20 min or so to run:

``` r
get_all_commits <- function (gh_cli)
{
    st0 <- Sys.time ()
    lapply (seq (repos), function (i) {
                   res <- process_commit_history (gh_cli, orgs [i], repos [i])
                   st <- difftime (Sys.time (), st0, units = "s") / i
                   # st is time per repo to that point
                   st <- hms::as_hms (round (st * (length (repos) - i)))
                   message (repos [i], ": ", i, " / ", length (repos),
                            "; estimated time left = ", st)
                   saveRDS (res, "results.Rds")
                   return (res)
       })
}
system.time (
             res <- get_all_commits (gh_cli)
             )
names (res) <- repos
saveRDS (res, "results.Rds")
```

## Temporal analyses of the data

``` r
library (magrittr)
library (dplyr)
library (ggplot2)
dat <- readRDS ("results.Rds")
nr <- vapply (dat, function (i)
              ifelse (is.null (i), 0L, nrow (i)), integer (1))
dat <- dat [which (nr > 0)]
stats <- lapply (dat, function (i) {
        # convert dates to quarterly fractions for grouping below
        y <- as.integer (lubridate::year (i$date))
        qtr <- ceiling (lubridate::month (i$date) / 3)
        i$date <- y + (qtr - 1) / 4

        # rate of change in numbers of quarterly contributors over
        # time:
        contrib_over_time <- group_by (i, date, name) %>%
            summarise (n = length (name)) %>%
            group_by (date) %>%
            summarise (n = length (name)) %>%
            summarise (summary (lm (n ~ date))$coefficients [2]) %>%
            as.numeric ()

        # name of primary contributor
        primary <- names (sort (table (i$name), decreasing = TRUE)) [1]
        # relative proportion of non-primary commits and lines
        non_primary_contribs <- group_by (i, date) %>%
            summarise (n_prim_commits = length (additions [name == primary]),
                       n_non_prim_commits = length (additions [name != primary]),
                       n_prim_lines = sum (additions [name == primary]),
                       n_non_prim_lines = sum (additions [name != primary])) %>%
            mutate (commits = n_non_prim_commits / n_prim_commits,
                    lines = n_non_prim_lines / n_prim_lines) %>%
            select (date, commits, lines) %>%
            filter (is.finite (commits))

        # calculate slopes from commits and lines of code
        slope_commits <- summary (lm (non_primary_contribs$commits ~
                                      non_primary_contribs$date))$coefficients [2]
        slope_lines <- summary (lm (non_primary_contribs$lines ~
                                    non_primary_contribs$date))$coefficients [2]
        c (contributors = contrib_over_time,
           commits = slope_commits,
           lines = slope_lines)
    })
```

    ## Warning in summary.lm(lm(n ~ date)): essentially perfect fit: summary may
    ## be unreliable
    
    ## Warning in summary.lm(lm(n ~ date)): essentially perfect fit: summary may
    ## be unreliable
    
    ## Warning in summary.lm(lm(n ~ date)): essentially perfect fit: summary may
    ## be unreliable
    
    ## Warning in summary.lm(lm(n ~ date)): essentially perfect fit: summary may
    ## be unreliable
    
    ## Warning in summary.lm(lm(n ~ date)): essentially perfect fit: summary may
    ## be unreliable
    
    ## Warning in summary.lm(lm(n ~ date)): essentially perfect fit: summary may
    ## be unreliable
    
    ## Warning in summary.lm(lm(n ~ date)): essentially perfect fit: summary may
    ## be unreliable
    
    ## Warning in summary.lm(lm(n ~ date)): essentially perfect fit: summary may
    ## be unreliable
    
    ## Warning in summary.lm(lm(n ~ date)): essentially perfect fit: summary may
    ## be unreliable
    
    ## Warning in summary.lm(lm(n ~ date)): essentially perfect fit: summary may
    ## be unreliable
    
    ## Warning in summary.lm(lm(n ~ date)): essentially perfect fit: summary may
    ## be unreliable
    
    ## Warning in summary.lm(lm(n ~ date)): essentially perfect fit: summary may
    ## be unreliable
    
    ## Warning in summary.lm(lm(n ~ date)): essentially perfect fit: summary may
    ## be unreliable
    
    ## Warning in summary.lm(lm(n ~ date)): essentially perfect fit: summary may
    ## be unreliable
    
    ## Warning in summary.lm(lm(n ~ date)): essentially perfect fit: summary may
    ## be unreliable
    
    ## Warning in summary.lm(lm(n ~ date)): essentially perfect fit: summary may
    ## be unreliable
    
    ## Warning in summary.lm(lm(n ~ date)): essentially perfect fit: summary may
    ## be unreliable
    
    ## Warning in summary.lm(lm(n ~ date)): essentially perfect fit: summary may
    ## be unreliable

``` r
stats <- data.frame (do.call (rbind, stats)) %>%
    tidyr::gather () %>%
    filter (is.finite (value))

g <- ggplot (stats, aes (x = value)) +
    geom_histogram (color = "#FF0076", fill = "#903495") +
    scale_y_log10 () +
    geom_vline (aes (xintercept = 0), color = "#9239F6", cex = 1, linetype = "dashed") +
    facet_grid (.~key, scales = "free")
print (g)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Transformation introduced infinite values in continuous y-axis

    ## Warning: Removed 54 rows containing missing values (geom_bar).

![](temporal-analyses-1.png)<!-- -->

``` r
t.test (stats$value [stats$key == "contributors"])
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  stats$value[stats$key == "contributors"]
    ## t = -1.2678, df = 196, p-value = 0.2064
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.13240193  0.02878438
    ## sample estimates:
    ##   mean of x 
    ## -0.05180878

``` r
t.test (stats$value [stats$key == "commits"])
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  stats$value[stats$key == "commits"]
    ## t = 2.6848, df = 195, p-value = 0.007881
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  0.04145424 0.27090439
    ## sample estimates:
    ## mean of x 
    ## 0.1561793

``` r
t.test (stats$value [stats$key == "lines"])
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  stats$value[stats$key == "lines"]
    ## t = 0.96202, df = 195, p-value = 0.3372
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  -3.530418 10.254636
    ## sample estimates:
    ## mean of x 
    ##  3.362109

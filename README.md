# Authorial contributions of rOpenSci packages

## get authors from description files

modified from @skcott’s code

``` r
library(crandb)
library(jsonlite)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library (magrittr)
source ("function-defs.R")

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

Extracting all of the commit histories takes about half an hour or so to
run:

``` r
get_all_summaries < function (gh_cli)
{
    st0 <- Sys.time ()
    res <- lapply (seq (repos), function (i) {
                       res <- overall_summary (gh_cli, orgs [i], repos [i], authors)
                       st <- difftime (Sys.time (), st0, units = "s") / i
                       # st is time per repo to that point
                       st <- hms::as_hms (round (st * (length (repos) - i)))
                       message (repos [i], ": ", i, " / ", length (repos),
                                "; estimated time left = ", st)
           })
    names (res) <- repos
    res <- do.call (rbind, res)
    nms <- row.names (res)
    row.names (res) <- NULL
    data.frame (repo = nms,
                res,
                stringsAsFactors = FALSE)
}
res <- get_all_summaries (gh_cli)
saveRDS (res, "results.Rds")
```

# Functions to extract repos and orgs for rOpenSci and RStudio packages

# rOpenSci repos can be read directly from a registry:
get_ros_repos <- function () {
    x <- fromJSON (paste0 ("https://raw.githubusercontent.com/ropensci/",
                           "roregistry/gh-pages/registry.json"))
    pkg_names <- filter (x$packages, on_cran) %>% .$name
    categories <- filter (x$packages, on_cran) %>% .$ropensci_category
    urls <- filter (x$packages, on_cran) %>% .$github
    urls <- gsub ("https://github.com/", "", urls)
    orgs <- vapply (urls, function (i) strsplit (i, "/") [[1]] [1],
                    character (1))
    repos <- vapply (urls, function (i) strsplit (i, "/") [[1]] [2],
                     character (1))
    data.frame (org = orgs,
                repo = repos,
                category = categories,
                stringsAsFactors = FALSE)
}

# in lieu of an "official" list of packages, this function gets all the package
# names that they deign as deserving of a hex sticker
get_rstudio_repo_names <- function () {
    u <- "https://github.com/rstudio/hex-stickers/tree/master/PNG"
    x <- httr::GET (u) %>%
        httr::content (as = "parsed") %>%
        rvest::html_table ()
    repos <- gsub ("\\.png", "", x [[1]]$Name)
    repos <- repos [(grep ("RStudio", repos) + 1):length (repos)]
    repos <- gsub ("\\-female", "", repos) # "plumber-female"
    repos <- repos [repos != "pipe"] # "magrittr"
    return (repos)
}

# github orgs are not necessarily rstudio. This function gets the org names from
# looking at the websites listed on CRAN as generated from the DESCRIPTION files
get_org <- function (repo) {
    u <- paste0 ("https://cran.r-project.org/package=", repo)
    x <- httr::GET (u) %>%
        httr::content (as = "parsed", encoding = "UTF-8") %>%
        rvest::html_table ()
    urls <- as.vector (x [[1]] [x [[1]] [, 1] == "URL:", ] [, 2]) %>%
        strsplit (",")
    if (!any (grepl ("github", urls [[1]])))
    { # no github URLs; look in BugReports
        urls <- as.vector (x [[1]] [x [[1]] [, 1] == "BugReports:", ] [, 2]) %>%
            strsplit (",")
    }
    if (length (urls) == 0)
        return ("")

    ret <- ""
    if (any (grepl ("github\\.com", urls [[1]])))
    {
        urls <- urls [[1]] [grep ("github\\.com", urls [[1]])]
        if (length (urls) > 0)
            ret <- strsplit (strsplit (urls, "github\\.com\\/") [[1]] [2],
                             "\\/") [[1]] [1]
    } else if (any (grepl ("github\\.io", urls [[1]])))
    {
        urls <- urls [[1]] [grep ("\\.github\\.io", urls [[1]])]
        if (length (urls) > 0)
            ret <- strsplit (strsplit (urls, "\\.github\\.io") [[1]] [1],
                             "https:\\/\\/") [[1]] [2]
    }

    return (ret)
}

get_rstudio_repos <- function () {
    repos <- get_rstudio_repos ()
    orgs <- vapply (repos, function (i) get_org (i), character (1))
    data.frame (org = orgs, repo = repos, stringsAsFactors = FALSE)
}

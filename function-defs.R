
get_pkg_authors <- function () # takes a minute or two ...
{
    authors <- lapply(pkg_names, function(w) {
                    pkg <- package(w, version = "all")
                    authors <- lapply (pkg$versions, function (z) {
                        # remove author roles, which may contain commas, as well as initial "\n":
                        authors <- gsub ("[\n]", "", gsub ("\\[.*?\\]", "", z$Author))
                        # also remove stuff between (), like orcid ids:
                        authors <- gsub ("\\(.*?\\)", "", authors)
                        # seperate individual authors:
                        authors <- strsplit (authors, ",") [[1]]
                        # remove leading and trailing whitespace and some other stuff
                        authors <- gsub ("^\\s+|\\s+$", "", authors)
                        gsub ("<U\\+000a>", "", authors)    })

                    # that is a list of all authors for each version, but for now
                    # just reduce that to overall unique authors
                    unique (do.call (c, authors))
    })
    names (authors) <- pkg_names
    return (authors)
}

# build the github graphql query for commit data. (See below for the
# "branch" parameter.)
get_qry <- function (gh_cli, org = "ropensci", repo, endCursor = NULL, branch = "master")
{
    after_txt <- ""
    if (!is.null (endCursor))
        after_txt <- paste0 ("(after:\"", endCursor, "\")")

    q <- paste0 ("{
        repository(owner:\"", org, "\", name:\"", repo, "\") {
            branch0: ref(qualifiedName: \"", branch, "\") {
                target {
                    ... on Commit {
                        history ", after_txt, "{
                            totalCount
                            nodes {
                                ... on Commit {
                                    committedDate
                                    additions
                                    message
                                    author {
                                        name
                                        user {
                                            login
                                            name
                                        }
                                    }
                                }
                            }
                            pageInfo {
                                hasNextPage
                                endCursor
                            }
                        }
                    }
                }
            }
        }
    }")
    qry <- ghql::Query$new()
    qry$query('get_commits', q)

    return (qry)
}

default_branch_name <- function (gh_cli, org = "ropensci", repo)
{
    q <- paste0 ("{
        search(query: \"", org, ":", repo, "\", type: REPOSITORY, last:100) {
            nodes {
                ... on Repository {
                    name
                    defaultBranchRef {
                        name
                    }
                }
            }
        }
    }")
    qry <- ghql::Query$new()
    qry$query('default_branch', q)

    dat0 <- gh_cli$exec(qry$queries$default_branch) %>%
        jsonlite::fromJSON ()
    res <- dat0$data$search$nodes$defaultBranchRef
    if (!is.null (res))
    {
        if (all (is.na (res)))
            res <- "master"
        else {
            res <- res$name
            if ("master" %in% res)
                res <- "master"
            else
                res <- res [1]
        }
    }
    return (res)
}


total_commits <- function (gh_cli, org = "ropensci", repo = "osmdata", branch = "master")
{
    q <- paste0 ("{
        repository(owner:\"", org, "\", name:\"", repo, "\") {
            branch0: ref(qualifiedName: \"", branch, "\") {
                target {
                    ... on Commit {
                        history {
                            totalCount
                        }
                    }
                }
            }
        }
    }")
    qry <- ghql::Query$new()
    qry$query('total_commits', q)

    dat <- gh_cli$exec(qry$queries$total_commits) %>%
        jsonlite::fromJSON ()
    dat$data$repository$branch0$target$history$totalCount
}

get_commit_history <- function (gh_cli, org = "ropensci", repo,
                                branch = "master", pb = FALSE)
{
    has_next_page <- TRUE
    endCursor <- NULL
    dat <- list ()
    if (pb)
    {
        nc <- ceiling (total_commits (gh_cli, org, repo, branch) / 100)
        pb <- txtProgressBar (style = 3)
        count <- 1
    }
    while (has_next_page)
    {
        qry <- get_qry (gh_cli, org, repo, endCursor, branch)

        x <- gh_cli$exec(qry$queries$get_commits) %>%
            jsonlite::fromJSON ()
        x <- x$data$repository$branch$target$history
        has_next_page <- x$pageInfo$hasNextPage
        endCursor <- x$pageInfo$endCursor
        x <- x$nodes

        # unnest the nested columns - tidyr is messier than this:
        user <- x$author$user
        if (all (is.na (user)))
        {
            user <- data.frame (login = as.character (user),
                                name = as.character (user),
                                stringsAsFactors = FALSE)
        }
        name <- x$author$name
        x <- data.frame (x [, !names (x) %in% "author"],
                         login = user$login,
                         user.name = user$name,
                         name = name,
                         stringsAsFactors = FALSE)
        dat [[length (dat) + 1]] <- x
        if (pb)
        {
            setTxtProgressBar (pb, count / nc)
            count <- count + 1
        }
    }
    if (pb)
        close (pb)
    return (dat)
}

summarise_commit_history <- function (gh_cli, org = "ropensci", repo)
{
    branch <- default_branch_name (gh_cli, org, repo) [1]
    if (is.null (branch)) # if repo does not exist on ropensci, like auk
        return (NULL)

    dat <- get_commit_history (gh_cli, org, repo, branch)
    dat <- do.call (rbind, dat)

    # then find user name from three potential columns of (login, user.name, name)
    full_name <- function (i) stringr::str_count (i, "\\w+") > 1
    user <- dat$user.name
    index <- which (!full_name (user))
    user [index] <- dat$name [index]
    # find whether any names are non-spaced versions of full names
    users <- unique (tolower (user))
    nm <- names (which (table (gsub (" ", "", users)) > 1))
    if (length (nm) > 0)
    {
        for (n in nm)
        {
            index <- which (gsub (" ", "", users) == n)
            the_name <- users [index] [which.max (nchar (users [index]))]
            # then replace all non-space versions
            user [which (gsub (" ", "", tolower (user)) == n)] <- the_name
        }
    }

    # summarise:
    data.frame (date = dat$committedDate,
                       additions = dat$additions,
                       name = user,
                       stringsAsFactors = FALSE) %>%
        group_by (name) %>%
        summarise (ncommits = length (additions),
                   nlines = sum (additions)) %>%
        arrange (desc (ncommits), desc (nlines)) %>%
        filter (!is.na (name))
}

# authors is result of first function above
overall_summary <- function (gh_cli, org = "ropensci", repo, authors)
{
    x <- summarise_commit_history (gh_cli, org, repo)
    if (is.null (x)) # empty repo
        return (NULL)

    authors_i <- tolower (authors [[repo]])
    authors_index <- which (!is.na (pmatch (tolower (x$name), authors_i)))

    # summary of official authorial contributions versus unacknowledged:
    ncommits_authors <- sum (x$ncommits [authors_index])
    nlines_authors <- sum (x$nlines [authors_index])
    ncommits <- sum (x$ncommits)
    nlines <- sum (x$nlines)
    ncommits_non_authors <- ncommits - ncommits_authors
    nlines_non_authors <- nlines - nlines_authors
    pccommits_non_authors <- ncommits_non_authors / ncommits
    pclines_non_authors <- nlines_non_authors / nlines

    # relative contributions of official authors
    x$nc <- x$ncommits / sum (x$ncommits)
    x$nl <- x$nlines / sum (x$nlines)

    # summarise with
    # 1. Number of official authors
    # 2. Percent of commits contributed by official authors
    # 3. Percent of lines contributed by official authors
    # 4. Percent of commits contributed by primary official author
    # 5. Perfect of lines contributed by primary official author

    c (nauthors = length (authors_index),
       pc_official_commits = ncommits_authors / ncommits,
       pc_official_lines = nlines_authors / nlines,
       pc_author1_commits = x$nc [1],
       pc_author1_lines = x$nl [1])
}


# Get all commit histories of all repos for one organisation
# dat is result of either `get_ros_repos` or `get_rstudio_repos`
get_all_commits <- function (gh_cli, dat)
{
    st0 <- Sys.time ()
    lapply (seq (repos), function (i) {
                   res <- process_commit_history (gh_cli,
                                                  dat$org [i],
                                                  dat$repo [i])
                   st <- difftime (Sys.time (), st0, units = "s") / i
                   # st is time per repo to that point
                   st <- hms::as_hms (round (st * (length (repos) - i)))
                   message (repos [i], ": ", i, " / ", length (repos),
                            "; estimated time left = ", st)
                   return (res)
       })
}

remove_empty_histories <- function (dat)
{
    nr <- vapply (dat, function (i)
                  ifelse (is.null (i), 0L, nrow (i)), integer (1))
    dat [which (nr > 0)]
}

convert_date_to_quarter <- function (x)
{
    y <- as.integer (lubridate::year (x$date))
    qtr <- ceiling (lubridate::month (x$date) / 3)
    x$date <- y + (qtr - 1) / 4
    return (x)
}

filter_out_primary_contributor <- function (x)
{
    primary <- names (sort (table (x$name), decreasing = TRUE)) [1]
    dplyr::filter (x, name != primary)
}

# label as primary vs non-primary contributors
label_contributors <- function (x)
{
    primary <- names (sort (table (x$name), decreasing = TRUE)) [1]
    x$contrib <- "secondary"
    x$contrib [x$name == primary] <- "primary"
    return (x)
}

# Add a column to results of the stats_ functions with names of repos
add_names <- function (res) {
    for (i in seq_along (res))
        res [[i]]$repo <- names (res) [i]
    return (res)
}

remove_empty <- function (res) {
    index <- which (vapply (res, function (i) !is.null (i), logical (1)))
    res [index]
}

# min_len is the minimal length over which a slope will be calculated
stats_commits <- function (dat, min_len = 2)
{
    dat <- remove_empty_histories (dat)
    out <- lapply (dat, function (i) {
        temp <- convert_date_to_quarter (i) %>%
            filter_out_primary_contributor ()
        if (nrow (temp) < 2)
            return (NULL)

        # decrease in relative contributions per author for that time period
        # first for number of commits:
        temp <- group_by (temp, date, name) %>%
            summarise (n = length (name)) %>%
            group_by (date) %>%
            mutate (len = length (n)) %>%
            filter (len >= min_len) %>%
            group_by (date) %>%
            mutate (n = sort (n / sum (n), decreasing = TRUE))
        if (nrow (temp) > 0)
        {
            suppressWarnings ({ # essentially perfect lm fit
                temp <- group_by (temp, date) %>%
                    summarise (slope =
                               summary (lm (n ~ seq (n)))$coefficients [2]) %>%
                    filter (is.finite (slope))
            })
        }
        if (nrow (temp) > 0)
            temp$var <- "commits"
        else
            temp <- NULL
        return (temp)
    })

    dplyr::bind_rows (remove_empty (add_names (out)))
}

stats_lines <- function (dat, min_len = 2) {
    dat <- remove_empty_histories (dat)
    out <- lapply (dat, function (i) {
        i <- convert_date_to_quarter (i) %>%
            filter_out_primary_contributor ()
        if (nrow (i) < 2)
            return (NULL)

        # then for lines of code:
        res <- group_by (i, date, name) %>%
            summarise (n = sum (additions)) %>%
            group_by (date) %>%
            mutate (len = length (n)) %>%
            filter (len >= min_len) %>%
            group_by (date) %>%
            mutate (n = sort (n / sum (n), decreasing = TRUE))
        if (nrow (res) > 0)
        {
            suppressWarnings ({ # essentially perfect lm fit
                res <- group_by (res, date) %>%
                    summarise (slope =
                               summary (lm (n ~ seq (n)))$coefficients [2]) %>%
                    filter (is.finite (slope))
            })
        }
        if (nrow (res) > 0)
            res$var <- "lines"
        else
            res <- NULL
        return (res)
    })
    dplyr::bind_rows (remove_empty (add_names (out)))
}

# proportion of commits by non-primary authors
prop_np_commits <- function (dat, quarterly = TRUE)
{
    dat <- remove_empty_histories (dat)
    dat <- lapply (dat, function (i) label_contributors (i))
    out <- lapply (dat, function (i) {
        if (quarterly)
            temp <- convert_date_to_quarter (i)
        else {
            temp <- i
            temp$date <- 2019
        }
        if (nrow (temp) < 2)
            return (NULL)

        # decrease in relative contributions per author for that time period
        # first for number of commits:
        temp <- group_by (temp, date, contrib) %>%
            summarise (n = length (contrib)) %>%
            group_by (date) %>%
            mutate (len = length (n)) %>%
            filter (len > 1)
        if (nrow (temp) > 0)
        {
            temp_p <- dplyr::filter (temp, contrib == "primary")
            temp_s <- dplyr::filter (temp, contrib == "secondary")
            temp_s$n <- temp_s$n / (temp_s$n + temp_p$n)
            temp <- dplyr::select (temp_s, date, n)
        } else
            temp <- NULL
            
        return (temp)
    })

    dplyr::bind_rows (remove_empty (add_names (out))) %>%
        dplyr::select (date, n, repo) %>%
        dplyr::filter (!is.na (n))
}


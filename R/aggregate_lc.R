melt_lc <- function(d) {
    add_column_id(d) %>%
        melt("I", variable.name = "LC") %>%
        data.table()
}

aggregate_lc <- function(df_diff) {
    lcs <- unique(LCs) %>%
        rm_empty() %>%
        set_names(., .) # NAs also removed
    ans <- foreach(lc = lcs, i = icount()) %do% {
        I_col <- which(LCs == lc)
        rowMeans2(df_diff, cols = I_col, na.rm = TRUE)
    }
    as.data.table(ans) %>% add_column_id()
}


# error result: lc should be sum, other than mean

#' @importFrom matrixStats rowSums2
aggregate_major <- function(grid) {
    lcs <- unique(LCs) %>%
        rm_empty() %>%
        set_names(., .) # NAs also removed
    df = grid@data %>% as.matrix()
    ans <- foreach(lc = lcs, i = icount()) %do% {
        I_col <- which(LCs == lc)
        rowSums2(df, cols = I_col, na.rm = TRUE)
    }
    as.data.table(ans) # %>% add_column_id()
}

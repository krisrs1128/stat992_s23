
###############################################################################
## Functions for simulating some example data
###############################################################################

rmat <- function(generator) {
  function(n, p, ...) {
    matrix(generator(n * p, ...), n, p)
  }
}

flip_sign <- function(x) {
  sample(c(1, -1), length(x), replace = TRUE) * x
}

###############################################################################
## Functions for interpretation
###############################################################################

fdps <- function(beta, beta_hat) {
  result <- vector(length = length(beta))
  for (k in seq_along(beta)) {
    fp <- (beta_hat[[k]] != 0) & (beta[[k]] == 0)
    result[k] <- sum(fp) / sum(beta_hat[[k]] != 0)
  }
  
  result
}

power <- function(beta, beta_hat) {
  result <- vector(length = length(beta))
  for (k in seq_along(beta)) {
    tp <- (beta_hat[[k]] != 0) & (beta[[k]] != 0)
    result[k] <- sum(tp) / sum(beta[[k]] != 0)
  }
  
  result
}

scatter <- function(x_df, features, tables) {
  x_df |>
    mutate(tbl_feature = str_c(table, "_", name)) |>
    filter(tbl_feature %in% str_c(tables, "_", features)) |>
    select(-table, -tbl_feature) |>
    pivot_wider(names_from = name, values_from = value) |>
    ggplot() +
    geom_point(aes(.data[[features[1]]], .data[[features[[2]]]]))
  
}
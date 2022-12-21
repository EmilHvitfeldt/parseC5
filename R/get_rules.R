#' Parse C5.0 model
#'
#' @param x A fitted C5.0 model
#'
#' @return a dplyr::tibble
#' @export
parse_model <- function(x) {
  tree_raw <- x$tree %>%
    stringr::str_split("\n") %>%
    purrr::pluck(1)

  tree <- tree_raw[seq(3, length(tree_raw) - 1)]
  tree <- stringr::str_subset(tree, "freq=")

  parse_tree(tree)
}

parse_tree <- function(tree) {
  trees <- list()

  index <- 1
  repeat {
    rules <- get_rule_index(index, tree)

    tree_tbl <- dplyr::tibble(
      node = seq_along(rules),
      rule = lapply(rules, parse_rule),
      freqs = lapply(rules, get_freqs, tree = tree)
    )

    trees <- c(trees, list(tree_tbl))

    if (max(unlist(rules)) == length(tree)) break
    index <- max(unlist(rules)) + 1
  }

  purrr::map_dfr(trees, identity, .id = "tree")
}

get_rule_index <- function(index, tree, history = c()) {
  res <- list()
  history <- c(history, index)
  curr <- tree[index]

  if (stringr::str_detect(curr, "cut=")) {
    att <- stringr::str_remove(stringr::str_remove(curr, ".*att=\""), "\".*")
    cut <- stringr::str_remove(stringr::str_remove(curr, ".*cut=\""), "\".*")
    rule1_name <- paste(att, "<=", cut)
    rule1_index <- stats::setNames(index + 1, rule1_name)

    rule1 <- get_rule_index(rule1_index, tree, history)

    rule2_name <- paste(att, ">", cut)
    rule2_index <- stats::setNames(max(unlist(rule1)) + 1, rule2_name)

    rule2 <- get_rule_index(rule2_index, tree, history)
    res <- c(res, rule1, rule2)
  } else if (stringr::str_detect(curr, "elts=")) {
    att <- stringr::str_remove(stringr::str_remove(curr, ".*att=\""), "\".*")
    new_rules <- list()
    elts <- strsplit(curr, " elts=")[[1]][-1]
    for (i in seq_along(elts)) {
      value <- paste0("c(", elts[i], ")")
      rule_name <- paste(att, "%in%", value)
      rule_index <- stats::setNames(max(c(index, unlist(new_rules))) + 1, rule_name)

      new_rule <- get_rule_index(
        index = rule_index,
        tree = tree,
        history = history
      )
      new_rules <- c(new_rules, new_rule)
    }
    res <- c(res, new_rules)
  } else {
    return(list(history))
  }

  res
}

parse_rule <- function(x) {
  x <- names(x)
  x <- x[-1]
  x <- paste(x, collapse = " & ")
  x <- rlang::parse_expr(x)
  x
}

get_freqs <- function(rule, tree) {
  last <- utils::tail(rule, 1)

  freqs <- stringr::str_remove(tree[last], ".*freq=")
  freqs <- stringr::str_extract_all(freqs, "[0-9]+")[[1]]
  freqs <- as.integer(freqs)

  freqs
}

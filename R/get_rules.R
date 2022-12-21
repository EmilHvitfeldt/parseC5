parse_tree <- function(tree) {
  rules <- get_rule_index(1, tree)

  tibble(
    node = seq_along(rules),
    rule = map(rules, parse_rule)
  )
}

get_rule_index <- function(index, tree, history = c()) {

  res <- list()
  history <- c(history, index)
  curr <- tree[index]

  if (str_detect(curr, "cut=")) {
    att <- str_remove(str_remove(curr, ".*att=\""), "\".*")
    cut <- str_remove(str_remove(curr, ".*cut=\""), "\".*")
    rule1_name <- paste(att, "<=", cut)
    rule1_index <- setNames(index + 1, rule1_name)

    rule1 <- get_rule_index(rule1_index, tree, history)

    rule2_name <- paste(att, ">", cut)
    rule2_index <- setNames(max(unlist(rule1)) + 1, rule2_name)

    rule2 <- get_rule_index(rule2_index, tree, history)
    res <- c(res, rule1, rule2)
  } else if (str_detect(curr, "elts=")) {
    att <- str_remove(str_remove(curr, ".*att=\""), "\".*")
    new_rules <- list()
    elts <- strsplit(curr, " elts=")[[1]][-1]
    for (i in seq_along(elts)) {
      value <- paste0("c(", elts[i], ")")
      rule_name <- paste(att, "%in%", value)
      rule_index <- setNames(max(c(index, unlist(new_rules))) + 1, rule_name)

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

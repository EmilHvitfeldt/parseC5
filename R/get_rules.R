get_rule_index <- function(index, tree, history = c()) {
  res <- list()
  history <- c(history, index)
  curr <- tree[index]

  if (str_detect(curr, "cut=")) {
    rule1 <- get_rule_index(index + 1, tree, history)
    rule2 <- get_rule_index(max(unlist(rule1)) + 1, tree, history)
    res <- c(res, rule1, rule2)
  } else if (str_detect(curr, "elts=")) {
    new_rules <- list()
    for (i in seq_len(length(strsplit(curr, " elts=")[[1]]) - 1)) {
      new_rule <- get_rule_index(
        index = max(c(index, unlist(new_rules))) + 1,
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

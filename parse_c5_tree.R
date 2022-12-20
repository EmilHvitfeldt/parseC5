library(stringr)
library(purrr)
library(dplyr)
library(C50)

data(ad_data, package = "modeldata")

# ------------------------------------------------------------------------------
# helpers

get_val <- function(x, numeric = FALSE) {
  x <- str_split(x, "=")[[1]][2]
  x <- str_remove_all(x, "\"")
  if (numeric) {
    x <- as.numeric(x)
  }
  x
}

get_elem <- function(x, elem = "att", lvls = NULL, ...) {
  has_att <- str_detect(x, paste0("^", elem, "="))
  if (any(has_att)) {
    # 'elts' can have have 2+ splits and multiple entries
    if (elem == "elts") {
      x <- parse_elts(x[has_att])
    } else {
      x <- get_val(x[has_att], ...)
      if (elem == "freq") {
        x <- str_split(x, ",")[[1]]
        x <- as.numeric(x)
        names(x) <- lvls
      }
    }
  } else {
    x <- NA
  }

  x
}

parse_elts <- function(x) {
  map(x, get_val) %>%
    map(~ str_split(.x, ",")[[1]])
}

# ------------------------------------------------------------------------------

ad_mod <- C5.0(Class ~ tau + Genotype, data = ad_data)
cat(ad_mod$tree)
debugonce(summary)
summary(ad_mod)

# ------------------------------------------------------------------------------

tree_raw <-
  ad_mod$tree %>%
  str_split("\n") %>%
  pluck(1)

start_ind <- which(str_detect(tree_raw, "^entries"))

tree_data <-
  tibble(string = tree_raw[-(1:start_ind)]) %>%
  filter(string != "") %>%
  mutate(
    splits = map(string, ~ str_split(.x, " ")[[1]]),
    # AFAICT: type 0 is non-terminal node, 2 is binary numeric split, 3 is
    # a categorical split (2+ paths)
    type = map_dbl(splits, ~ get_val(.x[1], TRUE)),
    # Predicted class at this point in the tree
    class = map_chr(splits, ~ get_val(.x[2])),
    # The split variable
    predictor = map_chr(splits, ~ get_elem(.x, "att")),
    # I think the number of rows to recurse
    forks = map_dbl(splits, ~ get_elem(.x, "forks", numeric = TRUE)),
    # The numeric cut point
    cut = map_dbl(splits, ~ get_elem(.x, "cut", numeric = TRUE)),
    # The categorical groupings
    levels = map(splits, ~ get_elem(.x, "elts")),
    # The terminal node class frequencies. They are printed out differently in
    # the summary() output
    freq = map(splits, ~ get_elem(.x, "freq", lvls = ad_mod$levels)),
  ) %>%
  select(-string, -splits)

# We'd like to get a tibble back with columns for:
#   The tree number (>1 when boosting)
#   The terminal node number
#   The logical condition for each terminal node
#   The frequencies for the terminal node

# tau <= 5.740789: Control (164/14)
# tau  > 5.740789:
#   :...Genotype in {E2E2,E2E3,E2E4}: Control (21/7)
#       Genotype = E4E4: Impaired (9/3)
#       Genotype = E3E3:
#       :...tau <= 6.435637: Control (55/15)
#       :   tau > 6.435637: Impaired (18/6)
#       Genotype = E3E4:
#       :...tau <= 6.152733: Control (29/10)
#           tau > 6.152733: Impaired (37/10)

tribble(
  ~tree, ~node, ~rule, ~freq,
  1, 1, "tau <= 5.7407894",                                                  c(14, 150),
  1, 2, "tau  > 5.7407894 & Genotype %in% c('E2E2','E2E3','E2E4')",          c( 7,  14),
  1, 3, "tau  > 5.7407894 & Genotype %in% c('E4E4')",                        c( 6,   3),
  1, 4, "tau  > 5.7407894 & Genotype %in% c('E3E3') & tau <= 6.4356375",     c(15,  40),
  1, 5, "tau  > 5.7407894 & Genotype %in% c('E3E3') & tau > 6.4356375",      c(12,   6),
  1, 6, "tau  > 5.7407894 & Genotype %in% c('E3E4') & tau <= 6.1527328",     c(10,  19),
  1, 7, "tau  > 5.7407894 & Genotype %in% c('E3E4') & tau > 6.1527328",      c(27,  10),
) %>%
  mutate(rule = map(rule, rlang::parse_expr))


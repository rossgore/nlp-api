library(tidyverse)
library(gutenbergr)
library(tidytext)
library(quanteda)
library(stm)
library(furrr)
plan(multiprocess)
library(dplyr)
source("topic-model-api.R")

normit <- function(x){((x-min(x))/(max(x)-min(x)))+.000001}

majority.vote <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

file.list = list.files("global_research/") 
# try and create a doc term matrix from a list of pdf and html docs
my.list.of.docs = paste0("global_research/", file.list)

my.docs.df = from.a.list.of.files.to.file.text.df(my.list.of.docs)

tidy.docs.df <- my.docs.df %>%
  mutate(line = row_number()) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy.docs.dfm <- tidy.docs.df %>%
  count(document, word, sort = TRUE) %>%
  cast_dfm(document, word, n)

tidy.docs.sparse <- tidy.docs.df %>%
  count(document, word, sort = TRUE) %>%
  cast_sparse(document, word, n)

topic_model <- stm(tidy.docs.dfm, K = 6, 
                   verbose = FALSE, init.type = "Spectral")

many_models <- tibble(K = c(3, 4, 5, 6, 7, 8)) %>%
  mutate(topic_model = future_map(K, ~stm(tidy.docs.sparse, K = .,
                                          verbose = FALSE)))
heldout <- make.heldout(tidy.docs.sparse)

# if statement around this, if it errors out to few documents to return the min choice.
k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, tidy.docs.sparse),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, tidy.docs.sparse),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result_tidy <- k_result %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K)

k_result_tidy.hold.out = k_result_tidy %>% filter(Metric == "Held-out likelihood")
index.best.hold.out = which.max(k_result_tidy.hold.out$Value)
best.hold.out = k_result_tidy.hold.out$K[index.best.hold.out]

k_result_tidy.residual = k_result_tidy %>% filter(Metric == "Residuals")
index.best.residual= which.min(k_result_tidy.residual$Value)
best.residual = k_result_tidy.residual$K[index.best.residual]

k_exclusivity <- k_result %>%
  select(K, exclusivity, semantic_coherence) %>%
  filter(K %in% c(3, 4, 5, 6, 7, 8)) %>%
  unnest() %>%
  mutate(norm.exclusivity = normit(exclusivity)) %>% 
  mutate(norm.semantic_coherence = normit(semantic_coherence)) %>% 
  mutate(norm.score = sqrt(norm.exclusivity*norm.semantic_coherence))

# we want ks with high hold-out likelihood, low residuals, and high semantic coherence and high exclusivity
index.best.exclusivity.coherence.balance = which.max(k_exclusivity$norm.score)
best.exclusivity.coherence.balance = k_exclusivity$K[index.best.exclusivity.coherence.balance]

# take the most common choice, if there is a common choice take the min
list.of.choices = c(best.hold.out, best.residual, best.exclusivity.coherence.balance) %>% sort()
the.choice.for.k = majority.vote(list.of.choices)
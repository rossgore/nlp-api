library(tidyverse)
library(gutenbergr)
library(tidytext)
library(quanteda)
library(stm)
library(furrr)
plan(multiprocess)
library(dplyr)

normit <- function(x){((x-min(x))/(max(x)-min(x)))+.000001}

majority.vote <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

sherlock_raw <- gutenberg_download(1661)

sherlock <- sherlock_raw %>%
  mutate(story = ifelse(str_detect(text, "ADVENTURE"),
                        text,
                        NA)) %>%
  fill(story) %>%
  filter(story != "THE ADVENTURES OF SHERLOCK HOLMES") %>%
  mutate(story = factor(story, levels = unique(story)))

sherlock

tidy_sherlock <- sherlock %>%
  mutate(line = row_number()) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(word != "holmes")

tidy_sherlock %>% count(word, sort = TRUE)


sherlock_tf_idf <- tidy_sherlock %>%
  count(story, word, sort = TRUE) %>%
  bind_tf_idf(word, story, n) %>%
  arrange(-tf_idf) %>%
  group_by(story) %>%
  top_n(10) %>%
  ungroup

sherlock_tf_idf %>%
  mutate(word = reorder_within(word, tf_idf, story)) %>%
  ggplot(aes(word, tf_idf, fill = story)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ story, scales = "free", ncol = 3) +
  scale_x_reordered() +
  coord_flip() +
  theme(strip.text=element_text(size=11)) +
  labs(x = NULL, y = "tf-idf",
       title = "Highest tf-idf words in Sherlock Holmes short stories",
       subtitle = "Individual stories focus on different characters and narrative elements")

sherlock_dfm <- tidy_sherlock %>%
  count(story, word, sort = TRUE) %>%
  cast_dfm(story, word, n)

sherlock_sparse <- tidy_sherlock %>%
  count(story, word, sort = TRUE) %>%
  cast_sparse(story, word, n)

topic_model <- stm(sherlock_dfm, K = 6, 
                   verbose = FALSE, init.type = "Spectral")

many_models <- tibble(K = c(3, 4, 5, 6, 7, 8)) %>%
  mutate(topic_model = future_map(K, ~stm(sherlock_sparse, K = .,
                                          verbose = FALSE)))
heldout <- make.heldout(sherlock_sparse)

k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, sherlock_sparse),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, sherlock_sparse),
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
library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr)
library(topicmodels)
library(tidytext)
library(pdftools)
library(tm)
library(rvest)
library(SnowballC)
library(ggwordcloud)
library(quanteda)
library(stm)
library(purrr)

# helper function for find.best.k.for.docs
normit = function(x){((x-min(x))/(max(x)-min(x)))+.000001}

# helper function for find.best.k.for.docs
majority.vote = function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

from.pdf.to.tbl = function(pdf.file)
{
  success = FALSE
  try({
    pdf_txt = pdf_text(pdf.file)
    pdf_txt = pdf_txt %>% str_squish() %>% str_replace_all(",","")
    pdf_txt = unlist(strsplit(pdf_txt, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))
    txt_this_iter = pdf_txt
    success = TRUE
  })
  if(success == TRUE)
  {
    cur_link = rep(pdf.file, length(pdf_txt))
    df = tibble(cur_link, pdf_txt)
    colnames(df) <- c('document', 'text')
    return(df)
  } else
  {
    df = data.frame(file=character(), text=character(), stringsAsFactors=FALSE) 
    return(df)
  }
 
}


from.html.to.tbl = function(html.file)
{
  success = FALSE
  try({
    txt = read_html(html.file)
    p.txt = html_nodes(txt, "p")
    p.txt = html_text(p.txt) %>% str_squish() %>% str_replace_all(",","")
    p.txt = unlist(strsplit(p.txt, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))
    
    h1.txt = html_nodes(txt, "h1")
    h1.txt = html_text(h1.txt) %>% str_squish() %>% str_replace_all(",","")
    h1.txt = unlist(strsplit(h1.txt, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))
    
    h2.txt = html_nodes(txt, "h2")
    h2.txt = html_text(h2.txt) %>% str_squish() %>% str_replace_all(",","")
    h2.txt = unlist(strsplit(h2.txt, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))
    
    h3.txt = html_nodes(txt, "h3")
    h3.txt = html_text(h3.txt) %>% str_squish() %>% str_replace_all(",","")
    h3.txt = unlist(strsplit(h3.txt, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))
    
    h4.txt = html_nodes(txt, "h4")
    h4.txt = html_text(h4.txt) %>% str_squish() %>% str_replace_all(",","")
    h4.txt = unlist(strsplit(h4.txt, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))
    
    h5.txt = html_nodes(txt, "h5")
    h5.txt = html_text(h5.txt) %>% str_squish() %>% str_replace_all(",","")
    h5.txt = unlist(strsplit(h5.txt, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))
    
    h6.txt = html_nodes(txt, "h6")
    h6.txt = html_text(h6.txt) %>% str_squish() %>% str_replace_all(",","")
    h6.txt = unlist(strsplit(h6.txt, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))
    
    txt_this_iter = c(p.txt, h1.txt, h2.txt, h3.txt, h4.txt, h5.txt, h6.txt)
    success = TRUE
  })
  
  if(success == TRUE)
  {
    cur_link <- rep(html.file, length(txt_this_iter))
    df = tibble(cur_link, txt_this_iter)
    colnames(df) <- c('document', 'text')
    return(df)
  }
  else
  {
    df = data.frame(file=character(), text=character(), stringsAsFactors=FALSE) 
    return(df)
  }
}

from.a.list.of.files.to.file.text.df = function(list.of.files)
{
  df = data.frame(file=character(), text=character(), stringsAsFactors=FALSE) 
  for (i in 1:length(list.of.files))
  {
    the.file = list.of.files[i]
    if (grepl(".pdf", the.file, ignore.case = TRUE) & file.exists(the.file)) {
      df.to.add = from.pdf.to.tbl(the.file)
      df = df %>% rbind.data.frame(df.to.add)
    }
    else if (file.exists(the.file)) {
      df.to.add = from.html.to.tbl(the.file)
      df = df %>% rbind.data.frame(df.to.add)
    }
  }
  # add a column based on file name that indicates id
  # df = df %>% mutate(ID = group_indices(., file))
  return(df)
}

get.tidy.text.from.dtm = function(the.doc.term.matrix)
{
  tidy.text.to.return = tidy(the.doc.term.matrix)
  colnames(tidy.text.to.return) = c("document", "word", "n")
  tidy.text.to.return = tidy.text.to.return %>% mutate(document=as.factor(document))
  return(tidy.text.to.return)
}

from.file.text.df.to.tidytext = function(file.text.df, remove.numbers=TRUE, remove.stop.words=TRUE, stem.words=TRUE)
{
  file.text.df.tokens = unnest_tokens(file.text.df, output = word, input = text)
  if (remove.numbers)
  {
    # remove numbers
    file.text.df.tokens = filter(file.text.df.tokens, !str_detect(word, "^[0-9]*$"))
  }
  if (remove.stop.words)
  {
    # remove stop words
    file.text.df.tokens = anti_join(file.text.df.tokens, stop_words)
  }
  if (stem.words)
  {
    # stem the words
    file.text.df.tokens = mutate(file.text.df.tokens, word = wordStem(word))
  }
  
  
  file.text.df.tokens = file.text.df.tokens %>% count(document, word, sort = TRUE) %>% ungroup()
  return(file.text.df.tokens)
}

from.tidy.text.to.dtm = function(tidy.text.tbl)
{
  dtm.to.return = tidy.text.tbl %>% cast_dtm(document, word, n)
  return(dtm.to.return)
}

get.lda = function(doc.term.matrix, k.parm, control.parm=1234)
{
  lda.to.return = LDA(doc.term.matrix, k = k.parm, control = list(seed = control.parm))
  return(lda.to.return)
}

get.tidy.topics.from.lda = function(lda.model, per.document=FALSE)
{
  if (per.document == FALSE)
  {
    topics.to.return =  tidy(lda.model, matrix = "beta")
  }
  else
  {
    topics.to.return =  tidy(lda.model, matrix = "gamma")
  }
  return(topics.to.return)
}

get.top.terms.from.topics = function(tidy.topics, top.n)
{
  top.terms.to.return = tidy.topics %>%  group_by(topic) %>% top_n(top.n, beta) %>% ungroup() %>% arrange(topic, -beta)
  return(top.terms.to.return)
}

get.word.cloud.from.tidy.text = function(tidy.text, per.document = FALSE, min.word.freq = 3, max.word.count = 100)
{
  tidy.text = tidy.text %>% filter(n >= min.word.freq)
  tidy.text = tidy.text %>% arrange(desc(n))
  
  if (per.document == FALSE)
  {
    tidy.text = tidy.text %>% mutate(count = sequence(n()))
    tidy.text = tidy.text %>% filter(count < max.word.count)
    # my.world.cloud = tidy.text %>% count(word) %>% with(wordcloud(word, n, min.freq =min.word.freq, max.words = max.word.count))
    my.world.cloud = ggplot(tidy.text, aes(label = word, size = n)) + geom_text_wordcloud_area() + scale_size_area(max_size = 20) + theme_minimal()
    return(my.world.cloud)
  } else
  {
    tidy.text = tidy.text %>% group_by(document) %>% mutate(count = sequence(n()))
    tidy.text = tidy.text %>% filter(count < max.word.count)
    my.world.cloud = ggplot(tidy.text, aes(label = word, size = n, color=document)) + geom_text_wordcloud_area() + scale_size_area(max_size = 20) + theme_minimal() + facet_wrap(~document)
    return(my.world.cloud)
  }

}

get.plot.for.top.terms = function(top.terms)
{
  plot.to.return = top.terms %>% mutate(term = reorder_within(term, beta, topic)) %>% 
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    scale_x_reordered()
  
  return(plot.to.return)
}

get.plot.for.per.document = function(tidy.topics.per.doc)
{
  plot.to.return = tidy.topics.per.doc %>%
    mutate(document = reorder(document, gamma * topic)) %>%
    ggplot(aes(factor(topic), gamma)) +
    geom_boxplot() +
    facet_wrap(~ document)
  
  return(plot.to.return)
}

get.tidy.document.classification.from.lda = function(tidy.topics.per.doc)
{
  topic.classifications.to.return = tidy.topics.per.doc %>%
    group_by(document) %>%
    top_n(1, gamma) %>%
    ungroup()
  
  return(topic.classifications.to.return)
}

find.best.k.for.docs = function(docs.df, list.of.ks.to.try)
{
  tidy.docs.df <- docs.df %>%
    mutate(line = row_number()) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
  
  tidy.docs.sparse <- tidy.docs.df %>%
    count(document, word, sort = TRUE) %>%
    cast_sparse(document, word, n)
  
  many_models <- tibble(K = list.of.ks.to.try) %>%
    mutate(topic_model = map(K, ~stm(tidy.docs.sparse, K = .,
                                            verbose = FALSE)))
  heldout <- make.heldout(tidy.docs.sparse)
  
  # if statement around this, if it errors out to few documents to return the min choice.
  successfully.gathered.metrics = FALSE
  try({
    k_result <- many_models %>%
      mutate(exclusivity = map(topic_model, exclusivity),
             semantic_coherence = map(topic_model, semanticCoherence, tidy.docs.sparse),
             eval_heldout = map(topic_model, eval.heldout, heldout$missing),
             residual = map(topic_model, checkResiduals, tidy.docs.sparse),
             bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
             lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
             lbound = bound + lfact,
             iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))
    
    successfully.gathered.metrics = TRUE
    
  })
  
  if (successfully.gathered.metrics == FALSE)
  {
    return(list.of.ks.to.try %>% min())
  }
  
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
    filter(K %in% list.of.ks.to.try) %>%
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
  return(the.choice.for.k)
}


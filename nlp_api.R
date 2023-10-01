shhh = suppressMessages

shhh(library(tidyverse))
shhh(library(dplyr))
shhh(library(stringr))
shhh(library(tidyr))
shhh(library(topicmodels))
shhh(library(tidytext))
shhh(library(pdftools))
shhh(library(tm))
shhh(library(rvest))
shhh(library(SnowballC))
shhh(library(quanteda))
shhh(library(purrr))
shhh(library(ggwordcloud))
shhh(library(stm))
shhh(library(officer))
shhh(library(textreadr))
shhh(library(xml2))
shhh(library(SentimentAnalysis))
shhh(library(sentimentr))
shhh(library(textcat))
shhh(library(NLP))
shhh(library(openNLP))
shhh(library(openNLPdata))
shhh(library(wordnet))

panas_lexicon = read_csv("dictionaries/panas_lexicon.csv")

pos_tag_annotator_model_path = "open_nlp_models/en-pos-maxent.bin"
wordnet::setDict("/usr/local/Cellar/wordnet/3.1_1/dict/")
mTurkWordBank = read_csv("dictionaries/wordbank_without_functions.csv")

# helper function for find.best.k.for.docs
normit = function(x){((x-min(x))/(max(x)-min(x)))+.000001}


valence_lexicon <- as.data.frame(lexicon::hash_valence_shifters)
valence_lexicon <- valence_lexicon %>% mutate(y = as.numeric(y))

polarity_lexicon = read_csv(file="dictionaries/custom.csv")
polarity_lexicon = polarity_lexicon %>% select(-lexicon)
colnames(polarity_lexicon) = c("x", "y")
valence_lexicon = valence_lexicon %>% anti_join(polarity_lexicon, by="x")


count_occurences_of_words = function(content_words, target_words)
{
  content_word_count = 0
  for (i in 1:length(content_words))
  {
    cur_word = content_words[i]
    for (j in 1:length(target_words))
    {
      cur_target_word = target_words[j]
      if (cur_word == cur_target_word)
      {
        content_word_count = content_word_count + 1
      }
    }
  }
  return(content_word_count)
}

score_sentiment_mturk <- function(content, min=10, max=0){
  content = tolower(content)
  content = gsub("[^[:alnum:] ]", "", content)
  scores = c()
  string_split <- strsplit(content, " ")[[1]]
  for (word in string_split)
  {
    word = as.character(word)
    if (length(which(mTurkWordBank$word==word)) > 0)
    {	
      index = which(mTurkWordBank$word==word)
      if ( (mTurkWordBank[index,]$happiness_average > max) |  (mTurkWordBank[index,]$happiness_average < min))
      {
        scores = c(scores, (mTurkWordBank[index,]$happiness_average))
      }
      else
      {
        scores = c(scores, NA)
      }
    }
    else
    {
      scores = c(scores, NA)
    }
  }
  return (mean(scores, na.rm=T))
}

score_polarity_mturk <- function(content, min=10, max=0){
  content = tolower(content)
  content = gsub("[^[:alnum:] ]", "", content)
  scores = c()
  string_split <- strsplit(content, " ")[[1]]
  for (word in string_split)
  {
    word = as.character(word)
    if (length(which(mTurkWordBank$word==word)) > 0)
    {	
      index = which(mTurkWordBank$word==word)
      if ( (mTurkWordBank[index,]$happiness_average > max) |  (mTurkWordBank[index,]$happiness_average < min))
      {
        scores = c(scores, (mTurkWordBank[index,]$happiness_average))
      }
      else
      {
        scores = c(scores, NA)
      }
    }
    else
    {
      scores = c(scores, NA)
    }
  }
  scores = abs(scores - 5)
  return (mean(scores, na.rm=T))
}

check_if_all_is_na = function(the_vector)
{
  for (i in length(the_vector))
  {
    cur_element = the_vector[i]
    if (cur_element %>% is.na() == FALSE)
    {
      return (FALSE)
    }
  }
  return (TRUE)
  
}

score_sentiment_sentimentr = function(content)
{
  content = tolower(content)
  sentiment_df = content %>% sentimentr::sentiment()
  sentiment_df = sentiment_df %>% mutate(weighted_sentiment = word_count * sentiment)
  overall_sentiment_df = sentiment_df %>% group_by(element_id) %>% summarise(weighted_avg_sentiment = sum(weighted_sentiment, na.rm=T)/sum(word_count))
  return(overall_sentiment_df$weighted_avg_sentiment[1])
}

score_domain_specific_sentiment = function(content, score_type="valence")
{
  content = tolower(content)
  
  if (score_type == "valence")
  {
    sentiment_df = content %>% sentimentr::sentiment_by(polarity_dt = polarity_lexicon %>% as_key(),
                                                        valence_shifters_dt = valence_lexicon %>% as_key(comparison = NULL))
    sentiment_score_for_content = sentiment_df$ave_sentiment[1]
    return(sentiment_score_for_content)
  }
  if (score_type == "no valence")
  {
    content_as_vector = strsplit(content, "\\s+")[[1]] %>% str_remove_all("[[:punct:]]")
    words_in_content = content_as_vector %>% length()
    positive_words = polarity_lexicon %>% filter(y == 1)
    positive_words = positive_words$x
    positive_word_count = positive_words %>% intersect(content_as_vector) %>% length()
    negative_words = polarity_lexicon %>% filter(y == -1)
    negative_words = negative_words$x
    negative_word_count = negative_words %>% intersect(content_as_vector) %>% length()
    sentiment_score_for_content = (positive_word_count - negative_word_count) / words_in_content
    return(sentiment_score_for_content)
  }
  if (score_type == "both")
  {
    
    sentiment_df = content %>% sentimentr::sentiment_by(polarity_dt = polarity_lexicon %>% as_key(),
                                                        valence_shifters_dt = valence_lexicon %>% as_key())
    sentiment_score_for_content = sentiment_df$ave_sentiment[1]
    if (sentiment_score_for_content == 0)
    {
      
      content_as_vector = strsplit(content, "\\s+")[[1]] %>% str_remove_all("[[:punct:]]")
      words_in_content = content_as_vector %>% length()
      positive_words = polarity_lexicon %>% filter(y == 1)
      positive_words = positive_words$x
      positive_word_count = count_occurences_of_words(content_as_vector, positive_words)
      negative_words = polarity_lexicon %>% filter(y == -1)
      negative_words = negative_words$x
      negative_word_count = count_occurences_of_words(content_as_vector, negative_words)
      sentiment_score_for_content = (positive_word_count - negative_word_count) / words_in_content
      
    }
    return(sentiment_score_for_content)
  }
}

score_sentiment_nrc = function(content)
{
  
  content = tolower(content)
  content = gsub("[^[:alnum:] ]", "", content)
  
  content_tibble <- strsplit(content, " ")[[1]] %>% tibble()
  colnames(content_tibble) = c("word")
  
  sentiment_tibble = get_sentiments("nrc")
  
  results_tibble = inner_join(content_tibble, sentiment_tibble, by=c("word"))
  
  #positive - negative word count divide by overall word count
  
  positive_word_count = results_tibble %>% filter(sentiment == "positive") %>% nrow()
  negative_word_count = results_tibble %>% filter(sentiment == "negative") %>% nrow()
  total_word_count = positive_word_count + negative_word_count
  
  if (total_word_count == 0)
  {
    return (NA)
  }
  else
  {
    total_sentiment = positive_word_count - negative_word_count
    avg_sentiment = total_sentiment / total_word_count
    return (avg_sentiment)
  }
  
}

score_sentiment_bing = function(content)
{
  
  content = tolower(content)
  content = gsub("[^[:alnum:] ]", "", content)
  
  content_tibble <- strsplit(content, " ")[[1]] %>% tibble()
  colnames(content_tibble) = c("word")
  
  sentiment_tibble = get_sentiments("bing")
  
  results_tibble = inner_join(content_tibble, sentiment_tibble, by=c("word"))
  
  #positive - negative word count divide by overall word count
  
  positive_word_count = results_tibble %>% filter(sentiment == "positive") %>% nrow()
  negative_word_count = results_tibble %>% filter(sentiment == "negative") %>% nrow()
  total_word_count = positive_word_count + negative_word_count
  
  if (total_word_count == 0)
  {
    return (NA)
  }
  else
  {
    total_sentiment = positive_word_count - negative_word_count
    avg_sentiment = total_sentiment / total_word_count
    return (avg_sentiment)
  }
  
}

score_sentiment_afinn = function(content)
{
  
  content = tolower(content)
  content = gsub("[^[:alnum:] ]", "", content)
  
  content_tibble <- strsplit(content, " ")[[1]] %>% tibble()
  colnames(content_tibble) = c("word")
  
  sentiment_tibble = get_sentiments("afinn")
  
  results_tibble = inner_join(content_tibble, sentiment_tibble, by=c("word"))
  avg_sentiment = mean(results_tibble$value, na.rm = TRUE)
  if (avg_sentiment %>% is.nan())
  {
    return (NA)
  }
  else
  {
    return (avg_sentiment)
  }
  
}

topic.model.from.pdf.to.tbl = function(pdf.file)
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

topic.model.from.pptx.to.tbl = function(pptx.file)
{
  success = FALSE
  try({
    pptx_obj = read_pptx(pptx.file)
    pptx_tbl = pptx_summary(pptx_obj)
    pptx_tbl = pptx_tbl %>% filter(nchar(text) > 3)
    pptx_txt = pptx_tbl$text
    txt_this_iter = pptx_txt
    success = TRUE
  })
  if(success == TRUE)
  {
    cur_link = rep(pptx.file, length(pptx_txt))
    df = tibble(cur_link, pptx_txt)
    colnames(df) <- c('document', 'text')
    return(df)
  } else
  {
    df = data.frame(file=character(), text=character(), stringsAsFactors=FALSE) 
    return(df)
  }
  
}

topic.model.from.document.to.tbl = function(txt.file)
{
  success = FALSE
  try({
    the_txt = read_document(txt.file)
    the_txt = the_txt %>% str_squish() %>% str_replace_all(",","")
    the_txt = unlist(strsplit(the_txt, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))
    txt_this_iter = the_txt
    success = TRUE
  })
  if(success == TRUE)
  {
    cur_link = rep(txt.file, length(the_txt))
    df = tibble(cur_link, the_txt)
    colnames(df) <- c('document', 'text')
    return(df)
  } else
  {
    df = data.frame(file=character(), text=character(), stringsAsFactors=FALSE) 
    return(df)
  }
  
}

topic.model.from.html.to.tbl = function(html.file)
{
  success = FALSE
  try({
    txt = xml2::read_html(html.file)
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
      df.to.add = topic.model.from.pdf.to.tbl(the.file)
      df = df %>% rbind.data.frame(df.to.add)
    }
    else if (is.valid.textreadr.file.format(the.file) & file.exists(the.file)) {
      df.to.add = topic.model.from.document.to.tbl(the.file)
      df = df %>% rbind.data.frame(df.to.add)
    }
    else if (grepl(".pptx", the.file, ignore.case = TRUE) & file.exists(the.file)) {
      df.to.add = topic.model.from.pptx.to.tbl(the.file)
      df = df %>% rbind.data.frame(df.to.add)
    }
    else if (file.exists(the.file)) { # default right now largely for html
      df.to.add = topic.model.from.html.to.tbl(the.file)
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

get.top.terms.from.topics = function(tidy.topics, top.n)
{
  top.terms.to.return = tidy.topics %>%  group_by(topic) %>% top_n(top.n, beta) %>% ungroup() %>% arrange(topic, -beta)
  return(top.terms.to.return)
}

get.top.tfidf.terms.from.document = function(tidy.text.words.tfidf, top.n)
{
  top.tidy.text.words.tfidf = tidy.text.words.tfidf %>% top_n(top.n, tf_idf) %>% arrange(-tf_idf)
  # this would force only top.n numbers even in case of ties, for now we allow ties
  # top.tidy.text.words.tfidf = top.tidy.text.words.tfidf[1:top.n,]
  return(top.tidy.text.words.tfidf)
}

get.top.terms.from.document = function(tidy.text.df, top.n)
{
  top.terms.to.return = tidy.text.df %>% top_n(top.n, n) %>% arrange(-n)
  return(top.terms.to.return)
}

is.valid.file.format = function(file.name)
{
  first.part = is.valid.textreadr.file.format(file.name)
  pptx.part = grepl(".pptx", file.name, ignore.case = TRUE)
  return (first.part | pptx.part)
}

is.valid.textreadr.file.format = function(file.name)
{
  if (grepl(".docx", file.name, ignore.case = TRUE) |
      grepl(".doc", file.name, ignore.case = TRUE)  |
      grepl(".rtf", file.name, ignore.case = TRUE)  |
      grepl(".txt", file.name, ignore.case = TRUE)  |
      grepl(".pdf", file.name, ignore.case = TRUE)  )
  {
    return (TRUE)
  } else {
    return (FALSE)
  }
}

get_all_panas_scores = function(tweet_df)
{
  # panas affect
  tweet_df$binary_positive = unlist(lapply(tweet_df$text, score_PANAS_binary, indicator="positive"))
  tweet_df$binary_negative = unlist(lapply(tweet_df$text, score_PANAS_binary, indicator="negative"))
  
  # panas category
  tweet_df$binary_interested_attentive_alert = unlist(lapply(tweet_df$text, score_PANAS_binary, indicator="interested_attentive_alert"))
  tweet_df$binary_excited_enthusiastic_inspired = unlist(lapply(tweet_df$text, score_PANAS_binary, indicator="excited_enthusiastic_inspired"))
  tweet_df$binary_proud_determined = unlist(lapply(tweet_df$text, score_PANAS_binary, indicator="proud_determined"))
  tweet_df$binary_strong_active = unlist(lapply(tweet_df$text, score_PANAS_binary, indicator="strong_active"))
  
  
  tweet_df$binary_distressed_upset = unlist(lapply(tweet_df$text, score_PANAS_binary, indicator="distressed_upset"))
  tweet_df$binary_guilty_ashamed = unlist(lapply(tweet_df$text, score_PANAS_binary, indicator="guilty_ashamed"))
  tweet_df$binary_hostile_irritable = unlist(lapply(tweet_df$text, score_PANAS_binary, indicator="hostile_irritable"))
  tweet_df$binary_nervous_jittery = unlist(lapply(tweet_df$text, score_PANAS_binary, indicator="nervous_jittery"))
  tweet_df$binary_afraid_scared = unlist(lapply(tweet_df$text, score_PANAS_binary, indicator="afraid_scared"))
  
  # panas affect
  tweet_df$cont_positive = unlist(lapply(tweet_df$text, score_PANAS, indicator="positive"))
  tweet_df$cont_negative = unlist(lapply(tweet_df$text, score_PANAS, indicator="negative"))
  
  # panas category
  tweet_df$cont_interested_attentive_alert = unlist(lapply(tweet_df$text, score_PANAS, indicator="interested_attentive_alert"))
  tweet_df$cont_excited_enthusiastic_inspired = unlist(lapply(tweet_df$text, score_PANAS, indicator="excited_enthusiastic_inspired"))
  tweet_df$cont_proud_determined = unlist(lapply(tweet_df$text, score_PANAS, indicator="proud_determined"))
  tweet_df$cont_strong_active = unlist(lapply(tweet_df$text, score_PANAS, indicator="strong_active"))
  
  
  tweet_df$cont_distressed_upset = unlist(lapply(tweet_df$text, score_PANAS, indicator="distressed_upset"))
  tweet_df$cont_guilty_ashamed = unlist(lapply(tweet_df$text, score_PANAS, indicator="guilty_ashamed"))
  tweet_df$cont_hostile_irritable = unlist(lapply(tweet_df$text, score_PANAS, indicator="hostile_irritable"))
  tweet_df$cont_nervous_jittery = unlist(lapply(tweet_df$text, score_PANAS, indicator="nervous_jittery"))
  tweet_df$cont_afraid_scared = unlist(lapply(tweet_df$text, score_PANAS, indicator="afraid_scared"))
  
  return(tweet_df)
  
}

score_PANAS_binary = function(text, indicator)
{
  cont_score = score_PANAS(text, indicator)
  if (cont_score == 0)
  {
    return(0)
  } else {
    return(1)
  }
}

get_PANAS_lexicon_for_category = function(panas_category)
{
  to_return = panas_lexicon %>% filter(category == panas_category)
  return (to_return$word)
}

get_PANAS_lexicon_for_affect = function(panas_affect)
{
  to_return = panas_lexicon %>% filter(affect == panas_affect)
  return (to_return$word)
}

score_PANAS = function(text, indicator)
{
  lexicon = NULL
  if (indicator == "positive" | indicator == "negative")
  {
    lexicon = get_PANAS_lexicon_for_affect(indicator)
  } else
  {
    lexicon = get_PANAS_lexicon_for_category(indicator)
  }
  if (lexicon %>% is.null())
  {
    stop("Can't find a lexicon for indicator.")
  }
  text_length = 0
  try({
    text_as_vector =strsplit(text, " ")[[1]]
    text_length = text_as_vector %>% length()
  })
  if (text_length == 0)
  {
    stop("Can't score empty text.")
  }
  score = NA
  try({
    hits_in_lexicon = text_as_vector %>% count_occurences_of_words(lexicon)
    score = hits_in_lexicon / text_length
  })
  if (is.na(score))
  {
    score = 0
    return(0)
  }
  if (hits_in_lexicon == 0)
  {
    score = 0
    return(score)
  } else
  {
    score = hits_in_lexicon / text_length
    return(score)
  }
}


tag_pos = function(single_string, ret_type = "VECTOR")
{
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  # define word annotator
  word_token_annotator <- Maxent_Word_Token_Annotator()
  # define pos annotator
  pos_tag_annotator <- Maxent_POS_Tag_Annotator(language = "en", probs = FALSE,                                           
                                                model = pos_tag_annotator_model_path)
  
  single_string <- as.String(single_string)
  y1 <- NLP::annotate(single_string, list(sent_token_annotator, word_token_annotator))
  y2<- NLP::annotate(single_string, pos_tag_annotator, y1)
  y2w <- subset(y2, type == "word")
  tags <- sapply(y2w$features, '[[', "POS")
  if(ret_type == "VECTOR")
  {
    return(tags)
  }
  
  if(ret_type == "CSV")
  {
    tags <- paste(tags, collapse=", ")
    return(tags)
  }
}

get_synonyms = function(word, syn_pos = "NOUN", return_type = "VECTOR")
{
  to_return = NULL
  #qdap_to_return = qdap::synonyms(word) %>% unlist()
  qdap_to_return = c()
  wordnet::setDict("/usr/local/Cellar/wordnet/3.1_1/dict/")
  wordnet_to_return = wordnet::synonyms(word, syn_pos)
  if(return_type == "VECTOR")
  {
    to_return = c(wordnet_to_return, qdap_to_return) %>% unique()
    if (to_return %>% is.null())
    {
      to_return = c()
    }
    return(to_return)
  }
  if (return_type == "CSV")
  {
    to_return = c(wordnet_to_return, qdap_to_return) %>% unique()
    if (to_return %>% is.null())
    {
      to_return = ""
    }
    to_return <- paste(to_return, collapse=", ")
    return(to_return)
  }
  
}






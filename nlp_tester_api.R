source("nlp_api.R")

document_directory = "documents/"

file_list = list.files(document_directory) 
# try and create a doc term matrix from a list of pdf and html docs
all_docs_in_a_directory = paste0(document_directory, file_list)

all_docs_in_a_directory_df = from.a.list.of.files.to.file.text.df(all_docs_in_a_directory)

# add line id based in dataframe
all_docs_in_a_directory_df = all_docs_in_a_directory_df %>% group_by(document) %>% mutate(line_id = row_number()) %>% ungroup()

# filter out stop words
stop_words_file_name = "stop_words/lexisnexis.txt"
stop_words = readLines(stop_words_file_name)
stop_words_pattern = stop_words %>% paste0(collapse = "| ")

all_docs_in_a_directory_df = all_docs_in_a_directory_df %>% mutate(stop_word_filtered_text= str_remove_all(text, stop_words_pattern))

# topic modeling

# unnest each word in the stop word filtered text column
tidy_docs_df <- all_docs_in_a_directory_df %>% unnest_tokens(word, stop_word_filtered_text) 

# using the unnested words, count their occurences and move the data structure into a document term matrix
tidy_docs_dfm <- tidy_docs_df %>%
  count(document, word, sort = TRUE) %>%
  cast_dfm(document, word, n)

# create the topic model with 6 topics.
topic_model <- stm(tidy_docs_dfm, K = 6,verbose = FALSE, init.type = "Spectral")

# score sentiment
#panas sentiment scoring (assumes a column named 'text' in the df), 1:100 just so it runs faster
panas_all_docs_in_a_directory_df = all_docs_in_a_directory_df %>% get_all_panas_scores()

# other sentiment scoring
all_docs_in_a_directory_df$line_mturk_sentiment = unlist(lapply(all_docs_in_a_directory_df$stop_word_filtered_text, score_sentiment_mturk)) # 0 to 10 bounds
all_docs_in_a_directory_df_document_level_sentiment = all_docs_in_a_directory_df %>% 
  group_by(document) %>% summarise(document_line_mturk_sentiment = mean(line_mturk_sentiment, na.rm = TRUE))

# other ways to score sentiment (even more in api just look around)
all_docs_in_a_directory_df$sentimentr_sentiment = unlist(lapply(all_docs_in_a_directory_df$text, score_sentiment_sentimentr)) # -1 to 1
all_docs_in_a_directory_df$custom_sentiment = unlist(lapply(all_docs_in_a_directory_df$text, score_domain_specific_sentiment)) # -1 to 1

# move document sentiment back into the all_docs_in_a_directory_df 
all_docs_in_a_directory_df = all_docs_in_a_directory_df %>% inner_join(all_docs_in_a_directory_df_document_level_sentiment, by="document")


# part of speech tagging and synomym lookup
parts_of_speech = tag_pos("turkey burgers are good")
syn <- get_synonyms("boy")
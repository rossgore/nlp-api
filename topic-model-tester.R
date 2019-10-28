source("topic-model-api.R")

# this tests everything once we have a doc-term-matrix
data("AssociatedPress")
my.desired.number.of.topics = 2
my.desired.number.of.top.words.per.topic = 10

ap.tidy.text = get.tidy.text.from.dtm(AssociatedPress)
# we can make a word cloud of all the text
ap.word.cloud = get.word.cloud.from.tidy.text(ap.tidy.text)
ap.word.cloud


# there are lots of documents in so to get a faceted by doc word cloud we do the first 5
list.of.documents.we.want = c("1", "2", "3", "4", "5")
filtered.ap.tidy.text = ap.tidy.text %>% filter(document %in% list.of.documents.we.want)
filtered.ap.word.cloud.per.document = get.word.cloud.from.tidy.text(filtered.ap.tidy.text, per.document = TRUE)
filtered.ap.word.cloud.per.document

ap.lda = get.lda(AssociatedPress, my.desired.number.of.topics)
ap.topics = get.tidy.topics.from.lda(ap.lda)
ap.top.terms = get.top.terms.from.topics(ap.topics, my.desired.number.of.top.words.per.topic)
ap.plot.of.top.terms = get.plot.for.top.terms(ap.top.terms)
ap.plot.of.top.terms

# ap.topics.per.doc
ap.topics.per.doc = get.tidy.topics.from.lda(ap.lda, per.document = TRUE)

# we can also plot this but there are way to many docs here, so we limit to the first eight
filtered.ap.topics.per.doc = ap.topics.per.doc %>% filter(document %in% list.of.documents.we.want)
filtered.ap.plot.of.topic.per.doc = get.plot.for.per.document(filtered.ap.topics.per.doc)
filtered.ap.plot.of.topic.per.doc

# or just use the top document classification and make it a tbl
ap.tidy.docs.classified.into.topics = get.tidy.document.classification.from.lda(ap.topics.per.doc)
ap.tidy.docs.classified.into.topics

# try and create a doc term matrix from a list of pdf and html docs
my.list.of.docs = c("global_research/156390893402424.pdf", 
                    "global_research/156398257833616.pdf",
                    "global_research/156501860173228.html")

my.docs.df = from.a.list.of.files.to.file.text.df(my.list.of.docs)
my.docs.as.tidy.txt = from.file.text.df.to.tidytext(my.docs.df)

# we can make a word cloud of all the text
my.word.cloud = get.word.cloud.from.tidy.text(my.docs.as.tidy.txt)
my.word.cloud

# and we can make a word cloud per document
my.word.cloud.per.doc = get.word.cloud.from.tidy.text(my.docs.as.tidy.txt, per.document = TRUE)
my.word.cloud.per.doc

# this gets our very own doc term matrix
my.dtm = from.tidy.text.to.dtm(my.docs.as.tidy.txt)

# now we can topic model with it
my.lda = get.lda(my.dtm, my.desired.number.of.topics)
my.topics = get.tidy.topics.from.lda(my.lda)
my.top.terms = get.top.terms.from.topics(my.topics, my.desired.number.of.top.words.per.topic)

# my.top.terms.plot
my.plot.of.top.terms = get.plot.for.top.terms(my.top.terms)
my.plot.of.top.terms

# my.topics.per.doc
my.topics.per.doc = get.tidy.topics.from.lda(my.lda, per.document = TRUE)

# we can also plot this
my.plot.of.topic.per.doc = get.plot.for.per.document(my.topics.per.doc)
my.plot.of.topic.per.doc

# or just use the top document classification and make it a tbl
my.tidy.docs.classified.into.topics = get.tidy.document.classification.from.lda(my.topics.per.doc)
my.tidy.docs.classified.into.topics

# now take everything in global research and determine the best k for a topic model.
file.list = list.files("global_research/") 
# try and create a doc term matrix from a list of pdf and html docs
my.list.of.docs = paste0("global_research/", file.list)
my.docs.df = from.a.list.of.files.to.file.text.df(my.list.of.docs)
list.of.possible.ks = c(3, 4, 5, 6, 7)
best.k = find.best.k.for.docs(my.docs.df, list.of.possible.ks)
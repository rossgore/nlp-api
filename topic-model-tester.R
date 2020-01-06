# HOLD OVERS FROM LAST TIME
# T/F is still used these should be converted to TRUE or FALSE
#
# even those that use output.pdf arguement should return a JSON output with the file path listed
# for thse please check that JSON is returned; then unpack the JSON and check that the file.exists that is specified in the unpacked JSON
#
# for either: (1) word cloud or (2) lda/dtm methods that are unsupported with JSON remove from tester

source("topic-model-api.R")

# this tests everything once we have a doc-term-matrix
data("AssociatedPress")
my.desired.number.of.topics = 2
my.desired.number.of.top.words.per.topic = 10

ap.tidy.text = get.tidy.text.from.dtm(AssociatedPress)
ap.tidy.text
ap.tidy.text.json = get.tidy.text.from.dtm(AssociatedPress, io.json = TRUE)
# we can make a word cloud of all the text
validate(ap.tidy.text.json) # use validate to test if it actually is json

# remove this from this tester file
ap.word.cloud = get.word.cloud.from.tidy.text(ap.tidy.text)
ap.word.cloud.pdf = get.word.cloud.from.tidy.text(ap.tidy.text, output.pdf=TRUE, io.json = TRUE)
# return as json object, then unpack json, then test with file.exists(result of unpacked json)

# there are lots of documents in so to get a faceted by doc word cloud we do the first 5

list.of.documents.we.want = c("1", "2", "3", "4", "5")

filtered.ap.tidy.text = ap.tidy.text %>% filter(document %in% list.of.documents.we.want)

# remove this from this tester file
filtered.ap.word.cloud.per.document = get.word.cloud.from.tidy.text(filtered.ap.tidy.text, per.document = TRUE, output.pdf = TRUE) 

ap.lda = get.lda(AssociatedPress, my.desired.number.of.topics) 

# so ldas are weird so don't JSON wrapper, remove this from this tester file



# T to TRUE

ap.topics = get.tidy.topics.from.lda(ap.lda, io.json.topics=TRUE)
ap.topics
validate(ap.topics) 


ap.top.terms = get.top.terms.from.topics(ap.topics,  toJSON(my.desired.number.of.top.words.per.topic), io.json = TRUE)
ap.top.terms
validate(ap.top.terms)




ap.plot.of.top.terms.pdf = get.plot.for.top.terms(ap.top.terms, output.pdf = TRUE, io.json = TRUE)
validate(ap.plot.of.top.terms.pdf)



ap.topics.per.doc.json = get.tidy.topics.from.lda(ap.lda, per.document = TRUE, io.json = TRUE) # let's label this last arguement to be specific; let's be consistent
validate(ap.topics.per.doc.json)

# we can also plot this but there are way to many docs here, so we limit to the first eight
filtered.ap.topics.per.doc = ap.topics.per.doc %>% filter(document %in% list.of.documents.we.want)
filtered.ap.topics.per.doc = toJSON(filtered.ap.topics.per.doc)
filtered.ap.plot.of.topic.per.doc.pdf = get.plot.for.per.document(filtered.ap.topics.per.doc, output.pdf = TRUE, io.json = TRUE) # successfully puts it in my directory
validate(filtered.ap.plot.of.topic.per.doc.pdf)

# this needs to return the full path to the file; not just the directory
# this needs to be json (filtered.ap.plot.of.topic.per.doc.pdf) and it needs to be validated and then unpacked to check that file.exists; see example above

# or just use the top document classification and make it a tbl
ap.tidy.docs.classified.into.topics = get.tidy.document.classification.from.lda(ap.topics.per.doc)
ap.tidy.docs.classified.into.topics

#should i remove ?
ap.tidy.docs.classified.into.topics.json = get.tidy.document.classification.from.lda(ap.topics.per.doc, io.json = TRUE)
validate(ap.tidy.docs.classified.into.topics.json)


# try and create a doc term matrix from a list of pdf and html docs
my.list.of.docs = c("global_research/156390893402424.pdf", 
                    "global_research/156398257833616.pdf",
                    "global_research/156501860173228.html")

my.list.of.docs.json <- toJSON(my.list.of.docs)
list.of.possible.ks = c(3, 4, 5, 6, 7) # this is used for best.k
list.of.possible.ks.json <- toJSON(list.of.possible.ks)

my.docs.df = from.a.list.of.files.to.file.text.df(my.list.of.docs.json, io.json = TRUE)
validate(my.docs.df.json)



best.k = find.best.k.for.docs(my.docs.df, list.of.possible.ks.json, io.json = TRUE)
validate(best.k)


my.docs.as.tidy.txt = from.file.text.df.to.tidytext(my.docs.df, io.json = TRUE) 
validate(my.docs.as.tidy.txt)

# remove this from this tester file
my.word.cloud = get.word.cloud.from.tidy.text(my.docs.as.tidy.txt, output.pdf = TRUE) 
my.word.cloud

# remove this from this tester file
my.word.cloud.per.doc = get.word.cloud.from.tidy.text(my.docs.as.tidy.txt, per.document = TRUE, output.pdf = TRUE, io.json = TRUE)
my.word.cloud.per.doc
validate(my.word.cloud.per.doc)

# this gets our very own doc term matrix
my.dtm = from.tidy.text.to.dtm(fromJSON(my.docs.as.tidy.txt))

# now we can topic model with it
my.lda = get.lda(my.dtm, fromJSON(best.k))




my.topics = get.tidy.topics.from.lda(my.lda,  io.json.topics= TRUE) # multiple arguements besure to specify all
validate(my.topics)

my.top.terms = get.top.terms.from.topics(my.topics, toJSON(my.desired.number.of.top.words.per.topic), io.json = TRUE)
validate(my.top.terms)


my.plot.of.top.terms.pdf = get.plot.for.top.terms(my.top.terms, output.pdf =  TRUE, io.json = TRUE)
validate(my.plot.of.top.terms.pdf)
# this needs to return the full path to the file; not just the directory
# this needs to be json (my.plot.of.top.terms.pdf) and it needs to be validated and then unpacked to be sure file.exits

# my.topics.per.doc

my.topics.per.doc = get.tidy.topics.from.lda(my.lda, per.document = TRUE, io.json = TRUE)
validate(my.topics.per.doc)

# we can also plot this

my.plot.of.topic.per.doc.pdf = get.plot.for.per.document(my.topics.per.doc, output.pdf = TRUE, io.json = TRUE)
my.plot.of.topic.per.doc.pdf
validate(my.plot.of.topic.per.doc.pdf)
# this needs to return the full path to the file; not just the directory
# this needs to be json (my.plot.of.topic.per.doc.pdf) and it needs to be validated and then unpacked to be sure file.exits


# or just use the top document classification and make it a tbl

my.tidy.docs.classified.into.topics = get.tidy.document.classification.from.lda(my.topics.per.doc, io.json = TRUE)
validate(my.tidy.docs.classified.into.topics)

# now take everything in global research and determine the best k for a topic model.
file.list = list.files("global_research/") 

# try and create a doc term matrix from a list of pdf and html docs
all.docs.in.a.directory = paste0("global_research/", file.list)


all.docs.in.a.directory.df = from.a.list.of.files.to.file.text.df(toJSON(all.docs.in.a.directory), io.json = TRUE)
validate(all.docs.in.a.directory.df)

# please do this both ways - first just for best.k; then next for best.k.json; i've already done that below

best.k = find.best.k.for.docs(all.docs.in.a.directory.df, list.of.possible.ks.json, io.json = TRUE)
validate(best.k)

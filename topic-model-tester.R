# for either: (1) word cloud or (2) lda/dtm methods that are unsupported with JSON remove from tester

source("topic-model-api.R")

# this tests everything once we have a doc-term-matrix
data("AssociatedPress")
my.desired.number.of.topics = 2
my.desired.number.of.top.words.per.topic = 10

ap.tidy.text = get.tidy.text.from.dtm(AssociatedPress)
ap.tidy.text.json = get.tidy.text.from.dtm(AssociatedPress, output.json = TRUE)
# we can make a word cloud of all the text
validate(ap.tidy.text.json) # use validate to test if it actually is json

ap.word.cloud = get.word.cloud.from.tidy.text(ap.tidy.text)
ap.word.cloud = get.word.cloud.from.tidy.text(ap.tidy.text, T) # this still returns a ggplot object; it should return the name
# then could test with file.exists(ap.word.cloud)
# ap.word.cloud don't run takes a while
#ggsave("apwordcloud.pdf")

# there are lots of documents in so to get a faceted by doc word cloud we do the first 5

list.of.documents.we.want = c("1", "2", "3", "4", "5")

filtered.ap.tidy.text = ap.tidy.text %>% filter(document %in% list.of.documents.we.want)
filtered.ap.word.cloud.per.document = get.word.cloud.from.tidy.text(filtered.ap.tidy.text, per.document = TRUE, output.pdf = TRUE) 

# this still returns a ggplot object; it should return the name


# then could test with file.exists(filtered.ap.word.cloud.per.document)
# filtered.ap.word.cloud.per.document don't run takes a while

ap.lda = get.lda(AssociatedPress, my.desired.number.of.topics) 
ap.lda.json = get.lda(AssociatedPress, my.desired.number.of.topics, T) # so ldas are weird so don't JSON wrapper, remove this from tester

ap.topics = get.tidy.topics.from.lda(ap.lda)
ap.topics.json = get.tidy.topics.from.lda(ap.lda, output.json.topics=T) # we need to make sure we provide a binding for everything to be safe.
validate(ap.topics.json)

ap.top.terms = get.top.terms.from.topics(ap.topics, my.desired.number.of.top.words.per.topic)
ap.top.terms.json = get.top.terms.from.topics(ap.topics, my.desired.number.of.top.words.per.topic, T)
validate(ap.top.terms.json)


ap.plot.of.top.terms = get.plot.for.top.terms(ap.top.terms)
ap.plot.of.top.terms.pdf = get.plot.for.top.terms(ap.top.terms, output.pdf = TRUE) # successfully puts it in my directory
ap.plot.of.top.terms # Fixed


# ap.topics.per.doc

ap.topics.per.doc = get.tidy.topics.from.lda(ap.lda, per.document = TRUE)
ap.topics.per.doc.json = get.tidy.topics.from.lda(ap.lda, per.document = TRUE, output.json = TRUE) # let's label this last arguement to be specific; let's be consistent
validate(ap.topics.per.doc.json)

# we can also plot this but there are way to many docs here, so we limit to the first eight
filtered.ap.topics.per.doc = ap.topics.per.doc %>% filter(document %in% list.of.documents.we.want)
filtered.ap.plot.of.topic.per.doc.pdf = get.plot.for.per.document(filtered.ap.topics.per.doc, output.pdf = TRUE) # successfully puts it in my directory
filtered.ap.plot.of.topic.per.doc.pdf # Finished.

# or just use the top document classification and make it a tbl
ap.tidy.docs.classified.into.topics = get.tidy.document.classification.from.lda(ap.topics.per.doc)
ap.tidy.docs.classified.into.topics.json = get.tidy.document.classification.from.lda(ap.topics.per.doc, output.json = TRUE)
validate(ap.tidy.docs.classified.into.topics.json)


# try and create a doc term matrix from a list of pdf and html docs

my.list.of.docs = c("global_research/156390893402424.pdf", 
                    "global_research/156398257833616.pdf",
                    "global_research/156501860173228.html")

list.of.possible.ks = c(3, 4, 5, 6, 7) # this is used for best.k

my.docs.df = from.a.list.of.files.to.file.text.df(my.list.of.docs)
my.docs.df.json = from.a.list.of.files.to.file.text.df(my.list.of.docs, output.json = TRUE) # this needs a TRUE flag and JSON return
validate(my.docs.df.json)



#NEED HELP HERE

best.k = find.best.k.for.docs(my.docs.df, list.of.possible.ks) 
best.k.json = find.best.k.for.docs(my.docs.df, list.of.possible.ks, output.json = TRUE)
validate(best.k.json)

my.docs.as.tidy.txt = from.file.text.df.to.tidytext(my.docs.df) 
my.docs.as.tidy.txt.json = from.file.text.df.to.tidytext(my.docs.df, output.json = TRUE) # this needs a TRUE flag and JSON return; be careful lots of default params
validate(my.docs.as.tidy.txt.json)

# we can make a word cloud of all the text

my.word.cloud = get.word.cloud.from.tidy.text(my.docs.as.tidy.txt, output.pdf = TRUE) # return path, then remove from tester b/c its a word cloud; be careful b/c multiple arguements
my.word.cloud
######ERROR HERE########
#Error in UseMethod("filter_") : 
#  no applicable method for 'filter_' applied to an object of class "json" 
#################################################################

# and we can make a word cloud per document

my.word.cloud.per.doc = get.word.cloud.from.tidy.text(my.docs.as.tidy.txt, per.document = TRUE, output.pdf = TRUE) # return path, then remove from tester b/c its a word cloud
my.word.cloud.per.doc
######ERROR HERE########
#Error in UseMethod("filter_") : 
#  no applicable method for 'filter_' applied to an object of class "json" 
#################################################################

# this gets our very own doc term matrix

my.dtm = from.tidy.text.to.dtm(my.docs.as.tidy.txt) # no need to worry about this one b/c dtm cannot go to JSON

# now we can topic model with it
my.lda = get.lda(my.dtm, best.k)
my.lda.json = get.lda(my.dtm, best.k, T) # no need to worry about this one b/c lda cannot go to JSON; can be removed from tester

my.topics = get.tidy.topics.from.lda(my.lda)
my.topics.json = get.tidy.topics.from.lda(my.lda,  output.json.topics= TRUE) # multiple arguements besure to specify all
validate(my.topics.json)

my.top.terms = get.top.terms.from.topics(my.topics, my.desired.number.of.top.words.per.topic)
my.top.terms.json = get.top.terms.from.topics(my.topics, my.desired.number.of.top.words.per.topic, output.json = TRUE)
validate(my.top.terms.json)

#my.top.terms.plot

my.plot.of.top.terms = get.plot.for.top.terms(my.top.terms) 
my.plot.of.top.terms = get.plot.for.top.terms(my.top.terms, output.pdf =  TRUE) # successfully puts it in my directory
my.plot.of.top.terms # FIXED


# my.topics.per.doc
my.topics.per.doc = get.tidy.topics.from.lda(my.lda, per.document = TRUE)
my.topics.per.doc.json = get.tidy.topics.from.lda(my.lda, per.document = TRUE, output.json = TRUE)
validate(my.topics.per.doc.json)

# we can also plot this
my.plot.of.topic.per.doc = get.plot.for.per.document(my.topics.per.doc)
my.plot.of.topic.per.doc.pdf = get.plot.for.per.document(my.topics.per.doc, output.pdf = TRUE)
my.plot.of.topic.per.doc.pdf #FIXED


# or just use the top document classification and make it a tbl
my.tidy.docs.classified.into.topics = get.tidy.document.classification.from.lda(my.topics.per.doc)
my.tidy.docs.classified.into.topics.json = get.tidy.document.classification.from.lda(my.topics.per.doc, output.json = TRUE)
my.tidy.docs.classified.into.topics
validate(my.tidy.docs.classified.into.topics.json)

# now take everything in global research and determine the best k for a topic model.
file.list = list.files("global_research/") 

# try and create a doc term matrix from a list of pdf and html docs
all.docs.in.a.directory = paste0("global_research/", file.list)

all.docs.in.a.directory.df = from.a.list.of.files.to.file.text.df(all.docs.in.a.directory) # takes a little bit to run
all.docs.in.a.directory.df.json = from.a.list.of.files.to.file.text.df(all.docs.in.a.directory, output.json = TRUE) # errors out; needs the JSON arguement
validate(all.docs.in.a.directory.df.json)

best.k = find.best.k.for.docs(all.docs.in.a.directory.df, list.of.possible.ks, output.json = TRUE) # needs the JSON arguement, takes a very long time to run; allow 30 minutes
validate(best.k)
# once done validate best.k

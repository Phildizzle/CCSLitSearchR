library("igraph")
library("ggraph")
library("litsearchr")

setwd("H:\\Meta-Rep\\R Code")
naive_results <- import_results(file="savedrecs.bib")

nrow(naive_results)
naive_results
colnames(naive_results)
naive_results[1, "title"]
naive_results[1, "keywords"]

sum(is.na(naive_results[, "keywords"]))

extract_terms(naive_results)

keywords <- extract_terms(keywords=naive_results[, "keywords"], method="tagged")

Keywords_abstr <- extract_terms(text=naive_results[, "abstract"], method="fakerake", min_freq=3, min_n=2)

Keywords_title <- extract_terms(text=naive_results[, "title"], method="fakerake", min_freq=3, min_n=2)

terms <- unique(c(keywords, Keywords_title))

docs <- paste(naive_results[, "title"], naive_results[, "abstract"])

dfm <- create_dfm(elements=docs, features=terms)

g <- create_network(dfm, min_studies=3)

ggraph(g, layout="stress") +
  coord_fixed() +
  expand_limits(x=c(-3, 3)) +
  geom_edge_link(aes(alpha=weight)) +
  geom_node_point(shape="circle filled", fill="white") +
  geom_node_text(aes(label=name), hjust="outward", check_overlap=TRUE) +
  guides(edge_alpha="none")

strengths <- strength(g)

data.frame(term=names(strengths), strength=strengths, row.names=NULL) %>%
  mutate(rank=rank(strength, ties.method="min")) %>%
  arrange(strength) ->
  term_strengths

term_strengths

cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE)

cutoff_fig
^
########################

strengths <- strength(g)
df <- as.data.frame(strengths)

df <- strengths %>% as_tibble()
df <- add_column(df,names(strengths))

wordcloud(words = df$`names(strengths)`, freq = df$value, min.freq = 10,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

    ########################


new_list <- naive_results$keywords

new_list <- list(naive_results$keywords)

new_list_clean <- gsub(";", "", as.character(new_list))
new_list_clean <- gsub("\"", "", as.character(new_list_clean))
new_list_clean <- gsub("NA", "", as.character(new_list_clean))
new_list_clean <- gsub("\n", "", as.character(new_list_clean))



new_list_clean <- gsub("\"", "", as.character(new_list))

wc <- wordcloud(words = docs, min.freq = 5, random.oder = FALSE, rot.per=0.10, colors = brewer.pal(8, "Dark2"))

new_list_clean_ready <- strsplit(new_list_clean, ";")

########



########
docs = corpus(VectorSource(naive_results$keywords)) 

stri_trans_tolower(docs)
stri_trans

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "NA")
docs <- tm_map(docs, toSpace, "\n")
docs <- tm_map(docs, toSpace, ";")

docs <- tm_map(docs, content_transformer(tolower))

docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

set.seed(1234)

wordcloud(words = d$word, freq = d$freq, min.freq = 10,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = df, min.freq = 10,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

findAssocs(dtm, terms = "data", corlimit = 0.3)

head(d, 10)

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

new_list <- naive_results$keywords
new_str <- toString(new_list)

wordcloud(words = new_str, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

##########

library(quanteda)

docs <- naive_results$keywords

docs <- gsub(";", ".", as.character(docs))
docs <- gsub("\"", "", as.character(docs))
docs <- gsub("NA", "", as.character(docs))
docs <- gsub("\n", "", as.character(docs))


corp_immig <- corpus(docs) 

toks_news <- tokens(corp_immig, remove_punct = TRUE)

tstat_col_caps <- tokens_select(toks_news, pattern = "^[A-Z]", 
                                valuetype = "regex", 
                                case_insensitive = FALSE, 
                                padding = TRUE) %>% 
  textstat_collocations(min_count = 100)

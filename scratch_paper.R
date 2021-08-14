library(tidytext)
library(tidyverse)
nobel <- read_csv("data/NobelPeace.csv", locale=locale(encoding = "latin1")) %>%
  select(!X1)
ex <- nobel[1,]
ex %>%
  unnest_tokens(output = words, input = AwardSpeech)

stop_words
?stop_words
nobel_tidy <- nobel %>%
  unnest_tokens(output = words, input = AwardSpeech) %>%
  anti_join(stop_words, by = c("words" = "word"))  # by= specifies which columns to use, had they been named the same thing we could have ommitted it

nobel_tidy %>%
  count(words, sort=TRUE)

nobel_tidy %>%
  filter(Year < 1945) %>%
  count(words, sort=TRUE)

nobel_tidy %>%
  filter(Year > 1945) %>%
  count(words, sort=TRUE)

nobel_tidy %>%
  count(words, sort=TRUE) %>%
  top_n(15) %>%                     # selecting to show only top 15 words
  mutate(words = reorder(words,desc(n))) %>%  # this will ensure that the highest frequency words appear to the left
  ggplot(aes(words, n)) +
    geom_col()

nobel_tidy %>%
  mutate(Period = ifelse(Year <= 1945, "Pre-WWII", "Post-WWII")) %>%
  group_by(Period) %>%
  count(words, sort=TRUE) %>%
  mutate(proportion = n / sum(n) * 1000) %>% 
  slice_max(order_by=proportion, n = 15) %>%                     # selecting to show only top 15 words
  ggplot(aes(reorder_within(x = words, by = proportion, within = Period), proportion)) +
    geom_col() +
    scale_x_reordered() +
    facet_wrap(~Period, ncol = 2, scales = "free_x")


library(wordcloud)

nobel_tidy %>%
  count(words, sort=TRUE) %>%
  with(wordcloud(words, n, max.words = 100))

txt <- c("This is a short tagging example, by John Doe.",
         "Too bad OpenNLP is so slow on large texts.")

require(spacyr)
## Loading required package: spacyr
#spacy_initialize()
spacy_initialize(python_executable = "C:\\6041041\\Anaconda3\\envs\\6.86x\\python")
## Finding a python executable with spacy installed...
## spaCy (language model: en) is installed in /usr/local/bin/python
## successfully initialized (spaCy Version: 1.8.2, language model: en)

spacy_parse(txt, pos = TRUE, tag = TRUE)

library(tidyverse)
library(tidytext)
nobel <- read_csv("data/NobelPeace.csv", locale=locale(encoding = "latin1")) %>%
  select(!X1) %>%
  mutate(Period = ifelse(Year <= 1945, "Pre-WWII", "Post-WWII"))
nobel_words <- nobel %>%
  unnest_tokens(words, AwardSpeech) %>%
  count(words, Period, sort = TRUE) %>%
  mutate(n_logged = ifelse(n > 0, 1+log10(n), 0))
wc <- nobel_words %>% 
  group_by(Period) %>% 
  summarize(wc = sum(n))
nobel_words <- left_join(nobel_words, wc)

book_tf_idf %>%
  group_by(Period) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(words, tf_idf), fill = Period)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Period, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

stop_words

my_words <- c("peace", "war")
custom_stop_words <- tibble(word = my_words, lexicon = "my_customization")
stop_words_custom <- rbind(stop_words, custom_stop_words)
tail(stop_words_custom)

library(tidytext)
# the first time you will need to say yes to download of the sentiment dictionary
get_sentiments("afinn")
get_sentiments("bing")
table(get_sentiments("nrc")$sentiment)
get_sentiments("nrc")

# calculating text sentiment by subtracting total positive sentiment words from total negative with bing lexicon
nobel %>%
  unnest_tokens(word, AwardSpeech) %>%  ## we call our new column "word" which makes inner_joins easier 
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, year = Year) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative) %>%
  ggplot(aes(year, sentiment)) +
           geom_line(show.legend = FALSE) +
           geom_hline(yintercept = 0, linetype = 2, alpha = .8)
           #facet_wrap(~book, ncol = 2, scales = "free_x")

nobel %>%
  unnest_tokens(word, AwardSpeech) %>%  ## we call our new column "word" which makes inner_joins easier 
  inner_join(get_sentiments("afinn")) %>%
  group_by(Year) %>%
  summarize(sentiment = sum(value)) %>%
    ggplot(aes(Year, sentiment)) +
    geom_line(show.legend = FALSE) +
    geom_hline(yintercept = 0, linetype = 2, alpha = .8)
    #facet_wrap(~book, ncol = 2, scales = "free_x")

sr <- srps <- read_csv("data/srps.csv")
sr %>%
  unnest_tokens(word, Text) %>%  ## we call our new column "word" which makes inner_joins easier 
  inner_join(get_sentiments("afinn")) %>%
  group_by(Company, Year) %>%
  summarize(sentiment = sum(value)) %>%
  ggplot(aes(Year, sentiment)) +
    geom_line(show.legend = FALSE) +
    geom_hline(yintercept = 0, linetype = 2, alpha = .8) +
    facet_wrap(~Company, ncol = 2, scales = "free_x")

anticipation <- get_sentiments("nrc") %>% 
  filter(sentiment == "anticipation")
negative <- get_sentiments("nrc") %>%
  filter(sentiment == "negative")
negAnt <- inner_join(anticipation, negative, by="word") %>%
  select(word)
nobel %>%
  mutate(Period = ifelse(Year <= 1945, "Pre-WWII", "Post-WWII")) %>% 
  unnest_tokens(word, AwardSpeech) %>%
  inner_join(negAnt) %>%
  group_by(Period) %>%
  count(word, sort = TRUE) %>%
  slice_max(order_by=n, n = 10) %>%                     # selecting to show only top 15 words within each group
  ggplot(aes(reorder_within(x = word, by = n, within = Period), n)) +    # reordering is a bit tricky, see ?reorder_within()
  geom_col() +
  scale_x_reordered() +
  facet_wrap(~Period, ncol = 2, scales = "free_x")


oil_dict <- read_lines("data/oil_theme.txt")
oil_dict[oil_dict != ""]
oil_dict <- tibble(word = oil_dict, dictionary = "oil dictionary")
oil_dict %>%
  filter(word != "")

nobel %>%
  mutate(Period = ifelse(Year <= 1945, "Pre-WWII", "Post-WWII")) %>% 
  unnest_tokens(word, AwardSpeech) %>%
  inner_join(oil_dict) %>%
  group_by(Period) %>%
  count(word, sort = TRUE) %>%
  slice_max(order_by=n, n = 10) %>%                     # selecting to show only top 15 words within each group
  ggplot(aes(reorder_within(x = word, by = n, within = Period), n)) +    # reordering is a bit tricky, see ?reorder_within()
  geom_col() +
  scale_x_reordered() +
  facet_wrap(~Period, ncol = 2, scales = "free_x")

dat <- nobel_tidy %>%
  count(words, sort=TRUE) %>%
  mutate(word = words) %>%
  mutate(freq = n) %>%
  select(word, freq) %>%
  top_n(200)
wordcloud2(dat, size = 2)

nobel_tidy %>%
  mutate(word_stem = wordStem(words))

library(hunspell)
library(SnowballC)
words_to_stem <- c("going", "represented", "wars", "similarity", "books")
SnowballC::wordStem(words_to_stem)
hunspell::hunspell_stem("going")
hunspell_stem(words_to_stem)

words <- c("love", "loving", "lovingly", "loved", "lover", "lovely")
hunspell_stem(words)
hunspell_analyze(words)

nobel_tidy <- nobel %>%
  unnest_tokens(output = words, input = AwardSpeech) %>%
  anti_join(stop_words, by = c("words" = "word"))

nobel %>%
  unnest_tokens(twogram, AwardSpeech, token = "ngrams", n = 2)

nobel %>%
  unnest_tokens(twogram, AwardSpeech, token = "ngrams", n = 2) %>%
  anti_join(stop_words, by = c("words" = "word")) %>%
  count(words, sort=TRUE) %>%
  top_n(15) %>%                     # selecting to show only top 15 words
  mutate(words = reorder(words,desc(n))) %>%  # this will ensure that the highest frequency words appear to the left
  ggplot(aes(words, n)) +
  geom_col()

library(forcats)
nob <- nobel %>%
  mutate(Period = ifelse(Year <= 1945, "Pre-WWII", "Post-WWII")) %>% 
  unnest_tokens(twogram, AwardSpeech, token = "ngrams", n = 2) %>%
  separate(twogram, into=c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  unite(twogram, word1, word2, sep = ' ') %>%              # now putting word1 and word2 back into a single column called twogram
  group_by(Period) %>%
  count(twogram, sort=TRUE) %>%
  slice_max(order_by=n, n = 10)                    # selecting to show only top 15 words within each group
nob %>%
 # ungroup() %>%
    ggplot(aes(reorder_within(x = twogram, by = n, within = Period), fct_reorder(twogram, n), fill=Period)) +    # reordering is a bit tricky, see ?reorder_within()
      geom_col() +
      scale_x_reordered() +
      coord_flip() +
      facet_wrap(~Period, ncol = 2, scales = "free") +
      labs(x = "n", y = NULL)
nob %>%
  mutate(twogram = reorder(twogram, n)) %>%
  ungroup() %>%
  ggplot(aes(reorder_within(x = twogram, by = n, within = Period), n, fill = Period)) +
    geom_col(show.legend = FALSE) +
  scale_x_reordered() +  
  coord_flip() +
    facet_wrap(~Period, ncol = 2, scales = "free") +
    ylab('n') +
    xlab("2-gram")



library(widyr)
nobel %>%
  unnest_tokens(word, AwardSpeech) %>%
  filter(!word %in% stop_words$word) %>%
  pairwise_count(word, Year, sort = TRUE)

nobel %>%
  mutate(index = row_number()) 

nobel <- read_csv("data/NobelPeace.csv", locale=locale(encoding = "latin1")) %>%
  select(!X1) %>%
  unnest_tokens(output = words, input = AwardSpeech)
cast_dtm(nobel, words, Year)

library(broom)
dt <- nobel %>%
  group_by(Year) %>%
  count(words, sort = TRUE) %>%
  cast_dtm(Year, words, n)

dfm <- nobel %>%
  group_by(Year) %>%
  count(words, sort = TRUE) %>%
  cast_dfm(Year, words, n)
View(dt)
tidy(dt)


nobel <- read_rds("data/nobel_cleaned.Rds")
dt <- nobel %>%
  unnest_tokens(output = words, input = AwardSpeech) %>%
  group_by(Year, Laureate) %>%
  count(words, sort = TRUE)

(nobel_df <- nobel %>%
    unnest_tokens(output = words, input = AwardSpeech) %>%
    group_by(Year, Laureate) %>% 
    summarize(AwardSpeech = str_c(words, collapse = " ")) %>%
    ungroup())


library(quanteda)
nobel_corp <- corpus(nobel_df, docid_field = "Year", text_field = "AwardSpeech") # corpus() automatically includes all other columns that are not text and document names as variables
nobel_tokens <- tokens(nobel_corp)
nobel_tokens <- tokens_wordstem(nobel_tokens, language = quanteda_options("english"))  # "Norwegian" will also work
dfm_nobel <- dfm(nobel_tokens)



library(SnowballC)
getStemLanguages()

## stemming
nobel <- read_csv("data/NobelPeace.csv", locale=locale(encoding = "latin1")) %>%
  select(!X1)
nobel_tidy <- nobel %>%
  unnest_tokens(output = words, input = AwardSpeech) %>%
  anti_join(stop_words, by = c("words" = "word"))
(nobel_tidy_stemmed <- nobel_tidy %>%
    mutate(word_stem = wordStem(words)))
write_rds(nobel_tidy_stemmed, "data/nobel_stemmed.Rds")

nobel_tidy <- read_rds("data/nobel_stemmed.Rds") %>%
  select(Year, Laureate, word_stem) %>%
  unnest_tokens(output = words, input = word_stem)
nobel_tidy


nobel_tidy <- read_rds("data/nobel_stemmed.Rds") %>%
  select(Year, Laureate, word_stem) %>%
  rename(Year = Year, Laureate = Laureate, words = word_stem)

nobel_tidy <- nobel_tidy %>%
  mutate(decade = Year %/% 10 * 10)
## we'll transform our tibble to a dfm through quanteda commands as we have document variables (decade) we want associated with the documents
nobel_df <- nobel_tidy %>%
    group_by(Year, Laureate, decade) %>% 
    summarize(AwardSpeech = str_c(words, collapse = " ")) %>%
    ungroup()
nobel_corp <- corpus(nobel_df, docid_field = "Year", text_field = "AwardSpeech") # corpus() automatically includes all other columns that are not text and document names as variables
docid <- paste(nobel_df$Year, 
               nobel_df$Laureate, sep = " - ")      # naming our documents in the corpus in "Year - Laureate" format
docnames(nobel_corp) <- docid
nobel_tokens <- tokens(nobel_corp)
dfm_nobel <- dfm(nobel_tokens, groups = 'decade')
dfm_nobel_grouped <- dfm_group(dfm_nobel, groups = decade)
library(quanteda.textstats)
cos_sim <- textstat_simil(dfm_nobel_grouped, margin = 'documents', method = 'cosine')
cos_sim <- as.matrix(cos_sim)
cos_sim_df <- as.data.frame(cos_sim)
cos_sim_df[lower.tri(cos_sim_df, diag = FALSE)] <- NA

cos_sim_df['Year'] <- colnames(cos_sim_df)
tot_gath <- gather(cos_sim_df, 1:as.integer(ncol(cos_sim_df)-1), key = 'to', value = 'cosine')
tot_gath <- tot_gath %>%
  mutate(cosine = round(cosine,2))
tot_gath %>%
  #filter(cosine < .99) %>%
  ggplot(aes(Year, to)) +
  geom_tile(aes(fill = cosine)) +
  scale_fill_continuous("",limits=c(.3, 1), breaks=seq(.3,1,by=0.2), low = "white", high = "blue", na.value = "white") +
  theme_bw() +
  geom_text(aes(label = format(cosine, nsmall=1)), color = 'white') +
  theme(axis.text.x=element_text(angle=0), axis.ticks=element_blank(), axis.line=element_blank(), panel.border=element_blank(),
        panel.grid.major=element_line(color='#eeeeee')) +
  labs(x = '', y = '', subtitle = 'Cosine Distances', title = 'Similarity of Nobel Peace Prize Award Speeches by Decade')


createnetwork <- function(fcm_freq) {
  feat <- names(topfeatures(fcm_freq, dim(fcm_freq)[1]))
  size <- log(colSums(dfm_select(fcm_freq, feat)))
  textplot_network(fcm_freq, min_freq = 0.9, vertex_size = size / max(size) * 3)
}

library(quanteda.textplots)
fcm_freq <- fcm(nobel_tokens, context = "window", tri = FALSE)
#fcm_freq <- fcm(dfm_nobel_grouped)
top <- names(topfeatures(fcm_freq, 30))
size <- log(colSums(dfm_select(fcm_freq, top)))
fcm_select(fcm_freq, pattern = top) %>%
  textplot_network(min_freq = .5)

toks <- data_char_ukimmig2010 %>%
  tokens(remove_punct = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(pattern = stopwords("english"), padding = FALSE)
fcmat <- fcm(toks, context = "window", tri = FALSE)
feat <- names(topfeatures(fcmat, 30))
fcm_select(fcmat, pattern = feat) %>%
  textplot_network(min_freq = 0.5)

# read in the dataframe into R as normal
library(tidyverse)
library(tidytext)
librayr(topicmodels)
nobel_tidy <- read_rds("data/nobel_stemmed.Rds") %>%
  select(Year, Laureate, word_stem) %>%
  rename(Year = Year, Laureate = Laureate, words = word_stem)
# transform dataframe to DTM
nobel_dtm <- nobel_tidy %>%
  group_by(Year) %>%
  count(words, sort = TRUE) %>%
  cast_dtm(Year, words, n)

k = 5
alpha = 5
nobel_tm <- LDA(nobel_dtm, k = k, alpha = alpha)
terms <- tidy(nobel_tm, matrix = "beta")
topics_in_documents <- tidy(nobel_tm, matrix = "gamma")
t <- topics_in_documents %>%
  filter(topic %in% 1:5)
t %>%
  ggplot(aes(document, gamma)) +
  geom_col(aes(group = topic, fill = topic))



words_in_topics <- terms %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

words_in_topics %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


topics(nobel_tm, 20)
top3 <- terms(nobel_tm, 3)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")


hand_topics <- tibble(old_topic = 1:3, new_topic = c("thi", "that", "tuff"))
auto_topics <- apply(terms(nobel_tm, 3), 2, paste, collapse = "-")
auto_topics <- tibble(old_topic = 1:20, new_topic = auto_topics)
topics_in_documents %>%
  left_join(auto_topics, by=c("topic" = "old_topic"))

(auto_topics <- apply(terms(nobel_tm, 3), 2, paste, collapse = "-"))  # pastes together the top three terms for each topic in the nobel topic model
(auto_topics <- tibble(old_topic = 1:20, new_topic = auto_topics)) # make as tibble where numeric topics are matched with the auto generated ones
topics_in_documents %>%
  left_join(auto_topics, by=c("topic" = "old_topic"))
(topics <- topics_in_documents %>%
    left_join(auto_topics, by=c("topic" = "old_topic")))

topics %>%
  filter(document == 1977 | document == 1983 | document == 1996) %>%
  ggplot(aes(new_topic, gamma, fill = document)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ document, ncol = 3)

t <- topics %>%
  filter(topic %in% c(1,3,8,15,19))
t %>%
  ggplot(aes(document, gamma)) +
    geom_line(aes(group = new_topic, color = new_topic))


topics %>%
  #filter(str_detect(new_topic, "war")) %>%
  ggplot(aes(document, gamma)) +
    geom_col(aes(group = new_topic, fill = new_topic)) +
    scale_x_discrete(breaks = seq(1905, 2019, 10))

topics %>%
  filter(str_detect(new_topic, "war")) %>%
  ggplot(aes(document, gamma)) +
  geom_line(aes(group = new_topic, color = new_topic)) +
  scale_x_discrete(breaks = seq(1905, 2019, 10))

tidy(nobel_dtm)


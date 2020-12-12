#Master Compilation0 but with the things I'm not using deleted
#Do NOT trust this file, it is VERY SKETCHY

#introducing Hamlet and cleaning it
library(gutenbergr)
full_text <- gutenberg_download(1122)

cleaned_text0 <- full_text[-c(1:279, 1290:1297, 2189:2196, 3313:3320, 3534:3541, 3627:3634, 3930:3937, 3996:3403, 4238:4245, 5145:5152),]

cleaned_text1 <- cleaned_text0

names_full <- c("Denmark", "Elsinore", "Norway", "Claudius", "Marcellus", "Hamlet", "Polonius", "Horatio", "Laertes", "Voltemand", "Cornelius", "Rosencrantz", "Guildenstern", "Osric", "Bernardo", "Francisco", "Reynaldo", "Fortinbras", "Gertrude", "Ophelia")
names_abrv <- c("Ber", "Fran", "Mar", "Hor", "King", "Queen", "Cor", "Volt", "Laer", "Pol", "Ham", "Oph", "Ghost", "Rey", "Ros", "Guil", "For", "Capt", "Sailor", "Mess", "Clown", "Osr")
shakes_stop <- c("thee", "thou", "thy", "tis", "Tis", "hath", "hast", "Enter", "twill", "art", "thyself", "ere", "whence", "Exeunt", "twixt", "Exit", "thine", "canst", "o'er", "is't", "on't", "wherefore", "wither", "wilt", "shalt", "shouldst", "wouldst", "nay", "yea", "Ay", "ay", "twere", "thence", "ye", "twas", "prithee", "doth", "th", "hither", "Act", "ACT", "Scene","II", "III", "IV", "V", "VI", "VII", "1")

library(tm)
cleaned_text1$text <- unlist(lapply(cleaned_text1$text, FUN=removeWords, words=names_full))
cleaned_text1$text <- unlist(lapply(cleaned_text1$text, FUN=removeWords, words=names_abrv))
cleaned_text1$text <- unlist(lapply(cleaned_text1$text, FUN=removeWords, words=shakes_stop))


#x
#x

#hi
#hi
#word frequency aka chapter 3 code
#this is kind of a disaster
#hi
#hi

#x
#x


library(dplyr)
library(tidytext)

book_words <- cleaned_text1 %>%
  unnest_tokens(word, text) %>%
  count(gutenberg_id, word, sort = TRUE)

total_words <- book_words %>% 
  group_by(gutenberg_id) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words

library(ggplot2)

book_words <- book_words %>%
  bind_tf_idf(word, gutenberg_id, n)

book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

#library(gutenbergr)
#shakesmarl <- gutenberg_download(c(1122, 779), 
#                                 meta_fields = "title")

#shake_words <- shakesmarl %>%
#  unnest_tokens(word, text) %>%
#  count(title, word, sort = TRUE)

#shake_words

#library(forcats)

#maybe this would work if I cleaned it all. Let's consider it
#plot_shakes <- shake_words %>%
#  bind_tf_idf(word, title, n) %>%
#  mutate(word = fct_reorder(word, tf_idf)) %>%
#  mutate(title = factor(title, levels = c("The Tragedy of Hamlet, Prince of Denmark", 
#                                          "The Tragical History of Doctor Faustus\r\nFrom the Quarto of 1604")))

#plot_shakes %>% 
#  group_by(title) %>% 
#  top_n(10, tf_idf) %>% 
#  ungroup() %>%
#  mutate(word = reorder(word, tf_idf)) %>%
#  ggplot(aes(word, tf_idf, fill = title)) +
#  geom_col(show.legend = FALSE) +
#  labs(x = NULL, y = "tf-idf") +
#  facet_wrap(~title, ncol = 2, scales = "free") +
#  coord_flip()


#x
#x

#hi
#hi
#word frequency, cont, aka two-word phrases, aka chapter 4
#for this section, to be as telling as possible, I'm going to use a much less cleaned Hamlet. It will have all those messages out and the abbreviations, but not names
#I'm quite pleased with this
#hi
#hi

#x
#x


library(gutenbergr)
full_text <- gutenberg_download(1122)

cleaned_text2 <- full_text[-c(1:279, 1290:1297, 2189:2196, 3313:3320, 3534:3541, 3627:3634, 3930:3937, 3996:3403, 4238:4245, 5145:5152),]

cleaned_text3 <- cleaned_text2

names_abrv <- c("Ber", "Fran", "Mar", "Hor", "King", "Queen", "Cor", "Volt", "Laer", "Pol", "Ham", "Oph", "Ghost", "Rey", "Ros", "Guil", "For", "Capt", "Sailor", "Mess", "Clown", "Osr")
shakes_stop <- c("thee", "thou", "thy", "tis", "Tis", "hath", "hast", "Enter", "twill", "art", "thyself", "ere", "whence", "Exeunt", "twixt", "Exit", "thine", "canst", "o'er", "is't", "on't", "wherefore", "wither", "wilt", "shalt", "shouldst", "wouldst", "nay", "yea", "Ay", "ay", "twere", "thence", "ye", "twas", "prithee", "doth", "th", "hither", "Act", "ACT", "Scene","II", "III", "IV", "V", "VI", "VII", "1")

library(tm)
cleaned_text3$text <- unlist(lapply(cleaned_text3$text, FUN=removeWords, words=names_abrv))
cleaned_text3$text <- unlist(lapply(cleaned_text3$text, FUN=removeWords, words=shakes_stop))

library(dplyr)
library(tidytext)

tidy_book <- cleaned_text3 %>%
  mutate(line = row_number()) %>%
  unnest_tokens(word, text)

tidy_book %>%
  count(word, sort = TRUE)


ham_bigrams <- cleaned_text3 %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

ham_bigrams

ham_bigrams %>%
  count(bigram, sort = TRUE)

library(tidyr)

bigrams_separated <- ham_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

#maybe cool
bigrams_filtered %>%
  filter(word2 == "death") %>%
  count(gutenberg_id, word1, sort = TRUE)

bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

AFINN <- get_sentiments("afinn")

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

library(ggplot2)

#DEFINITELY
not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment value * number of occurrences") +
  coord_flip()

# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n >3) %>%
  graph_from_data_frame()

bigram_graph

ham_section_words <- cleaned_text3 %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

ham_section_words

library(widyr)

# count words co-occuring within sections
word_pairs <- ham_section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs

word_pairs %>%
  filter(item1 == "lord")

# we need to filter for at least relatively common words first
word_cors <- ham_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

#this is nice
word_cors

word_cors %>%
  filter(item1 == "lord")

#one of my favourites
word_cors %>%
  filter(item1 %in% c("lord", "hamlet", "death", "love")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

set.seed(2016)

#what I have been calling the scatterplot
word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


#x
#x

#hi
#hi
#word sentiments, aka chapter 2
#this is a little messy, but it just needs to be cleaned out. I want to get that ncr thing to work
#I think the "not" words will be good in this section of the English paper
#hi
#hi

#x
#x


library(tidytext)

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

library(dplyr)
library(stringr)

tidy_books <- cleaned_text1 %>%
  group_by(gutenberg_id) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

nrc_anger <- get_sentiments("nrc") %>% 
  filter(sentiment == "anger")

tidy_books %>%
  filter(gutenberg_id == "1122") %>%
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE)

library(tidyr)

shakespeare_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(gutenberg_id, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

library(ggplot2)

ggplot(shakespeare_sentiment, aes(index, sentiment, fill = gutenberg_id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~gutenberg_id, ncol = 2, scales = "free_x")

hamlet <- tidy_books %>% 
  filter(gutenberg_id == "1122")

hamlet

afinn <- hamlet %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(hamlet %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          hamlet %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#this also proves pos/neg point
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", 
                          "negative")) %>% 
  count(sentiment)

#big
get_sentiments("bing") %>% 
  count(sentiment)

bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

bing_word_counts

#another great one
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


#x
#x

#hi
#hi
#






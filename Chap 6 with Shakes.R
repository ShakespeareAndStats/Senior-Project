#Silge code from chap6 but with my data
#problem

library(gutenbergr)
full_text <- gutenberg_download(1122)

cleaned_text2 <- full_text[-c(1:279, 1290:1297, 2189:2196, 3313:3320, 3534:3541, 3627:3634, 3930:3937, 3996:3403, 4238:4245, 5145:5152),]

cleaned_text3 <- cleaned_text2

rand <- c("now", "may", "let", "shall", "come", "upon", "like", "o")
names_abrv <- c("Ber", "Fran", "Mar", "Hor", "King", "Queen", "Cor", "Volt", "Laer", "Pol", "Ham", "Oph", "Ghost", "Rey", "Ros", "Guil", "For", "Capt", "Sailor", "Mess", "Clown", "Osr")
shakes_stop <- c("thee", "thou", "thy", "tis", "Tis", "hath", "hast", "Enter", "twill", "art", "thyself", "ere", "whence", "Exeunt", "twixt", "Exit", "thine", "canst", "o'er", "is't", "on't", "wherefore", "wither", "wilt", "shalt", "shouldst", "wouldst", "nay", "yea", "Ay", "ay", "twere", "thence", "ye", "twas", "prithee", "doth", "th", "hither", "Act", "ACT", "Scene","II", "III", "IV", "V", "VI", "VII", "1")

library(tm)
cleaned_text3$text <- unlist(lapply(cleaned_text3$text, FUN=removeWords, words=names_abrv))
cleaned_text3$text <- unlist(lapply(cleaned_text3$text, FUN=removeWords, words=shakes_stop))
cleaned_text3$text <- unlist(lapply(cleaned_text3$text, FUN=removeWords, words=rand))

library(tidyr)
library(tidytext)
library(topicmodels)
library(Matrix)
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)

hamlet_dtm <- cleaned_text3 %>%
  unnest_tokens(word, text) %>%
  count(gutenberg_id, word) %>%
  anti_join(get_stopwords()) %>%
  cast_dtm(gutenberg_id, word, n)

# set a seed so that the output of the model is predictable
ap_lda <- LDA(hamlet_dtm, k = 2, control = list(seed = 1234))
ap_lda

library(tidytext)

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

library(tidyr)

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

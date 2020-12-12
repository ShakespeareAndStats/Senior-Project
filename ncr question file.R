#ncr questions for professor sage

library(textdata)
library(tidytext)

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

library(dplyr)
library(stringr)

library(gutenbergr)
full_text <- gutenberg_download(1122)

tidy_books <- full_text %>%
  group_by(gutenberg_id) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

nrc_joy <- get_sentiments("nrc") %>% 
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

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", 
                          "negative")) %>% 
  count(sentiment)


#Silge Code from Chap 3, but with my data
#done with full text, not cleaned
#works, see note at line 105

library(dplyr)
library(janeaustenr)
library(tidytext)

library(gutenbergr)
some_shakes <- gutenberg_download(c(1122, 1112, 1519, 1514))

book_words <- some_shakes %>%
  unnest_tokens(word, text) %>%
  count(gutenberg_id, word, sort = TRUE)

total_words <- book_words %>% 
  group_by(gutenberg_id) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words

library(ggplot2)

ggplot(book_words, aes(n/total, fill = gutenberg_id)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~gutenberg_id, ncol = 2, scales = "free_y")

freq_by_rank <- book_words %>% 
  group_by(gutenberg_id) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

freq_by_rank

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = gutenberg_id)) + 
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

book_words <- book_words %>%
  bind_tf_idf(word, gutenberg_id, n)

book_words

book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(gutenberg_id) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = gutenberg_id)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~gutenberg_id, ncol = 2, scales = "free") +
  coord_flip()

library(gutenbergr)
some_shakes <- gutenberg_download(c(1122, 1112, 1519, 1514), 
                              meta_fields = "title")

shake_words <- some_shakes %>%
  unnest_tokens(word, text) %>%
  count(title, word, sort = TRUE)

shake_words

library(forcats)

plot_shakes <- shake_words %>%
  bind_tf_idf(word, title, n) %>%
  mutate(word = fct_reorder(word, tf_idf)) %>%
  mutate(title = factor(title, levels = c("The Tragedy of Romeo and Juliet",
                                            "The Tragedy of Hamlet, Prince of Denmark", 
                                            "Much Ado about Nothing",
                                            "A Midsummer Night's Dream")))

plot_shakes %>% 
  group_by(title) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = title)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~title, ncol = 2, scales = "free") +
  coord_flip()

library(stringr)

#this part isn't relevent in my case. Thankfully, we learned how to do this in summer 2019. I stil have the files

physics %>% 
  filter(str_detect(text, "_k_")) %>% 
  select(text)

physics %>% 
  filter(str_detect(text, "RC")) %>% 
  select(text)

mystopwords <- tibble(word = c("eq", "co", "rc", "ac", "ak", "bn", 
                               "fig", "file", "cg", "cb", "cm",
                               "ab", "_k", "_k_", "_x"))

physics_words <- anti_join(physics_words, mystopwords, 
                           by = "word")

plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  mutate(word = str_remove_all(word, "_")) %>%
  group_by(author) %>% 
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, author)) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

ggplot(plot_physics, aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_reordered()


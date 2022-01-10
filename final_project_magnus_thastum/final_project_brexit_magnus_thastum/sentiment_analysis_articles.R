## Sentiment analysis of Daily Mail Articles
library(tidyverse)
library(here)
library(pdftools)
library(tidytext)
library(textdata) 
library(ggwordcloud)

mail_path <- here("data","daily_mail_brexit.pdf")
mail_text <- pdf_text(mail_path)

mail_df <- data.frame(mail_text) %>% 
  mutate(text_full = str_split(mail_text, pattern = "\\n")) %>% 
  unnest(text_full) %>% 
  mutate(text_full = str_trim(text_full))

mail_tokens <- mail_df %>%
  unnest_tokens(word, text_full)

mail_wc <- mail_tokens %>% 
  count(word) %>% 
  arrange(-n)
mail_wc

mail_stop <- mail_tokens %>% 
  anti_join(stop_words) %>% 
  select(-mail_text)

mail_swc <- mail_stop %>% 
  count(word) %>% 
  arrange(-n)
mail_swc

length(unique(mail_stop$word))

mail_top100 <- mail_stop %>% 
  count(word) %>% 
  arrange(-n) %>% 
  head(100)

mail_cloud <- ggplot(data = mail_top100, aes(label = word, size = n)) +
  geom_text_wordcloud_area(aes(color = n), shape = "diamond") +
  scale_size_area(max_size = 12) +
  scale_color_gradientn(colors = c("darkgreen", "blue", "red")) +
  theme_minimal()
mail_cloud

get_sentiments(lexicon = "afinn")

mail_afinn <- mail_stop %>% 
  inner_join(get_sentiments("afinn"))

mail_afinn_hist <- mail_afinn %>% 
  count(value)

ggplot(data = mail_afinn_hist, aes(x = value, y = n)) +
  geom_col()

mail_afinn2 <- mail_afinn %>% 
  filter(value == -2)

unique(mail_afinn2$word)

mail_afinn2_n <- mail_afinn2 %>% 
  count(word, sort = TRUE) %>% 
  head(25) %>% 
  mutate(word = fct_reorder(factor(word), n))

ggplot(data = mail_afinn2_n, aes(x = word, y = n)) +
  geom_col() +
  coord_flip()

mail_summary <- mail_afinn %>% 
  summarise(
    mean_score = mean(value),
    median_score = median(value)
  )

mail_summary

mail_nrc <- mail_stop %>% 
  inner_join(get_sentiments("nrc"))

mail_nrc_n <- mail_nrc %>% 
  count(sentiment, sort = TRUE)

ggplot(data = mail_nrc_n, aes(x = sentiment, y = n)) +
  geom_col()

mail_nrc_n5 <- mail_nrc %>% 
  count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>% 
  top_n(5) %>% 
  ungroup()

mail_nrc_gg <- ggplot(data = mail_nrc_n5, aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, ncol = 2, scales = "free") +
  coord_flip() +
  theme_minimal() +
  labs(x = "Word", y = "count")

mail_nrc_gg


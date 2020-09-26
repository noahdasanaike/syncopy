library(tidytext)
library(tidyverse)
library(pdftools)
library(tidyr)
library(tm)
library(textdata)
library(reshape2)
library(wordcloud)
library(qdapDictionaries)
library(ggthemes)
library(textclean)
library(ngram)

# all
# beginning script analysis

x <- {".pdf"}
y <- list.files(path = ".")

movies <- y[apply(sapply(x, function(q) {grepl(q, y)}), 1, function(x) {sum(as.numeric(x)) > 0})]
scripts <- data.frame()

for (i in 1:length(movies)){
  name <- movies[i]
  film_name <- str_split(name, pattern = "\\.")[[1]][1]
  
  temp <- sapply(name, pdf_text) %>%
    str_replace_all(pattern = "[[:punct:]]", replacement = " ") %>%
    tibble() %>%
    rename(text = ".") %>%
    mutate(linenumber = row_number(),
           film = film_name) %>%
    unnest_tokens(word, text)
  scripts <- rbind(scripts, temp)
}

# end script formation

analysis <- scripts %>% 
  inner_join(get_sentiments("bing")) %>%
  group_by(film, linenumber) %>%
  count(sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)

scripts %>% 
  group_by(film) %>%
  count(word)

# graphs

ggplot(analysis, aes(linenumber, sentiment, color = film)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~film, ncol = 2) +
  labs(x = "Line Number", y = "Sentiment")

analysis %>%
  ggplot(aes(linenumber, sentiment, fill = film)) +
    geom_col(width = 1.5) +
    ylim(-20, 20) +
    labs(x = "Line Number", y = "Sentiment") +
    theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "grey26"),
         legend.position = "bottom") +
    geom_hline(yintercept = 0, color = "lightcoral", size = 1.5) +
    scale_fill_manual(name = "Film", values = c("lightcoral", "snow2",
                                              "plum3", "coral", 
                                              "azure4", "lightsteelblue", "lightpink1",
                                              "lightskyblue2"))

scripts %>% 
  filter(film == "batman_begins") %>%
  group_by(film) %>%
  count(word) %>%
  bind_tf_idf(word, film, n) %>%
  arrange(desc(tf_idf)) %>%
  group_by(film) %>% 
  slice_max(20) %>% 
  ungroup() %>%
  arrange(desc(tf_idf)) %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = film)) +
  geom_col() +
  coord_flip() +
  labs(x = "Words", y = "Term Frequency–Inverse Document Frequency") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey26"),
        plot.background = element_rect(fill = "grey26", color = NA),
        legend.background = element_rect(fill = "grey26"),
        legend.position = "bottom",
        legend.text = element_text(size = 15, color = "#ffffff"),
        legend.title = element_text(size = 15, color = "#ffffff"),
        axis.title = element_text(size = 15, color = "#ffffff"),
        axis.text = element_text(size = 15, color = "#ffffff")) +
  scale_fill_manual(name = "Film", values = c("lightcoral", "snow2",
                                              "plum3", "tomato2", 
                                              "azure4", "lightsteelblue", "lightpink1",
                                              "mediumpurple1"))


# histogram

scripts %>% 
  group_by(film) %>%
  count(word) %>%
  mutate(total = sum(n)) %>%
  arrange(desc(n)) %>%
  mutate(rank = row_number(),
         term_freq = n/total) %>%
  arrange(desc(term_freq)) %>%
  ggplot(aes(rank, term_freq, color = film)) +
  geom_line(size = 1.1, alpha = 0.8) + 
  scale_x_log10() +
  scale_y_log10() +
  theme_hc(style = "darkunica") +
  scale_color_hc("darkunica") +
  labs(x = "Rank", y = "Term Frequency")

# word clouds
is.word  <- function(x) x %in% GradyAugmented
scripts <- scripts[which(is.word(scripts$word))]

scripts %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

scripts %>%
  filter(film == "inception",
         word != "the") %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

#  bind_tf_idf

scripts %>% 
  group_by(film) %>%
  count(word) %>%
  bind_tf_idf(word, film, n) %>%
  arrange(desc(tf_idf)) %>%
  group_by(film) %>% 
  top_n(8) %>% 
  ungroup() %>%
  arrange(desc(tf_idf)) %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = film)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~film, ncol = 2, scales = "free") +
    coord_flip() +
    theme_hc(style = "darkunica") +
    scale_fill_hc("darkunica") +
    labs(x = "Words", y = "Term Frequency–Inverse Document Frequency")








### exploratory text modeling

inception <- sapply("inception.pdf", pdf_text)
text <- paste(inception, collapse = " ") %>%
  tolower() %>%
  str_replace_all(pattern = "\\(v.o.\\)", replacement = " ") %>%
  str_replace_all(pattern = "what\'re", replacement = "what are") %>%
  str_replace_all(pattern = "can\'t", replacement = "can not") %>%
  str_replace_all(pattern = "won\'t", replacement = "will not") %>%
  replace_contraction(contraction.key = lexicon::key_contractions,
                      ignore.case = TRUE) %>%
  str_replace_all(pattern = "cont\'d", replacement = " ") %>%
  str_replace_all(pattern = "cut to:\n.*\\n", replacement = " ") %>%
  str_replace_all(pattern = "int.*\\n", replacement = " ") %>%
  str_replace_all(pattern = "ext.*\\n", replacement = " ") %>%
  str_replace_all(pattern = "-", replacement = " ") %>%
  str_replace_all(pattern = "cut to:", replacement = " ") %>%
  str_replace_all(pattern = "\\n", replacement = " ") %>%
  str_replace_all(pattern = "[[:punct:]]", replacement = " ") %>%
  str_replace_all(pattern = "\\s+", replacement = " ") %>%
  str_replace_all(pattern = " s ", replacement = " ") %>%
  str_replace_all(pattern = " m ", replacement = " ") %>%
  ngram(n = 2) %>%
  get.phrasetable() %>%
  separate(ngrams, into = c("word", "word2"), sep = " ") %>%
  arrange(desc(prop)) %>%
  head(50)
  
ggraph(text, layout = "fr") +
  geom_edge_link(mapping = aes(edge_alpha = prop),
                 color = "red", 
                 end_cap = circle(0.05, 'inches'), 
                 edge_width = 1) +
  geom_node_point(color = 'lightblue', size = 2) +
  geom_node_text(aes(label = name))
  


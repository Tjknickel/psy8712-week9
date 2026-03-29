# Script Settings and Resources 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(RedditExtractoR)

# Data Import and Cleaning 
rstats_df <- find_thread_urls(subreddit = "rstats", period = "month")$url %>%
  get_thread_content()
rstats_tbl <-  rstats_df$threads %>%
  select(post = title,
         upvotes = upvotes,
         comments = comments) %>%
  as.tibble()

# Visualization 
(rstats_tbl %>%
    ggplot(aes(upvotes, comments)) +
  geom_point() +
  labs(title = "Number of comments vs. upvotes on r/rstats", x = "Number of upvotes", y = "Number of comments")
) %>%
  ggsave("../figs/fig1.png",., height = 1080, width = 1920, units = "px", dpi = 600) 

# Analysis 
cor.test(rstats_tbl$upvotes, rstats_tbl$comments)

# Publication
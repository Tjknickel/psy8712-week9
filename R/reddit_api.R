# Script Settings and Resources 
# Set the working directory and the necessary library calls in order to run the code in the following sections (tidyverse and RedditExtractoR)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(RedditExtractoR)

# Data Import and Cleaning 
# Imported the data from the r/rstats subreddit using functions within the RedditExtractoR package (find_thread_urls and get_thread_content) in order to extract the relevant information (title of posts, number of upvotes, and number of comments). This information was then stored onto a tibble (from a dataframe) to be used in later sections. 
rstats_df <- find_thread_urls(subreddit = "rstats", period = "month")$url %>%
  get_thread_content()
rstats_tbl <-  rstats_df$threads %>%
  select(post = title,
         upvotes = upvotes,
         comments = comments) %>%
  as.tibble()

# Visualization 
# Created a scatterplot of the number of comments vs. the number of upvotes on the r/rstats subreddit to visualize this relationship using ggplot and saving the resuling plot as a figure (fig1) png file. 
(rstats_tbl %>%
    ggplot(aes(upvotes, comments)) +
  geom_point() +
  labs(title = "Number of comments vs. upvotes on r/rstats", x = "Number of upvotes", y = "Number of comments")
) %>%
  ggsave("../figs/fig1.png",., height = 1080, width = 1920, units = "px", dpi = 600) 

# Analysis 
# Peformed a correlation test between number of upvotes and the number of comments using cor.test in order to extract both the correlation (r-value) and the p-value. The output is saved to a list named "cor_list" in order to be utilized in the following section.
cor_list <- cor.test(rstats_tbl$upvotes, rstats_tbl$comments)
cor_list

# Publication
#"The correlation between upvotes and comments was r(71) = .35, p = .00. This test was statistically significant."
paste("The correlation between upvotes and comments was r(", 
      cor_list$parameter, ") = ", 
      str_remove(round(cor_list$estimate, 2), "^0"), 
      ", p = ", str_remove(formatC(cor_list$p.value, digits = 2, format = "f"), "^0"),
      ". This test ", ifelse(cor_list$p.value < 0.05, "was", "was not"), 
      " statistically significant.", 
      sep = "")

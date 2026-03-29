# Script Setting and Resources 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rvest)
library(httr)

# Data Import and Cleaning 
get_cnbc_section <- function(section_name) {
  url_path <- str_to_lower(section_name)
  if (url_path == "tech") url_path <- "technology"
  target_url <- paste("https://www.cnbc.com", url_path, sep = "/")
  print(paste("Now scraping section:", section_name))
  response <- GET(target_url, user_agent("UMN Researcher knick071@umn.edu"))
  print(paste("Just received data from", response$url))
  Sys.sleep(2)
  if (http_error(response)) {
    return(tibble(headline = NA, source = section_name, length = NA))
  } else {
    headlines <- content(response, as = "parsed") %>%
      html_elements(".Card-title") %>%
      html_text()
    return(tibble(
      headline = headlines,
      source = section_name,
      length = str_count(headline, "\\w+")
    ))
  }
}
sections <- c("Business", "Investing", "Tech", "Politics")
cnbc_list <- list()
for (s in sections) {
  cnbc_list[[s]] <- get_cnbc_section(s)
}
cnbc_tbl <- bind_rows(cnbc_list)

# Visualization
(cnbc_tbl %>% 
    ggplot(aes(source, length)) + 
    geom_boxplot() +
    labs(title = "Headline word count by news section of CNBC", x = "News section", y = "Number of words in headline")
  ) %>%
  ggsave("../figs/fig2.png",., height = 1080, width = 1920, units = "px", dpi = 600)

# Analysis 
cnbc_anova <- aov(length ~ source, data = cnbc_tbl)
summary(cnbc_anova)

# Publication

# Script Setting and Resources 
# Set the working directory and necessary libraries to run the following sections of code
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rvest)
library(httr)

# Data Import and Cleaning 
# A tibble was made with information from 4 sections of the cnbc.com website under the "Business", "Investing", "Tech", and "Politics" sections. The loop goes through each URL specific to the section and extracts the headline, source, and length of the headline and combines the elements into a tibble. The user agent was necessary in order to identify yourself and the content() function was able to reformat the extracted information into an R object. The same CSS was able to work for all sections as they had the same format (.Card-title). Sys.sleep was set to 2 in order to not be detected as a hacker to pause 2 sections between iterations rather than send the requests as fast as possible. 
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
# A boxplot was created to visualize the comparison of headline lengths across the 4 different sources using ggplot, the figure was then saved as a png file under the "figs" subdirectory
(cnbc_tbl %>% 
    ggplot(aes(source, length)) + 
    geom_boxplot() +
    labs(title = "Headline word count by news section of CNBC", x = "News section", y = "Number of words in headline")
  ) %>%
  ggsave("../figs/fig2.png",., height = 1080, width = 1920, units = "px", dpi = 600)

# Analysis 
# An ANOVA was performed to compare the mean length of headlines across the 4 different sources using the aov() function, which was then passed onto the summary() function in order to display the results including the F-value and the p-value. This was then saved as a list object in order to be used for the next section
cnbc_anova <- aov(length ~ source, data = cnbc_tbl)
anova_list <- summary(cnbc_anova)[[1]]

# Publication
# "The results of an ANOVA comparing lengths across sources was F(3, 130) = 1.34, p = .27. This test was not statistically significant."
paste("The results of an ANOVA comparing lengths across sources was F(", 
      anova_list$Df[1], ", ", 
      anova_list$Df[2], ") = ", 
      formatC(anova_list$`F value`[1], digits = 2, format = "f"), 
      ", p = ", 
      str_remove(formatC(anova_list$`Pr(>F)`[1], digits = 2, format = "f"), "^0"),
      ". This test ", 
      ifelse(anova_list$`Pr(>F)`[1] < 0.05, "was", "was not"), 
      " statistically significant.", 
      sep = "")
library(rvest)
library(httr)
library(countrycode)
library(xml2)
library(magrittr)
library(tidytext)
library(textdata)
library(dplyr)
library(ggplot2)
# Task: Scrape the pages linked here: https://www.cia.gov/the-world-factbook/countries/
# Analyze the text on those pages for general positive/negative sentiment
# Next steps: Up to you

countries <- unique(countrycode::countryname_dict$country.name.en)

sentiments <- get_sentiments('afinn')

sentiment_analysis <- function(p_tags, sentiments){
  sentiment_means <- vector(length = length(p_tags))
  iterator = 1
  for (line in p_tags){
    line <- strsplit(line, ' ')
    line <- tibble(word = as.character(sapply(line, function(y) tolower(gsub('[[:space:]]', '', y)))))
    line_sentiments <- line %>% 
      inner_join(sentiments, by = 'word')
    sentiment_means[iterator] = as.numeric(mean(line_sentiments$value, na.rm = TRUE)[[1]])
    iterator = iterator + 1
  }
  return(mean(sentiment_means, na.rm = TRUE))
      # if (iterator > 3){
      #   stop()
      # }

}

grab_country_data <- function(country_name, sentiments){
  country_name <- tolower(gsub(' ', '-', country_name))
  output <- tryCatch(
    {
      p_tags <- read_html(paste0(c('https://www.cia.gov/the-world-factbook/countries/', country_name), collapse = '')) %>%
        html_nodes('p')
      sentiment_means <- sentiment_analysis(html_text(p_tags), sentiments)
      return(sentiment_means)
    },
    error=function(cond) {
      message(paste("URL does not exist:", paste0(c('https://www.cia.gov/the-world-factbook/countries/', country_name))))
      return(NA)
    }
    
  )
  return(output)
}

plot_top_n <- function(countries){
  countries %>% 
    ggplot2::ggplot(aes(Country, Sentiment)) +
    geom_col()
}

countries_df <- data_frame(Country = countries)
average_sentiment <- vector(mode = 'list', length = length(countries))
for (country_index in seq_along(countries)){
  # print(country_index)
  average_sentiment[country_index] <- grab_country_data(countries_df[country_index,1][[1]], sentiments = sentiments)
}
countries_df['Sentiment'] <- unlist(average_sentiment)
countries_df <- countries_df %>%
  arrange(desc(Sentiment))
plot_top_n(top_n(countries_df, 10))
plot_top_n(top_n(countries_df, -10))

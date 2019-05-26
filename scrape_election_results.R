library(tidyverse)
library(rvest)

# retrieve page content - takes either a URL string or a vector of strings which constitute a url (will be collapsed to one string using "/")
get_content <- function(url) {
  if (is.vector(url)) {
    url <- str_c(url, collapse = "/")
  }
  url %>%
    read_html()
}

# parses a character vector formatted like "00.0 %" into double
parse_percentages <- function(x) {
  formatted <- str_replace(x, ",", ".") %>%
    str_replace(" %", "") %>%
    as.numeric()
  formatted
}


# extract result table using xpath and transform into data frame

url_districts <- "https://wahlen.stadt-koeln.de/prod/EUW2019/05315000/html5/Europawahl_40_Uebersicht_stadtteil.html"
page <- get_content(url_districts)

#table_id <- "table285"
#table_xpath <- str_c("//table[@id=", table_id, "']")
# table id changes with each table update, hence use absolute path instead
table_xpath <- "//body/div/table"

result_table <- html_node(page, xpath = table_xpath)
result_df <- html_table(result_table, dec = ",", fill = FALSE, header = TRUE)

election_results <- result_df %>%
  mutate_if(~ any(str_detect(., "%")), parse_percentages) %>%
  mutate(
    Wahlberechtigte = str_replace(Wahlberechtigte, "\\.", "") %>% as.numeric(),
    completeness = Stand %>% (function(x) {
      numerator = str_extract(x, "([0-9]+)") %>% as.double()
      denominator = str_match(x, "von ([0-9]+)")[, 2] %>% as.double()
      ratio = numerator / denominator
      ratio
    })
  ) %>%
  rename(
    Wahlbeteiligung = `Wähler/innen`,
    gueltig = `gültig`,
    GRUENE = `GRÜNE`
  )


# cleanup
rm(list = c("page", "result_table", "result_df"))




### scrape boroughs to districts for Cologne



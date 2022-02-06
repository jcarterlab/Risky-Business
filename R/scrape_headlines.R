library(tidyverse)
library(rvest)
library(stringr)
library(knitr)

# date  the headlines are collected. 
collected <- "20220206"

# pulls the relevant information from the links data frame. 
pull_info <- function(link, object) {
  index <- which(
    str_detect(link, links$Site)
    )
  object <- if(object=="link") {
    pull(links[index,1])
  }
  else if(object=="name") {
    pull(links[index,2])
  }
  else if(object=="country") {
    pull(links[index,3])
  }
  else if(object=="region") {
    pull(links[index,4])
  }
  else if(object=="continent") {
    pull(links[index,5])
  }
  else {
    paste0(
      "'", object, " (", index, ")", "' ", "not found"
      )
  }
  return(object)
}

# reads in the link html text. 
read_link <- function(link) {
  elements <- c(
    "h1", "h2", "h3", "h4", "h5"
    )
  headlines <- read_html(
    link
    )
  html <- lapply(
    elements, html_elements, x = headlines
    )
  text <- unlist(
    lapply(html, html_text)
  )
  on.exit(
    closeAllConnections()
  )
  return(text)
}

# creates a pattern to filter out from the headline text. 
pattern <- c("PRIME\n|\"|#|«|»")

# returns headlines without any extra white space nor 
# the punctuation elements in the pattern above. 
clean_text <- function(text) {
  whitespace <- str_trim(
    text
    )
  clean <- str_replace_all(
    whitespace, pattern, ""
    )
  return(clean)
}

# drops text returning less than three words as this 
# is likely not a headline. 
drop_short_headlines <- function(text) {
  clean_text <- clean_text(
    text
    )
  index <- sapply(
    str_split(text, " "), length
    ) >= 4
  return(text[index])
}

# returns the desired headlines information. 
get_raw_data <- function(link) {
  text <- read_link(
    link
    )
  clean_text <- clean_text(
    text
    )
  final_text <- drop_short_headlines(
    clean_text
    )
  tibble <- final_text %>%
    as_tibble() %>%
    mutate(
      Site = pull_info(link, "link"),
      Name = pull_info(link, "name"),
      Country = pull_info(link, "country"),
      Region = pull_info(link, "region"),
      Continent = pull_info(link, "continent"),
      Collected = collected
      ) %>%
    rename(
      Headline = value
      )
  return(tibble)
}

# error handling to counter potential connection problems. 
get_headline_data <- function(link) {
  counter <- 1L
  result <- tibble()
  repeat{try(query <- get_raw_data(link))
    if(exists("query")) {
      result <- query
      rm(query)
      break
    } else {
      if(counter <= 5L) {
        message("Re-trying connection: attempt ", counter, " of 5.")
        counter <- counter +1L
        Sys.sleep(2)
      } else {
        message("Retry limit reached: connection attempt unsuccessful.")
        break
      }
    }
  }
  return(result)
}

# creates a table of headlines for all given sites. 
get_headlines <- function(links) {
  table <- lapply(
    links, get_headline_data
    )
  headlines <- distinct(
    bind_rows(list(table))
    )
  headlines$Headline <- replace_na(
    headlines$Headline, ""
    )
  return(headlines)
}

# reads in the relvant links data frame. 
read_link <- function(region) {
  links <- read_csv(
    paste0(
      "C:\\Users\\HUAWEI\\Desktop\\Projects\\Risky Business\\data\\Links\\", 
      region, 
      "_links.csv"
      )
    )
  return(links)
}

# reads in the links data frame. 
links <- read_link("africa") %>%
  rbind(read_link("asia")) %>%
  rbind(read_link("europe")) %>%
  rbind(read_link("latin_america")) %>%
  rbind(read_link("mena"))

# creates a data frame of headlines. 
headlines <- get_headlines(links$Site)


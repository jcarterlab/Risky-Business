# cleans the data.
get_cleaned_words <- function(strings) {
  remove_regex <- unlist(
    lapply(strings, str_remove, fixed("\\s([\\w_.,!']+\\s){0,2}"))
  )
  remove_brackets <- unlist(
    lapply(remove_regex, str_replace_all, fixed("(("), "(")
  ) 
  removed_yet_more_brackets <- unlist(
    lapply(remove_brackets, str_sub, 2, -2)
  )
  remove_more_brackets <- unlist(
    lapply(removed_yet_more_brackets, str_replace_all, fixed(")("), "|")
  ) 
  remove_additional_brackets <- unlist(
    lapply(remove_more_brackets, str_replace_all, fixed("|("), "|")
  ) 
  remove_even_more_brackets <- unlist(
    lapply(remove_additional_brackets, str_replace_all, fixed(") h"), "|")
  ) 
  remove_even_even_more_brackets <- unlist(
    lapply(remove_even_more_brackets, str_replace_all, fixed("))"), ")")
  ) 
  cleaned_text <- paste0("|", sapply(
    remove_even_even_more_brackets, paste0, sep = "")
  )
  return(cleaned_text)
}

# gets the words. 
get_tokens <- function(strings) {
  clean <- get_cleaned_words(strings)
  tokens <- unlist(
    str_split(clean, "\\\\b")
  )
  delete_empty_str <- tokens[-length(tokens)]
  clean_tokens <- unlist(
    lapply(delete_empty_str, str_sub, 2)
  )
  clean_tokens <- unlist(
    lapply(clean_tokens, paste0, "\\b")
  )
  return(trimws(clean_tokens))
}

# gets the words for each category. 
get_word_categories <- function(group_no) {
  category_words <- unlist(
    lapply(categories, parse_text, group_no)
  )
  tokens <- unlist(
    lapply(category_words, get_tokens)
  )
  return(tokens)
}

# gets the words for each group. 
get_word_groups <- function() {
  final_tokens <- unlist(
    lapply(c("_1", "_2", "_3", "_4"), get_word_categories)
  )
  return(final_tokens)
}

# final tokens. 
final_tokens <- unique(get_word_groups())
final_tokens

# finds out which words in a headline factored into its score. 
which_words <- function(text) {
  words <- final_tokens
  index <- which(
    str_detect(as_vector(headlines$Headline), text)
  )
  indices <- list()
  for(i in 1:length(words)) {
    indices[i] <- str_detect(str_to_lower(headlines$Headline[index]), words[i])
  }
  which_words <- words[unlist(indices)]
  return(which_words)
}

# gets the strings. 
get_string_categories <- function(group_no) {
  category_strings <- unlist(
    lapply(categories, parse_text, group_no)
  )
  return(category_strings)
}

# gets the strings for each group. 
get_string_groups <- function() {
  final_strings <- unlist(
    lapply(c("_1", "_2", "_3", "_4"), get_string_categories)
  )
  return(final_strings)
}

# final tokens. 
final_strings <- unique(get_string_groups())
final_strings

# finds out which strings in a headline factored into its score. 
which_strings <- function(text) {
  strings <- final_strings
  index <- which(
    str_detect(as_vector(headlines$Headline), text)
  )
  indices <- list()
  for(i in 1:length(strings)) {
    indices[i] <- str_detect(str_to_lower(headlines$Headline[index]), strings[i])
  }
  which_strings <- strings[unlist(indices)]
  return(which_strings)
}


# ANALYSIS ####################################################################

# string
str <- "56-Year-Old Bitola Man Detained over Threats Against"

# which words in a headline factored into the political risk score (prs).
which_words(str)

# which strings in a headline factored into the political risk score (prs).
which_strings(str)



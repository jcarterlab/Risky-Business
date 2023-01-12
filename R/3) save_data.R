# adds the total and category scores to the headlines. 
combined_headlines <- prs(headlines$Headline) %>%
  cbind(headlines[,c(3, 4, 5, 6, 2, 7)]) %>%
  tibble()

# excludes duplicates. 
old_headlines <- read_csv(
  paste0("risk_headlines", ".csv")) %>%
  rbind(read_csv(paste0("falseflag_headlines", ".csv"))) %>%
  rbind(read_csv(paste0("random_headlines", ".csv"))) %>% 
  rbind(read_csv(paste0("previous_scores", ".csv")))

exclude_duplicates <- list()
for(i in 1:nrow(combined_headlines)) {
  exclude_duplicates[i] <- pull(combined_headlines[i,1]) %in% pull(old_headlines[,1]) 
}
non_duplicates <- combined_headlines[unlist(exclude_duplicates)==FALSE,]

# sorts the data frame by the highest PR scores. 
score_index <- order(desc(non_duplicates$Score))
score_ordered <- non_duplicates[score_index,]

# filters out all headlines mentioning Ukraine. 
score_ordered <- score_ordered[str_detect(str_to_lower(score_ordered$Headline), "ukraine")==FALSE,]

# writes data to csv in case scores must be checked at a later date. 
write_csv(score_ordered, "scored_ordered.csv")

# opens data from csv file format. 
score_ordered <- read_csv("scored_ordered.csv")

# lets us view the scored_ordered headlines. 
view(score_ordered)


# 7.2) CREATE LISTS #############################################################

# creates a list of mislabeled headlines. 
mislabeled <- tibble(
  number = c(3,
             4,
             5,
             65,
             148),
  category = c("social_unrest",
               "social_unrest",
               "social_unrest",
               "social_unrest",
               "social_unrest")
)

# creates a index of risk headlines. 
risk_index <- c(9,
                11,
                20,
                22,
                34,
                36,
                48,
                55,
                56,
                73,
                77,
                84,
                113,
                116,
                198)

# 7.3) ADJUST ISSUES ###########################################################

# creates a scores and non-scores list. 
scores <- score_ordered %>%
  filter(Score > 0)
non_scores <- score_ordered %>%
  filter(Score == 0)

# adjusts mislabeled headlines. 
mislabeled_adjust <- function(scores) {
  scores <- scores
  for(i in 1:nrow(mislabeled))
    scores[pull(mislabeled[i,1]),3] <- mislabeled$category[i]
  return(scores)
}
mislabeled_adjusted <- mislabeled_adjust(scores)

# drops false-flag headlines.
falseflag_headlines <- mislabeled_adjusted[-risk_index,][,-2]

# risk headlines.
risk_headlines <- mislabeled_adjusted[risk_index,-2] %>%
  rbind(mislabeled_adjusted[mislabeled$number,-2])


# 7.4) ADD "NONE" CATEGORIES ####################################################

# creates the counts for adding "none" categories. 
count <- nrow(risk_headlines)
falseflag_no <- ceiling(count/2)
random_no <- floor(count/2)

# creates a sample of false flag "none" categories.
falseflag_indices <- sample(nrow(scores[-c(risk_index, mislabeled$number),]), falseflag_no, replace = FALSE)
falseflag_samples <- scores[-c(risk_index, mislabeled$number),][falseflag_indices,][,-2] %>%
  mutate(Category = "none")

# creates a sample of random "none" categories. 
random_indices <- sample(nrow(non_scores), random_no, replace = FALSE)
random_samples <- non_scores[random_indices,][,-2]


# 7.5) SAVE THE DATA #########################################################

# opens, adds to and saves the data frame. 
save_data <- function (name, new_data) {
  open <- read_csv(paste0(name, ".csv"))
  add_new <- open %>%
    rbind(new_data)
  save_data <- write_csv(add_new, paste0(paste0(name, ".csv")))
  return(save_data)
}

# saves the four data frames. 
save_data("previous_scores", scores[,-2])
save_data("risk_headlines", risk_headlines)
save_data("falseflag_headlines", falseflag_samples)
save_data("random_headlines", random_samples)


set.seed(1983)
setwd("C:\\Users\\HUAWEI\\Desktop\\Projects\\Risky-Business\\data")

# 1) ANALYSIS #################################################################

# adds the total and category scores to the headlines. 
combined_headlines <- prs(headlines$Headline) %>%
  cbind(headlines[,c(3, 4, 5, 6, 2, 7)]) %>%
  tibble()

# excludes duplicates. 
old_headlines <- read_csv(
  paste0("risk_headlines", ".csv")
) %>%
  rbind(falseflag_headlines <- read_csv(
    paste0("falseflag_headlines", ".csv")
  )) %>%
  rbind(risk_headlines <- read_csv(
    paste0("random_headlines", ".csv")
  )) %>% 
  rbind(previous_headlines <- read_csv(
    paste0("previous_scores", ".csv")
  ))

all_headlines <- old_headlines[,c(1,3)] %>%
  rbind(combined_headlines[,c(1, 4)])

duplicated_index <- duplicated(all_headlines)
duplicates <- all_headlines[duplicated_index,]

exclude_duplicates <- list()
for(i in 1:nrow(combined_headlines)) {
  exclude_duplicates[i] <- if(mean(str_detect(pull(combined_headlines[i,1]), pull(duplicates[,1]))) >0) FALSE else TRUE 
}
non_duplicates <- combined_headlines[unlist(exclude_duplicates),]

# sorts the data frame by the highest PR scores. 
score_index <- order(desc(non_duplicates$Score))
score_ordered <- non_duplicates[score_index,]
view(score_ordered)


# 2) CREATE LISTS #############################################################

# creates a list of mislabeled headlines. 
mislabeled <- tibble(
  number = c(108,
             95,
             70),
  category = c("social unrest",
               "regime_instability",
               "terrorism")
)

# creates a index of risk headlines. 
risk_index <- c(122,
                115,
                107,
                106,
                103,
                102,
                100,
                96,
                93,
                89,
                88,
                83,
                79,
                68,
                67,
                66,
                65,
                56,
                54,
                53,
                52,
                47,
                38,
                37,
                36,
                31,
                29,
                24,
                22,
                17,
                7,
                5,
                3,
                1)

# 3) ADJUST ISSUES ############################################################

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


# 4) ADD "NONE" CATEGORIES ####################################################

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


# 5) SAVE DATA FRAMES #########################################################

# opens, adds to, excludes duplicates from and saves the data frames. 
save_data <- function (name, new_data) {
  open <- read_csv(
    paste0(name, ".csv")
  )
  add_new <- open %>%
    rbind(
      new_data
    )
  save_data <- write_csv(
    add_new, paste0(
      paste0(name, ".csv")
    )
  )
  return(save_data)
}

save_data("previous_scores", scores[,-2])
save_data("risk_headlines", risk_headlines)
save_data("falseflag_headlines", falseflag_samples)
save_data("random_headlines", random_samples)

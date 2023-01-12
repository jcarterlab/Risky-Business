library(tidyverse)
library(dplyr)
library(jsonlite)
library(knitr)
library(ggplot2)
library(ggthemes)

# reads in the risk headlines. 
data <- read_csv("C:\\Users\\HUAWEI\\Desktop\\Projects\\Risky-Business\\Data\\risk_headlines.csv") %>%
  filter(!Continent == "Europe",
         !Category == "regime_instability")

# gets the z score for a specific set of data points.  
get_z_score <- function(data) {
  mean <- mean(data)
  sigma <- sd(data)
  z_scores <- list()
  for(i in 1:length(data)) {
    z_scores[i] <- (data[i] - mean) / sigma
  }
  return(unlist(z_scores))
}

# get the z-scores for each category of a single continent. 
get_continent_z_scores <- function(continent) {
  data <- data[data["Continent"]==continent,]
  count <- data %>%
    group_by(Category) %>%
    count()
  z_scores <- tibble(z_score = get_z_score(count$n))
  final_data <- count[,1] %>%
    cbind(z_scores) %>%
    mutate(Continent = continent)
  return(final_data)
}

# gets the z-scores for each category by continent.  
z_scores <- rbind_pages(lapply(unique(data$Continent), get_continent_z_scores))

# my personal plot theme for data visualizations. 
my_theme <- theme_economist_white(gray_bg = FALSE) +
  theme(plot.title = element_text(hjust = 0.5,
                                  vjust = 10,
                                  size = 10,
                                  color = "#474747"),
        plot.margin = unit(c(1.5, 1, 1.5, 1), "cm"),
        axis.text = element_text(size = 9,
                                 color = "gray30"),
        axis.text.x=element_text(vjust = -2.5),
        axis.title.x = element_text(size = 9,
                                    color = "gray30",
                                    vjust = -10),
        axis.title.y = element_text(size = 9,
                                    color = "gray30",
                                    vjust = 10),
        legend.direction = "vertical", 
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 11,
                                   color = "gray20"),
        legend.margin=margin(1, -15, 1, 0),
        legend.spacing.x = unit(0.25, "cm"),
        legend.key.size = unit(1, "cm"), 
        legend.key.height = unit(0.75, "cm"),
        strip.text = element_text(hjust = 0.5,
                                  vjust = 1,
                                  size = 10,
                                  color = "#474747"),
        panel.spacing = unit(2, "lines"))

# total bar plot. 
total_data <- data %>%
  group_by(Category, Continent) %>%
  count()

total_data %>%
  ggplot(aes(x=Continent, y=n, fill=Category)) +
  geom_bar(stat = "identity", position = "fill") +
  ggtitle("Total") +
  ylab("Relative Frequency") +
  xlab("") + 
  my_theme +
  theme(axis.text.y=element_blank()) +
  scale_fill_discrete(name = "Risks", labels = c("Military Conflict", 
                                                 "Organized Crime", 
                                                 "Social Unrest",
                                                 "Terrorism"))

# monthly line plot. 
monthly_data <- data %>%
  mutate(Month = round((Collected-20220000), -2)/100) %>%
  group_by(Category, Continent, Month) %>%
  count() %>%
  filter(!Month == 5)
 
monthly_data %>%
  group_by(Month) %>%
  mutate(n = get_z_score(n)) %>%
  ungroup() %>%
  ggplot(aes(x=Month, y=n, color=Category)) +
  geom_smooth(se=F, 
              span = 0.5, 
              size = 0.5) +
  ggtitle("Monthly") +
  ylab("Z-scores") +
  xlab("") + 
  my_theme +
  scale_x_continuous(labels = paste0("0", as.character(unique(monthly_data$Month)))) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_color_discrete(name = "Risks", labels = c("Military Conflict", 
                                                  "Organized Crime", 
                                                  "Social Unrest",
                                                  "Terrorism"))










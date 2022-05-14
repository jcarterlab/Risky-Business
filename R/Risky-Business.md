Risky Business
================
Jack Carter
2/21/2022

## **Summary**

This project uses web scraping, regex and a labeling mechanism to
uncover the relative distribution of different violence risks in the
emerging world. The results include 822 risk headlines from 140 news
sources. Just as Tom Cruise discovered in Risky Business, those seeking
to venture into the unknown could find things get out of hand quickly if
they’re not careful.

 

## Results

The results show a strong risk of military conflict in Europe due to the
Ukraine crisis and a high risk of internal conflict, such as organized
crime, social unrest and terrorism, in regions with weak states like
Africa and Latin America.

![](Risky-Business_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

 

## **Disclaimer**

The data above show only the relative distribution of headlines for each
region, not differences in the total number of headlines. This means we
can only hope to better understand variations in the types of risks in
such regions, not how dangerous they are overall.

 

## **Method**

### **1) Assemble Links Database:**

A database containing over 250 online newspapers in the emerging world
is assembled.

| Africa | Asia | Europe | Latin America | MENA |
| -----: | ---: | -----: | ------------: | ---: |
|     34 |   77 |     56 |            45 |   55 |

 

### **2) Scrape Headlines:**

The links in the database is scraped several times a week from 17
January 2022 to present.

—EXAMPLE CODE SNIPET—

``` r
# reads in the link html text. 
read_link <- function(link) {
  elements <- c("h1", "h2", "h3", "h4", "h5")
  headlines <- read_html(link)
  html <- lapply(elements, html_elements, x = headlines)
  text <- unlist(lapply(html, html_text))
  on.exit(closeAllConnections())
  return(text)
}
```

 

### **3) Regex:**

A series of regex strings, including 1) keywords, 2) actors and actions,
3) and hyperbolic situations, are combined to calculate the scores of
four different kinds of violence.

—EXAMPLE CODE SNIPET—

``` r
# strong capture words.
strong_capture_words <-  paste0(
  "(",
  capture <-  "captur(e|es|ed|ing)\\b|",
  hijack <-  "hijac(k|ks|ked|king)\\b|",
  hold_prisoner <-  "h(old|olds|eld|olding) (hostag(e|es)|to ransom)\\b|",
  take_prisoner <-  "t(ake|akes|ook|aken|aking) (prisoner|hostage)\\b|",
  abduct <-  "abduc(t|ts|ted|ting)\\b|",
  kidnap <-  "kidna(p|ps|ped|ping)\\b",
  ")"
)
```

 

### **4) Labeling:**

The headlines are assigned one of the four violence type labels,
including military conflict, organized crime, social unrest and
terrorism.

—EXAMPLE CODE SNIPET—

``` r
# determines the main category under which the headlines fall. 
assign_category <- function(scores) {
  data <- tibble()
  for(i in 1:nrow(scores)) {
    data[i,1] <- if(sum(scores[i,1:5])==0) "None"
    else {
      categories[max(which(scores[i,1:5]==max(scores[i,1:5])))]
    } 
  }
  return(data)                
}
```

 

### **5) Check & Re-assign:**

The top 100 scoring headlines are checked each time the data is scraped,
with only those deemed genuine violence risks kept and those mislabeled
re-assigned.

—EXAMPLE CODE SNIPET—

``` r
# creates a list of mislabeled headlines. 
mislabeled <- tibble(
  number = c(2),
  category = c("organized_crime")
)

# adjusts mislabeled headlines. 
mislabeled_adjust <- function(scores) {
  scores <- scores
  for(i in 1:nrow(mislabeled))
    scores[pull(mislabeled[i,1]),3] <- mislabeled$category[i]
  return(scores)
}
mislabeled_adjusted <- mislabeled_adjust(scores)
```

 

## **Sources**

  - Chong (2021)
    <https://towardsdatascience.com/regular-expressions-clearly-explained-with-examples-822d76b037b4>

  - The Economist (2021)
    <https://www.economist.com/leaders/2021/07/31/unrest-and-economic-underperformance-stalk-the-emerging-world>

  - UN (2020) <https://www.un.org/en/un75/new-era-conflict-and-violence>

  - World Bank (2022)
    <https://www.worldbank.org/en/topic/fragilityconflictviolence/overview#1>

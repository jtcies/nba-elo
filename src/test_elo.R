library(tidyverse)
library(elo)

set.seed(2018)

ex <- data.frame(
    event = c(1:15),
    group1 = c(rep("a", 5), rep("b", 5), rep("c", 5)),
    val1 = round(runif(15, 0, 100), 0),
    group2 = c(rep("b", 5), rep("a", 5), rep("b", 5)),
    val2 = round(runif(15, 0, 100), 0),
    stringsAsFactors = FALSE
  )

teams <- data.frame(
  event = 1,
  group = c("a", "b", "c"),
  elo = 1500,
  stringsAsFactors = FALSE
  
)

running_elo <- function() {
  for(i in seq_len(nrow(ex))) {
    
    match <- ex[i, ]
    
    elo1 <- teams[teams$group == match$group1, "elo"][[1]]
    elo2 <- teams[teams$group == match$group2, "elo"][[1]]
    
    elo_update <- elo.calc(
      score(match["val1"], match["val2"]),
      elo1,
      elo2,
      k = 20
    )
    
    new_elo <- elo_update %>% 
      gather() %>% 
      rename(elo = value) %>% 
      mutate(
        group = c(match$group1, match$group2),
        event = match$event
      ) %>% 
      select(event, group, elo)
      
    teams <- teams %>% 
      bind_rows(new_elo) %>% 
      arrange(desc(event))
    }
  return(teams)
}

running_elo()
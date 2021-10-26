# Author: Tyler Otto
# Title: 2021  NFL Scorigami
# Date: 10/26/2021 #

# Step 1: Load the Tidyverse, Rvest (html), janitor (less mess)
library(tidyverse)
library(rvest)
library(janitor)

# Step 2: Read in Pro Football Reference table from html link
pro_football <- read_html("https://www.pro-football-reference.com/boxscores/game-scores.htm")

pf_tables <- pro_football %>%
  html_table(fill = TRUE)

nfl_game_scores <- pf_tables[[1]]

# Step 3: Extract date from the 'last game' column

###  "September 19, 2021" <- "Chicago Bears vs. Cincinnati Bengals, September 19, 2021"

nfl_game_scores <- nfl_game_scores %>%
  clean_names() %>% 
  mutate(game_date = str_trim(str_split_fixed(last_game, ",", 2)[,2]))

#Step 4: Extract the url from the table for dynamic dashboard 

urls <- pro_football %>%
  html_nodes("a") %>%
  html_attr("href") %>% 
  as_tibble() %>% 
  filter(str_detect(value,"/boxscores/game_scores_find.cgi\\?p")) %>% 
  mutate(url = paste0('https://www.pro-football-reference.com',value)) %>% 
  select(url)

nfl_game_scores <- bind_cols(nfl_game_scores,urls)


# Step 5: Cross join for all possible scoring scenarios and rule out impossible scores
pts_w <- seq(0,73,1)
pts_l <- seq(0,51,1)

combinations <- expand.grid(pts_w = pts_w, pts_l = pts_l) %>% 
  as_tibble() 

nfl_game_scores_final <- left_join(combinations, nfl_game_scores, by = c('pts_w','pts_l')) %>% 
  mutate(impossible_score = case_when(pts_w < pts_l ~ "yes",
    pts_w <=5 & pts_l == 1 ~ "yes",
    pts_w == 7 & pts_l == 1 ~ "yes",
    pts_w == 1 & pts_l == 1 ~ "yes",
    pts_w == 1 & pts_l == 0 ~ "yes",
      TRUE ~ 'no')
         )

# Step 6: Write the final dataset to a csv for Tableau
write_csv(nfl_game_scores_final,'nfl_game_scores.csv')



############

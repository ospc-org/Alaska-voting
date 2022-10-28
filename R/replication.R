library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)
raw <- read_csv("../data/Alaska_08162022_HouseofRepresentativesSpecial.csv")

raw_wide <- raw %>% 
  mutate(error_flag1 = case_when(rank1 == rank2 & rank1 != "skipped" & rank1 != "Write-in" ~ 1,
                               rank1 == rank3 & rank1 != "skipped" & rank1 != "Write-in" ~ 1,
                               rank1 == rank4 & rank1 != "skipped" & rank1 != "Write-in" ~ 1,
                               rank2 == rank3 & rank2 != "skipped" & rank2 != "Write-in" ~ 1,
                               rank2 == rank4 & rank2 != "skipped" & rank2 != "Write-in" ~ 1,
                               rank3 == rank4 & rank3 != "skipped" & rank3 != "Write-in" ~ 1,
                               TRUE ~ 0)) %>%
  filter(error_flag1 == 0) %>%
  mutate(error_flag2 = case_when(rank1 == "overvote" ~ 1,
                                 rank2 == "overvote" ~ 1,
                                 rank3 == "overvote" ~ 1,
                                 rank4 == "overvote" ~ 1,
                                 TRUE ~ 0)) %>%
  filter(error_flag2 == 0) %>%
  select(-error_flag1, -error_flag2)

# head-to-head

candidates <- c("Begich, Nick", "Peltola, Mary S.", "Palin, Sarah")

raw_long <- raw_wide %>% select(ballotID, rank1:rank4) %>%
  pivot_longer(!ballotID, names_to = "rank", values_to = "candidate") %>%
  filter(!(candidate %in% c("skipped", "overvote", "Write-in")))

raw_list <- raw_long %>% group_split(ballotID)

h2h_list <- lapply(raw_list, function(x) {
  xx <- list()
  ranked <- x %>% pull(candidate)
  excluded <- candidates[!(candidates %in% ranked)]
  
  begich_rank <- ifelse("Begich, Nick" %in% ranked, which(ranked == "Begich, Nick"), 5)
  peltola_rank <- ifelse("Peltola, Mary S." %in% ranked, which(ranked == "Peltola, Mary S."), 5)
  palin_rank <- ifelse("Palin, Sarah" %in% ranked, which(ranked == "Palin, Sarah"), 5)
  
  xx$begich_peltola <- case_when(begich_rank < peltola_rank ~ 1,
                                 peltola_rank < begich_rank ~ -1,
                                 TRUE ~ 0)

  xx$begich_palin <- case_when(begich_rank < palin_rank ~ 1,
                               palin_rank < begich_rank ~ -1,
                                 TRUE ~ 0)
  
  return(xx)
  })

# BEGICH > PELTOLA
lapply(h2h_list, function(x) x$begich_peltola) %>% unlist %>% table
lapply(h2h_list, function(x) x$begich_peltola) %>% unlist %>% sum

# BEGICH > PALIN
lapply(h2h_list, function(x) x$begich_palin) %>% unlist %>% table
lapply(h2h_list, function(x) x$begich_palin) %>% unlist %>% sum

# Borda Count

borda_list <- lapply(raw_list, function(x) {
  xx <- list()
  ranked <- x %>% pull(candidate)
  excluded <- candidates[!(candidates %in% ranked)]
  
  xx$begich_points <- ifelse("Begich, Nick" %in% ranked, which(rev(ranked) == "Begich, Nick") + length(excluded) - 1, 0)
  xx$peltola_points <- ifelse("Peltola, Mary S." %in% ranked, which(rev(ranked) == "Peltola, Mary S.") + length(excluded) - 1, 0)
  xx$palin_points <- ifelse("Palin, Sarah" %in% ranked, which(rev(ranked) == "Palin, Sarah") + length(excluded) - 1, 0)
  
  return(xx)
})

# BEGICH POINTS
lapply(borda_list, function(x) x$begich_points) %>% unlist %>% sum
# PELTOLA POINTS
lapply(borda_list, function(x) x$peltola_points) %>% unlist %>% sum
# PALIN POINTS
lapply(borda_list, function(x) x$palin_points) %>% unlist %>% sum


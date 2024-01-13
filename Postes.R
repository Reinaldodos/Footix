
Europeens = 
  bind_rows(C1, C3, C4) %>%
  filter(Season_End_Year == 2023) %>% 
  select(Home, Away) %>% 
  gather(key = TOTO, value = Team) %>% 
  distinct(Team)

Europe = 
  Vivier$Squad %>% unique() %>%
  purrr::set_names() %>%  
  map(.f = grep, x = Europeens$Team, value = TRUE) %>% 
  compact() %>% names()

require(magrittr)
# Europe %<>% setdiff("Nantes")

Selection = 
  Vivier %>% 
  filter(Squad %in% Europe) 

Selection %>% 
  select(Player, Squad, Comp, TmPos) %>% 
  split(f = .$TmPos)


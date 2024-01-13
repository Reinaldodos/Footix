Rank_clubs = 
  FB_stats$playing_time %>% 
  mutate(Matches = Starts_Starts+Subs_Subs+unSub_Subs,
         Temps_de_jeu = Min_Playing.Time / Matches) %>%
  group_by(Comp, Squad) %>% 
  summarise(
    Score = weighted.mean(x = xGplus_per__minus_90_Team.Success..xG,
                          w = Temps_de_jeu, na.rm = T)) %>%
  data.table::data.table() %>% 
  arrange(Comp, -Score) 

CUT = 
  Rank_clubs %>% 
  group_by(Comp) %>% 
  top_n(n = 4, wt = Score) %>% 
  filter(Comp != "Ligue 1") %>%
  ungroup() %>% 
  top_n(n = 1, wt = -Score) %>% 
  pull(Score)

Selection =
  Rank_clubs %>% 
  group_by(Comp) %>%
  filter(Score >= CUT) %>%
  # filter(Score >= 0.19) %>%
  # filter(
  #   Comp == "Ligue 1" & Score > max(Score) / 2 |
  #   Comp != "Ligue 1" & Score > max(Score) / 3) %>%
  data.table::data.table() 

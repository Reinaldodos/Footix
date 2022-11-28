pacman::p_load(tidyverse, worldfootballR)

YEAR = 2023

Correpondance = worldfootballR::player_dictionary_mapping()

FB_stats =
  c(
    "standard",
    "shooting",
    "passing",
    "passing_types",
    "gca",
    "defense",
    "possession",
    "playing_time",
    "misc",
    "keepers",
    "keepers_adv"
  ) %>%
  purrr::set_names() %>%
  map(
    .f = worldfootballR::load_fb_big5_advanced_season_stats,
    season_end_year = YEAR,
    team_or_player = "player"
  )

Vivier =
  FB_stats$playing_time %>% 
  left_join(y = Correpondance,
            by = c("Url"= "UrlFBref")) %>% 
  filter(Nation == "FRA") %>% 
  arrange(-xGplus_per__minus_90_Team.Success..xG)

Choix = 
  Vivier %>% 
  filter(Starts_Starts > unSub_Subs) %>%
  filter(Mn_per_MP_Playing.Time > 30) %>%
  drop_na(TmPos) %>% 
  split(x = .$Player, f = .$TmPos)

FILTER <- function(Liste, N) {
  Vivier %>%
    filter(Player %in% Liste) %>%
    top_n(n = N, 
          wt = xGplus_per__minus_90_Team.Success..xG) %>% 
    select(Player, Age, Squad, Comp) %>% 
    return()
}

Vivier %>% 
  count(Comp, Squad)


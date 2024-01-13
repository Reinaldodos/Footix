pacman::p_load(tidyverse, worldfootballR, lubridate)

YEAR = today() %>% year()

Correpondance =
  worldfootballR::player_dictionary_mapping() %>%
  mutate(
    UrlFBref = case_match(
      .x = UrlFBref,
      "https://fbref.com/en/players/7d9397f8/Khephren-Thuram-Ulie"  ~ "https://fbref.com/en/players/7d9397f8/Khephren-Thuram",
      .default = UrlFBref
    )
  )

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
    season_end_year = 2024,
    team_or_player = "player"
  )

C1 = worldfootballR::load_match_comp_results(comp_name = "UEFA Champions League")
C3 = worldfootballR::load_match_comp_results(comp_name = "UEFA Europa League")
C4 = worldfootballR::load_match_comp_results(comp_name = "UEFA Europa Conference League")

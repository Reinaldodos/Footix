pacman::p_load(tidyverse, worldfootballR, lubridate)

Vivier =
  FB_stats$playing_time %>% 
  filter(Nation == "FRA") %>%
  mutate(Matches = Starts_Starts+Subs_Subs+unSub_Subs,
         Temps_de_jeu = Min_Playing.Time / Matches) %>%
  filter(Temps_de_jeu > 45,
         MP_Playing.Time > 3) %>%
  left_join(y = Correpondance,
            by = c("Url" = "UrlFBref")) %>%
  arrange(-xGplus_per__minus_90_Team.Success..xG) %>%
  drop_na(TmPos) %>% 
  tidyr::separate(col = Age,
                  into = c("Years", "Days"),
                  sep = "-", 
                  remove = FALSE, convert = TRUE) %>% 
  mutate(Age = dyears(Years) + ddays(Days))

FILTER <- function(Liste, N) {
  Vivier %>%
    filter(Player %in% Liste) %>%
    top_n(n = N, 
          wt = xGplus_per__minus_90_Team.Success..xG) %>%
    arrange(-xGplus_per__minus_90_Team.Success..xG) %>% 
    select(Player, Age, Squad, Comp) %>% 
    return()
}
fetch_time <- function(input) {
  input %<>% 
    janitor::clean_names() %>% 
    discard(.p = ~ any(is.na(.))) 
  
  Time = input[names(input)[ncol(input)]] %>% flatten_chr()
  cbind.data.frame(Poste=input$pos,
                   Min=Time %>% str_remove_all(pattern = "'") %>%
                     as.numeric()) %>% 
    drop_na() %>% 
    return()
}

safe_fetch = safely(fetch_time)

Get_Postes <- function(test) {
  require(rvest)
  require(magrittr)
  test %<>% 
    str_replace_all(pattern = "profil",
                    replacement = "leistungsdaten") %>% 
    str_c(., "/plus/0?saison=2023")
  Donnees = test %>% read_html() 
  Donnees %<>% html_table() %>% compact()
  
  
  input =
    Donnees %>% 
    map(safe_fetch) %>% 
    transpose()
  
  input$result %>% bind_rows() %>% 
    filter(nchar(Poste) > 0) %>%
    group_by(Poste) %>% 
    summarise_at(.vars = "Min", .funs = sum) %>% 
    return()
}


Europeens = 
  # C1 %>%
  bind_rows(C1, C3) %>%
  # bind_rows(C1, C3, C4) %>%
  filter(Season_End_Year == 2024) %>% 
  select(Home, Away) %>% 
  gather(key = TOTO, value = Team) %>% 
  distinct(Team)

Europe = 
  FB_stats$playing_time$Squad %>% unique() %>%
  purrr::set_names() %>%  
  map(.f = grep, x = Europeens$Team, value = TRUE) %>% 
  compact() %>% names()

require(magrittr)

pacman::p_load(furrr, magrittr)
plan(strategy = multisession, workers = availableCores())
Vivier %<>%
# Selection %<>%
  mutate(Postes = future_map(.x = UrlTmarkt,
                             .f = Get_Postes)) 
plan(sequential)


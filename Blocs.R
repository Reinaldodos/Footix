pacman::p_load(tidygraph, igraph)

source(file = "Fetch data.R", echo = TRUE, encoding = "UTF-8")
source(file = "EdF.R", echo = TRUE, encoding = "UTF-8")
source(file = "Clubs.R", echo = TRUE, encoding = "UTF-8")

Get_Blocs <- function(Selection) {
  Postes = 
    Selection %>%
    # filter(Squad %in% Europe) %>%
    select(Player, Squad, Postes) %>% 
    unnest(cols = c(Postes)) 
  
  # Postes %>%
  #   group_by(Poste) %>%
  #   filter(Min >= max(Min, na.rm = TRUE) / 3) %>%
  #   ungroup() %>% 
  #   select(Player, Poste) %>% 
  #   tidygraph::as_tbl_graph() %>% 
  #   igraph::plot.igraph()
  
  Blocs = 
    Postes %>% 
    select(Player, Poste, weight = Min) %>% 
    tidygraph::as_tbl_graph(directed = T) %>% 
    igraph::cluster_walktrap() %>% 
    igraph::communities()
  
  Get_noms <- function(bloc) {
    bloc[bloc %in% Postes$Poste] %>% paste(collapse = "/") %>% 
      return()
  }
  
  names(Blocs) = Blocs %>% map(Get_noms) %>% flatten_chr()
  
  Blocs %>% 
    map(.f = FILTER, N = 50) %>% 
    bind_rows(.id = "Poste") %>%  
    semi_join(y = Postes, by = "Player") %>% 
    distinct(Player, Comp, Squad, Poste) %>% 
    left_join(y = Rank_clubs,
              by = join_by(Comp, Squad)) %>% 
    arrange(-Score) %>% 
    return()
}

Vivier %>%
  semi_join(y = Selection) %>%
  Get_Blocs() %>%
  # filter(Squad %in% Europe) %>%
  group_split(Poste)
  split(x = .$Player, f = .$Poste) %>%
  map(.f = unique)

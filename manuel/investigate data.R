# Clean data provided by https://github.com/williamorim/pokemon. No cleaning was necessary.

# install.packages("pokemon")

# libraries
library(pokemon)
library(dplyr)
library(tidyr)
library(tibble)
library(plotly)
library(purrr)

pokemon = pokemon


#--- Helper: build a sunburst table for a given generation ---------------------
prep_sun_df <- function(pokemon, gen) {
  df <- pokemon %>%
    filter(generation_id == gen) %>%
    select(id, pokemon, type_1, type_2) %>%
    mutate(type_2 = replace_na(type_2, "None")) %>%
    count(type_1, type_2, name = "n_pair") %>%
    group_by(type_1) %>%
    mutate(n_type1 = sum(n_pair)) %>%
    ungroup()
  
  root_label <- paste0("Generation ", gen)
  
  parents <- df %>%
    distinct(type_1, n_type1) %>%
    transmute(
      id     = type_1,
      label  = type_1,
      parent = root_label,
      value  = n_type1
    )
  
  children <- df %>%
    transmute(
      id     = paste(type_1, type_2, sep = " | "),
      label  = type_2,
      parent = type_1,
      value  = n_pair
    )
  
  bind_rows(
    tibble(id = root_label, label = root_label, parent = "", value = sum(parents$value)),
    parents,
    children
  )
}

#--- Build one trace per generation -------------------------------------------
gens <- sort(unique(pokemon$generation_id))
sun_dfs <- setNames(lapply(gens, function(g) prep_sun_df(pokemon, g)), gens)

p <- plot_ly()
for (i in seq_along(gens)) {
  g  <- gens[i]
  df <- sun_dfs[[i]]
  
  p <- add_trace(
    p, data = df,
    type = "sunburst",
    ids = ~id, labels = ~label, parents = ~parent, values = ~value,
    branchvalues = "total",
    maxdepth = -1,
    hovertemplate = "%{label}<br>%{value} Pokémon<extra></extra>",
    visible = (i == 1)  # show only the first generation initially
  )
}

#--- Dropdown to toggle visibility --------------------------------------------
buttons <- lapply(seq_along(gens), function(i) {
  list(
    method = "update",
    args = list(
      list(visible = seq_along(gens) == i),                # which trace is visible
      list(title   = paste("Pokémon types — Generation", gens[i]))
    ),
    label = paste("Gen", gens[i])
  )
})

p <- layout(
  p,
  title = paste("Pokémon types — Generation", gens[1]),
  updatemenus = list(list(
    type = "dropdown",
    active = 0,
    buttons = buttons,
    x = 0.02, y = 1.08, xanchor = "left"
  )),
  margin = list(t = 60, l = 10, r = 10, b = 10)
)

p


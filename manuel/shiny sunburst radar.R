# app.R
# Shiny app: Pokémon type sunbursts by generation + radar plot and info panel
# Data source: {pokemon} package (https://github.com/williamorim/pokemon)

# ---- packages ----
# install.packages(c("shiny","plotly","dplyr","tidyr","tibble","purrr","pokemon"))
library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(pokemon)
library(stringr)

# ---- data ----
data("pokemon", package = "pokemon")

# numeric stats to show on radar
stat_cols <- c("hp", "attack", "defense", "special_attack", "special_defense", "speed")
# (optionally) include base_experience if you want it on the radar as well:
# stat_cols <- c("base_experience","hp","attack","defense","special_attack","special_defense","speed")

# normalize each stat to 1–100 for radar
pokemon_norm <- pokemon %>%
  mutate(across(all_of(c("base_experience", stat_cols)), ~ {
    rng <- range(.x, na.rm = TRUE)
    # protect against zero range
    if (diff(rng) == 0) return(rep(50, length(.)))
    ((.x - rng[1]) / max(1e-9, (rng[2] - rng[1]))) * 99 + 1
  }, .names = "{.col}_norm"))

# ---- helper: build a sunburst table for a given generation ----
prep_sun_df <- function(pokemon, gen) {
  df <- pokemon %>%
    filter(generation_id == gen) %>%
    select(id, pokemon, type_1, type_2) %>%
    mutate(type_2 = tidyr::replace_na(type_2, "None")) %>%
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

# ---- UI ----
ui <- fluidPage(
  tags$head(tags$title("Pokémon — Types, Stats & Details")),
  titlePanel("Pokémon — Types, Stats & Details"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # Multi-select generations controls the Pokémon dropdown
      selectizeInput(
        "gens", "Generation(s):",
        choices  = sort(unique(pokemon$generation_id)),
        selected = sort(unique(pokemon$generation_id))[1],
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      # Pokémon dropdown is populated from the selected generations
      selectizeInput(
        "poke", "Pokémon:",
        choices = NULL, multiple = FALSE
      ),
      helpText("The Pokémon list is filtered by the selected generation(s).")
    ),
    mainPanel(
      width = 9,
      # Sunburst stays as a top visualization
      plotlyOutput("sunburst", height = "650px"),
      br(),
      fluidRow(
        column(
          width = 6,
          h4("Stats Radar"),
          plotlyOutput("radar", height = "520px")
        ),
        column(
          width = 6,
          h4("Information"),
          uiOutput("poke_info")
        )
      )
    )
  )
)

# ---- server ----
server <- function(input, output, session) {
  # Precompute sunburst data per generation for responsiveness
  gens    <- sort(unique(pokemon$generation_id))
  sun_dfs <- setNames(lapply(gens, function(g) prep_sun_df(pokemon, g)), gens)
  
  # --- populate Pokémon dropdown based on selected generations ---
  observeEvent(input$gens, {
    req(input$gens)
    pool <- pokemon_norm %>%
      filter(generation_id %in% input$gens) %>%
      arrange(pokemon) %>%
      pull(pokemon) %>%
      unique()
    
    updateSelectizeInput(session, "poke",
                         choices = pool,
                         selected = if (length(pool)) pool[1] else NULL,
                         server = TRUE
    )
  }, ignoreInit = FALSE)
  
  # --- selected row reactive ---
  selected_row <- reactive({
    req(input$poke)
    pokemon_norm %>% filter(.data$pokemon == input$poke) %>% slice(1)
  })
  
  # --- sunburst (unchanged logic; choose generation via internal dropdown) ---
  output$sunburst <- renderPlotly({
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
        visible = (i == 1)  # initial generation
      )
    }
    
    buttons <- lapply(seq_along(gens), function(i) {
      list(
        method = "update",
        args = list(
          list(visible = seq_along(gens) == i),
          list(title   = paste("Pokémon types — Generation", gens[i]))
        ),
        label = paste("Gen", gens[i])
      )
    })
    
    layout(
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
  })
  
  # --- radar plot of stats for the selected Pokémon ---
  output$radar <- renderPlotly({
    row <- selected_row()
    req(nrow(row) == 1)
    
    # Prepare long-format normalized stats
    cats <- stat_cols
    vals <- as.numeric(row[1, paste0(cats, "_norm"), drop = TRUE])
    
    # close the polygon by repeating the first point
    theta <- c(cats, cats[1])
    r     <- c(vals, vals[1])
    
    plot_ly(
      type = "scatterpolar",
      mode = "lines",
      fill = "toself"
    ) |>
      add_trace(
        r = r,
        theta = theta,
        name = row$pokemon[1],
        hovertemplate = paste0(
          "<b>", row$pokemon[1], "</b><br>",
          "%{theta}: %{r:.0f}/100",
          "<extra></extra>"
        )
      ) |>
      layout(
        showlegend = FALSE,
        polar = list(
          radialaxis = list(range = c(0, 100), dtick = 20, tickfont = list(size = 10)),
          angularaxis = list(tickfont = list(size = 10))
        ),
        margin = list(t = 20, l = 20, r = 20, b = 20)
      )
  })
  
  # --- information panel for the selected Pokémon ---
  output$poke_info <- renderUI({
    row <- selected_row()
    req(nrow(row) == 1)
    
    # Helper to conditionally extract fields if they exist
    get_if <- function(df, nm, fmt = identity) {
      if (nm %in% names(df) && !is.na(df[[nm]][1])) fmt(df[[nm]][1]) else NULL
    }
    
    # Build key-value items
    items <- list(
      list("Name", row$pokemon[1]),
      list("ID", row$id[1]),
      list("Generation", row$generation_id[1]),
      list("Primary type", row$type_1[1]),
      list("Secondary type", ifelse(is.na(row$type_2[1]), "None", row$type_2[1])),
      list("Base experience", get_if(row, "base_experience")),
      list("HP", row$hp[1]),
      list("Attack", row$attack[1]),
      list("Defense", row$defense[1]),
      list("Sp. Attack", row$special_attack[1]),
      list("Sp. Defense", row$special_defense[1]),
      list("Speed", row$speed[1]),
      # Common optional columns in some datasets:
      list("Height (m)", get_if(row, "height_m")),
      list("Weight (kg)", get_if(row, "weight_kg")),
      list("Legendary", get_if(row, "is_legendary", function(x) ifelse(isTRUE(x), "Yes", ifelse(isFALSE(x), "No", NULL))))
    )
    
    # Abilities if present (could be separate columns or a single field)
    ability_fields <- c("abilities", "ability_1", "ability_2", "ability_hidden")
    present_abilities <- intersect(ability_fields, names(row))
    if (length(present_abilities) > 0) {
      vals <- row[1, present_abilities, drop = TRUE] |> unlist(use.names = FALSE)
      vals <- vals[!is.na(vals) & vals != ""]
      if (length(vals) > 0) {
        items <- append(items, list(list("Abilities", paste(unique(vals), collapse = ", "))))
      }
    }
    
    # Render as a compact definition list
    tags$div(
      tags$style(HTML("
      .kv { display:flex; gap:8px; margin-bottom:6px; }
      .kv .k { min-width:140px; font-weight:600; }
      ")),
      lapply(items, function(kv) {
        if (is.null(kv[[2]])) return(NULL)
        tags$div(class = "kv", tags$div(class = "k", kv[[1]]), tags$div(class = "v", kv[[2]]))
      })
    )
  })
}

shinyApp(ui, server)

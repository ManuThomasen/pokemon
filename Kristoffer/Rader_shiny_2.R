library(pokemon)

# app.R
# Packages ---------------------------------------------------------------
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(stringr)
library(scales)

# ---- Stat definitions used everywhere ---------------------------------
stat_cols   <- c("hp","attack","defense","special_attack","special_defense","speed")
stat_labels <- c("HP","Attack","Defense","Sp. Atk","Sp. Def","Speed")


# ---- Helper: build a radar ggplot and convert to plotly ---------------
make_radar <- function(df, stat_cols, title = NULL) {
  stopifnot(nrow(df) >= 1)
  
  norm_max <- getOption("radar_norm_max")
  if (is.null(norm_max) || !is.numeric(norm_max)) stop("Normalization maxima not set.")
  
  p <- plotly::plot_ly(
    type = "scatterpolar",
    mode = "lines+markers",
    hoverinfo = "text"
  )
  
  # Optional: set your own custom colors for consistency
  custom_colors <- c("#E74C3C", "#3498DB")  # Red and blue for A and B
  
  for (i in 1:nrow(df)) {
    long <- df[i, ] %>%
      tidyr::pivot_longer(dplyr::all_of(stat_cols), names_to = "stat", values_to = "value_raw") %>%
      dplyr::mutate(
        stat  = factor(stat, levels = stat_cols, labels = stat_labels),
        denom = unname(norm_max[as.character(stat)]),
        value = pmin(value_raw / denom, 1)
      ) %>%
      dplyr::arrange(stat)
    
    long_closed <- dplyr::bind_rows(long, dplyr::slice(long, 1))
    
    hovertext <- with(
      long_closed,
      paste0(
        "<b>", df$pokemon[i], "</b><br>",
        stat, ": ", value_raw,
        " (", scales::percent(value, accuracy = 1), " of max)"
      )
    )
    
    # Use color from your dataset or your custom palette
    # col_hex <- if (!is.na(df$color_1[i])) df$color_1[i] else "#1f77b4"
    col_hex <- custom_colors[i]
    
    p <- p %>%
      plotly::add_trace(
        r = long_closed$value,
        theta = as.character(long_closed$stat),
        text = hovertext,
        name = df$pokemon[i],
        line = list(color = col_hex),
        marker = list(color = col_hex),
        fillcolor = plotly::toRGB(col_hex, alpha = 0.35),
        fill = "toself"
      )
  }
  
  p %>%
    plotly::layout(
      title = list(text = title, x = 0.5, xanchor = "center"),
      polar = list(
        radialaxis = list(range = c(0, 1), showticklabels = FALSE, ticks = ""),
        angularaxis = list(direction = "clockwise")
      ),
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.2,
        yanchor = "top"
      ),
      margin = list(l = 50, r = 50, t = 50, b = 80)
    ) %>%
    plotly::config(responsive = TRUE)
  
}


# ---- UI ----------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Pokémon Radar Comparison"),
  
  tags$head(
    tags$style(HTML("
    .no-pad { padding-left: 0 !important; padding-right: 0 !important; }
    .overflow-visible { overflow: visible; }

    /* Crucial: let Plotly's SVG draw outside its box (prevents clipped labels) */
    .js-plotly-plot .main-svg { overflow: visible !important; }
  "))
  )
  ,
  
  # Top filter bar (full width)
  wellPanel(
    fluidRow(
      column(3, uiOutput("generation_ui")),  # multi-select dropdown
      column(3, uiOutput("type_ui")),        # multi-select dropdown
      column(3, uiOutput("pokeA_ui")),       # single-select dropdown
      column(3, uiOutput("pokeB_ui"))        # single-select dropdown
    )
  ),
  
  fluidRow(
    column(width = 2, uiOutput("infoA")),
    column(
      width = 8,
      div(class = "overflow-visible",
          plotlyOutput("radarCombined", height = "500px", width = "100%"))
    ),
    column(width = 2, uiOutput("infoB"))
  )
)

# ---- Server ------------------------------------------------------------
server <- function(input, output, session) {
  
  # ---- Data prep and global normalization maxima ----------------------
  # Ensure required columns exist
  req_cols <- c(
    "pokemon","generation_id","type_1","type_2","height","weight","base_experience",
    "hp","attack","defense","special_attack","special_defense","speed",
    "egg_group_1","egg_group_2","url_image","url_icon","color_1","color_2","color_f"
  )
  missing <- setdiff(req_cols, names(pokemon))
  if (length(missing)) {
    stop("pokemon data.frame is missing columns: ", paste(missing, collapse = ", "))
  }
  
  # Compute global maxima and store as a named *numeric vector*
  norm_tbl <- pokemon %>%
    summarise(across(all_of(stat_cols), \(x) max(x, na.rm = TRUE)))
  
  norm_max <- setNames(
    as.numeric(norm_tbl[1, stat_cols]),
    stat_labels
  )
  
  options(radar_norm_max = norm_max)
  
  # ---- Filter UIs ------------------------------------------------------
  output$generation_ui <- renderUI({
    gens <- sort(unique(pokemon$generation_id))
    selectizeInput("gens", "Generation (multi-select)", choices = gens,
                   selected = gens[1], multiple = TRUE, options = list(plugins = list("remove_button")))
  })
  
  output$type_ui <- renderUI({
    types <- pokemon %>%
      distinct(type_1) %>%
      filter(!is.na(type_1)) %>%
      arrange(type_1) %>%
      pull(type_1)
    selectizeInput("types", "Primary type_1 (multi-select)", choices = types,
                   selected = types[1], multiple = TRUE, options = list(plugins = list("remove_button")))
  })
  
  # ---- Reactive: filtered pool ----------------------------------------
  pool <- reactive({
    df <- pokemon
    # Generation filter
    if (!is.null(input$gens) && length(input$gens) > 0) {
      df <- df %>% filter(generation_id %in% input$gens)
    }
    # type_1 filter
    if (!is.null(input$types) && length(input$types) > 0) {
      df <- df %>% filter(type_1 %in% input$types)
    }
    df %>% arrange(pokemon)
  })
  
  # ---- Pokemon selectors (update with pool) ---------------------------
  output$pokeA_ui <- renderUI({
    choices <- pool()$pokemon
    selectizeInput("pokeA", "Pokémon A", choices = choices,
                   selected = head(choices, 1), multiple = FALSE)
  })
  
  output$pokeB_ui <- renderUI({
    choices <- pool()$pokemon
    default <- if (length(choices) >= 2) choices[2] else head(choices, 1)
    selectizeInput("pokeB", "Pokémon B", choices = choices,
                   selected = default, multiple = FALSE)
  })
  
  # ---- Retrieve the two rows ------------------------------------------
  oneA <- reactive({
    req(input$pokeA)
    pool() %>% filter(pokemon == input$pokeA) %>% slice(1)
  })
  
  oneB <- reactive({
    req(input$pokeB)
    pool() %>% filter(pokemon == input$pokeB) %>% slice(1)
  })
  
  # ---- Radar plots -----------------------------------------------------
  output$radarCombined <- renderPlotly({
    dfA <- oneA(); dfB <- oneB()
    req(nrow(dfA) == 1, nrow(dfB) == 1)
    
    df <- bind_rows(dfA, dfB)
    make_radar(df, stat_cols, title = "Stat Comparison")
  })
  
  
  # ---- Info side panels ------------------------------------------------
  render_info_panel <- function(df) {
    req(nrow(df) == 1)
    # Build a compact HTML summary with key fields and raw base stats
    tagList(
      if (!is.na(df$url_image)) tags$img(src = df$url_image, style = "max-width:100%; height:auto; margin-bottom:10px;"),
      tags$h4(df$pokemon),
      tags$p(
        HTML(paste0(
          "<b>Generation:</b> ", df$generation_id,
          "<br><b>Types:</b> ", df$type_1, if (!is.na(df$type_2)) paste0(" / ", df$type_2) else "",
          "<br><b>Height:</b> ", df$height, " m",
          "<br><b>Weight:</b> ", df$weight, " kg",
          "<br><b>Base XP:</b> ", df$base_experience,
          "<br><b>Egg groups:</b> ", df$egg_group_1, if (!is.na(df$egg_group_2)) paste0(" / ", df$egg_group_2) else ""
        ))
      ),
      tags$hr(),
      tags$div(
        tags$strong("Base Stats"),
        tags$table(
          class = "table table-sm",
          tags$tbody(
            tags$tr(tags$td("HP"), tags$td(df$hp)),
            tags$tr(tags$td("Attack"), tags$td(df$attack)),
            tags$tr(tags$td("Defense"), tags$td(df$defense)),
            tags$tr(tags$td("Sp. Atk"), tags$td(df$special_attack)),
            tags$tr(tags$td("Sp. Def"), tags$td(df$special_defense)),
            tags$tr(tags$td("Speed"), tags$td(df$speed))
          )
        )
      )
    )
  }
  
  output$infoA <- renderUI(render_info_panel(oneA()))
  output$infoB <- renderUI(render_info_panel(oneB()))
}

# ---- Run ---------------------------------------------------------------
shinyApp(ui, server)

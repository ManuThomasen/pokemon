# Who's That Pokémon? — Plotly + Shiny version
# -------------------------------------------------
# Requirements: shiny, plotly, magick, dplyr, tibble, pokemon, base64enc
# Optional: fonts if you want the Pokémon fonts

# install.packages(c("shiny","plotly","magick","dplyr","tibble","pokemon","base64enc"))

library(shiny)
library(plotly)
library(magick)
library(dplyr)
library(tibble)
library(pokemon)
library(base64enc)

# --- Configuration -----------------------------------------------------------
# Provide a local/remote path to the WTP background PNG
# Example background: any transparent PNG of the classic "Who's That Pokémon?" card
bg_path <- "wtp_background.png"  # <- set to a valid file path or URL

# Utility: read an image (file path or URL) and return a data URI for plotly
image_to_data_uri <- function(path_or_url) {
  img <- image_read(path_or_url)
  # Write to raw PNG in memory and base64-encode
  blob <- image_write(img, format = "png")
  uri <- dataURI(blob, mime = "image/png")
  uri
}

# Build silhouette (black with original alpha) and return both URIs
make_pokemon_layers <- function(poke_url) {
  img <- image_read(poke_url)
  info <- image_info(img)
  black <- image_blank(info$width, info$height, color = "black")
  sil <- image_composite(black, img, operator = "copy_opacity")
  list(
    silhouette_uri = dataURI(image_write(sil, format = "png"), mime = "image/png"),
    color_uri      = dataURI(image_write(img, format = "png"), mime = "image/png")
  )
}

# Construct a plotly scene with background + either silhouette or color image
build_plot <- function(bg_uri, pok_uri, label = NULL, reveal = FALSE) {
  # Canvas coordinates: [0,1] x [0,1]
  p <- plot_ly(type = "scatter", mode = "markers") %>%
    layout(
      xaxis = list(range = c(0, 1), visible = FALSE, fixedrange = TRUE),
      yaxis = list(range = c(0, 1), visible = FALSE, fixedrange = TRUE),
      dragmode = FALSE,
      margin = list(l = 0, r = 0, t = 0, b = 0)
    )
  
  # Compose layout images (background + Pokémon). In plotly R, images are layout-level.
  images_list <- list(
    list(
      source = bg_uri,
      xref = "x", yref = "y",
      x = 0, y = 1,
      sizex = 1, sizey = 1,
      sizing = "stretch",
      layer = "below",
      xanchor = "left", yanchor = "top"
    ),
    list(
      source = pok_uri,
      xref = "x", yref = "y",
      # place center at (0.3, 0.5) with size ~0.32 of canvas
      x = 0.3, y = 0.5,
      sizex = 0.50, sizey = 0.50,
      xanchor = "center", yanchor = "middle",
      sizing = "contain",
      layer = "above",
      opacity = 1
    )
  )
  
  p <- p %>% layout(images = images_list)
  
  # When revealed, add the Pokémon name as an annotation
  if (isTRUE(reveal) && !is.null(label)) {
    p <- p %>% layout(annotations = list(# Outline
      list(
        x = 0.75, y = 0.90,
        text = label,
        showarrow = FALSE,
        font = list(size = 103, color = "yellow")  # slightly larger, background color
      ),
      # Main text
      list(
        x = 0.75, y = 0.90,
        text = label,
        showarrow = FALSE,
        font = list(size = 100, color = "#1e3a8a")  # main color
      )
    ))
  }
  
  p
}

# --- Shiny app ---------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML(
    ".controls { display:flex; gap:10px; margin-bottom:10px }\n")),
  ),
  titlePanel("Who's That Pokémon? — Plotly Edition"),
  div(class = "controls",
      actionButton("reveal", "Reveal!", icon = icon("eye"), class = "btn-primary"),
      actionButton("reset",  "New random Pokémon", icon = icon("shuffle"), class = "btn-secondary")
  ),
  plotlyOutput("wtp", height = "600px")
)

server <- function(input, output, session) {
  # Precompute background URI once (fail with notification if not found)
  bg_uri <- tryCatch({ image_to_data_uri(bg_path) }, error = function(e) NULL)
  observe({
    if (is.null(bg_uri)) {
      showNotification("Could not load background image. Please set 'bg_path' to a valid PNG.", type = "error", duration = NULL)
    }
  })
  
  # Generation 1 subset and state
  kanto <- pokemon %>% filter(generation_id == 1)
  
  current_idx <- reactiveVal(sample.int(nrow(kanto), 1))
  revealed    <- reactiveVal(FALSE)
  
  # Helper to refresh Pokémon selection
  pick_new <- function() {
    current_idx(sample.int(nrow(kanto), 1))
    revealed(FALSE)
  }
  
  observeEvent(input$reset, { pick_new() })
  observeEvent(input$reveal, { revealed(TRUE) })
  
  # Render plot
  output$wtp <- renderPlotly({
    validate(need(!is.null(bg_uri), "Background image missing."))
    
    row <- kanto %>% slice(current_idx())
    poke_url  <- row$url_image %>% as.character()
    poke_name <- row$pokemon %>% as.character()
    
    layers <- make_pokemon_layers(poke_url)
    
    # Choose which layer to display based on revealed()
    pok_uri <- if (isTRUE(revealed())) layers$color_uri else layers$silhouette_uri
    
    build_plot(
      bg_uri = bg_uri,
      pok_uri = pok_uri,
      label = if (isTRUE(revealed())) poke_name else NULL,
      reveal = revealed()
    ) %>%
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui, server)

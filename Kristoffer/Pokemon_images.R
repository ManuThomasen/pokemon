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


library(shiny)
library(ggplot2)
library(ggimage)
library(dplyr)
library(tidyr)
library(cowplot)

# Stats to plot (edit if desired)
stat_cols <- c("hp", "attack", "defense", "special_attack", "special_defense", "speed")
# Normalize each stat to 1–100 (for cross-stat comparability)
pokemon_norm <- pokemon::pokemon %>%
  mutate(across(all_of(stat_cols), ~ {
    rng <- range(.x, na.rm = TRUE)
    if (!is.finite(diff(rng)) || diff(rng) == 0) return(rep(50, length(.x)))
    ((.x - rng[1]) / (rng[2] - rng[1])) * 99 + 1
  }, .names = "{.col}_norm"))

ui <- fluidPage(
  titlePanel("Pokémon Stats Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("pokemon", "Select Pokémon:", choices = NULL, selected = NULL, multiple = FALSE)
    ),
    mainPanel(
      plotOutput("poke_plot", height = "400px")
    )
  )
)

server <- function(input, output, session) {
  
  # Load dataset once here
  pokemon_df <- pokemon::pokemon
  
  # Update selectInput choices dynamically once data is loaded
  updateSelectInput(session, "pokemon", choices = pokemon_df$pokemon)
  
  output$poke_plot <- renderPlot({
    req(input$pokemon)
    
    poke <- pokemon_df %>% filter(pokemon == input$pokemon)
    
    stats_df <- poke %>%
      select(attack, defense, speed, hp) %>%
      pivot_longer(everything(), names_to = "stat", values_to = "value")
    
    bar_plot <- ggplot(stats_df, aes(x = value, y = stat, fill = stat)) +
      geom_col(width = 0.4) +
      scale_y_discrete(expand = expansion(add = c(0.1, 0.1))) +
      scale_fill_manual(values = c(
        attack = "red",
        defense = "blue",
        speed = "green",
        hp = "orange"
      )) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(margin = margin(r = 10)),
        axis.ticks = element_blank()
      ) +
      xlim(0, max(stats_df$value) * 1.2)
    
    image_plot <- ggplot(poke) +
      geom_image(aes(x = 0, y = 0, image = url_image), size = 0.5) +
      theme_void() +
      coord_fixed()
    
    plot_grid(image_plot, bar_plot,  nrow = 1, rel_widths = c(1, 2))
  })
}

shinyApp(ui, server)


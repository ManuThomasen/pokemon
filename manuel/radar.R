# --- Minimal Plotly radar (spider) plot for one Pokémon, with center image ---

# install.packages(c("plotly","dplyr","pokemon","htmlwidgets","base64enc")) # if needed
library(plotly)
library(dplyr)
library(pokemon)
library(htmlwidgets)
library(base64enc)  # only needed if embed_image = TRUE

# Load data
data("pokemon", package = "pokemon")

# Stats to plot (edit if desired)
stat_cols <- c("hp", "attack", "defense", "special_attack", "special_defense", "speed")

# Normalize each stat to 1–100 (for cross-stat comparability)
pokemon_norm <- pokemon %>%
  mutate(across(all_of(stat_cols), ~ {
    rng <- range(.x, na.rm = TRUE)
    if (!is.finite(diff(rng)) || diff(rng) == 0) return(rep(50, length(.x)))
    ((.x - rng[1]) / (rng[2] - rng[1])) * 99 + 1
  }, .names = "{.col}_norm"))

# Function: create a plotly radar for a given Pokémon name (exact match) + center image
pokemon_radar <- function(pokemon_name,
                          title        = NULL,
                          show_legend  = FALSE,
                          image_size   = 0.15,   # fraction of the plot area (0–1)
                          image_opacity= 0.95,   # 0–1
                          embed_image  = FALSE   # TRUE to embed as base64 (for selfcontained HTML)
) {
  row <- pokemon_norm %>% filter(.data$pokemon == pokemon_name) %>% slice(1)
  if (nrow(row) == 0) stop("Pokémon not found: ", pokemon_name)
  
  cats  <- stat_cols
  rvals <- as.numeric(row[1, paste0(cats, "_norm"), drop = TRUE])
  
  # Close the polygon by repeating the first point
  theta <- c(cats, cats[1])
  r     <- c(rvals, rvals[1])
  
  # Optional: actual (unnormalized) values for hover text
  actual <- as.numeric(row[1, cats, drop = TRUE])
  hovertext <- paste0(
    "<b>", row$pokemon[1], "</b><br>",
    paste0(cats, ": ", actual, " (", round(rvals), "/100)"),
    collapse = "<br>"
  )
  
  # Build base radar
  p <- plot_ly(type = "scatterpolar", mode = "lines", fill = "toself") |>
    add_trace(
      r = r, theta = theta,
      name = row$pokemon[1],
      hovertemplate = paste0(hovertext, "<extra></extra>")
    ) |>
    layout(
      title = if (is.null(title)) paste0("Stats Radar — ", row$pokemon[1]) else title,
      showlegend = show_legend,
      polar = list(
        bgcolor     = "rgba(0,0,0,0)",
        radialaxis  = list(range = c(0, 100), dtick = 20, showline = FALSE, gridcolor = "rgba(0,0,0,0.15)"),
        angularaxis = list(direction = "clockwise", gridcolor = "rgba(0,0,0,0.15)")
      ),
      margin = list(t = 60, l = 20, r = 20, b = 20)
    )
  
  # Determine image source
  src <- row$url_image[1]
  if (isTRUE(embed_image)) {
    # Download and embed so selfcontained HTML works offline
    tf <- tempfile(fileext = tools::file_ext(src) |> (\(ext) if (nzchar(ext)) paste0(".", ext) else ".png")())
    utils::download.file(src, tf, mode = "wb", quiet = TRUE)
    # best-guess MIME based on extension
    mime <- switch(tolower(tools::file_ext(tf)),
                   "jpg" = "image/jpeg",
                   "jpeg"= "image/jpeg",
                   "gif" = "image/gif",
                   "webp"= "image/webp",
                   "svg" = "image/svg+xml",
                   "png" = "image/png",
                   "image/png")
    src <- base64enc::dataURI(file = tf, mime = mime)
  }
  
  # Add the image centered on the plot (paper coords 0..1)
  p <- p |>
    layout(
      images = list(list(
        source  = src,
        xref    = "paper", yref = "paper",
        x       = 0.5,     y    = 0.5,
        sizex   = image_size,
        sizey   = image_size,
        xanchor = "center",
        yanchor = "middle",
        layer   = "top",     # behind the polygon
        opacity = image_opacity,
        sizing  = "contain"
      ))
    )
  
  p
}

# ---- Example usage ----
p <- pokemon_radar("charizard", embed_image = FALSE)  # set TRUE for fully self-contained HTML
p

# Save as standalone HTML
# Note: If embed_image = FALSE, the icon is referenced by URL and requires internet
#       when the HTML is opened. Use embed_image = TRUE to embed it.
# saveWidget(p, "pokemon_radar.html", selfcontained = TRUE)

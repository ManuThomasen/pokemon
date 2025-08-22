# Clean data provided by https://github.com/williamorim/pokemon. No cleaning was necessary.
# Packages
library(showtext)
library(ggplot2)
library(ggimage)
library(magick)
library(pokemon)
library(tidyverse)

# 0) pick a random number
pokemon_1 <- pokemon %>%
  filter(generation_id == 1)
random_pokemon <- sample_n(pokemon_1, 1)

# 1) Pick a Pokémon image (transparent PNG works best)
poke_url <- random_pokemon$url_image  # choose any row you like
poke_name <- random_pokemon$pokemon

# 2) Create a black silhouette using the original alpha
img <- image_read(poke_url)
info <- image_info(img)
black <- image_blank(info$width, info$height, color = "black")
silhouette <- image_composite(black, img, operator = "copy_opacity")

# Save to a temporary PNG for ggimage
sil_path <- file.path(tempdir(), "pokemon_silhouette.png")
image_write(silhouette, sil_path)

# 3) Path/URL to the classic WTP background (provide your file here)
# Example: bg_path <- "wtp_background.png"
# (You can download any WTP background PNG and point to it locally.)
bg_path <- "wtp_background.png"

# 4) Build the plot in a [0,1] x [0,1] canvas for easy positioning
df_bg  <- data.frame(x = 0.5, y = 0.5, image = bg_path)
df_pok_sil <- data.frame(x = 0.3, y = 0.5, image = sil_path)  # tweak position as needed
df_pok <- data.frame(x = 0.3, y = 0.5, image = poke_url)  # tweak position as needed
df_label <- data.frame(x = 0.75, y = 0.70, label = poke_name)


ggplot() +
  # Background stretched to fill the panel
  geom_image(data = df_bg, aes(x, y, image = image), size = 1) +
  # Silhouette on top (left side); adjust 'size' to scale
  geom_image(data = df_pok_sil, aes(x, y, image = image), size = 0.3) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  theme_void()

font_add("pokemon_solid", "Pokemon Solid.ttf")
font_add("pokemon_hollow", "Pokemon Hollow.ttf")
showtext_auto() 

ggplot() +
  # Background stretched to fill the panel
  geom_image(data = df_bg, aes(x, y, image = image), size = 1) +
  # Silhouette on top (left side); adjust 'size' to scale
  geom_image(data = df_pok, aes(x, y, image = image), size = 0.3) +
  geom_text(data = df_label, aes(x, y, label = label),
            family = "pokemon_hollow",
            colour = "blue",     # fill inside letters
            size = 12) +
  geom_text(data = df_label, aes(x, y, label = label),
            family = "pokemon_solid",
            colour = "yellow",     # fill inside letters
            size = 12) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  theme_void()

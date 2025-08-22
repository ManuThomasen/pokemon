install.packages("pokemon")
pokemon_df <- pokemon::pokemon

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggimage)

# Reshape stats to long format
top_pokemon <- pokemon_df %>%
  filter(generation_id == 1) %>%
  group_by(type_1) %>%
  slice_max(order_by = attack, n = 1, with_ties = FALSE) %>%
  ungroup()


# Reshape stats for plotting
top_pokemon_long <- top_pokemon %>%
  select(pokemon, type_1, attack, defense, special_attack, special_defense) %>%
  pivot_longer(
    cols = attack:special_defense,
    names_to = "stat",
    values_to = "value"
  )

# Add coordinates for the image (one row per pokemon)
top_pokemon_images <- top_pokemon %>%
  mutate(
    facet_label = paste0(pokemon, " (", type_1, ")"),
    x = -50,  # horizontal position for images
    y = mean(seq_along(c("attack", "defense", "special_attack", "special_defense"))) # mean of the stat positions (auto-centred)
  )

# Merge facet_label into long data
top_pokemon_long <- top_pokemon_long %>%
  left_join(
    top_pokemon_images %>% select(pokemon, facet_label),
    by = "pokemon"
  )

# Plot
p <- ggplot(top_pokemon_long, aes(x = value, y = stat, colour = stat)) +
  geom_segment(aes(x = 0, xend = value, yend = stat), linewidth = 1) +
  geom_point(aes(x = value, y = stat), size = 3) +
  geom_image(
    data = top_pokemon_images,
    aes(x = x, y = y, image = url_image),
    inherit.aes = FALSE,
    size = 1.0
  ) +
  facet_wrap(~ facet_label) +
  expand_limits(x = -90) +
  theme_minimal() +
  labs(
    title = "Key stats of highest-attack pokemon in each type",
    y = NULL,
    colour = "Stat") +
  scale_colour_manual(values = c(
    attack = "#E63946",         # red
    defense = "#457B9D",        # blue
    special_attack = "#F1FA3C", # yellow
    special_defense = "#2A9D8F" # green
  )) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    plot.margin = margin(10, 10, 10, 60) # give space for the icons
  )
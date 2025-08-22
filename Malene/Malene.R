# Clean data provided by https://github.com/williamorim/pokemon. No cleaning was necessary.

install.packages("pokemon")
pokemon_df <- pokemon::pokemon

library(tidyverse)

ggplot(data = pokemon_df) + 
  geom_histogram(aes(x = type_1, y = generation), color = "blue")

pokemon_df %>%
  count(type_1) %>%
  ggplot(aes(x = reorder(type_1, n), y = n)) +
  geom_col(fill = "blue") +
  coord_flip() +  # optional: makes it horizontal for easier reading
  labs(
    title = "Number of pokemon by primary type",
    x = "Primary type",
    y = "Count"
  ) +
  theme_minimal()

pokemon_df %>%
  count(type_2) %>%
  ggplot(aes(x = reorder(type_2, n), y = n)) +
  geom_col(fill = "blue") +
  coord_flip() +  # optional: makes it horizontal for easier reading
  labs(
    title = "Number of pokemon by secondary type",
    x = "Secondary type",
    y = "Count"
  ) +
  theme_minimal()


pokemon_df %>%
  pivot_longer(cols = c(type_1, type_2), names_to = "slot", values_to = "type") %>%
  filter(!is.na(type)) %>%
  count(slot, type) %>%
  ggplot(aes(x = reorder(type, n), y = n, fill = slot)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Number of pokemon by primary vs secondary type",
    x = "Type",
    y = "Count",
    fill = "Slot"
  ) +
  theme_minimal()

pokemon_df %>%
  count(generation_id, type_1) %>%
  ggplot(aes(x = factor(generation_id), y = n, fill = type_1)) +
  geom_line() +
  theme_minimal()

library(RColorBrewer)

pokemon_df %>%
  count(generation_id, type_1) %>%
  ggplot(aes(x = generation_id, y = n, colour = type_1)) +
  geom_line(size = 1) +          
  geom_point(size = 2) +  
  scale_colour_brewer(palette = "Set1") +
  labs(
    x = "Generation",
    y = "Number of Pokémon",
    colour = "Type"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:8)

pokemon_df %>%
  mutate(total_stats = hp + attack + defense + special_attack + special_defense + speed) %>%
  ggplot(aes(x = legendary, y = total_stats, fill = legendary)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Total Base Stats: Legendary vs Non-Legendary", y = "Total Stats") +
  theme_minimal()

### 
top_attack <- pokemon_df %>%
  group_by(type_1) %>%
  slice_max(order_by = attack, n = 1, with_ties = FALSE) %>%
  ungroup()

# Horizontal bar plot
ggplot(top_attack, aes(x = reorder(pokemon, attack), y = attack, fill = type_1)) +
  geom_col() +
  coord_flip() +  # makes it horizontal
  facet_wrap(~ type_1, scales = "free_y") +  # separate panel per type
  labs(
    title = "Top 5 pokemon by attack for each type",
    x = "Pokemon",
    y = "Attack",
    fill = "Type"
  ) +
  theme_minimal()

library(dplyr)
library(ggplot2)

# get top pokemon per type
top_attack <- pokemon_df %>%
  group_by(type_1) %>%
  slice_max(order_by = attack, n = 1, with_ties = FALSE) %>%
  ungroup()

# Plot with horizontal lines instead of bars
ggplot(top_attack, aes(x = reorder(pokemon, attack), y = attack, colour = type_1)) +
  geom_segment(aes(xend = pokemon, y = 0, yend = attack), linewidth = 1.2) +
  geom_point(size = 3) +
  coord_flip() +
  facet_wrap(~ type_1, scales = "free_y") +
  labs(
    title = "Highest Attack Pokémon in Each Type",
    x = "Pokémon",
    y = "Attack",
    colour = "Type"
  ) +
  theme_minimal()

# Get top Pokémon per type (highest attack)
top_pokemon <- pokemon_df %>%
  group_by(type_1) %>%
  slice_max(order_by = attack, n = 1, with_ties = FALSE) %>%
  ungroup()

# Reshape the data: pick only relevant stats
top_pokemon_long <- top_pokemon %>%
  select(pokemon, type_1, attack, defense, special_attack, special_defense) %>%
  pivot_longer(
    cols = c(attack, defense, special_attack, special_defense),
    names_to = "stat",
    values_to = "value"
  )

# Plot: horizontal lines for each stat
ggplot(top_pokemon_long, aes(x = stat, y = value, colour = stat, group = stat)) +
  geom_segment(aes(xend = stat, y = 0, yend = value), linewidth = 1.2) +
  geom_point(size = 3) +
  coord_flip() +
  facet_wrap(~ type_1, scales = "free_y") +
  labs(
    title = "Key Stats of Highest-Attack Pokémon in Each Type",
    x = "Stat",
    y = "Value",
    colour = "Stat"
  ) +
  theme_minimal()

# Get top Pokémon per type
top_pokemon <- pokemon_df %>%
  group_by(type_1) %>%
  slice_max(order_by = attack, n = 1, with_ties = FALSE) %>%
  ungroup()

# Reshape to long format
top_pokemon_long <- top_pokemon %>%
  select(pokemon, type_1, attack, defense, special_attack, special_defense) %>%
  pivot_longer(
    cols = c(attack, defense, special_attack, special_defense),
    names_to = "stat",
    values_to = "value"
  )

top_pokemon <- pokemon_df %>%
  group_by(type_1) %>%
  slice_max(order_by = attack, n = 1, with_ties = FALSE) %>%
  ungroup()

# Reshape to long format
top_pokemon_long <- top_pokemon %>%
  select(pokemon, type_1, attack, defense, special_attack, special_defense) %>%
  pivot_longer(
    cols = c(attack, defense, special_attack, special_defense),
    names_to = "stat",
    values_to = "value"
  )

# Plot
ggplot(top_pokemon_long, aes(x = stat, y = value, colour = stat, group = stat)) +
  geom_segment(aes(xend = stat, y = 0, yend = value), linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = pokemon), hjust = -0.2, vjust = 0.5, colour = "black") +
  coord_flip() +
  facet_wrap(~ type_1, scales = "free_y") +
  labs(
    title = "Key Stats of Highest-Attack Pokémon in Each Type",
    x = "Stat",
    y = "Value",
    colour = "Stat"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 10) # clearer type labels
  )


# Plot
ggplot(top_pokemon_long, aes(x = value, y = reorder(pokemon, value), colour = stat)) +
  geom_segment(aes(x = 0, xend = value, yend = pokemon), linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Stat Profiles of Highest-Attack Pokémon per Type",
    x = "Stat Value",
    y = "Pokémon",
    colour = "Stat"
  ) +
  theme_minimal()

top_pokemon <- pokemon_df %>%
  group_by(type_1) %>%
  slice_max(order_by = attack, n = 1, with_ties = FALSE) %>%
  ungroup()

# Reshape to long format
top_pokemon_long <- top_pokemon %>%
  select(pokemon, type_1, attack, defense, sp_attack, sp_defense) %>%
  pivot_longer(
    cols = c(attack, defense, sp_attack, sp_defense),
    names_to = "stat",
    values_to = "value"
  )

# Plot
ggplot(top_pokemon_long, aes(x = value, y = stat, colour = stat)) +
  geom_segment(aes(x = 0, xend = value, yend = stat), linewidth = 1) +
  geom_point(size = 3) +
  # add pokemon name on the left side, once per facet
  geom_text(
    data = top_pokemon, 
    aes(x = 0, y = 0.5, label = pokemon), 
    inherit.aes = FALSE,
    hjust = 1.1, vjust = 0.5,
    colour = "black", fontface = "bold"
  ) +
  facet_wrap(~ type_1, scales = "free_y") +
  labs(
    title = "Key Stats of Highest-Attack Pokémon in Each Type",
    x = "Value",
    y = NULL,
    colour = "Stat"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 10)
  )

### 
# Get top Pokémon per type
top_pokemon <- pokemon_df %>%
  group_by(type_1) %>%
  slice_max(order_by = attack, n = 1, with_ties = FALSE) %>%
  ungroup()

# Reshape to long format
top_pokemon_long <- top_pokemon %>%
  select(pokemon, type_1, attack, defense, special_attack, special_defense) %>%
  pivot_longer(
    cols = c(attack, defense, special_attack, special_defense),
    names_to = "stat",
    values_to = "value"
  )

# Plot
ggplot(top_pokemon_long, aes(x = value, y = stat, colour = stat)) +
  geom_segment(aes(x = 0, xend = value, yend = stat), linewidth = 1) +
  geom_point(size = 3) +
  # add pokemon name on the left side, once per facet
  geom_text(
    data = top_pokemon, 
    aes(x = 0, y = 0.5, label = pokemon), 
    inherit.aes = FALSE,
    hjust = 1.1, vjust = 0.5,
    colour = "black", fontface = "bold"
  ) +
  facet_wrap(~ type_1, scales = "free_y") +
  labs(
    title = "Key Stats of Highest-Attack Pokémon in Each Type",
    x = "Value",
    y = NULL,
    colour = "Stat"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 10)
  )

###
top_pokemon <- pokemon_df %>%
  group_by(type_1) %>%
  slice_max(order_by = attack, n = 1, with_ties = FALSE) %>%
  ungroup()

# Reshape to long format
top_pokemon_long <- top_pokemon %>%
  select(pokemon, type_1, attack, defense, special_attack, special_defense) %>%
  pivot_longer(
    cols = c(attack, defense, special_attack, special_defense),
    names_to = "stat",
    values_to = "value"
  )

# Plot
ggplot(top_pokemon_long, aes(x = value, y = stat, colour = stat)) +
  geom_segment(aes(x = 0, xend = value, yend = stat), linewidth = 1) +
  geom_point(size = 3) +
  # add pokemon name once per facet, on the left
  geom_text(
    data = top_pokemon,
    aes(x = -20, y = 2.5, label = pokemon),   # fixed y = middle of stats
    inherit.aes = FALSE,
    hjust = 1, colour = "black", fontface = "bold"
  ) +
  facet_wrap(~ type_1, scales = "free_y") +
  labs(
    title = "Key Stats of Highest-Attack Pokémon in Each Type",
    x = "Value",
    y = NULL,
    colour = "Stat"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 10)
  )

###
top_pokemon <- pokemon_df %>%
  group_by(type_1) %>%
  slice_max(order_by = attack, n = 1, with_ties = FALSE) %>%
  ungroup()

# Reshape to long format
top_pokemon_long <- top_pokemon %>%
  select(pokemon, type_1, attack, defense, special_attack, special_defense) %>%
  pivot_longer(
    cols = c(attack, defense, special_attack, special_defense),
    names_to = "stat",
    values_to = "value"
  )

# Plot
ggplot(top_pokemon_long, aes(x = value, y = stat, colour = stat)) +
  geom_segment(aes(x = 0, xend = value, yend = stat), linewidth = 1) +
  geom_point(size = 3) +
  # add stat labels on the right side of each line
  geom_text(aes(x = value + 5, y = stat, label = stat),
            inherit.aes = FALSE, colour = "black", hjust = 0) +
  # add pokemon name once per facet, on the left
  geom_text(
    data = top_pokemon,
    aes(x = -20, y = 2.5, label = pokemon),
    inherit.aes = FALSE,
    hjust = 1, colour = "black", fontface = "bold"
  ) +
  facet_wrap(~ type_1) +
  labs(
    title = "Key Stats of Highest-Attack Pokémon in Each Type",
    x = "Value",
    y = NULL,
    colour = "Stat"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),   # remove default stat labels on y-axis
    axis.ticks.y = element_blank(),
    strip.text = element_text(face = "bold", size = 10)
  )

###
top_pokemon <- pokemon_df %>%
  group_by(type_1) %>%
  slice_max(order_by = attack, n = 1, with_ties = FALSE) %>%
  ungroup()

# Reshape to long format
top_pokemon_long <- top_pokemon %>%
  select(pokemon, type_1, attack, defense, special_attack, special_defense) %>%
  pivot_longer(
    cols = c(attack, defense, special_attack, special_defense),
    names_to = "stat",
    values_to = "value"
  )

# Plot
ggplot(top_pokemon_long, aes(x = value, y = stat, colour = stat)) +
  geom_segment(aes(x = 0, xend = value, yend = stat), linewidth = 1) +
  geom_point(size = 3) +
  # add pokemon name once per facet (left side)
  geom_text(
    data = top_pokemon,
    aes(x = -20, y = 2.5, label = pokemon),  # y=2.5 ~ middle of 4 stats
    inherit.aes = FALSE,
    hjust = 1, colour = "black", fontface = "bold"
  ) +
  facet_wrap(~ type_1) +
  labs(
    title = "Key Stats of Highest-Attack Pokémon in Each Type",
    x = "Value",
    y = NULL,
    colour = "Stat"
  ) +
  theme_minimal() +
  theme(
    plot.margin = margin(10, 10, 10, 80),  # increase left margin
    axis.text.y = element_blank(),   # remove stat labels
    axis.ticks.y = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "right",# keep one legend for the whole plot
  )

top_pokemon_long <- top_pokemon_long %>%
  mutate(facet_label = paste0(pokemon, " (", type_1, ")"))

ggplot(top_pokemon_long, aes(x = value, y = stat, colour = stat)) +
  geom_segment(aes(x = 0, xend = value, yend = stat), linewidth = 1) +
  geom_point(size = 3) +
  facet_wrap(~ facet_label) +
  labs(
    title = "Key Stats of Highest-Attack Pokémon in Each Type",
    x = "Value",
    y = NULL,
    colour = "Stat"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "right"
  )

install.packages("ggimage")
library(ggimage)

top_pokemon <- pokemon_df %>%
  group_by(type_1) %>%
  slice_max(order_by = attack, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    x = -30,       # horizontal position for image
    y = 2.5        # vertical centre of stats
  )

# 2. Reshape stats for plotting
top_pokemon_long <- top_pokemon %>%
  select(pokemon, type_1, attack, defense, special_attack, special_defense, url_icon) %>%
  pivot_longer(
    cols = attack:special_defense,
    names_to = "stat",
    values_to = "value"
  )

# 3. Plot with images
ggplot(top_pokemon_long, aes(x = value, y = stat, colour = stat)) +
  geom_segment(aes(x = 0, xend = value, yend = stat), linewidth = 1) +
  geom_point(size = 3) +
  geom_image(
    data = top_pokemon,
    aes(x = x, y = y, image = url_icon),
    inherit.aes = FALSE,
    size = 0.1
  ) +
  facet_wrap(~ type_1) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    plot.margin = margin(10, 10, 10, 60)
  )

### 

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

# Add coordinates for the image (one row per Pokémon)
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

# 4. Plot
ggplot(top_pokemon_long, aes(x = value, y = stat, colour = stat)) +
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

####

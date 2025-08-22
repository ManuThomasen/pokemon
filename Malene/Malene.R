# Clean data provided by https://github.com/williamorim/pokemon. No cleaning was necessary.

# install.packages("pokemon")
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
  slice_max(order_by = attack, defense, special_attack, speciel_defence, n = 1, with_ties = FALSE) %>%
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

top_attack <- pokemon %>%
  group_by(type_1) %>%
  slice_max(order_by = attack, n = 5, with_ties = FALSE) %>%
  ungroup()

# Horizontal bar plot
ggplot(top_attack, aes(x = reorder(Name, attack), y = attack, fill = type_1)) +
  geom_col() +
  coord_flip() +  # makes it horizontal
  facet_wrap(~ type_1, scales = "free_y") +  # separate panel per type
  labs(
    title = "Top 5 Pokémon by Attack for Each Type",
    x = "Pokémon",
    y = "Attack",
    fill = "Type"
  ) +
  theme_minimal()

?geom_violine
pokemon_df %>%
  filter(!is.na(type_2)) %>%
  count(type_1, type_2) %>%
  ggplot(aes(x = type_1, y = type_2, fill = n)) +
  geom_line() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Common type combinations")

library(ggplot2)
library(dplyr)

pokemon_stats <- pokemon_df %>%
  mutate(total_stats = hp + attack + defense + special_attack + special_defense + speed) %>%
  filter(!is.na(height), !is.na(weight), !is.na(type_1))

ggplot(pokemon_stats, aes(x = height, y = weight, 
                          color = type_1, size = total_stats)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(2, 10)) +  # adjust point size range
  labs(
    x = "Height",
    y = "Weight",
    color = "Primary type",
    size = "Total stats"
  ) +
  theme_minimal(base_size = 14)

pokemon_df %>%
  pivot_longer(cols = c(egg_group_1, egg_group_2),
               names_to = "slot", values_to = "egg_group") %>%
  filter(!is.na(egg_group)) %>%
  count(egg_group, type_1) %>%
  ggplot(aes(x = type_1, y = egg_group, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Primary Type by Egg Group",
       x = "Primary Type", y = "Egg Group") +
  theme_minimal()

pokemon_df %>%
  pivot_longer(cols = c(egg_group_1, egg_group_2),
               names_to = "slot", values_to = "egg_group") %>%
  filter(!is.na(egg_group)) %>%
  count(egg_group, type_1) %>%
  ggplot(aes(x = type_1, y = egg_group, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Primary Type by Egg Group",
       x = "Primary Type", y = "Egg Group") +
  theme_minimal()

pokemon_stats <- pokemon %>% 
  filter(!is.na(type_1))

ggplot(pokemon_stats, aes(x = attack, y = defense, color = type_1)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Stat Trade-off: Attack vs Defence",
    x = "Attack",
    y = "Defence",
    color = "Primary Type"
  ) +
  theme_minimal(base_size = 14)

ggplot(pokemon_stats, aes(x = speed, y = hp, color = type_1)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Stat Trade-off: Speed vs HP",
    x = "Speed",
    y = "HP",
    color = "Primary Type"
  ) +
  stat_ellipse(type = "norm", linetype = 2) +
  facet_wrap(~generation_id) +
  theme_minimal(base_size = 14)

pokemon_eggs <- pokemon_df %>%
  pivot_longer(cols = c(egg_group_1, egg_group_2),
               names_to = "slot", values_to = "egg_group") %>%
  filter(!is.na(egg_group))

ggplot(pokemon_eggs, aes(x = speed, y = hp, color = egg_group)) +
  geom_point(alpha = 0.7, size = 3) +
  labs(
    title = "Stat Trade-off: Speed vs HP by Egg Group",
    x = "Speed",
    y = "HP",
    color = "Egg Group"
  ) +
  stat_ellipse(type = "norm", linetype = 2) +
  facet_wrap(~generation_id) +
  theme_minimal(base_size = 14)


# 📦 fit model
mod <- lm(hp ~ height + weight + base_experience + defense + attack + speed,
          data = pokemon_df)

# 📊 collect predicted + residuals
df_mod <- tibble(
  id = pokemon_df$id,
  pokemon = pokemon_df$pokemon,
  y = pokemon_df$hp,
  y_hat = fitted.values(mod),
  res = residuals(mod)
) |>
  mutate(rank = rank(-res)) |>   # large residuals first
  filter(rank <= 16)             # keep top 16 Pokémon

# 🖼️ plot predicted vs actual
ggplot(df_mod, aes(x = y_hat, y = y)) +
  geom_point(data = tibble(
    y = pokemon_df$hp,
    y_hat = fitted.values(mod)
  ), aes(x = y_hat, y = y), alpha = 0.3, colour = "grey40") +
  geom_smooth(method = "lm", se = FALSE, colour = "steelblue") +
  geom_point(colour = "red", size = 3) +
  ggrepel::geom_text_repel(aes(label = pokemon), colour = "red") +
  labs(
    title = "Pokémon with Unexpectedly High HP",
    subtitle = "Top 16 Pokémon with the largest positive residuals (actual HP > predicted HP)",
    x = "Predicted HP",
    y = "Actual HP"
  ) +
  theme_minimal(base_size = 14)

library(tidyverse)
library(ggrepel)

# 1. Fit model
mod <- lm(hp ~ height + weight + base_experience + defense + attack + speed,
          data = pokemon_df)

# 2. Extract predictions and residuals
df_mod <- pokemon_df |>
  mutate(
    y_hat = fitted(mod),
    res   = resid(mod),
    rank  = rank(-res)   # order by positive residual
  ) |>
  filter(rank <= 16)     # top 16 biggest HP-surprises

# 3. Scatterplot of predicted vs actual HP
ggplot(pokemon_df, aes(x = fitted(mod), y = hp)) +
  geom_point(alpha = 0.3, colour = "grey50") +
  geom_smooth(method = "lm", se = FALSE, colour = "steelblue") +
  geom_point(data = df_mod, aes(x = y_hat, y = hp), colour = "red", size = 3) +
  geom_text_repel(data = df_mod, aes(x = y_hat, y = hp, label = pokemon),
                  colour = "red", size = 3.5) +
  labs(
    title = "Pokémon with Unexpectedly High HP",
    subtitle = "Top 16 Pokémon with the largest positive residuals (actual HP > predicted HP)",
    x = "Predicted HP",
    y = "Actual HP"
  ) +
  theme_minimal(base_size = 14)
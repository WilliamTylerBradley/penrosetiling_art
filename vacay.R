##-----
# Description
##-----
# This output freezes sets of 10 rhombi that make a circle,
# then subdivides the rest

# Libraries
# devtools::install_github("WilliamTylerBradley/penrosetiling")
library(penrosetiling)
library(tidyverse)
library(ragg)
library(ggforce)

tiles <- return_starting_circle(0, 0, 180, 10, "t") %>%
  substitution() %>%
  substitution() %>%
  substitution()

tiles_circle <- tiles %>%
  filter(shape == "T") %>%
  pivot_down() %>%
  group_by(x, y) %>%
  mutate(total = n(), .groups = "drop") %>%
  ungroup() %>%
  filter(total == 10) %>%
  select(triangle)

# first level
pulled_tiles <- tiles %>%
  pivot_down() %>%
  inner_join(tiles_circle, by = "triangle") %>%
  mutate(pulled_set = 1)

tiles <- tiles %>%
  anti_join(tiles_circle, by = "triangle") %>%
  substitution() %>%
  substitution()

# second level
tiles_circle <- tiles %>%
  filter(shape == "T") %>%
  pivot_down() %>%
  group_by(x, y) %>%
  mutate(total = n(), .groups = "drop") %>%
  ungroup() %>%
  filter(total == 10) %>%
  select(triangle)

pulled_tiles <- tiles %>%
  pivot_down() %>%
  inner_join(tiles_circle, by = "triangle") %>%
  mutate(pulled_set = 2) %>%
  bind_rows(pulled_tiles)

tiles <- tiles %>%
  anti_join(tiles_circle, by = "triangle") %>%
  substitution() %>%
  substitution()

# third level
tiles_circle <- tiles %>%
  filter(shape == "T") %>%
  pivot_down() %>%
  group_by(x, y) %>%
  mutate(total = n(), .groups = "drop") %>%
  ungroup() %>%
  filter(total == 10) %>%
  select(triangle)

pulled_tiles <- tiles %>%
  pivot_down() %>%
  inner_join(tiles_circle, by = "triangle") %>%
  mutate(pulled_set = 3) %>%
  bind_rows(pulled_tiles) %>%
  mutate(color = "#CE4758") %>%
  group_by(pulled_set, rhombus) %>%
  mutate(rhombus = cur_group_id()) %>%
  ungroup() %>%
  group_by(rhombus) %>%
  select(-triangle) %>%
  distinct()

# Clean up
tiles <- tiles %>%
  pivot_down() %>%
  mutate(pulled_set = 0,
         color = ifelse(shape == 't', "#3F7E36", "#EDE1D1")) %>%
  group_by(rhombus) %>%
  select(-triangle) %>%
  distinct()

background <- return_starting_circle(0, 0, 180, 10, "t") %>%
  pivot_down()

agg_jpeg(filename = "vacay.jpeg",
         width = 750,
         height = 750)
ggplot() +
  geom_shape(data = background,
             aes(x = x,
                 y = y,
                 group = rhombus),
             fill = "#EDE1D1",
             color = "#EDE1D1") +
  geom_shape(data = tiles,
             aes(x = x,
                 y = y,
                 fill = color,
                 group = rhombus),
             radius = unit(0.05, 'cm'),
             color = "#EDE1D1") +
  geom_shape(data = pulled_tiles,
             aes(x = x,
                 y = y,
                 fill = color,
                 group = rhombus),
             expand = unit(-0.05, 'cm'),
             radius = unit(0.1, 'cm'),
             color = "#DC7E8A") +
  scale_fill_identity() +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none")
invisible(dev.off())

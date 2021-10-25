##-----
# Description
##-----
# This file produces a set of four images. They each start with the same setup,
# but triangles are randomized to be subdivided.

# Libraries
# devtools::install_github("WilliamTylerBradley/penrosetiling")
library(penrosetiling)
library(tidyverse)
library(ragg)
library(patchwork)

# Parameters
g_ratio <- (1 + sqrt(5)) / 2

clean_bounds_x <- 100
clean_bounds_y <- clean_bounds_x / g_ratio

spacing <- .25 / 4

set.seed(3333)

## Main function
get_tiles <- function() {
  tiles <- return_starting_circle(0, 0, 270, 150, "T") %>%
    substitution() %>%
    substitution() %>%
    substitution() %>%
    clean(-clean_bounds_x, -clean_bounds_y,
          clean_bounds_x, clean_bounds_y) %>%
    group_by(rhombus) %>%
    mutate(subdivide = sample(c(TRUE, FALSE), 1, prob = c(.33, 1 - .33))) %>%
    ungroup()

  t1 <- tiles %>%
    filter(subdivide == FALSE) %>%
    select(-subdivide)

  tiles <- tiles %>%
    filter(subdivide == TRUE) %>%
    substitution() %>%
    substitution() %>%
    substitution() %>%
    clean(-clean_bounds_x, -clean_bounds_y,
          clean_bounds_x, clean_bounds_y) %>%
    group_by(rhombus) %>%
    mutate(subdivide = sample(c(TRUE, FALSE), 1, prob = c(.33, 1 - .33))) %>%
    ungroup

  t2 <- tiles %>%
    filter(subdivide == FALSE) %>%
    select(-subdivide)

  tiles <- tiles %>%
    filter(subdivide == TRUE) %>%
    substitution() %>%
    substitution() %>%
    substitution() %>%
    clean(-clean_bounds_x, -clean_bounds_y,
          clean_bounds_x, clean_bounds_y) %>%
    select(-subdivide)

  tiles <- bind_rows(t1, t2, tiles) %>%
    select(-c(triangle, rhombus)) %>% # reset triangle and rhombus
    rowid_to_column("triangle") %>%
    group_by(point1_x, point1_y, point3_x, point3_y) %>%
    mutate(rhombus = cur_group_id()) %>%
    ungroup() %>%
    pivot_down()

  return(tiles)
}

# Save four sets
tiles1 <- get_tiles()
tiles1_plot <- ggplot(data = tiles1) +
  geom_polygon(aes(x = x,
                   y = y,
                   color = shape,
                   fill = shape,
                   group = triangle)) +
  geom_polygon(aes(x = x,
                   y = y,
                   fill = shape,
                   group = triangle),
               color = NA) +
  scale_color_manual(values = c("#FF7734",
                                "#0753B3")) +
  scale_fill_manual(values = c("#FF7734",
                               "#0753B3")) +
  coord_fixed(xlim = c(-clean_bounds_x, clean_bounds_x),
              ylim = c(-clean_bounds_y, clean_bounds_y),
              expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = unit(c(spacing, spacing, spacing, spacing), "in"))

tiles2 <- get_tiles()
tiles2_plot <- ggplot(data = tiles2) +
  geom_polygon(aes(x = x,
                   y = y,
                   color = shape,
                   fill = shape,
                   group = triangle)) +
  geom_polygon(aes(x = x,
                   y = y,
                   fill = shape,
                   group = triangle),
               color = NA) +
  scale_color_manual(values = c("#ECE356",
                                "#0753B3")) +
  scale_fill_manual(values = c("#ECE356",
                               "#0753B3")) +
  coord_fixed(xlim = c(-clean_bounds_x, clean_bounds_x),
              ylim = c(-clean_bounds_y, clean_bounds_y),
              expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = unit(c(spacing, spacing, spacing, spacing), "in"))

tiles3 <- get_tiles()
tiles3_plot <- ggplot(data = tiles3) +
  geom_polygon(aes(x = x,
                   y = y,
                   color = shape,
                   fill = shape,
                   group = triangle)) +
  geom_polygon(aes(x = x,
                   y = y,
                   fill = shape,
                   group = triangle),
               color = NA) +
  scale_color_manual(values = c("#EE4037",
                                "#0753B3")) +
  scale_fill_manual(values = c("#EE4037",
                               "#0753B3")) +
  coord_fixed(xlim = c(-clean_bounds_x, clean_bounds_x),
              ylim = c(-clean_bounds_y, clean_bounds_y),
              expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = unit(c(spacing, spacing, spacing, spacing), "in"))

tiles4 <- get_tiles()
tiles4_plot <- ggplot(data = tiles4) +
  geom_polygon(aes(x = x,
                   y = y,
                   color = shape,
                   fill = shape,
                   group = triangle)) +
  geom_polygon(aes(x = x,
                   y = y,
                   fill = shape,
                   group = triangle),
               color = NA) +
  scale_color_manual(values = c("#58E6F4",
                                "#0753B3")) +
  scale_fill_manual(values = c("#58E6F4",
                               "#0753B3")) +
  coord_fixed(xlim = c(-clean_bounds_x, clean_bounds_x),
              ylim = c(-clean_bounds_y, clean_bounds_y),
              expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = unit(c(spacing, spacing, spacing, spacing), "in"))

agg_png(filename = "variations2.png",
        width = 2400,
        height = 2400 / g_ratio)
(tiles1_plot | tiles2_plot) / (tiles3_plot | tiles4_plot)
invisible(dev.off())

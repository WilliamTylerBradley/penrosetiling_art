##-----
# Description
##-----
# This file produces a set of four images. They're subsets of one big image.
# The big image is a penrose pattern that's been randomly substituted.

# Libraries
# devtools::install_github("WilliamTylerBradley/penrosetiling")
library(penrosetiling)
library(tidyverse)
library(ragg)
library(patchwork)

set.seed(1000)

# Parameters
g_ratio <- (1 + sqrt(5)) / 2

clean_bounds_x <- 100
clean_bounds_y <- clean_bounds_x / g_ratio

spacing <- .25 / 4

# Start out with a circle and substitute a couple times
# Select around a third to subdivide
tiles <- return_starting_circle(0, 0, 270, 150, "T") %>%
  substitution() %>%
  substitution() %>%
  substitution() %>%
  substitution() %>%
  group_by(rhombus) %>%
  mutate(subdivide = sample(c(TRUE, FALSE), 1, prob = c(.33, 1 - .33))) %>%
  ungroup()

t1 <- tiles %>%
  filter(subdivide == FALSE) %>%
  select(-subdivide)

# Select a third from the already selected set to subdivide again
tiles <- tiles %>%
  filter(subdivide == TRUE) %>%
  substitution() %>%
  substitution() %>%
  substitution() %>%
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
  select(-subdivide)

tiles <- bind_rows(t1, t2, tiles) %>%
  select(-c(triangle, rhombus)) %>% # reset triangle and rhombus
  rowid_to_column("triangle") %>%
  group_by(point1_x, point1_y, point3_x, point3_y) %>%
  mutate(rhombus = cur_group_id()) %>%
  ungroup() %>%
  pivot_down()

tiles <- tiles %>%
  pivot_down()

# Plot four different areas by switching around the bounds in coord_fixed
tiles_plot1 <- ggplot(data = tiles) +
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
  scale_color_manual(values = c("#FD7543",
                                "#3EC2CF")) +
  scale_fill_manual(values = c("#FD7543",
                               "#3EC2CF")) +
  coord_fixed(xlim = c(-clean_bounds_x, 0),
              ylim = c(0, clean_bounds_y),
              expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = unit(c(spacing, spacing, spacing, spacing), "in"))

tiles_plot2 <- ggplot(data = tiles) +
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
  scale_color_manual(values = c("#FD7543",
                                "#3EC2CF")) +
  scale_fill_manual(values = c("#FD7543",
                               "#3EC2CF")) +
  coord_fixed(xlim = c(0, clean_bounds_x),
              ylim = c(0, clean_bounds_y),
              expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = unit(c(spacing, spacing, spacing, spacing), "in"))

tiles_plot3 <- ggplot(data = tiles) +
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
  scale_color_manual(values = c("#FD7543",
                                "#3EC2CF")) +
  scale_fill_manual(values = c("#FD7543",
                               "#3EC2CF")) +
  coord_fixed(xlim = c(0, clean_bounds_x),
              ylim = c(-clean_bounds_y, 0),
              expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = unit(c(spacing, spacing, spacing, spacing), "in"))

tiles_plot4 <- ggplot(data = tiles) +
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
  scale_color_manual(values = c("#FD7543",
                                "#3EC2CF")) +
  scale_fill_manual(values = c("#FD7543",
                               "#3EC2CF")) +
  coord_fixed(xlim = c(-clean_bounds_x, 0),
              ylim = c(-clean_bounds_y, 0),
              expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = unit(c(spacing, spacing, spacing, spacing), "in"))

agg_png(filename = "variations1.png",
        width = 2400,
        height = 2400 / g_ratio)
(tiles_plot4 | tiles_plot3) / (tiles_plot2 | tiles_plot1)
invisible(dev.off())

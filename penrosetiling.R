##-----
# Description
##-----
# This file produces the hex sticker for the penrosetiling package

# Libraries
# devtools::install_github("WilliamTylerBradley/penrosetiling")
library(penrosetiling)
library(tidyverse)
library(magick)

# Hexagon shape
hex <- data.frame(x = cos( (seq(1, 6) * 60 + 90) * pi/180 ),
                  y = sin( (seq(1, 6) * 60 + 90) * pi/180 ))
hex_x <- range(hex$x)
hex_y <- range(hex$y)

ggplot(data = hex) +
  geom_polygon(aes(x = x,
                   y = y)) +
  coord_fixed(xlim = hex_x,
              ylim = hex_y,
              expand = FALSE) +
  theme_void() +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))
ggsave(here::here("penrosetiling", "hex_shape.png"),
       width = 2,
       height = 2,
       bg = "transparent")

# Background
tiles <- return_starting_circle(0, 0, 180, 10, "t") %>%
  substitution() %>%
  substitution() %>%
  substitution() %>%
  substitution() %>%
  substitution() %>%
  substitution() %>%
  pivot_down() %>%
  select(-triangle) %>%
  distinct()

ggplot(data = tiles) +
  geom_polygon(aes(x = x,
                   y = y,
                   fill = shape,
                   group = rhombus),
               color = "#434343") +
  scale_color_manual(values = c("#7E3A21",
                                "#1F6167")) +
  scale_fill_manual(values = c("#7E3A21",
                               "#1F6167")) +
  coord_fixed(xlim = c(-5, 5),
              ylim = c(-5, 5),
              expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")
ggsave(here::here("penrosetiling", "background.png"),
       width = 2,
       height = 2)

# Text coloring
tiles <- return_starting_circle(0, 0, 180, 10, "t") %>%
  substitution() %>%
  substitution() %>%
  substitution() %>%
  substitution() %>%
  substitution() %>%
  substitution() %>%
  substitution() %>%
  substitution() %>%
  substitution() %>%
  pivot_down() %>%
  select(-triangle) %>%
  distinct()

ggplot(data = tiles) +
  geom_polygon(aes(x = x,
                   y = y,
                   fill = shape,
                   group = rhombus),
               color = "#F3F3F3",
               size = .15) +
  scale_color_manual(values = c("#FEBAA1",
                                "#9EE0E7")) +
  scale_fill_manual(values = c("#FEBAA1",
                               "#9EE0E7")) +
  coord_fixed(xlim = c(-5, 5),
              ylim = c(-5, 5),
              expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")
ggsave(here::here("penrosetiling", "foreground.png"),
       width = 2,
       height = 2)

# Text
ggplot() +
  geom_text(aes(x = 0,
            y = 0,
            label = "penrosetiling"),
            size = 6.5,
            hjust = .5,
            vjust = .5,
            fontface = "bold") +
  theme_void() +
  theme(legend.position = "none") +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))
ggsave(here::here("penrosetiling", "text.png"),
       width = 2,
       height = 2,
       bg = "transparent")

# Set up images
hex_shape_img <- image_read(here::here("penrosetiling", "hex_shape.png"))
background_img <- image_read(here::here("penrosetiling", "background.png"))
foreground_img <- image_read(here::here("penrosetiling", "foreground.png"))
text_img <- image_read(here::here("penrosetiling", "text.png"))

# Combine images
hex_sticker <- image_composite(text_img, foreground_img, "in")
hex_sticker <- image_composite(background_img, hex_sticker, "over")
hex_sticker <- image_composite(hex_shape_img, hex_sticker, "in")
image_write(hex_sticker, here::here("penrosetiling.png"))

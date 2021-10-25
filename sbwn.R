##-----
# Description
##-----
# Standard black and white noise
# Outputs a gif that different areas are subdivided a different amount
# Levels are determined by Simplex noise

# Libraries
# devtools::install_github("WilliamTylerBradley/penrosetiling")
library(penrosetiling)
library(tidyverse)
library(ambient)
library(gifski)

check_substitution <- function(df, cut_level, frame) {
  df <- df %>%
    rowwise() %>%
    mutate(center_x = mean((point1_x + point2_x + point3_x) / 3),
           center_y = mean((point1_y + point2_y + point3_y) / 3)) %>%
    mutate(z = 15 * cos(frame / 60 * 2*pi), # 60 needs to be total frames
           t = 15 * sin(frame / 60 * 2*pi)) %>%
    mutate(noise = fracture(gen_simplex, fbm, octaves = 3,
                            x = center_x, y = center_y, t = t, z = z,
                            freq_init = .005, seed = 5)) %>%
    mutate(subdivide = if_else(noise < cut_level, TRUE, FALSE))

  df_subdivide <- df %>%
    filter(subdivide == TRUE) %>%
    select(-subdivide) %>%
    substitution()

  df <- df %>%
    filter(subdivide == FALSE) %>%
    select(-subdivide) %>%
    bind_rows(df_subdivide) %>%
    select(-c(triangle, rhombus)) %>% # reset triangle and rhombus
    rowid_to_column("triangle") %>%
    group_by(point1_x, point1_y, point3_x, point3_y) %>%
    mutate(rhombus = cur_group_id()) %>%
    ungroup() %>%
    clean(0, 0, 100, 100)

  return(df)
}

# https://necessarydisorder.wordpress.com/2017/11/15/drawing-from-noise-and-then-making-animated-loopy-gifs-from-there/
for(frame in 1:60) {
  print(frame)
  tiles <- return_starting_circle(50, 50, 0, 75, "t") %>%
    rowwise() %>%
    mutate(subdivide = TRUE,
           triangle = cur_group_id()) %>%
    substitution() %>%
    substitution() %>%
    substitution() %>%
    clean(0, 0, 100, 100)

  tiles <- reduce(.x = as.list(seq(1, -1, -.25)),
                  .f = check_substitution,
                  .init = tiles,
                  frame = frame)

  tiles <- tiles %>%
    pivot_down() %>%
    mutate(color = if_else(shape == 'T', "#FFFFFF", "#000000"))

  ggplot() +
    geom_polygon(data = tiles[tiles$shape == "T", ],
                 aes(x = x,
                     y = y,
                     fill = color,
                     group = triangle),
                 color = "white") +
    geom_polygon(data = tiles[tiles$shape == "t", ],
                 aes(x = x,
                     y = y,
                     fill = color,
                     group = triangle),
                 color = "black") +
    scale_fill_identity() +
    coord_fixed(xlim = c(0, 100),
                ylim = c(0, 100),
                expand = FALSE) +
    theme_void() +
    theme(legend.position = "none")
  ggsave(filename = here::here("sbwn", paste0("test_", stringr::str_pad(frame, 2, pad = "0"), ".png")),
         width = 2.5, height = 2.5)
}

imgs <- file.path("sbwn", list.files(here::here("sbwn")))
gifski(imgs,
       delay = 1/10,
       gif_file = here::here("sbwn.gif"))

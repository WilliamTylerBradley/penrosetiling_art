##-----
# Description
##-----
# This file produces output with a penrose pattern centered horizontally but
# lifted up vertically. As the pattern moves away from this point, triangles are
# more likely to be subdivide and more likely to be colored gray.

# Libraries
# devtools::install_github("WilliamTylerBradley/penrosetiling")
library(penrosetiling)
library(tidyverse)
library(ragg)

set.seed(10)

# Set up image size and center of pattern
x_limits <- c(0, 18)
y_limits <- c(0, 24)
circle_h <- 9
circle_k <- 15

# This function checks the distance from the point 1 to then center of the
# pattern. This value is used to determine probability of subdividing based on
# a logit function
check_substitution <- function(df, x) {
  df <- df %>%
    rowwise() %>%
    mutate(distance =
             sqrt((point1_x - circle_h)^2 + (point1_y - circle_k)^2)) %>%
    mutate(prob = 1 / (1 + exp(-.5 * (distance - 10)))) %>%
    mutate(subdivide = if_else(runif(1) < prob, TRUE, FALSE))

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
    clean(x_limits[1], y_limits[1],
          x_limits[2], y_limits[2])

  return(df)
}

# Start with a large circle, substitute down several levels
tiles <- return_starting_circle(circle_h, circle_k, 90, 20, "T") %>%
  rowwise() %>%
  mutate(subdivide = TRUE,
         triangle = cur_group_id()) %>%
  substitution() %>%
  substitution() %>%
  substitution() %>%
  substitution() %>%
  clean(x_limits[1], y_limits[1],
        x_limits[2], y_limits[2])

# Run checks on substitution
tiles <- reduce(.x = as.list(seq(1, 7)),
                .f = check_substitution,
                .init = tiles)

# Assign random colors
tiles <- tiles %>%
  rowwise() %>%
  mutate(distance =
           sqrt((point1_x - circle_h)^2 + (point1_y - circle_k)^2)) %>%
  mutate(prob = 1 / (1 + exp(-.5 * (distance - 10)))) %>%
  mutate(color = if_else(runif(1) < prob, "#333333",
                         hcl(h = runif(1, 0, 360),
                             c = runif(1, 60, 80),
                             l = runif(1, 50, 70)))) %>%
  pivot_down()

agg_jpeg(filename = "cathedral.jpeg",
        width = 9,
        height = 12,
        units = "in")
ggplot(data = tiles) +
  geom_polygon(aes(x = x,
                   y = y,
                   fill = color,
                   group = triangle),
               color = "#333333") +
  scale_fill_identity() +
  coord_fixed(xlim = c(x_limits[1], x_limits[2]),
              ylim = c(y_limits[1], y_limits[2]),
              expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")
invisible(dev.off())

##### Check out function
# df <- expand.grid(x = seq(x_limits[1], x_limits[2], by = .5),
#                   y = seq(y_limits[1], y_limits[2], by = .5)) %>%
#   mutate(distance = sqrt((x - circle_h)^2 + (y - circle_k)^2)) %>%
#   mutate(prob = 1 / (1 + exp(-.5 * (distance - 10))))
#
# ggplot(data = df) +
#   geom_point(aes(x, y, color = prob)) +
#   coord_equal()

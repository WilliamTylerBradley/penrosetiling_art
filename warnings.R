##-----
# Description
##-----
# This file produces an output that has v-shaped lines with alternating
# big yellow shapes and small red shapes. There is a transition between them
# that goes to black.

# Libraries
# devtools::install_github("WilliamTylerBradley/penrosetiling")
library(penrosetiling)
library(tidyverse)

# This section starts off by getting a rhombus and
# subdividing down 6 times
check_substitution <- function(df, x) {
  substitution(df)
}

tiles <- return_rhombus(-150, 0, 36, 200, 'T')

tiles <- reduce(.x = rep(list(1), 6),
                .f = check_substitution,
                .init = tiles)

# This function checks where the mean of the triangle falls along a sin curve.
# The curve moves along the y-axis and is offset based on the x value.
# If the value is lower than x, it is subdivided.
# So triangles that fall in valleys will not be subdivided a lot while
# triangles in valleys will be.
check_substitution <- function(df, x) {
  df <- df %>%
    group_by(rhombus) %>%
    mutate(center_x = mean((point1_x + point2_x + point3_x) / 3),
           center_y = mean((point1_y + point2_y + point3_y) / 3)) %>%
    ungroup() %>%
    mutate(subdivide_amount = ceiling(asin(sin((abs(center_x) - center_y)/12)) /
                                        (pi/2)
                               * 3.5 + 3.5)) %>% # 12 and 3.5 make it look nice
    mutate(subdivide = if_else(subdivide_amount < x, TRUE, FALSE))

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
    ungroup()

  return(df)
}

tiles <- reduce(.x = as.list(seq(2, 7)),
                .f = check_substitution,
                .init = tiles)

tiles <- pivot_down(tiles)

# Use center of rhombus to decide the color value
tiles <- tiles %>%
  group_by(rhombus) %>%
  mutate(center_x = mean(x),
         center_y = mean(y)) %>%
  mutate(depth = asin(sin((abs(center_x) - center_y)/12)) / (pi/2))

ggplot(data = tiles) +
  geom_polygon(aes(x = x,
                   y = y,
                   color = depth,
                   fill = depth,
                   group = triangle),
               lwd = .15) +
  geom_polygon(aes(x = x,
                   y = y,
                   fill = depth,
                   group = triangle),
               color = NA) +
  scale_color_gradient2(low = "#F32A2A", # Red
                       mid = "#6B6355", # Gray
                       high = "#FFE500") + # Yellow
  scale_fill_gradient2(low = "#F32A2A",
                       mid = "#6B6355",
                       high = "#FFE500") +
  coord_equal(xlim = c(-50, 50),
                  ylim = c(0, 170),
                  expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")

ggsave("warnings.png",
       width = 3,
       height = 3 * 1.7)

##### Check out function
# df <- expand.grid(x = seq(-50, 50),
#                   y = seq(0, 170)) %>%
#   mutate(subdivide_value = ceiling(asin(sin((abs(x) - y)/12)) /
#                            (pi/2) * 3.5 + 3.5),
#          color_value = asin(sin((abs(x) - y)/12)) / (pi/2))
#
# ggplot(data = df) +
#   geom_point(aes(x, y, color = subdivide_value))
#
# ggplot(data = df) +
#   geom_point(aes(x, y, color = color_value)) +
#   scale_color_gradient2(low = "#F32A2A", # Red
#                         mid = "#6B6355", # Gray
#                         high = "#FFE500")

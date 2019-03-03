# shelf(desiderata, dplyr, tidyr)
# 
# round_rgb <- function(hexcol, to = 1, dir = NULL) {
#     col2rgb(hexcol) %>%
#         round_to_nearest(to = to, dir = dir) %>%
#         paste(collapse = " ")
# }
# 
# round_hsv <- function(hexcol, to = 1, dir = NULL) {
#     col2hsv(hexcol) %>% 
#         as.matrix() %>% 
#         round_to_nearest(to = to, dir = dir) %>%
#         paste(collapse = " ")
# }



# Generate all possible #RGB colours --------------------------------------

# #RGB expands to #RRGGBB. The full #RRGGBB space has 16,777,215 combinations,
# so I will just stick with my 4,096 combinations.


# This approach did not work very well!

# values <- c("00", "11", "22", "33", "44", "55", "66", "77", "88", "99", "AA", "BB", "CC", "DD", "EE", "FF")
# 
# all_hex <- 
#     tibble(r = NA_character_, g = NA_character_, b = NA_character_) %>% 
#     complete(r = values, g = values, b = values) %>% 
#     mutate(hex = paste0("#", r, g, b)) %>% 
#     select(-(r:b)) %>% 
#     rowwise() %>%
#     mutate_at(vars(hex),
#               list(rgb_any =  ~ round_rgb(., 50),
#                    rgb_down = ~ round_rgb(., 50, "down"),
#                    rgb_up =   ~ round_rgb(., 50, "up"),
#                    hsv =      ~ round_hsv(., 0.25, "up"),
#                    hsv_down = ~ round_hsv(., 0.25, "up"),
#                    hsv_up =   ~ round_hsv(., 0.25, "up")))
# 
# all_hex %>%
#     distinct(rgb_any,  .keep_all = TRUE) %>%
#     distinct(rgb_down, .keep_all = TRUE) %>%
#     distinct(rgb_up,   .keep_all = TRUE) %>%
#     distinct(hsv,      .keep_all = TRUE) %>%
#     distinct(hsv_down, .keep_all = TRUE) %>%
#     distinct(hsv_up,   .keep_all = TRUE) %>%
#     use_series("hex") %>% 
#     show_colours()





# Generate distant colours from the palettes I have -----------------------

# raw_cols <-
#     tibble(hex = c(palette_builtin(),
#                    palette_distinct(),
#                    palette_mrmrs(),
#                    palette_picked(),
#                    c("#A9A9A9", "#000075", "#FFD8B1", "#808000", "#AAFFC3", "#800000", "#FFFAC8", "#9A6324", "#E6BEFF", "#469990",
#                      "#FABEBE", "#BFEF45", "#F032E6", "#42D4F4", "#911EB4", "#F58231", "#4363D8", "#FFE119", "#3CB44B", "#E6194B",
#                      "#666666", "#A6761D", "#E6AB02", "#66A61E", "#E7298A", "#7570B3", "#D95F02", "#1B9E77", "#A6CEE3", "#1F78B4",
#                      "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928",
#                      "#B3B3B3", "#E5C494", "#FFD92F", "#A6D854", "#E78AC3", "#8DA0CB", "#FC8D62", "#66C2A5", "#CD7EAA", "#D5683A",
#                      "#2A78B5", "#F1E54E", "#2AA179", "#62B6E6", "#E7A337"))) %>%
#     filter(hex %notin% c("#FFFFFF", "#000000", "#FCFCFC", "#030303")) %>%
#     distinct() %>%
#     bind_cols(., col2hsv(.$hex)) %>%
#     arrange(h, s, v)
# 
# 
# 
# unique_cols <-
#     raw_cols %>%
#     rowwise() %>%
#     mutate_at(vars(starts_with("hex")),
#               list(rgb_any =  ~ round_rgb(., 50),
#                    rgb_down = ~ round_rgb(., 50, "down"),
#                    rgb_up =   ~ round_rgb(., 50, "up"))) %>%
#     mutate_at(vars(h, s, v), list(any =  ~ round_to_nearest(., 0.0001),
#                                   down = ~ round_to_nearest(., 0.0001, "down"),
#                                   up =   ~ round_to_nearest(., 0.0001, "up"))) %>%
#     distinct(rgb_any,  .keep_all = TRUE) %>%
#     distinct(rgb_down, .keep_all = TRUE) %>%
#     distinct(rgb_up,   .keep_all = TRUE) %>%
#     distinct(h_any,  .keep_all = TRUE) %>%
#     distinct(s_any, .keep_all = TRUE) %>%
#     distinct(v_any,   .keep_all = TRUE) %>%
#     distinct(h_down,  .keep_all = TRUE) %>%
#     distinct(s_down, .keep_all = TRUE) %>%
#     distinct(v_down,   .keep_all = TRUE) %>%
#     distinct(h_up,  .keep_all = TRUE) %>%
#     distinct(s_up, .keep_all = TRUE) %>%
#     distinct(v_up,   .keep_all = TRUE)
# 
# cols <- sample(unique_cols$hex)
# show_colours(unique_cols$hex)
# clippy(unique_cols$hex)





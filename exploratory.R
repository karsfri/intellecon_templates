
# Notes -------------------------------------------------------------------

# Look at patchwork https://github.com/thomasp85/patchwork#patchwork
# and ggThemeAssist https://github.com/calligross/ggthemeassist


# Setup -------------------------------------------------------------------

library(tidyverse)
library(extrafont)
library(scales)

source('C:/Users/ksf/Google Drive/R/intellecon_theme/owerwrite_geoms.R')

# Pallettes ---------------------------------------------------------------
red <- "#832301"         # Vínrauður

gold <- "#A7913D"        # Gull,
gray <- "#999999"        # Grár)
purple <-"#77567A"       # Fjólublár
dark_gray <- "#585E6E"   # Dökk grár
green <- "#588157"       # Green
cream <- "#CAC1AC"       # Krem
orange <- "#D16014" # Ljósbrúnn


panel.grid.colour <- "gray"




# intellecon_colours <- c(
#     gold,
#     gray,
#     purple,
#     light_brown,
#     dark_gray,
#     green,
#     cream
# ) 

intellecon_colours <- c(
    "#e5a233",
    "#ec5b33",
    "#978d83",
    "#51b2e4",
    "#a7af4c",
    "#58564e",
    "#fab23f",
    "#436164",
    "#687237",
    "#cb5200",
    "#960600",
    "#532200"
)

# sf_obj %>%
#     transform_ice_map() %>%
#     mutate(test = runif(n = n(), 0, 1)) %>%
#     ggplot(aes(fill = NRUMDSYSLU %>% as.factor)) +
#     theme_void() +
#     geom_sf() +
#     theme(panel.grid = element_blank()) +
#     scale_fill_manual(values = intellecon_colours)
#     # scale_fill_manual(values = intellecon_colours %>% rep(2)) +
#     # coord_sf(xlim = c(-22.1, -21.6), ylim = c(64.05, 64.2)) +
#     NULL
#     
# sf_obj %>%
#     transform_ice_map() %>%
#     mutate(test = runif(n = n(), -1, 1)) %>%
#     ggplot(aes(fill = test)) +
#     theme_void() +
#     geom_sf() +
#     theme(panel.grid = element_blank()) +
#     scale_fill_gradient(high = intellecon_colours[5], low = intellecon_colours[2])+
# # scale_fill_manual(values = intellecon_colours %>% rep(2)) +
# # coord_sf(xlim = c(-22.1, -21.6), ylim = c(64.05, 64.2)) +
# NULL
# 
# brewer_pal
#     
# 
# ggplot(dsamp, aes(carat, price)) + 
#     geom_col(aes(fill = clarity), position = position_stack()) +
#     scale_fill_manual(values = intellecon_colours) +
#     coord_cartesian(xlim = c(1, 1.3)) 
# 
# dsamp %>% 
#     group_by(carat) %>% 
#     summarise(price = mean(price)) %>% 
#     ggplot(aes(x = carat, y = price ^ 2, fill = price)) + 
#     geom_col(position = position_stack()) +
#     scale_fill_gradient(low = intellecon_colours[1], high = intellecon_colours[2]) +
#     coord_cartesian(xlim = c(1, 1.3)) 
    

intellecon_colours <- intellecon_colours %>% rep(30)

# For diverging colour scales use gradient2
# ggplot(df, aes(x, y)) +
#     geom_point(aes(colour = z1)) +
#     scale_colour_gradient2(mid = intellecon_colours[1], low = intellecon_colours[2], high = intellecon_colours[5])

col_title <- red

# Fonts -------------------------------------------------------------------

windowsFonts(Garamond = windowsFont("Garamond"))


# Helpfunctions -----------------------------------------------------------



my_case <- function(x){
    require(Hmisc)
    capitalize(tolower(x))
}


# General_theme -----------------------------------------------------------
theme_intellecon <- 
    theme_grey() +
    theme(
        # Default colours
        rect = element_rect(fill = red, color = NA),
        text = element_text(family = "Garamond", color = red, size = 10 * 4 / 3),
        # aspect.ratio = aspect_ratio,
        
        # Plot
        plot.background = element_rect(fill = NA, color = NA),
        plot.margin = margin(t = 0.2, b = 0.2, l = 0.2, r = 0.2, unit = "cm"),
        # aspect.ratio = 9/18,
        
        # legend
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(), 
        legend.key.size = unit(0.4, "cm"),
        legend.justification = 0.5,
        legend.box.background = element_rect(fill = NA, colour = NA),
        # legend.box.margin = margin(t = -0.2, b = -0.2, unit = "cm"),
        # legend.title.align = 1,
        
        # Panel
        panel.background = element_rect(fill = NA),
        # panel.border = element_blank(), 
        panel.grid.major.y = element_line(colour = gray, linetype = 2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.spacing.x = unit(0.2, "cm"),
        panel.spacing.y = unit(0.2, "cm"),

        # Axis
        axis.ticks = element_blank(),
        axis.line = element_line(),
        axis.line.y = element_blank(),
        axis.line.x.bottom = element_line(colour = "black", linetype = 1, size = 0.2),
        
        # Strips
        strip.background = element_rect(fill = "grey70", colour = NA),
        
        # Text
        plot.title    = element_text(size = 18 * 4 / 3, hjust = 0),
        plot.subtitle = element_text(size = 12 * 4 / 3, hjust = 0, colour = gray),
        plot.caption  = element_text(size = 12 * 4 / 3, hjust = 1, colour = gray, face = "italic"),
        axis.title.x  = element_text(size = 12 * 4 / 3, hjust = 0.5,
                                     margin = margin(0, 0, 0, 0, unit = "cm")),
        axis.title.y  = element_text(size = 12 * 4 / 3, vjust = 0.5,
                                     angle = 90, hjust = 0.5, 
                                     margin = margin(t = 0, r = 5, b = 0, l = 5, unit = "pt")),
        axis.text = element_text(colour = red),
        legend.text = element_text(size = 9 * 4 / 3),
        strip.text.x = element_text(colour = red, size = 7 * 4 / 3),
        strip.text.y = element_text(colour = red, size = 7 * 4 / 3, angle = 90),
        # strip.text = element_text(colour = grey, size = rel(0.8)),
        
        complete = TRUE
    ) 


theme_set(new = theme_intellecon)

theme_intellecon_sleek <- theme_intellecon +
    theme(axis.line = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_blank(),
          plot.caption = element_blank(),
          plot.subtitle = element_blank(),
          axis.text = element_blank(),
          legend.key = element_blank()) 


# Specific Themes ---------------------------------------------------------
theme_facet <- 
    theme_intellecon + 
    theme()


# Theme elements ----------------------------------------------------------




# ggplot elements ---------------------------------------------------------

# Geoms
# geom_col <- function(...){geom_col(scales = scale_y_continous, ...)}

# ScalesS
labels_isl <- scales::format_format(big.mark = ".",
                                   scientific = FALSE,
                                   decimal.mark = ",")

labels_us <- scales::comma_format()

labels_kr <-  scales::dollar_format(prefix = "",
                                    suffix = " kr.",
                                    largest_with_cents = 0, 
                                    big.mark = ".",
                                    decimal.mark = ",",
                                    negative_parens = FALSE,
                                    scientific = FALSE)

scale_fill_discrete <- function(...){
    ggplot2::scale_fill_manual(values = intellecon_colours,
                      ...)
}

scale_colour_discrete <- function(...){
    ggplot2::scale_colour_manual(values = intellecon_colours,
                               ...)
}

scale_colour_ordinal <- function(...){
    ggplot2::scale_colour_manual(values = intellecon_colours,
                                 ...)
}



scale_y_continuous <- function(labels = labels_isl,
                               expand = expand_scale(mult = c(0 , 0.05)),
                              ...){
    ggplot2::scale_y_continuous(labels = labels, expand = expand, ...)
}


# geom_bar <- function(width = 0.5, ...){ggplot2::geom_bar(width = width, ...)}

# ggplot defaults ---------------------------------------------------------

update_geom_defaults("boxplot", list(fill = intellecon_colours[1]))
update_geom_defaults("boxplot", list(outlier.colour = intellecon_colours[2]))
update_geom_defaults("col", list(fill = intellecon_colours[1], colour = NA))
update_geom_defaults("bar", list(fill = intellecon_colours[1], colour = NA))
update_geom_defaults("line", list(colour = intellecon_colours[1]))
update_geom_defaults("point", list(colour = intellecon_colours[1], size = 3))

GeomBar$default_aes["fill"] <- intellecon_colours[1]

# Saving ------------------------------------------------------------------



save_intellecon_word <- function(filename, plot = last_plot(), device = "svg", width = 12 * 4 / 3,
                                     height = 6, units = "cm", dpi = 720, ...){
    ggsave(filename = filename, plot = plot, device = device, width = width, height = height, units = units,
           dpi = dpi, ...)
}

save_intellecon_vertical <- function(filename, plot = last_plot(), device = "svg", width = 8 * 4 / 3,
                      height = 12, units = "cm", dpi = 720, ...){
    ggsave(filename = filename, plot = plot, device = device, width = width, height = height, units = units,
           dpi = dpi, ...)
}

save_intellecon_horizontal <- function(filename, plot = last_plot(), device = "svg", width = 18 * 4 / 3,
                                     height = 7, units = "cm", dpi = 720, ...){
    ggsave(filename = filename, plot = plot, device = device, width = width, height = height, units = units,
           dpi = dpi, ...)
}


save_intellecon_full <- function(filename, plot = last_plot(), device = "svg", width = 18 * 4 / 3,
                                       height = 14, units = "cm", dpi = 720, ...){
    ggsave(filename = filename, plot = plot, device = device, width = width, height = height, units = units,
           dpi = dpi, ...)
}


save_intellecon_facebook <- function(filename, plot = last_plot(), device = "tiff", width = 7 * 4 / 3,
                                 height = 7, units = "cm", dpi = 200, scale = 2, ...){
    ggsave(filename = filename, plot = plot, device = device, width = width, height = height, units = units,
           dpi = dpi, scale = scale, ...)
}


# Test plots --------------------------------------------------------------

# library(tidyverse)
# mtcars
# 
# mtcars %>% 
#     group_by(gear, cyl) %>% 
#     summarise(mpg = sum(mpg)) %>% 
#     ggplot(aes(x = cyl, y = mpg * 1000, fill = gear %>% as.factor())) + 
#     geom_col() + 
#     labs(x = "Cylinders", y = "Miles per Gallon", 
#          fill = "Gears") 
# 
# save_intellecon_full("test_col_full.svg")
# save_intellecon_horizontal("test_col_horizontal.svg")
# save_intellecon_vertical("test_col_vertical.svg")
# save_intellecon_word("test_col_word.svg")
# 
# mtcars %>% 
#     group_by(carb, cyl) %>% 
#     summarise(mpg = sum(mpg)) %>% 
#     ggplot(aes(x = cyl %>% as.factor(), y = mpg, fill = carb %>% as.factor())) + 
#     geom_col(position = position_dodge()) +
#     geom_text(aes(label = mpg), position = position_dodge(width = 0.5), colour = "white") +
#     theme_intellecon_sleek +
#     theme(axis.text = element_blank()) +
#     labs(x = NULL, fill = NULL)
# 
# save_intellecon_full("test_col_full_sleek.svg")
# save_intellecon_horizontal("test_col_horizontal_sleek.svg")
# save_intellecon_vertical("test_col_vertical_sleek.svg")
# save_intellecon_word("test_col_word_sleek.svg")
# 
# mtcars %>% ggplot(aes(x = cyl, y = mpg * 1000, fill = gear %>% as.factor())) + 
#     geom_col() + 
#     labs(x = "Cylinders", y = "Miles per Gallon", 
#          title = "Þetta er titill myndar", 
#          subtitle = "Og þetta er undirtitill hennar",
#          caption = "Hér eru heimild og þess háttar",
#          fill = "Gears")
# 
# 
# 
# save_plot("test_titles.png")
# 
# mtcars %>% ggplot(aes(x = gear)) + geom_bar()
# save_plot("test_bar.png")
# 
# mtcars %>% ggplot(aes(x = gear, y = mpg)) + geom_point()
# save_plot("test_point.png")
# 
# 
# 
# # Examples from help ------------------------------------------------------
# # geom_bar is designed to make it easy to create bar charts that show
# # counts (or sums of weights)
# g <- ggplot(mpg, aes(class))
# # Number of cars in each class:
# g + geom_bar()
# # Total engine displacement of each class
# g + geom_bar(aes(weight = displ), width = 0.5)
# 
# # Bar charts are automatically stacked when multiple bars are placed
# # at the same location. The order of the fill is designed to match
# # the legend
# g + geom_bar(aes(fill = drv))
# 
# # If you need to flip the order (because you've flipped the plot)
# # call position_stack() explicitly:
# g +
#     geom_bar(aes(fill = drv), position = position_stack(reverse = TRUE)) +
#     coord_flip() +
#     theme(legend.position = "top")
# 
# # To show (e.g.) means, you need geom_col()
# df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
# ggplot(df, aes(trt, outcome)) +
#     geom_col()
# # But geom_point() displays exactly the same information and doesn't
# # require the y-axis to touch zero.
# ggplot(df, aes(trt, outcome)) +
#     geom_point()
# 
# # You can also use geom_bar() with continuous data, in which case
# # it will show counts at unique locations
# df <- data.frame(x = rep(c(2.9, 3.1, 4.5), c(5, 10, 4)))
# ggplot(df, aes(x)) + geom_bar()
# # cf. a histogram of the same data
# ggplot(df, aes(x)) + geom_histogram(binwidth = 0.5)
# 
# dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
# 
# 
# (d <- ggplot(dsamp, aes(carat, y = price)) + geom_point(aes(colour = clarity)))
# 
# 
# # Default colour scale colours from light blue to dark blue
# df <- data.frame(
#     x = runif(100),
#     y = runif(100),
#     z1 = rnorm(100),
#     z2 = abs(rnorm(100))
# )
# # Equivalent fill scales do the same job for the fill aesthetic
# ggplot(faithfuld, aes(waiting, eruptions)) +
#     geom_raster(aes(fill = density)) +
#     scale_fill_gradientn(colours = terrain.colors(10))
# 
# 
# 
# # accommodation - gif -----------------------------------------------------
# 
# 
# gn_tidy <- gn %>% 
#     mutate(month = Mánuður %>% as_factor() %>% fct_inorder() %>% as.numeric(),
#            time = myd(paste(month, Ár, "01"))) %>% 
#     rename(n = `Gistinætur og gestakomur á öllum tegundum skráðra gististaða 1998-2018`,
#            Land = Ríkisfang)
# 
# cards %>% 
#     mutate(month = Mánuður %>% as_factor() %>% fct_inorder() %>% as.numeric(),
#            time = myd(paste(month, Ár, "01"))) %>% 
#     rename(Land = Útgáfuland) %>% 
#     left_join(gn_tidy) %>% 
#     ggplot(aes(x = n, y = `Mánaðarleg kortavelta erlendra ferðamanna eftir þjóðerni`, color = Land)) +
#     geom_point() +
#     gganimate::transition_time(time) +
#     gganimate::ease_aes('linear')
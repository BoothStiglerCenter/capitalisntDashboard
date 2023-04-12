require(showtext)
require(ggpubr)
require(ggtext)




theme_stigler <- function() {
    font_add(
        family = "Trade Gothic LT Std",
        regular = "theme_materials/Trade Gothic LT Std/Trade Gothic LT Std Regular.otf",
        bold = "theme_materials/Trade Gothic LT Std/Trade Gothic LT Std Bold.otf",
        italic = "theme_materials/Trade Gothic LT Std/Trade Gothic LT Std Oblique.otf"
    )
    showtext_auto()

    font <- "Trade Gothic LT Std"
    theme_minimal() %+replace%
        theme(

            ### DEBUGGING
            plot.background = element_rect(
                color = "palevioletred1",
                linewidth = 3
            ),

            ### Setup
            plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
            line = element_line(
                color = "#676E73"
            ),
            text = element_text(
                family = font
            ),

            ### Labelling
            plot.title = element_markdown(
                # face = "bold", # Typeface style
                size = 18, # Size (pt)
                hjust = 0, # hjust = 0 "left-aligned"
                vjust = 0, # vjust = 0 "top-aligned"
                margin = margin(t = 2.5, r = 0, b = 0, l = 0, unit = "pt"),
                debug = FALSE
            ),
            plot.subtitle = element_markdown(
                size = 16, # Size (pt)
                hjust = 0, # hjust = 0 "left-aligned"
                vjust = 0, # vjust = 0 "top-aligned"
                margin = margin(t = 5, r = 0, b = 0, l = 0, unit = "pt"),
                debug = FALSE
            ),
            plot.caption.position = "panel",
            plot.caption = element_text(
                size = 12,
                hjust = 0,
                vjust = 0,
                margin = margin(t = 5, r = 0, b = 0, l = 0, unit = "pt"),
                debug = FALSE
            ),
            plot.tag.position = c(0, 1),
            plot.tag = element_text(
                size = 14,
                color = "#800000",
                hjust = 0,
                vjust = 0,
                debug = FALSE
            ),

            ### Axes
            axis.title.x = element_text(
                family = font,
                face = "italic",
                size = 12,
                hjust = 0.5,
                vjust = 1,
                margin = margin(t = 5, r = 0, b = 0, l = 0, unit = "pt"),
                debug = FALSE
            ),
            axis.title.y = element_blank(),
            axis.text = element_text(
                family = font
            ),
            axis.text.y.right = element_text(
                hjust = 1,
                vjust = -0.5,
                margin = margin(t = -0, r = 10, b = 0, l = -40, unit = "pt"),
                debug = FALSE
            ),
            axis.line.x = element_line(
                color = "black",
                linewidth = 0.75
            ),
            axis.ticks.x = element_line(
                color = "black",
            ),
            axis.ticks.length.x = unit(5, unit = "pt"),

            ### Panel features
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),

            ### Legend features
            legend.position = "top",
            legend.justification = "left",
            legend.title.align = 0,
            legend.title = element_markdown(
                family = font,
                size = 12,
                hjust = 0,
                vjust = 0.5,
                debug = FALSE
            ),
            legend.text = element_markdown(
                family = font,
                size = 12,
                hjust = 0,
                vjust = 0.5,
                debug = FALSE
            )
        )
}


stigler_colors <- c(
    booth_maroon = "#800000",
    booth_teal = "#115E67",
    black_bean = "#4D0000",
    persian_red = "#BA402C",
    burnt_sienna = "#EA6A51",
    pale_tangerine = "#FFA487",
    midnight_green = "#00323B",
    munsell_blue = "#4C8F98",
    sky_blue = "#7EC0CA",
    celeste = "#BAF5FF"
)

stigler_cols <- function(...) {
    cols <- c(...)
    if (is.null(cols)) {
        return(stigler_colors)
    }
    stigler_colors[cols]
}

stigler_palettes <- list(
    `main` = stigler_cols(
        "booth_maroon",
        "booth_teal",
        "persian_red",
        "munsell_blue",
        "burnt_sienna",
        "sky_blue",
        "pale_tangerine",
        "celeste",
        "black_bean",
        "midnight_green"
    ),
    `reds` = stigler_cols(
        "booth_maroon",
        "persian_red",
        "burnt_sienna",
        "pale_tangerine",
        "black_bean"
    ),
    `blues` = stigler_cols(
        "booth_teal",
        "munsell_blue",
        "sky_blue",
        "celeste",
        "midnight_green"
    ),
    `red_to_blue` = stigler_cols(
        "booth_maroon",
        "booth_teal"
    ),
    `reds_2` = stigler_cols(
        "black_bean",
        "pale_tangerine"
    ),
    `blues_2` = stigler_cols(
        "midnight_green",
        "celeste"
    )
)

stigler_pal <- function(palette = "main", reverse = FALSE, ...) {
    pal <- stigler_palettes[[palette]]

    if (reverse) pal <- rev(pal)

    colorRampPalette(pal)
}

scale_color_stigler <- function(
    palette = "main",
    reverse = FALSE,
    discrete = TRUE,
    ...
    ) {
        pal <- stigler_pal(palette = palette, reverse = reverse, ...)
        if (discrete) {
            discrete_scale("color", paste0("stigler_", palette), palette = pal, ...)
        } else {
            scale_color_gradientn(colors = pal(256), ...)
        }
}

scale_fill_stigler <- function(
    palette = "main",
    reverse = FALSE,
    discrete = TRUE,
    ...
) {
    pal <- stigler_pal(palette = palette, reverse = reverse,...)
    if (discrete) {
        discrete_scale("fill", paste0("stigler_", palette), palette = pal, ...)
    } else {
        scale_fill_gradient(colors = pal(256), ...)
    }

}
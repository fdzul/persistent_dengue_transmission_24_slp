mp_transmission_chains <- function(data,
                                   yrs,
                                   locality,
                                   cve_edo){
    # Step 2. 
    data <- data |>
        dplyr::filter(ANO %in% c(yrs))
    
    #
    loc <- rgeomex::extract_locality(cve_edo = cve_edo,
                                     locality = locality) |>
        dplyr::select(-NOMGEO)
    data <- data[loc, ]
    
    knox_res <- denhotspots::knox(x = data,
                                  crs = "+proj=eqc",
                                  ds = 400, # distance in meters
                                  dt = 20,  # days 0 to 20 day
                                  sym = 1000,
                                  sp_link = FALSE, # for sf
                                  planar_coord = FALSE)
    ###################
    z <- knox_res$x |>
        dplyr::mutate(week = lubridate::epiweek(onset)) |>
        dplyr::mutate(week_factor = dplyr::case_when(week <= 10 ~ "1-10",
                                                     week > 10 & week <= 20 ~ "11-20",
                                                     week > 20 & week <= 25 ~ "21-25",
                                                     week > 25 & week <= 30 ~ "26-30",
                                                     week > 30 & week <= 35 ~ "31-35",
                                                     week > 35 & week <= 40 ~ "36-40",
                                                     week > 40 & week <= 45 ~ "41-45",
                                                     week > 45 & week <= 53 ~ "46-53")) |>
        dplyr::mutate(week_factor = factor(week_factor,
                                           levels = c("1-10",
                                                      "11-20",
                                                      "21-25",
                                                      "26-30",
                                                      "31-35",
                                                      "36-40",
                                                      "41-45",
                                                      "46-53"),
                                           ordered = TRUE)) |>
        sf::st_as_sf(coords = c("x", "y"),
                     crs = "+proj=eqc") |>
        sf::st_transform(crs = 4326)
    
    
    # Step 5. load the Space-Time link ####
    st_link <- knox_res$space_time_link |>
        sf::st_set_crs(value = 4326)
    
    # Step 6. extract the dengue cases of space links ####
    w <- z[st_link,] |>
        dplyr::mutate(week = lubridate::epiweek(onset)) |>
        dplyr::mutate(week_factor = dplyr::case_when(week <= 10 ~ "1-10",
                                                     week > 10 & week <= 20 ~ "11-20",
                                                     week > 20 & week <= 25 ~ "21-25",
                                                     week > 25 & week <= 30 ~ "26-30",
                                                     week > 30 & week <= 35 ~ "31-35",
                                                     week > 35 & week <= 40 ~ "36-40",
                                                     week > 40 & week <= 45 ~ "41-45",
                                                     week > 45 & week <= 53 ~ "46-53")) |>
        dplyr::mutate(week_factor = factor(week_factor,
                                           levels = c("1-10",
                                                      "11-20",
                                                      "21-25",
                                                      "26-30",
                                                      "31-35",
                                                      "36-40",
                                                      "41-45",
                                                      "46-53"),
                                           ordered = TRUE)) |>
        dplyr::mutate(x = sf::st_coordinates(geometry)[,1],
                      y = sf::st_coordinates(geometry)[,2])
    
    
    
    
    
    # Step 7. add the week  to space-time link  ####
    st_link_week <- sf::st_join(x = st_link,
                                y = w[, c("week_factor")])
    
    
    w$week_factor <- factor(w$week_factor, 
                            levels = sort(unique(w$week_factor)))
    
    st_link_week$week_factor <- factor(st_link_week$week_factor,
                                       levels = sort(unique(st_link_week$week_factor)))
    

    ###############
    ####
    # Paletas de colores
    n_weeks_w <- length(unique(w$week_factor))
    pal_w <- fishualize::fish(n = n_weeks_w,
                              option = "Scarus_hoefleri",
                              direction = -1)
    
    n_weeks_l <- length(unique(st_link_week$week_factor))
    pal_l <- fishualize::fish(n = n_weeks_l,
                              option = "Scarus_hoefleri",
                              direction = -1)
    
    # Niveles para leyenda y match_expr
    week_levels_w <- if (!is.null(levels(w$week_factor))) {
        levels(w$week_factor)
    } else {
        sort(unique(w$week_factor))
    }
    
    week_levels_l <- if (!is.null(levels(st_link_week$week_factor))) {
        levels(st_link_week$week_factor)
    } else {
        sort(unique(st_link_week$week_factor))
    }
    
    # match_expr
    make_match_expr <- function(data, col, palette) {
        lvls <- levels(data[[col]])
        if (is.null(lvls)) lvls <- sort(unique(data[[col]]))
        match_expr(
            column  = col,
            values  = lvls,
            stops   = palette[seq_along(lvls)],
            default = "#CCCCCC"
        )
    }
    
    color_expr_w <- make_match_expr(w, "week_factor", pal_w)
    color_expr_l <- make_match_expr(st_link_week, "week_factor", pal_l)
    #
    maplibre_view(loc,
                  color = NA,
                  #style = carto_style(style_name = "voyager")
                  style = carto_style(style_name = "positron")) |>
        add_source("area",  data = loc) |>
        add_source("casos", data = w) |>
        add_source("links", data = st_link_week) |>
        
        # Límites de la ciudad
        add_fill_layer(id           = "ciudad-fill",
                       source       = "area",
                       fill_color   = "transparent",
                       fill_opacity = 0) |>
        add_line_layer(id             = "ciudad-borde",
                       source         = "area",
                       line_color     = "#444444",
                       #line_dasharray = c(1, 1)
                       line_width     = 1) |>
        
        # transmission chains (links)
        add_line_layer(id         = "cadenas-links",
                       source     = "links",
                       line_color = color_expr_l,
                       line_width = 2,
                       tooltip    = "week_factor") |>
        
        # transmission chains (dengue cases)
        add_circle_layer(id                  = "cadenas-casos",
                         source              = "casos",
                         circle_color        = color_expr_w,
                         circle_stroke_color = "white",
                         circle_stroke_width = 1,
                         circle_radius       = 6,
                         tooltip             = "week_factor") |>
        # legend
        add_categorical_legend(legend_title = "Cadenas de Transmisión",
                               values       = week_levels_w,
                               colors       = pal_w[seq_along(week_levels_w)],
                               patch_shape = "square",
                               interactive = TRUE,
                               draggable = TRUE,
                               position     = "top-left") |>
        add_fullscreen_control(position = "top-left") |> 
        add_navigation_control() |>
        add_globe_control() |>
        add_scale_control()
}

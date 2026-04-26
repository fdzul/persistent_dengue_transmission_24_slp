mp_dengue_cases <- function(data,
                            yrs,
                            locality,
                            cve_edo){
    
    # Step 1. create the week
    data <- data |>
        dplyr::filter(ANO %in% c(yrs)) |>
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
                                           ordered = TRUE)) 
    # Step 2. extract the locality
    loc <- rgeomex::extract_locality(cve_edo = cve_edo,
                                     locality = locality) |>
        dplyr::select(-NOMGEO)
    
    # Step 3. extract the dengue cases in the locality
    data <- data[loc, ] 
    
    
    # map
    # Step 3. 1. crea the palette
    pal_colores <- fishualize::fish(n = 8,
                                    option = "Scarus_hoefleri",
                                    direction = -1)
    
    # week_num como character
    data$week_num <- as.character(as.integer(data$week_factor))
    
    # map
    # map
    c <- sf::st_centroid(loc) |>
        sf::st_coordinates()
    mapgl::maplibre(style  = carto_style("positron"),
                    #bounds = st_bbox(loc)
                    zoom = 10,
                    center = c(c[1], c[2])) |>
        mapgl::add_source("area", data = loc) |>
        mapgl::add_line_layer(id         = "ciudad-borde",
                              source     = "area",
                              line_color = "#444444",
                              line_width = 1) |>
        mapgl::add_circle_layer(id     = "casos-puntos",
                                source = data,
                                circle_color = mapgl::match_expr(column  = "week_num",
                                                                 values  = as.character(1:8),
                                                                 stops   = pal_colores),
                                circle_stroke_color = "white",
                                circle_stroke_width = 1,
                                circle_radius       = 6,
                                tooltip             = "week_factor") |>
        mapgl::add_categorical_legend(legend_title = "Semana",
                                      values       = c("1-10","11-20","21-25","26-30",
                                                       "31-35","36-40","41-45","46-53"),
                                      colors       = pal_colores,
                                      patch_shape  = "circle") |>
        mapgl::add_fullscreen_control(position = "top-left") |> 
        mapgl::add_navigation_control() |>
        mapgl::add_globe_control() |>
        mapgl::add_scale_control()
    
    
}
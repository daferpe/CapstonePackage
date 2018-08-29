
#' Clean data
#'
#' This is a simple function that clean the data
#'
#' @param data A data frame with raw data
#'
#' @return This function returns a dataframe with the information from the file after being cleaned.
#'
#' @importFrom tidyr unite
#' @importFrom dplyr %>% tbl_df select mutate
#' @importFrom lubridate ymd_h
#'
#' @examples
#' \dontrun{
#' data <- readr::read_delim("signif.tsv.gz", delim = "\t")
#' eq_clean_data(data)
#' }
#'
#' @export
eq_clean_data<-function(data){
  data %>%
    dplyr::select(COUNTRY,LOCATION_NAME, LATITUDE, LONGITUDE,YEAR, MONTH, DAY, HOUR, EQ_MAG_ML,DEATHS) %>%
    dplyr::mutate(LATITUDE= as.numeric(LATITUDE)) %>%
    dplyr::mutate(LONGITUDE= as.numeric(LONGITUDE))%>%
    tidyr::unite(Date, YEAR, MONTH, DAY, HOUR) %>%
    dplyr::mutate(Date = lubridate::ymd_h(Date))%>%
    dplyr::mutate(DEATHS=as.numeric(DEATHS)) %>%
    eq_location_clean()
}

#' Clean location
#'
#' This is a simple function that clean the column location in a dataframe
#'
#' @param data A data frame with raw data
#'
#' @return This function returns a dataframe with a cleaned LOCATION_NAME column
#'
#' @importFrom dplyr %>% mutate funs
#' @importFrom stringi stri_trans_totitle
#' @importFrom stringr str_trim
#'
#' @examples
#' \dontrun{
#' eq_location_clean(data)
#' }
#'
#' @export
eq_location_clean<-function(data){
  data%>%
    dplyr::mutate_each(dplyr::funs(gsub(".*: ", "", LOCATION_NAME)),LOCATION_NAME)%>%
    dplyr::mutate(LOCATION_NAME=stringi::stri_trans_totitle(LOCATION_NAME)) %>%
    dplyr::mutate(LOCATION_NAME=stringr::str_trim(LOCATION_NAME))
}

#' Time line of earthquakes
#'
#' @description This geom for ggplot2 called geom_timeline() for plotting a time line
#'  of earthquakes ranging from xmin to xmaxdates with a point for each earthquake.
#'  Optional aesthetics include color, size, and alpha (for transparency)
#'
#' @importFrom ggplot2 layer
#'
#' @examples
#' \dontrun{
#' data %>% eq_clean_data() %>%
#' dplyr::filter(Date >= "1980-01-01" & Date <="2014-01-01" & COUNTRY == c("ITALY","USA", "JORDAN")) %>%
#'   ggplot(aes(x = Date,
#'              y = COUNTRY,
#'              color = DEATHS,
#'              size = EQ_MAG_ML
#'   )) +
#'   geom_timeline() +
#'   theme_timeline()
#'
#' }
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {

  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @importFrom ggplot2 ggproto Geom aes draw_key_point
#' @importFrom grid pointsGrob gList gpar
#' @importFrom scales alpha
GeomTimeline <-
  ggplot2::ggproto(
    "GeomTimeline", ggplot2::Geom,
    required_aes = c("x"),
    default_aes = ggplot2::aes(colour = "grey", size = 1.5, alpha = 0.5,
                               shape = 21, fill = "grey", stroke = 0.5),
    draw_key = ggplot2::draw_key_point,
    draw_panel = function(data, panel_scales, coord) {

      if (!("y" %in% colnames(data))) {
        data$y <- 0.15
      }

      coords <- coord$transform(data, panel_scales)

      points <- grid::pointsGrob(
        coords$x, coords$y,
        pch = coords$shape, size = unit(coords$size / 4, "char"),
        gp = grid::gpar(
          col = scales::alpha(coords$colour, coords$alpha),
          fill = scales::alpha(coords$colour, coords$alpha)
        )
      )
      y_lines <- unique(coords$y)

      lines <- grid::polylineGrob(
        x = unit(rep(c(0, 1), each = length(y_lines)), "npc"),
        y = unit(c(y_lines, y_lines), "npc"),
        id = rep(seq_along(y_lines), 2),
        gp = grid::gpar(col = "grey",
                        lwd = .pt)
      )

      grid::gList(points, lines)
    }
  )

#' Time line labels of earthquakes
#'
#' @description This geom for adding annotations to the earthquake data. This geom adds a vertical line
#'  to each data point with a text annotation (e.g. the location of the earthquake) attached to each line
#'
#' @param n_max An integer. number of earthquakes, where we take the n_max largest (by magnitude) earthquakes
#'
#' @importFrom ggplot2 layer
#'
#' @examples
#' \dontrun{
#' data %>% eq_clean_data() %>%
#' dplyr::filter(Date >= "1980-01-01" & Date <="2014-01-01" & COUNTRY == c("ITALY","USA", "JORDAN")) %>%
#'   ggplot(aes(x = Date,
#'              y = COUNTRY,
#'              color = DEATHS,
#'              size = EQ_MAG_ML
#'   )) +
#'   geom_timeline() +
#'   geom_timeline_label(aes(label = LOCATION_NAME), n_max = 5) +
#'   theme_timeline()
#'
#' }
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", ..., na.rm = FALSE,
                                n_max = NULL, show.legend = NA,
                                inherit.aes = TRUE) {

  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n_max = n_max, ...)
  )
}

#' @importFrom ggplot2 ggproto Geom aes draw_key_point
#' @importFrom grid polylineGrob gList gpar textGrob
#' @importFrom dplyr %>% group_by_ top_n ungroup
GeomTimelineLabel <-
  ggplot2::ggproto(
    "GeomTimelineLabel", ggplot2::Geom,
    required_aes = c("x", "label"),
    draw_key = ggplot2::draw_key_blank,
    setup_data = function(data, params) {
      if (!is.null(params$n_max)) {
        if (!("size" %in% colnames(data))) {
          stop(paste("'size' aesthetics needs to be",
                     "provided when 'n_max' is defined."))
        }
        data <- data %>%
          dplyr::group_by_("group") %>%
          dplyr::top_n(params$n_max, size) %>%
          dplyr::ungroup()
      }
      data
    },
    draw_panel = function(data, panel_scales, coord, n_max) {

      if (!("y" %in% colnames(data))) {
        data$y <- 0.15
      }

      coords <- coord$transform(data, panel_scales)
      n_grp <- length(unique(data$group))
      offset <- 0.2 / n_grp

      lines <- grid::polylineGrob(
        x = unit(c(coords$x, coords$x), "npc"),
        y = unit(c(coords$y, coords$y + offset), "npc"),
        id = rep(1:dim(coords)[1], 2),
        gp = grid::gpar(
          col = "grey"
        )
      )

      names <- grid::textGrob(
        label = coords$label,
        x = unit(coords$x, "npc"),
        y = unit(coords$y + offset, "npc"),
        just = c("left", "bottom"),
        rot = 45
      )

      grid::gList(lines, names)
    }
  )

#' Theme for a correct visualization
#'
#' @description This is a theme to configure the look of the geom
#'
#' @importFrom ggplot2 theme element_blank element_line
#'
#' @examples
#' \dontrun{
#' data %>% eq_clean_data() %>%
#' dplyr::filter(Date >= "1980-01-01" & Date <="2014-01-01" & COUNTRY == c("ITALY","USA", "JORDAN")) %>%
#'   ggplot(aes(x = Date,
#'              y = COUNTRY,
#'              color = DEATHS,
#'              size = EQ_MAG_ML
#'   )) +
#'   geom_timeline() +
#'   theme_timeline()
#'
#' }
#'
#' @export
theme_timeline <- function() {
  ggplot2::theme(
    plot.background = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_line(size = 1),
    axis.ticks.y = ggplot2::element_blank(),
    legend.position = "bottom"
  )
}


#' Map of earthquakes
#'
#' @description The function maps the epicenters (LATITUDE/LONGITUDE) and annotates
#'  each point with in pop up window containing annotation data stored
#'  in a column of the data frame
#'
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#' @importFrom dplyr %>%
#'
#' @param data A data frame with the earthquake data
#' @param annot_col A string with the name of the column to show in the pop up
#'
#' @return A Map with earthquakes and annotations.
#'
#' @examples
#' \dontrun{
#' eq_map(data, annot_col = "Date")
#' }
#' @export
eq_map <- function(data, annot_col) {

  map <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lng = data$LONGITUDE, lat = data$LATITUDE,
                              radius = data$EQ_MAG_ML, weight = 1,
                              popup = data[[annot_col]])

  map
}


#' Creates a label for leaflet map
#'
#' @description This function takes the dataset as an argument and
#'  creates an HTML label that can be used as the annotation text in the leaflet map
#'
#' @param data A data frame with the earthquake data
#'
#' @return A vector with labels
#'
#' @export
#'
#' @examples
#' \dontrun{
#' eq_create_label(data)
#' }
eq_create_label <- function(data) {
  popup_text <- with(data, {
    part1 <- ifelse(is.na(LOCATION_NAME), "",
                    paste("<strong>Location:</strong>",
                          LOCATION_NAME))
    part2 <- ifelse(is.na(EQ_MAG_ML), "",
                    paste("<br><strong>Magnitude</strong>",
                          EQ_MAG_ML))
    part3 <- ifelse(is.na(DEATHS), "",
                    paste("<br><strong>Total deaths:</strong>",
                          DEATHS))
    paste0(part1, part2, part3)
  })
}

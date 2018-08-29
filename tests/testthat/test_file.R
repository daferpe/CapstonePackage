context("test_file.R")

filename <- system.file("data/signif.tsv.gz", package = "CapstonePackage")
data <- readr::read_delim(filename, col_names=T,delim = "\t",
                                  na = "-99")

test_that("eq_clean_data", {
  expect_is(eq_clean_data(data), "data.frame")
})

test_that("eq_location_clean", {
  expect_is(eq_clean_data(data)$LOCATION_NAME, "character")
})

test_that("geom_timeline", {
  g <- data %>% eq_clean_data() %>%
    dplyr::filter(Date >= "1980-01-01" & Date <="2014-01-01" & COUNTRY == c("ITALY","USA", "JORDAN")) %>%
    ggplot2::ggplot(ggplot2::aes(x = Date,
               y = COUNTRY,
               color = DEATHS,
               size = EQ_MAG_ML
    )) +
    geom_timeline()
  expect_is(g, "ggplot")
})

test_that("geom_timeline_label", {
  g <- data %>% eq_clean_data() %>%
    dplyr::filter(Date >= "1980-01-01" & Date <="2014-01-01" & COUNTRY == c("ITALY","USA", "JORDAN")) %>%
    ggplot2::ggplot(ggplot2::aes(x = Date,
                                 y = COUNTRY,
                                 color = DEATHS,
                                 size = EQ_MAG_ML
    )) +
    geom_timeline_label(ggplot2::aes(label = LOCATION_NAME))
  expect_is(g, "ggplot")
})

test_that("theme_timeline", {
  g <- data %>% eq_clean_data() %>%
    dplyr::filter(Date >= "1980-01-01" & Date <="2014-01-01" & COUNTRY == c("ITALY","USA", "JORDAN")) %>%
    ggplot2::ggplot(ggplot2::aes(x = Date,
                                 y = COUNTRY,
                                 color = DEATHS,
                                 size = EQ_MAG_ML
    )) +
    theme_timeline()
  expect_is(g, "ggplot")
})

test_that("eq_map", {
  l <- data %>%
    eq_clean_data() %>%
    dplyr::filter(Date >= "1980-01-01" & Date <="2014-01-01" & COUNTRY == c("ITALY","USA", "JORDAN")) %>%
    dplyr::mutate(popup_text = eq_create_label(.)) %>%
    eq_map(annot_col = "popup_text")
  expect_is(l, "leaflet")
})

test_that("eq_create_label", {
  expect_is(eq_create_label(data), "character")
})

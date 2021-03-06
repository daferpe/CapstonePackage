% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{geom_timeline_label}
\alias{geom_timeline_label}
\title{Time line labels of earthquakes}
\usage{
geom_timeline_label(mapping = NULL, data = NULL, stat = "identity",
  position = "identity", ..., na.rm = FALSE, n_max = NULL,
  show.legend = NA, inherit.aes = TRUE)
}
\arguments{
\item{n_max}{An integer. number of earthquakes, where we take the n_max largest (by magnitude) earthquakes}
}
\description{
This geom for adding annotations to the earthquake data. This geom adds a vertical line
 to each data point with a text annotation (e.g. the location of the earthquake) attached to each line
}
\examples{
\dontrun{
data \%>\% eq_clean_data() \%>\%
dplyr::filter(Date >= "1980-01-01" & Date <="2014-01-01" & COUNTRY == c("ITALY","USA", "JORDAN")) \%>\%
  ggplot(aes(x = Date,
             y = COUNTRY,
             color = DEATHS,
             size = EQ_MAG_ML
  )) +
  geom_timeline() +
  geom_timeline_label(aes(label = LOCATION_NAME), n_max = 5) +
  theme_timeline()

}

}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot_functions.R
\name{get_substation_waveform_plot_gg}
\alias{get_substation_waveform_plot_gg}
\title{Function to get meter waveform ggplot}
\usage{
get_substation_waveform_plot_gg(channel_reads, device_hierarchy, phase_groups,
  include_amps = T)
}
\arguments{
\item{channel_reads}{Channel reads dataframe}

\item{device_hierarchy}{Device hierarchy dataframe}

\item{phase_groups}{phase groups dataframe}

\item{include_amps}{Boolean flag on whether an amps plot should be included}
}
\value{
cowplot object
}
\description{
Function to get meter waveform ggplot
}

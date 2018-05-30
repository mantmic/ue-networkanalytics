#' Function to get channel voltage plotly plot
#' @export
#' @param cd output dataframe from format_channel_reads_plot()
#' @return plotly object
#' @import plotly
get_channel_voltage_plot_plotly <- function(cd){
  phase_colours <- get_phase_colours()
  return(
    plot_ly(
      cd, 
      x = ~interval_ts, 
      y = ~voltage_lvt, 
      split = ~nic_channel_id, 
      type = 'scatter', 
      mode = 'lines', 
      color = ~phase_group, 
      colors = phase_colours, 
      name = ~nic_channel_id , 
      opacity = 0.7, 
      line = list ( width = 1 ), 
      showlegend = F, 
      hoverinfo = 'text', 
      text = ~paste(nic_channel_id, '</br></br>', round(voltage_lvt,2), 'V</br>', round(amps_lct,2), 'A</br>', round(network_impedance_imp,2), 'Ω</br>', round(power_factor_pf,2), 'PF</br>', address, '</br>', as.character(interval_ts, format = '%D %H:%M', tz = 'Australia/Melbourne'), sep = '')
    )
  )
}

#' Function to get amps plotly plot
#' @export
#' @param cd output dataframe from format_channel_reads_plot()
#' @return plotly object
#' @import plotly
get_channel_amps_plot_plotly <- function(cd, aggregate_by_phase = F){
  phase_colours <- get_phase_colours()
  if(aggregate_by_phase == F){
    return(
      plot_ly(
        cd, 
        x = ~interval_ts, 
        y = ~amps_lct, 
        split = ~nic_channel_id, 
        type = 'scatter', 
        mode = 'lines', 
        color = ~phase_group, 
        colors = phase_colours, 
        name = ~nic_channel_id , 
        opacity = 0.7, 
        line = list ( width = 1 ), 
        showlegend = F, 
        hoverinfo = 'text', 
        text = ~paste(nic_channel_id, '</br></br>', round(voltage_lvt,2), 'V</br>', round(amps_lct,2), 'A</br>', round(network_impedance_imp,2), 'Ω</br>', round(power_factor_pf,2), 'PF</br>', address, '</br>', as.character(interval_ts, format = '%D %H:%M', tz = 'Australia/Melbourne'), sep = '')
      )
    )
  } else {
    #else aggregate amps
    #subset to only 5 minute intervals
    amps_data <- subset(cd,  as.numeric(format(interval_ts, '%M')) %in% ( (0:11) * 5 ) )
    amps_data <- subset(amps_data,  as.numeric(format(interval_ts, '%S')) == 0 )
    amps_data <- aggregate(amps_data$amps_lct ~ amps_data$interval_ts + amps_data$phase_group, FUN = sum )
    colnames(amps_data) <- c('interval_ts', 'phase_group', 'amps_lct')
    return(
      plot_ly(amps_data, x = ~interval_ts, y = ~amps_lct, split = ~phase_group, type = 'scatter', mode = 'lines', color = ~phase_group, colors = phase_colours, opacity = 1, line = list ( width = 2 ), showlegend = F, hoverinfo = 'text', text = ~paste( amps_lct, 'A</br></br>', as.character(interval_ts, format = '%D %H:%M', tz = 'Australia/Melbourne'), sep = ''))
    )
  }
}

#' Function to get power factor plotly plot
#' @export
#' @param cd output dataframe from format_channel_reads_plot()
#' @return plotly object
#' @import plotly
get_channel_pf_plot_plotly <- function(cd){
  phase_colours <- get_phase_colours()
  return(
    plot_ly(
      cd, 
      x = ~interval_ts, 
      y = ~power_factor_pf, 
      split = ~nic_channel_id, 
      type = 'scatter', 
      mode = 'lines', 
      color = ~phase_group, 
      colors = phase_colours, 
      name = ~nic_channel_id , 
      opacity = 0.7, 
      line = list ( width = 1 ), 
      showlegend = F, 
      hoverinfo = 'text', 
      text = ~paste(nic_channel_id, '</br></br>', round(voltage_lvt,2), 'V</br>', round(amps_lct,2), 'A</br>', round(network_impedance_imp,2), 'Ω</br>', round(power_factor_pf,2), 'PF</br>', address, '</br>', as.character(interval_ts, format = '%D %H:%M', tz = 'Australia/Melbourne'), sep = '')
    )
  )
}

#' Function to get impedance plotly plot
#' @export
#' @param cd output dataframe from format_channel_reads_plot()
#' @return plotly object
#' @import plotly
get_channel_imp_plot_plotly <- function(cd){
  phase_colours <- get_phase_colours()
  imp_cd <- subset(cd, !is.na(network_impedance_imp))
  if(nrow(imp_cd) == 0){
    return(
      plot_ly(
        cd, 
        x = ~interval_ts, 
        y = 0, 
        split = ~nic_channel_id, 
        type = 'scatter', 
        mode = 'lines', 
        color = ~phase_group, 
        colors = phase_colours, 
        name = ~nic_channel_id , 
        opacity = 0.7, 
        line = list ( width = 1 ), 
        showlegend = F, 
        hoverinfo = 'text', 
        text = ~paste(nic_channel_id, '</br></br>', round(voltage_lvt,2), 'V</br>', round(amps_lct,2), 'A</br>', round(network_impedance_imp,2), 'Ω</br>', round(power_factor_pf,2), 'PF</br>', address, '</br>', as.character(interval_ts, format = '%D %H:%M', tz = 'Australia/Melbourne'), sep = '')
      )
    )
  }
  return(
    plot_ly(
      imp_cd, 
      x = ~interval_ts, 
      y = ~network_impedance_imp, 
      split = ~nic_channel_id, 
      type = 'scatter', 
      mode = 'lines', 
      color = ~phase_group, 
      colors = phase_colours, 
      name = ~nic_channel_id , 
      opacity = 0.7, 
      line = list ( width = 1 ), 
      showlegend = F, 
      hoverinfo = 'text', 
      text = ~paste(nic_channel_id, '</br></br>', round(voltage_lvt,2), 'V</br>', round(amps_lct,2), 'A</br>', round(network_impedance_imp,2), 'Ω</br>', round(power_factor_pf,2), 'PF</br>', address, '</br>', as.character(interval_ts, format = '%D %H:%M', tz = 'Australia/Melbourne'), sep = '')
    )
  )
}

#' Function to get device plotly plot
#' @export
#' @param channel_reads Channel reads dataframe
#' @param device_hierarchy Device hierarchy dataframe
#' @param phase_groups Phase groups dataframe
#' @return plotly object
#' @import plotly
get_device_waveform_plotly <- function(channel_reads,phase_groups,device_hierarchy, aggregate_amps = F, include_imp = F, include_pf = F){
  #not a dataframe
  if(is.null(nrow(channel_reads))){
    return(plot_ly())
  }
  #no data
  if(nrow(channel_reads) == 0){
    return(plot_ly())
  }
  #combine and format data
  cd <- format_channel_reads_plot(channel_reads,phase_groups,device_hierarchy)
  #not a dataframe
  if(is.null(nrow(cd))){
    return(plot_ly())
  }
  #no data
  if(nrow(cd) == 0){
    return(plot_ly())
  }
  #get voltage plot
  volt_plot <- get_channel_voltage_plot_plotly(cd)
  #get amps plot
  amps_plot <- get_channel_amps_plot_plotly(cd,aggregate_amps)
  #there doesn't seem to be a clean way to append subplots, so the permutations here are going to be manual for now
  if(include_imp == F & include_pf == F){
    #Combine plots
    dw <- subplot(volt_plot, amps_plot, nrows = 2, shareX = T, shareY = F) %>%
      layout(
        title = '',
        hovermode = 'closest',
        xaxis = list(
          title = NULL
        ),
        yaxis = list (
          title = 'Voltage'
        ),
        yaxis2 = list (
          title = 'Current'
          , autorange = T 
        )
      )
  } else if (include_imp == F & include_pf == T){
    pf_plot <- get_channel_pf_plot_plotly(cd)
    #Combine plots
    dw <- subplot(volt_plot, amps_plot, pf_plot, nrows = 3, shareX = T, shareY = F) %>%
      layout(
        title = '',
        hovermode = 'closest',
        xaxis = list(
          title = NULL
        ),
        yaxis = list (
          title = 'Voltage'
        ),
        yaxis2 = list (
          title = 'Current'
          , autorange = T 
        ),
        yaxis3 = list (
          title = 'Power Factor'
          , autorange = T 
        )
      )
  } else if (include_imp == T & include_pf == F){
    imp_plot <- get_channel_imp_plot_plotly(cd)
    #Combine plots
    dw <- subplot(volt_plot, amps_plot, imp_plot, nrows = 3, shareX = T, shareY = F) %>%
      layout(
        title = '',
        hovermode = 'closest',
        xaxis = list(
          title = NULL
        ),
        yaxis = list (
          title = 'Voltage'
        ),
        yaxis2 = list (
          title = 'Current'
          , autorange = T 
        ),
        yaxis3 = list (
          title = 'Impedance'
          , autorange = T 
        )
      )
  } else {
    pf_plot <- get_channel_pf_plot_plotly(cd)
    imp_plot <- get_channel_imp_plot_plotly(cd)
    #Combine plots
    dw <- subplot(volt_plot, amps_plot, pf_plot, imp_plot, nrows = 4, shareX = T, shareY = F) %>%
      layout(
        title = '',
        hovermode = 'closest',
        xaxis = list(
          title = NULL
        ),
        yaxis = list (
          title = 'Voltage'
        ),
        yaxis2 = list (
          title = 'Current'
          , autorange = T 
        ),
        yaxis3 = list (
          title = 'Power Factor'
          , autorange = T 
        ),
        yaxis4 = list (
          title = 'Impedance'
          , autorange = T 
        )
      )
  }
  return(dw)
}

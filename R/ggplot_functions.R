#' Function to get channel waveform ggplot parameters, this may take environment parameters in future
#' @return list
#' @import ggplot2
get_channel_waveform_parameters <- function(){
  return(list(
    #duration in seconds for each X axis tick on the time series
    'timeseries_tick_seconds' = 3600 * 3,
    #if voltage is filtered, this is the minimum voltage value
    'filter_voltage_min' = 120,
    #this is the ggplot theme to use for plots
    'ggplot_theme' = theme(
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      legend.position = 'bottom',
      axis.text.x = element_text(angle = 90, hjust = 1)
    ),
    #number of channels before the legend disappears
    'legend_channel_limit' = 12
  ))
}

#' Function to get voltage histogram ggplot parameters, this may take environment parameters in future
#' @return list
get_voltage_histogram_plot_parameters <- function(){
  return(list(
    #minimum voltage in histogram data
    'min_voltage' = 202,
    #maximum voltage in histogram data
    'max_voltage' = 278,
    #how often to display voltage value on X axis
    'voltage_break' = 5
  ))
}

#' Function to get voltage ggplot plot
#' @export
#' @param cd output dataframe from format_channel_reads_plot()
#' @return ggplot object
#' @import ggplot2
#' @import scales
get_channel_voltage_plot_gg <- function(cd, filter_voltage = F){
  params <- get_channel_waveform_parameters()
  #getting X axis values correct
  x_min <- min(cd$interval_ts)
  x_max <- max(cd$interval_ts)
  #get min tick (ceiling the min hour)
  min_tick <- as.POSIXct(ceiling(as.numeric(x_min) / 3600) * 3600, origin = '1970-01-01')
  #get the max tick (floor the max hour)
  max_tick <- as.POSIXct(floor(as.numeric(x_max) / 3600) * 3600, origin = '1970-01-01')
  time_ticks <- seq(min_tick,max_tick,params$timeseries_tick_seconds)
  
  if(filter_voltage == T){
    cd <- subset(cd, voltage_lvt >= params$filter_voltage_min)
  }
  
  volt_plot <-  ggplot(cd, mapping = aes(x = interval_ts, y = voltage_lvt, color = nic_channel_id, group = nic_channel_id)) +
    geom_line(alpha = 0.8) +
    theme_minimal() +
    scale_x_datetime(limits = c(x_min, x_max), labels = date_format("%d/%m %H:%M", tz = "Australia/Melbourne"), breaks = time_ticks, timezone = "Australia/Melbourne", minor_breaks = time_ticks) +
    ylab('Voltage') +
    params$ggplot_theme
  return(volt_plot)
}

#' Function to get current ggplot plot
#' @export
#' @param cd output dataframe from format_channel_reads_plot()
#' @return ggplot object
#' @import ggplot2
#' @import scales
get_channel_current_plot_gg <- function(cd, aggregate_amps = F){
  params <- get_channel_waveform_parameters()
  #getting X axis values correct
  x_min <- min(cd$interval_ts)
  x_max <- max(cd$interval_ts)
  #get min tick (ceiling the min hour)
  min_tick <- as.POSIXct(ceiling(as.numeric(x_min) / 3600) * 3600, origin = '1970-01-01')
  #get the max tick (floor the max hour)
  max_tick <- as.POSIXct(floor(as.numeric(x_max) / 3600) * 3600, origin = '1970-01-01')
  time_ticks <- seq(min_tick,max_tick,params$timeseries_tick_seconds)
  
  if(aggregate_amps == T){
    #aggregate amps data
    amps_channel_data <- subset(cd, as.numeric(format(cd$interval_ts, '%M')) %in% ( (0:11) * 5 ) )
    amps_channel_data <- subset(amps_channel_data,  as.numeric(format(amps_channel_data$interval_ts, '%S')) == 0 )
    amps_data <- aggregate(amps_channel_data$amps_lct ~ amps_channel_data$interval_ts, FUN = sum )
    colnames(amps_data) <- c('interval_ts', 'amps_lct')
    
    amps_plot <- ggplot(amps_data, mapping = aes(x = interval_ts, y = amps_lct)) +
      geom_line(alpha = 0.8) +
      theme_minimal() +
      scale_x_datetime(limits = c(x_min, x_max), labels = date_format("%d/%m %H:%M", tz = "Australia/Melbourne"), breaks = time_ticks, timezone = "Australia/Melbourne", minor_breaks = time_ticks) +
      ylab('Current') +
      params$ggplot_theme
  } else {
    amps_plot <-  ggplot(cd, mapping = aes(x = interval_ts, y = amps_lct, color = nic_channel_id, group = nic_channel_id)) +
      geom_line(alpha = 0.8) +
      theme_minimal() +
      scale_x_datetime(limits = c(x_min, x_max), labels = date_format("%d/%m %H:%M", tz = "Australia/Melbourne"), breaks = time_ticks, timezone = "Australia/Melbourne", minor_breaks = time_ticks) +
      ylab('Current') +
      params$ggplot_theme
  }
  return(amps_plot)
}

#' Function to get power factor ggplot plot
#' @export
#' @param cd output dataframe from format_channel_reads_plot()
#' @return ggplot object
#' @import ggplot2
#' @import scales
get_channel_pf_plot_gg <- function(cd){
  params <- get_channel_waveform_parameters()
  #getting X axis values correct
  x_min <- min(cd$interval_ts)
  x_max <- max(cd$interval_ts)
  #get min tick (ceiling the min hour)
  min_tick <- as.POSIXct(ceiling(as.numeric(x_min) / 3600) * 3600, origin = '1970-01-01')
  #get the max tick (floor the max hour)
  max_tick <- as.POSIXct(floor(as.numeric(x_max) / 3600) * 3600, origin = '1970-01-01')
  time_ticks <- seq(min_tick,max_tick,params$timeseries_tick_seconds)
  
  pf_plot <-  ggplot(cd, mapping = aes(x = interval_ts, y = power_factor_pf, color = nic_channel_id, group = nic_channel_id)) +
    geom_line(alpha = 0.8) +
    theme_minimal() +
    scale_x_datetime(limits = c(x_min, x_max), labels = date_format("%d/%m %H:%M", tz = "Australia/Melbourne"), breaks = time_ticks, timezone = "Australia/Melbourne", minor_breaks = time_ticks) +
    scale_y_continuous(limits = c(-1, 1)) +
    ylab('Power Factor') +
    params$ggplot_theme
  
  return(pf_plot)
}

#' Function to get impedance ggplot plot
#' @export
#' @param cd output dataframe from format_channel_reads_plot()
#' @return ggplot object
#' @import ggplot2
#' @import scales
get_channel_imp_plot_gg <- function(cd){
  params <- get_channel_waveform_parameters()
  #getting X axis values correct
  x_min <- min(cd$interval_ts)
  x_max <- max(cd$interval_ts)
  #get min tick (ceiling the min hour)
  min_tick <- as.POSIXct(ceiling(as.numeric(x_min) / 3600) * 3600, origin = '1970-01-01')
  #get the max tick (floor the max hour)
  max_tick <- as.POSIXct(floor(as.numeric(x_max) / 3600) * 3600, origin = '1970-01-01')
  time_ticks <- seq(min_tick,max_tick,params$timeseries_tick_seconds)
  #subset so we draw straight lines between points
  #imp_cd <- subset(cd, !is.na(network_impedance_imp))
  imp_cd <- cd
  imp_plot <-  ggplot(imp_cd, mapping = aes(x = interval_ts, y = network_impedance_imp, color = nic_channel_id, group = nic_channel_id)) +
    geom_line(alpha = 0.8) +
    theme_minimal() +
    scale_x_datetime(limits = c(x_min, x_max), labels = date_format("%d/%m %H:%M", tz = "Australia/Melbourne"), breaks = time_ticks, timezone = "Australia/Melbourne", minor_breaks = time_ticks) +
    ylab('Impedance') +
    params$ggplot_theme
  
  return(imp_plot)
}

#' Function to get meter waveform ggplot
#' @export
#' @param channel_reads Channel reads dataframe
#' @param device_hierarchy Device hierarchy dataframe
#' @param filter_voltage Boolean flag on whether voltages should be filtered
#' @param aggregate_amps Boolean flag on whether amps should be aggregated
#' @param include_pf Boolean flag on whether the plot should include a power factor plot
#' @param include_imp Boolean flag on whether the plot should include an impedance plot
#' @return cowplot object
#' @import ggplot2
#' @import scales
#' @import cowplot
#' @import grid
#' @import gridExtra
get_meter_waveform_plot_gg <- function(channel_reads, device_hierarchy, filter_voltage = F, aggregate_amps = F, include_imp = F, include_pf = F){
  params <- get_channel_waveform_parameters()
  if(nrow(channel_reads)==0){
    return(ggplot())
  }
  cd <- format_channel_reads_plot(channel_reads = channel_reads,phase_groups = NULL,device_hierarchy = device_hierarchy)
  if(nrow(cd)==0){
    return(ggplot())
  }
  #get all the plots
  plot_objects <- list()
  #get voltage plot
  plot_objects$voltage_plot <- get_channel_voltage_plot_gg(cd = cd , filter_voltage = (filter_voltage == TRUE))
  #get current plot
  plot_objects$current_plot <- get_channel_current_plot_gg(cd = cd , aggregate_amps = (aggregate_amps == TRUE))
  #get power factor plot if the parameter is included
  if(include_pf == TRUE){
    plot_objects$pf_plot <- get_channel_pf_plot_gg(cd)
  }
  #get impedance plot if the parameter is included
  if(include_imp == TRUE){
    plot_objects$imp_plot <- get_channel_imp_plot_gg(cd)
  }
  #remove legend and x axis from every plot but the last one
  for(i in 1:(length(plot_objects)-1)){
    plot_objects[[i]] <- plot_objects[[i]] + theme(axis.title.x = element_blank(),
                                                   axis.text.x = element_blank(), 
                                                   axis.ticks.x = element_blank(),
                                                   #axis.title.y = element_blank(),
                                                   #legend.title = element_blank(),
                                                   legend.position = 'none'
    )
  }
  #remove legend if there are more channels than threshold
  if(length(unique(cd$nic_channel_id)) > params$legend_channel_limit){
    this_theme <- params$ggplot_theme
    this_theme$legend.position <- 'none'
    plot_objects[[length(plot_objects)]] <- plot_objects[[length(plot_objects)]] + this_theme
  }
  return(cowplot::plot_grid(plotlist = plot_objects, ncol = 1, align = 'v'))
}


#' Function to get substation voltage ggplot plot
#' @export
#' @param cd output dataframe from format_channel_reads_plot()
#' @return ggplot object
#' @import ggplot2
#' @import scales
get_substation_voltage_plot_gg <- function(cd, filter_voltage = T){
  params <- get_channel_waveform_parameters()
  phase_colours <- get_phase_colours()
  #getting X axis values correct
  x_min <- min(cd$interval_ts)
  x_max <- max(cd$interval_ts)
  #get min tick (ceiling the min hour)
  min_tick <- as.POSIXct(ceiling(as.numeric(x_min) / 3600) * 3600, origin = '1970-01-01')
  #get the max tick (floor the max hour)
  max_tick <- as.POSIXct(floor(as.numeric(x_max) / 3600) * 3600, origin = '1970-01-01')
  time_ticks <- seq(min_tick,max_tick,params$timeseries_tick_seconds)
  
  if(filter_voltage == T){
    cd <- subset(cd, voltage_lvt >= params$filter_voltage_min)
  }
  
  volt_plot <-  ggplot(cd, mapping = aes(x = interval_ts, y = voltage_lvt, color = phase_group, group = nic_channel_id)) +
    geom_line(alpha = 0.5) +
    scale_colour_manual(values = phase_colours) +
    theme_minimal() +
    scale_x_datetime(limits = c(x_min, x_max), labels = date_format("%d/%m %H:%M", tz = "Australia/Melbourne"), breaks = time_ticks, timezone = "Australia/Melbourne", minor_breaks = time_ticks) +
    ylab('Voltage') +
    params$ggplot_theme +
    theme (
      legend.position = 'none'
    )
  
  return(volt_plot)
}

#' Function to get substation voltage ggplot plot
#' @export
#' @param cd output dataframe from format_channel_reads_plot()
#' @return ggplot object
#' @import ggplot2
#' @import scales
get_substation_current_plot_gg <- function(cd){
  params <- get_channel_waveform_parameters()
  phase_colours <- get_phase_colours()
  #getting X axis values correct
  x_min <- min(cd$interval_ts)
  x_max <- max(cd$interval_ts)
  #get min tick (ceiling the min hour)
  min_tick <- as.POSIXct(ceiling(as.numeric(x_min) / 3600) * 3600, origin = '1970-01-01')
  #get the max tick (floor the max hour)
  max_tick <- as.POSIXct(floor(as.numeric(x_max) / 3600) * 3600, origin = '1970-01-01')
  time_ticks <- seq(min_tick,max_tick,params$timeseries_tick_seconds)
  #aggregate amps data
  amps_channel_data <- subset(cd, as.numeric(format(cd$interval_ts, '%M')) %in% ( (0:11) * 5 ) )
  amps_channel_data <- subset(amps_channel_data,  as.numeric(format(amps_channel_data$interval_ts, '%S')) == 0 )
  amps_data <- aggregate(amps_channel_data$amps_lct ~ amps_channel_data$interval_ts + amps_channel_data$phase_group, FUN = sum )
  colnames(amps_data) <- c('interval_ts', 'phase_group', 'amps_lct')
  
  amps_plot <- ggplot(amps_data, mapping = aes(x = interval_ts, y = amps_lct,color = phase_group, group = phase_group)) +
    geom_line(alpha = 0.8) +
    scale_colour_manual(values = phase_colours) +
    theme_minimal() +
    scale_x_datetime(limits = c(x_min, x_max), labels = date_format("%d/%m %H:%M", tz = "Australia/Melbourne"), breaks = time_ticks, timezone = "Australia/Melbourne", minor_breaks = time_ticks) +
    ylab('Current') +
    params$ggplot_theme +
    theme (
      legend.position = 'none'
    )
  return(amps_plot)
}


#' Function to get meter waveform ggplot
#' @export
#' @param channel_reads Channel reads dataframe
#' @param device_hierarchy Device hierarchy dataframe
#' @param phase_groups phase groups dataframe
#' @param include_amps Boolean flag on whether an amps plot should be included
#' @return cowplot object
#' @import ggplot2
#' @import scales
#' @import cowplot
#' @import grid
#' @import gridExtra
get_substation_waveform_plot_gg <- function(channel_reads, device_hierarchy, phase_groups, include_amps = T){
  params <- get_channel_waveform_parameters()
  if(nrow(channel_reads)==0){
    return(ggplot())
  }
  cd <- format_channel_reads_plot(channel_reads = channel_reads,phase_groups = phase_groups,device_hierarchy = device_hierarchy)
  if(nrow(cd)==0){
    return(ggplot())
  }
  #get all the plots
  plot_objects <- list()
  #get voltage plot
  plot_objects$voltage_plot <- get_substation_voltage_plot_gg(cd = cd)
  #get current plot
  if(include_amps == TRUE){
    plot_objects$current_plot <- get_substation_current_plot_gg(cd = cd)
  }
  #remove legend and x axis from every plot but the last one
  if(length(plot_objects)>1){
    for(i in 1:(length(plot_objects)-1)){
      plot_objects[[i]] <- plot_objects[[i]] + theme(axis.title.x = element_blank(),
                                                     axis.text.x = element_blank(), 
                                                     axis.ticks.x = element_blank(),
                                                     #axis.title.y = element_blank(),
                                                     #legend.title = element_blank(),
                                                     legend.position = 'none'
      )
    }
  }
  return(cowplot::plot_grid(plotlist = plot_objects, ncol = 1, align = 'v'))
}

#' Function to format histogram data for plots
#' @export
#' @param histogram_data Histogram dataframe
#' @param voltage_offset Integer value to shift voltages
#' @return Dataframe
#' @examples
#' format_voltage_histogram_data(get_zone_substation_voltage_histogram_data(zone_substation = 'CDA', interval_ts = as.POSIXct('2018-05-01 12:30:00')), voltage_offset = 5)
format_voltage_histogram_data <- function(histogram_data, voltage_offset = 0){
  params <- get_voltage_histogram_plot_parameters()
  #get only first row
  df <- histogram_data[1,]
  #unpivot data
  dfg <- gather(df[,as.character(c(params$min_voltage:params$max_voltage))], 'voltage_lvt', 'channel_count_z', as.character(c(params$min_voltage:params$max_voltage)), factor_key=F)
  dfg$voltage_lvt <- as.numeric(dfg$voltage_lvt)
  #offset voltages
  dfg$voltage_lvt <- dfg$voltage_lvt + voltage_offset
  dfg$voltage_lvt <- floor(dfg$voltage_lvt)
  #truncate to min and max
  try(dfg$voltage_lvt[dfg$voltage_lvt < params$min_voltage] <- params$min_voltage, silent = T)
  try(dfg$voltage_lvt[dfg$voltage_lvt > params$max_voltage] <- params$max_voltage, silent = T)
  
  voltage_vector <- c()
  for(i in 1:nrow(dfg)){
    voltage_vector <- c(voltage_vector, rep(dfg$voltage_lvt[i], dfg$channel_count_z[i]))
  }
  #add a dummy 1
  voltage_vector <- c(voltage_vector, params$min_voltage:params$max_voltage)
  dfg <- data.frame(table(voltage_vector), stringsAsFactors = F)
  colnames(dfg) <- c('voltage_lvt','channel_count_z')
  dfg$voltage_lvt <- as.numeric(as.character(dfg$voltage_lvt))
  dfg$channel_count_z <- dfg$channel_count_z - 1
  return(dfg)
}

#' Function to get a histogram plot
#' @export
#' @param histogram_data Voltage histogram dataframe
#' @param voltage_offset Integer value to shift voltages
#' @param reference_lines Vector of voltages to draw horizontal lines for on the plot
#' @param chart_title Chart title text to show above plot
#' @param percentiles Vector of percentiles to calculate and include in plot subtitle
#' @return ggplot objet
#Get plot function
get_voltage_histogram_plot_gg <- function( histogram_data, voltage_offset = 0, reference_lines = c(), chart_title = '', percentiles = c(1,99)){
  params <- get_voltage_histogram_plot_parameters()
  #format voltage histogram data
  hd <- format_voltage_histogram_data(histogram_data = histogram_data, voltage_offset = voltage_offset)
  
  #factorize voltages
  hd$voltage_lvt <- factor(hd$voltage_lvt, levels <- c(params$min_voltage:params$max_voltage), labels <- c(paste('<', params$min_voltage), (params$min_voltage+1):(params$max_voltage-1), paste('>', params$max_voltage)), ordered = T)
  #calculate percentiles
  percentile_calcs <- list()
  total_channels <- sum(hd$channel_count_z)
  
  for(percentile in percentiles){
    #Get percentiles indexes
    percentile_idx  <- which(cumsum(hd$channel_count_z) >= ( total_channels * percentile / 100 ))[1]
    percentile_calcs[as.character(percentile)] <- as.character(hd$voltage_lvt[percentile_idx])
  }
  
  #form chart subtitle
  chart_subtitle <- paste('Offset:', voltage_offset, 'V', sep = "")
  percentiles_st <- c()
  if(length(percentile_calcs)>0){
    chart_subtitle <- paste(chart_subtitle, '\nPercentiles ', sep = '')
    for(percentile in names(percentile_calcs)){
      percentiles_st <- c(percentiles_st,paste('[',percentile,']:', percentile_calcs[percentile], sep = ''))
    }
  }
  chart_subtitle <- paste(chart_subtitle, paste(percentiles_st, collapse = ','), sep = '')
  
  #figure out breaks (show a voltage every 10 volts)
  min_break <- ceiling(params$min_voltage / params$voltage_break) * params$voltage_break
  max_break <- floor(params$max_voltage / params$voltage_break) * params$voltage_break
  breaks <- seq(min_break,max_break,params$voltage_break)
  histogram_plot <- ggplot(hd, aes ( x = voltage_lvt, y = channel_count_z)) +
    geom_bar( stat = "sum") +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_x_discrete(breaks = breaks) +
    xlab('Voltage') +
    ylab("") +
    ggtitle(label = chart_title, subtitle = chart_subtitle)
  for(i in reference_lines){
    histogram_plot <- histogram_plot + 
      geom_vline(xintercept = which(hd$voltage_lvt == i), linetype = "dashed", color = 'red', size = 1, alpha = 0.5)
  }
  return(histogram_plot)
}


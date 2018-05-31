#' Function to perform GET request with the supplied url
#' @param request_url Url of get request
#' @return Response object
#' @import httr
#' @import jsonlite
#' @examples
#' perform_get_request(request_url = "http://rtssbx005.domain.dev.int:8889/hierarchy/meter/meter/0013500300086c70")
perform_get_request <- function(request_url){
  retry_max <- 10
  retry_count <- 0
  r <- NULL
  while(is.null(r) & retry_count < retry_max){
    retry_count <- retry_count + 1
    #print(paste('Try',retry_count))
    r <- tryCatch(GET(request_url), error = function(e){return(NULL)})
    if(is.null(r)){
      Sys.sleep(retry_count)
    }
  }
  return(r)
}

#' Function to perform POST request with the supplied url, request body
#' @param request_url Url of get request
#' @param body List, body of request
#' @return Response object
#' @import httr
#' @import jsonlite
#' @examples
#' perform_post_request(request_url = "http://rtssbx005.domain.dev.int:8889/hierarchy/meter/meter", body = list('meters' = as.list("0013500300086c70")))
perform_post_request <- function(request_url,body){
  retry_max <- 10
  retry_count <- 0
  r <- NULL
  while(is.null(r) & retry_count < retry_max){
    retry_count <- retry_count + 1
    #print(paste('Try',retry_count))
    r <- tryCatch(POST(url = request_url, 
                       config = list(add_headers("Content-Type" = "application/json")),
                       #body = toJSON(r_body, auto_unbox = T), 
                       body = body,
                       encode = 'json'
    ), error = function(e){return(NULL)})
    if(is.null(r)){
      Sys.sleep(retry_count)
    }
  }
  return(r)
}

#' Function to parse out device input into channels, meters, serialnums, nmis
#' @export
#' @param input_text A string containing channels, nic mac ids, serial numbers and nmis each on it's own line 
#' @return list containing channels,meters,serial_nums,nmis
#' @examples
#' parse_device_string("0013500300101ce4_b\n0013500300135a31_a\n00135003001144df\n01254662\n6407275764")
#' parse_device_string("0013500300101ce4_b")
parse_device_string <- function(input_text){
  #split by line
  i1 <- gsub('\n',',',input_text)
  #remove spaces
  i1 <- gsub(' ','',i1)
  #turn into vector
  devices <- unlist(strsplit(i1, ','))
  #remove blanks
  devices <- devices[devices != '']
  #lowercase all the strings
  devices <- tolower(devices)
  #pull out each device type
  channels <- devices[nchar(devices) == 18]
  nicmacs <- devices[nchar(devices) == 16]
  nmis <- devices[nchar(devices) == 10]
  #serial numbers, handle various amounts of 0 padding
  serialnums <- devices[nchar(devices) == 8]
  #no padding
  s_np <- paste('0',devices[nchar(devices) == 7], sep = '')
  #2 pads
  n_tp <- substr(devices[nchar(devices) == 9], 2, 9)
  serialnums <- c(serialnums,s_np,n_tp)
  serialnums <- serialnums[nchar(serialnums)==8]
  return(list(
    'channels'=channels,
    'meters'=nicmacs,
    'serial_nums'=serialnums,
    'nmis'=nmis
  ))
}

#' Function to get the phase colours configured in the system environment
#' @return named vector
#' @examples
#' get_phase_colours()
get_phase_colours <- function(){
  nap_phase_colours <- c()
  phase_colours <- Sys.getenv('nap_phase_colours')
  if(is.null(phase_colours) | phase_colours == ""){
    stop("No environment variable found for nap_phase_colours, please set via Sys.setenv('nap_phase_colours'=)")
  }
  #turn string into vector
  elements <- unlist(strsplit(phase_colours, split = ','))
  for(i in 1:length(elements)){
    this_element <- unlist(strsplit(elements[i],'='))
    nap_phase_colours[this_element[1]] <- this_element[2]
  }
  return(nap_phase_colours)
}

#' Function to get the nap api url configured in the system environment
#' @return character
#' @examples
#' get_nap_api_url()
get_nap_api_url <- function(){
  api_url <- Sys.getenv('nap_url')
  if(is.null(api_url) | api_url == ""){
    stop("No environment variable found for nap_url, please set via Sys.setenv('nap_url'=)")
  }
  return(api_url)
}

#' Function to get channel hierarchy information for a list of channels, meters, serialnums, nmis
#' @export
#' @param devices A list containing channels, meters,serial_nums and nmis
#' @return dataframe
#' @import jsonlite
#' @examples
#' get_device_hierarchy(parse_device_string("0013500300101ce4_b\n0013500300135a31_a\n00135003001144df\n01254662\n6407275764"))
#' get_device_hierarchy(parse_device_string("0013500300101ce4_b"))
get_device_hierarchy <- function(devices){
  device_hierarchy <- NULL
  api_url <- get_nap_api_url()
  iteration_size <- 2500
  device_count <- lapply(devices,FUN=length)
  iterations <- ceiling(max(unlist(device_count)) / iteration_size)
  for(i in 1:iterations){
    start_idx <- ((i - 1) * iteration_size)+1
    end_idx <- iteration_size * i
    #get iteration items
    this_c <- devices$channels[start_idx:end_idx]
    this_m <- devices$meters[start_idx:end_idx]
    this_s <- devices$serial_nums[start_idx:end_idx]
    this_n <- devices$nmis[start_idx:end_idx]
    #remove NAs
    this_c <- this_c[!is.na(this_c)]
    this_m <- this_m[!is.na(this_m)]
    this_s <- this_s[!is.na(this_s)]
    this_n <- this_n[!is.na(this_n)]
    body <- list(
      'channels'=as.list(this_c),
      'meters'=as.list(this_m),
      'serialNums'=as.list(this_s),
      'nmis'=as.list(this_n)
    )
    response <- perform_post_request(paste(api_url,'hierarchy/summary/channelSummary', sep = ''), body)
    r <- fromJSON(content(response, 'text'))
    dh <- r$data
    if(!is.null(nrow(dh))){
      if(is.null(device_hierarchy)){
        device_hierarchy <- dh
      } else {
        device_hierarchy <- rbind(device_hierarchy, dh)
      }
    }
  }
  return(device_hierarchy)
}

#' Function to get channel reads
#' @export
#' @param channels A vector of nic channel ids to pull channel reads for
#' @param start_time Start of period to pull channel reads for
#' @param end_time End of period to pull channel reads for
#' @return dataframe
#' @import jsonlite
#' @examples
#' get_channel_reads(channels = c('n0013500300135a31_a','0013500300101ce4_b'), start_time = as.POSIXct('2018-01-01 12:30:00', tz='Australia/Melbourne'),end_time = as.POSIXct('2018-01-01 13:30:00', tz='Australia/Melbourne'))
get_channel_reads <- function(channels, start_time, end_time){
  #break call into 2000 channel chunks
  channel_reads <- NULL
  batch_size <- 500
  iterations <- ceiling(length(channels) / batch_size)
  for(i in 1:iterations){
    start_idx <- ((i-1)*batch_size)+1
    end_idx <- min(batch_size * i,length(channels))
    this_channels <- channels[start_idx:end_idx]
    api_url <- get_nap_api_url()
    r_body <- list(
      'startTime'=paste(as.character(start_time, format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC'),'+00:00', sep = ''),
      'endTime'=paste(as.character(end_time, format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC'),'+00:00', sep = ''),
      'channels'= as.list(this_channels)
    )
    response <- POST(url = paste(api_url,'channelData/channelReads', sep = ''), 
                     config = list(add_headers("Content-Type" = "application/json")),
                     #body = toJSON(r_body, auto_unbox = T), 
                     body = r_body,
                     encode = 'json'
    )
    r <- fromJSON(content(response, 'text'))
    cr <- r$data
    if(!is.null(nrow(cr))){
      if(is.null(channel_reads)){
        channel_reads <- cr
      } else {
        channel_reads <- rbind(channel_reads,cr)
      }
    }
  }
  #format datetimes
  channel_reads$interval_ts <- as.POSIXct(gsub('T',' ', channel_reads$interval_ts), tz = 'UTC')
  channel_reads <- channel_reads[order(channel_reads$interval_ts),]
  return(channel_reads)
}

#' Function to combine and format channel reads and reference data ready for plotting
#' @export
#' @param channel_reads Channel reads dataframe
#' @param device_hierarchy Device hierarchy dataframe
#' @param phase_groups Phase groups dataframe
#' @return dataframe
format_channel_reads_plot <- function(channel_reads,phase_groups,device_hierarchy){
  #treat impedance values
  cd <- channel_reads
  cd[(!is.na(cd$amps_delta_lct) & abs(cd$amps_delta_lct) < 2),]$network_impedance_imp <- NA
  #merge dataframes
  if(!is.null(phase_groups)){
    cd <- merge(cd[,c('nic_channel_id','interval_ts','voltage_lvt', 'amps_lct','network_impedance_imp','power_factor_pf')], 
                phase_groups, 
                all.x = T,
                by = 'nic_channel_id'
    )
  } else {
    cd$phase_group <- NA
  }
  cd <- merge(cd, device_hierarchy[,c('nic_channel_id','address')], by = 'nic_channel_id')
  #treat phase groups
  cd$phase_group <- cd$phase_group
  cd[is.na(cd$phase_group),'phase_group'] <- 0
  cd$phase_group <- factor ( cd$phase_group, levels = 0:3, labels = c('-', '1', '2', '3'))
  #format interval ts
  cd$interval_ts <- as.POSIXct(as.character(cd$interval_ts, tz = 'Australia/Melbourne'), tz = 'Australia/Melbourne')
  cd <- cd[order(cd$interval_ts),]
  return(cd)
}

#' Function to get distribution substations
#' @export
#' @return dataframe
#' @import jsonlite
#' @examples
#' get_distribution_substation()
get_distribution_substation <- function(){
  api_url <- get_nap_api_url()
  url <- paste(api_url, 'hierarchy/distributionSubstation/', sep = '')
  response <- perform_get_request(url)
  r <- fromJSON(content(response, 'text'))
  return(r$data)
}

#' Function to get channels under a distribution substation
#' @export
#' @param distribution_substation The distribution substation name
#' @return dataframe
#' @import jsonlite
#' @examples
#' get_distribution_substation_channels('MILNE-JAMES')
get_distribution_substation_channels <- function(distribution_substation){
  api_url <- get_nap_api_url()
  substation <- gsub('/', '%2F', distribution_substation)
  substation <- gsub(' ', '%20', substation)
  url <- paste(api_url, 'hierarchy/distributionSubstation/channel/', substation,sep = '')
  response <- perform_get_request(url)
  r <- fromJSON(content(response, 'text'))
  return(r$data)
}

#' Function to get transformers under a distribution substation
#' @export
#' @param distribution_substation The distribution substation name
#' @return dataframe
#' @import jsonlite
#' @examples
#' get_distribution_substation_transformer('MILNE-JAMES')
get_distribution_substation_transformer <- function(distribution_substation){
  api_url <- get_nap_api_url()
  substation <- URLencode(distribution_substation)
  url <- paste(api_url, 'hierarchy/distributionSubstation/distributionTransformer/', substation,sep = '')
  response <- perform_get_request(url)
  r <- tryCatch(fromJSON(content(response, 'text')), error = function(e){return(list())})
  return(r$data)
}

#' Function to get transformers under a distribution substation
#' @export
#' @param distribution_transformer_id The distribution transformer id
#' @return dataframe
#' @import jsonlite
#' @examples
#' get_distribution_transformer_cuit('000000000004014234')
get_distribution_transformer_lv_circuit <- function(distribution_transformer_id){
  api_url <- get_nap_api_url()
  url <- paste(api_url, 'hierarchy/distributionTransformer/lvCircuit',sep = '')
  r_body <- list(
    'distributionTransformer'= as.list(distribution_transformer_id)
  )
  response <- perform_post_request(url, r_body)
  r <- fromJSON(content(response, 'text'))
  return(r$data)
}

#' Function to get channels under lv circuits
#' @export
#' @param lv_circuit
#' @return dataframe
#' @import jsonlite
#' @examples
#' get_lv_circuit_channels(c("112807948","140987703"))
get_lv_circuit_channels <- function(lv_circuit){
  aapi_url <- get_nap_api_url()
  url <- paste(api_url, 'hierarchy/lvCircuit/channel',sep = '')
  r_body <- list(
    'lvCircuit'= as.list(lv_circuit)
  )
  response <- perform_post_request(url, r_body)
  r <- fromJSON(content(response, 'text'))
  return(r$data)
}

#' Function to get channels under a topology id
#' @export
#' @param topology_id Integer of the topology node id (gis)
#' @return dataframe
#' @import jsonlite
#' @examples
#' get_node_channels(86223764)
get_node_channels <- function(topology_id){
  api_url <- get_nap_api_url()
  url <- paste(api_url, 'hierarchy/topology/channel/', topology_id,sep = '')
  response <- perform_get_request(url)
  r <- fromJSON(content(response, 'text'))
  return(r$data)
}

#' Function to get pole information
#' @export
#' @param pole_id The pole id (lis number)
#' @return dataframe
#' @import jsonlite
#' @examples
#' get_pole(1109226)
get_pole <- function(pole_id){
  api_url <- Sys.getenv('nap_url')
  if(is.null(api_url) | api_url == ""){
    stop("No environment variable found for nap_url, please set via Sys.setenv('nap_url'=)")
  }
  url <- paste(api_url, 'hierarchy/pole/', pole_id,sep = '')
  response <- perform_get_request(url)
  r <- fromJSON(content(response, 'text'))
  return(r$data)
}


#' Function to calculate phase group clusters
#' @export
#' @param channel_reads Channel reads dataframe to calculate phase groups for
#' @return dataframe
get_phase_groups <- function(channel_reads){
  #filter out unwanted channel data
  cd <- subset(channel_reads, !is.na(voltage_lvt))
  #Create cross correlation matrix
  channels <- unique(cd$nic_channel_id)
  channel_matrix <- matrix(nrow = length(channels), ncol = length(channels), dimnames = list(channels, channels))
  
  #create must-not-link pairs (3 phase meters)
  n_channels <- length(channels)
  if(n_channels <= 3){
    phase_group_data <- data.frame('phase_group' = seq(1,n_channels), 'nic_channel_id' = channels)
  } else if(n_channels > 1){
    for(i in 1:(n_channels-1)){
      channel_1 <- channels[i]
      channel_matrix[channel_1, channel_1] <- 1
      for(j in (i+1):n_channels){
        channel_2 <- channels[j]
        cor_df <- merge(subset(cd, nic_channel_id == channel_1, c('interval_ts', 'voltage_lvt') ), subset(cd, nic_channel_id == channel_2, c('interval_ts', 'voltage_lvt')  ), by = 'interval_ts')
        cor <- cor(cor_df$voltage_lvt.x, cor_df$voltage_lvt.y)
        channel_matrix[channel_1, channel_2] <- cor
        channel_matrix[channel_2, channel_1] <- cor
      }
    }
    channel_matrix[channels[n_channels], channels[n_channels]] <- 1
    #Turn into distance matrix
    dist_matrix <- as.dist(1-channel_matrix)
    #Calculate clusters
    kmeans_output <- kmeans(x = dist_matrix, centers = min(n_channels - 1,3))
    kmeans_phases <- kmeans_output$cluster
    
    #ckmeans(data = dist_matrix, k = min(n_channels - 1,3), mustLink = matrix(c('dummy','dummy'),nrow=1), cantLink = matrix(c('dummy','dummy'),nrow=1))
    #starts.via.svd(dist_matrix, nclass = 3, method = "kmeans")
    #Clean output,
    phase_group_data <- as.data.frame(kmeans_phases)
    phase_group_data$nic_channel_id <- rownames(phase_group_data)
    colnames(phase_group_data)[1] <- 'phase_group'
  } else {
    phase_group_data <- NULL
  }
  phase_group_data
}

#' Function to get device waveform data export
#' @export
#' @param channel_reads Channel reads dataframe
#' @param device_hierarchy Device hierarchy dataframe
#' @param phase_groups Phase groups dataframe
#' @return dataframe
get_device_waveform_export <- function(channel_reads,phase_groups,device_hierarchy){
  #merge data
  ex_cd <- merge(channel_reads[,c('nic_channel_id','interval_ts','voltage_lvt','amps_lct','power_factor_pf','network_impedance_imp')], device_hierarchy[,c('nic_channel_id','nmi_id','circuit_num_cd','address','distance_to_sub_mt')], by = 'nic_channel_id', all.x = T)
  #format datetime
  if(!is.null(nrow(phase_groups))){
    ex_cd <- merge(ex_cd,phase_groups, by = 'nic_channel_id', all.x = T)
  }
  ex_cd$interval_ts <- as.character(ex_cd$interval_ts, tz = 'Australia/Melbourne', format = '%Y-%m-%d %H:%M:%S')
  return(ex_cd)
}

#' Function to get event types
#' @export
#' @return Dataframe
#' @examples
#' get_event_type()
get_event_type <- function(){
  api_url <- Sys.getenv('nap_url')
  r <- perform_get_request(paste(api_url,'channelData/eventType', sep = ''))
  r <- fromJSON(content(r, 'text'))
  event_type <- r$data
  event_type$event_display_name <-  paste('[',event_type$source_system, '] ', event_type$event_type, ' - ', event_type$event_description, sep = '')
  return(event_type)
}

#' Function to get event data for supplied meters for a given period of time
#' @export
#' @param meter A vector of nic mac ids
#' @param event_type A vector of event type ids
#' @param start_time Datetime start time of period 
#' @param end_time Datetime end time of period 
#' @return dataframe
#' @examples
#' get_event_data(meter = c('00135003001e913d'), event_type = c('15565','15566','15567','15568'), start_time = as.POSIXct('2018-05-01 00:00:00', tz = 'Australia/Melbourne'), end_time = as.POSIXct('2018-05-25 23:59:00', tz = 'Australia/Melbourne'))
get_event_data <- function(meter,event_type,start_time, end_time){
  api_url <- Sys.getenv('nap_url')
  r_body <- list(
    'startTime'=paste(as.character(start_time, format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC'),'+00:00', sep = ''),
    'endTime'=paste(as.character(end_time, format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC'),'+00:00', sep = ''),
    'meters'= as.list(meter),
    'eventType'=as.list(event_type)
  )
  response <- perform_post_request(
    request_url = paste(api_url,'channelData/event/meterEvent', sep = ''),
    body = r_body
  )
  r <- fromJSON(content(response, 'text'))
  event_data <- r$data
  if(!is.null(nrow(event_data))){
    #format datetimes
    event_data[which(event_data$received_ts == '+00:00'),'received_ts'] <- NA
    event_data$event_ts <- as.POSIXct(gsub('T',' ', event_data$event_ts), tz = 'UTC')
    event_data$received_ts <- as.POSIXct(gsub('T',' ', event_data$received_ts), tz = 'UTC')
  }
  return(event_data)
}

#' Function to format event data for presentation / csv export
#' @export
#' @param event_data Dataframe from get_event_data
#' @param device_hierarchy Dataframe from get_device_hierarchy
#' @return dataframe
#' @examples
#' format_event_data(get_event_data(meter = '00135003001e913d', event_type = c('15565','15566','15567','15568'), start_time = as.POSIXct('2018-05-01 00:00:00', tz = 'Australia/Melbourne'), end_time = as.POSIXct('2018-05-25 23:59:00', tz = 'Australia/Melbourne')),get_device_hierarchy(parse_device_string('00135003001e913d')))
format_event_data <- function(event_data,device_hierarchy){
  combined_data <- merge(event_data[,c('nic_mac_id','event_type','nic_channel_id','event_ts','received_ts','raw_alert_msg','severity_cd','raise_by_cd')],unique(device_hierarchy[,c('nic_mac_id','nmi_id','serial_num')]), by = 'nic_mac_id')
  #set datetimes to strings
  combined_data$event_ts <- format(combined_data$event_ts, '%Y-%m-%d %H:%M:%S', tz = 'Australia/Melbourne')
  combined_data$received_ts <- format(combined_data$received_ts, '%Y-%m-%d %H:%M:%S', tz = 'Australia/Melbourne')
  #reorder columns
  return(combined_data[,c('nic_mac_id','nmi_id','serial_num','nic_channel_id','event_type','event_ts','received_ts','raw_alert_msg','severity_cd','raise_by_cd')])
}


#' Function to get meter hierachy information as requested by webservice
#' @export
#' @param input_string A string containing comma separated channels, nic mac ids, serial numbers and nmis 
#' @return dataframe
#' @examples
#' get_meter_details_ws('01332167')
get_meter_details_ws <- function(input_string){
  devices <- parse_device_string(input_string)
  device_hierarchy <- get_device_hierarchy(devices)
  md <- unique(device_hierarchy[,c('nmi_id','serial_num','nic_mac_id','address','distribution_substation_id','circuit_num_cd')])
  colnames(md) <- c('nmi', 'serial_number', 'nic_mac_id', 'premise_details', 'substation_dsc', 'lv_circuit')
  return(md)
}

#' Function to get zone substation histogram data
#' @export
#' @param zone_substation The zone substation label
#' @param interval_ts Timestamp to get zone substation histogram
#' @return Dataframe
#' @examples
#' get_zone_substation_voltage_histogram_data(zone_substation = 'CDA', interval_ts = as.POSIXct('2018-05-01 12:30:00'))
get_zone_substation_voltage_histogram_data <- function(zone_substation, interval_ts){
  api_url <- Sys.getenv('nap_url')
  #treat zone substation parameter
  zs <- gsub(' ', '%20', zone_substation)
  zs <- gsub('/','%2F', zs)
  #format interval_ts
  interval_format <- format(interval_ts, '%Y-%m-%dT%H:%M:%S', tz = 'UTC')
  url <- paste(api_url, 'channelData/zoneSubstationHistogram?zoneSubstation=', zs, '&startTime=',interval_format,'&endTime=', interval_format, sep = '')
  r <- perform_get_request(url)
  r <- fromJSON(content(r, 'text'))
  zs_hist <- r$data
  zs_hist$interval_ts <- as.POSIXct(gsub('T',' ', zs_hist$interval_ts), tz = 'UTC')
  return(zs_hist)
}

#' Function to get distribution substation histogram data
#' @export
#' @param distribution_substation The distribution substation name
#' @param interval_ts Timestamp to get zone substation histogram
#' @return Dataframe
#' @examples
#' get_distribution_substation_voltage_histogram_data(distribution_substation = 'MILNE-JAMES', interval_ts = as.POSIXct('2018-05-01 12:30:00'))
get_distribution_substation_voltage_histogram_data <- function(distribution_substation, interval_ts){
  api_url <- Sys.getenv('nap_url')
  substation <- gsub('/', '%2F', distribution_substation)
  substation <- gsub(' ', '%20', substation)
  #format interval_ts
  interval_format <- format(interval_ts, '%Y-%m-%dT%H:%M:%S', tz = 'UTC')
  url <- paste(api_url, 'channelData/distributionSubstationHistogram?distributionSubstation=', substation, '&startTime=',interval_format,'&endTime=', interval_format, sep = '')
  r <- perform_get_request(url)
  r <- fromJSON(content(r, 'text'))
  hist_data <- r$data
  hist_data$interval_ts <- as.POSIXct(gsub('T',' ', hist_data$interval_ts), tz = 'UTC')
  return(hist_data)
}


#' Function to get distribution substations under a zone substation
#' @export
#' @param zone_substation The zone substation label
#' @return Dataframe
#' @examples
#' get_zone_substation_distribution_substation(zone_substation = 'CDA')
get_zone_substation_distribution_substation <- function(zone_substation){
  api_url <- Sys.getenv('nap_url')
  #treat zone substation parameter
  zs <- gsub(' ', '%20', zone_substation)
  zs <- gsub('/','%2F', zs)
  url <- paste(api_url, 'hierarchy/zoneSubstation/distributionSubstation/', zs, sep = '')
  r <- perform_get_request(url)
  r <- fromJSON(content(r, 'text'))
  return(r$data)
}

#' Function to get channels under a zone substation
#' @export
#' @param zone_substation The zone substation label
#' @return dataframe
#' @import jsonlite
#' @examples
#' get_zone_substation_channels('CDA')
get_zone_substation_channels <- function(zone_substation){
  api_url <- get_nap_api_url()
  zs <- gsub(' ', '%20', zone_substation)
  zs <- gsub('/','%2F', zs)
  url <- paste(api_url, 'hierarchy/zoneSubstation/channel/', zs,sep = '')
  response <- perform_get_request(url)
  r <- fromJSON(content(response, 'text'))
  return(r$data)
}

#' Function to get all zone substations
#' @export
#' @return dataframe
#' @import jsonlite
#' @examples
#' get_zone_substation()
get_zone_substation <- function(){
  api_url <- get_nap_api_url()
  url <- paste(api_url, 'hierarchy/zoneSubstation/zoneSubstation',sep = '')
  response <- perform_get_request(url)
  r <- fromJSON(content(response, 'text'))
  zone_subs <- r$data
  zone_subs <- zone_subs[order(order(zone_subs$zone_substation_id)),]
  return(zone_subs)
}


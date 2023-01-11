# creating acsn_map for the ~15 endpoints that were selected in Marissa's paper
# based on their two-step analysis
# but now, we want to run all of the endpoints

get_acsn_map <- function() {
  # conversation with Tim 06/16/2020 - 
  # Half Width at Half Height of Cross-Correlation and 
  # Width at Half Height of Cross-Correlation are the same thing. 
  # Axion just misnamed it at first time.
  # Half Width at Half Height of Cross-Correlation is the correct name
  
  file_acsn_using = c(
    'Number of Spikes',
    'Mean Firing Rate (Hz)',
    'Number of Bursts',
    'Burst Duration - Avg (s)',
    'Number of Spikes per Burst - Avg',
    'Mean ISI within Burst - Avg',
    'Burst Percentage - Avg',
    'Burst Percentage - Std',
    'Number of Spikes per Network Burst - Avg',
    'Number of Spikes per Network Burst - Std',
    'Number of Elecs Participating in Burst - Avg',
    'Network Burst Percentage',
    'Area Under Cross-Correlation',
    'Synchrony Index',
    'Half Width at Half Height of Cross-Correlation',
    'Width at Half Height of Cross-Correlation'
  )
  
  tcpl_acsn <- c(
    'NHEERL_MEA_acute_spike_number',
    'NHEERL_MEA_acute_firing_rate_mean',
    'NHEERL_MEA_acute_burst_number',
    'NHEERL_MEA_acute_burst_duration_mean',
    'NHEERL_MEA_acute_per_burst_spike_number_mean',
    'NHEERL_MEA_acute_interburst_interval_mean',
    'NHEERL_MEA_acute_burst_percentage_mean',
    'NHEERL_MEA_acute_burst_percentage_std',
    'NHEERL_MEA_acute_per_network_burst_spike_number_mean',
    'NHEERL_MEA_acute_per_network_burst_spike_number_std',
    'NHEERL_MEA_acute_bursting_electrodes_number_mean',
    'NHEERL_MEA_acute_network_burst_percentage',
    'NHEERL_MEA_acute_cross_correlation_area',
    'NHEERL_MEA_acute_synchrony_index',
    'NHEERL_MEA_acute_cross_correlation_HWHM',
    'NHEERL_MEA_acute_cross_correlation_HWHM'
  )
  
  acsn_map <- data.table(file_acsn_using, tcpl_acsn)
  assign("acsn_map",acsn_map,envir = .GlobalEnv)
}
# acsn_map <- get_acsn_map()
# write.csv(acsn_map, file = "neural_stats_endpoint_to_tcpl_acsn_map.csv", row.names = T)

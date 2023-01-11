# ------------------------------------------------------------------------ #
# Supplement to the RMD
# for makign pdfs
# ------------------------------------------------------------------------ #

# load packages
library(data.table)
library(ggplot2)
library(cowplot)

# source all functions in folder 'mea-acute-neural-stats-to-mc0-scripts'
scripts <- list.files(path = "mea-acute-neural-stats-to-mc0-scripts", pattern = "\\.R$", full.names = T, recursive = F)
sapply(scripts, source)
# Note: on branch "experimental1"

# load data
dat1 <- get_latest_dat(lvl = "dat1", 'BaselineDeclineAnalysis')
# save(dat1, file = 'C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_acute/data/BaselineDeclineAnalysis_dat1_2022-02-14.RData')


# Create pdfs for all plates for wMFR -------------------------------------

plates <- dat1[, unique(.SD), .SDcols = c('experiment.date','plate.id','maestro_type_foldername')][order(experiment.date, maestro_type_foldername, plate.id)]
setkeyv(dat1, cols = c('experiment.date','plate.id','maestro_type_foldername'))

# Plot it!
acnmi <- 'CCTE_Shafer_MEA_acute_firing_rate_mean_weighted'
yrange <- dat1[acnm == acnmi, range(activity_value, na.rm = T)]

# Color in where the baselien and treated recordings would be taking place
# The usual rval components:
# - baseline recording (40 min), after 20 min rest, which is already included
# - 20 min rest after dosing, then 40 min recording

bas.poly <- data.table(x_pts = c(20, 60, 60, 20),
                       y_pts = c(0,   0,  yrange[2], yrange[2]), 
                       id = 'baseline')
trt.poly <- data.table(x_pts = c(80, 120, 120, 80),
                       y_pts = c(0,   0,  yrange[2], yrange[2]), 
                       id = 'treated')
data.poly <- rbind(bas.poly, trt.poly)
data.poly$id <- factor(data.poly$id, levels = c('baseline','treated'), ordered = T)

pdf(file = paste0('investigations/baseline_decline/',acnmi,'_by_plate.pdf'), width = 12, height = 5)
print_plot <- ggplot()
for (i in 1:nrow(plates)) {
  plotdat <- dat1[.(plates[i])][acnm == acnmi]
  p <- ggplot(plotdat, aes(x = min_after_exp_start_time, y = activity_value)) +
    geom_line(aes(group = well))+
    geom_point()+
    scale_y_continuous(limits = c(0,yrange[2]), expand = expansion(mult = c(0.01,0.01)))+
    geom_polygon(data = data.poly, aes(x = x_pts, y = y_pts, group = id, fill = id), alpha = 0.35)+
    scale_fill_viridis_d(option = 'magma', begin = 0.2, end = 0.7)+
    ggtitle(paste0(sub('CCTE_Shafer_MEA_acute_','',unique(plotdat$acnm)),
                   '\n',unique(plotdat$experiment.date),', ',unique(plotdat$plate.id),', ',unique(plotdat$maestro_type_foldername)))+
    theme(legend.position = 'top',
          legend.direction = 'horizontal')
  
  
  print_plot <- print_plot +
    draw_plot(p, x = ((i-1)%%3)*(1/3), y = 0, width = 1/3, height = 1)
  if (i%%3 == 0) {
    print(print_plot)
    print_plot <- ggplot()
  }
}
graphics.off()



# Function ----------------------------------------------------------------

make_da_plots <- function(acnmi, y_col = 'activity_value', yrange_consistent = T) {
  
  if(yrange_consistent) {
    # yrange <- dat1[acnm == acnmi & !is.infinite(get(y_col)), range(get(y_col), na.rm = T)]
    y_lb <- dat1[acnm == acnmi & !is.infinite(get(y_col)), quantile(get(y_col), probs = 0.01, na.rm = T)]
    y_ub <- dat1[acnm == acnmi & !is.infinite(get(y_col)), quantile(get(y_col), probs = 0.99, na.rm = T)]
    yrange <- c(y_lb, y_ub)    
  }
  else {
    yrange <- c(NA, NA)
  }
  
  # Color in where the baselien and treated recordings would be taking place
  # The usual rval components:
  # - baseline recording (40 min), after 20 min rest, which is already included
  # - 20 min rest after dosing, then 40 min recording
  
  # Using geom_ribbon instead, so that don't have to define yrange
  # bas.poly <- data.table(x_pts = c(20, 60, 60, 20),
  #                        y_pts = c(0,   0,  yrange[2], yrange[2]), 
  #                        id = 'baseline')
  # trt.poly <- data.table(x_pts = c(80, 120, 120, 80),
  #                        y_pts = c(0,   0,  yrange[2], yrange[2]), 
  #                        id = 'treated')
  # data.poly <- rbind(bas.poly, trt.poly)
  # data.poly$id <- factor(data.poly$id, levels = c('baseline','treated'), ordered = T)
  # geom_polygon(data = data.poly, aes(x = x_pts, y = y_pts, group = id, fill = id), alpha = 0.35)
  
  print_plot <- ggdraw()
  for (i in 1:nrow(plates)) {
    plotdat <- dat1[.(plates[i])][acnm == acnmi]
    if (nrow(plotdat) == 0) next # old maestro doesn't include per_network_burst_interspike_interval_cv
    # oh wait, I need to make it a factor rather than continous...
    # med.tb <- plotdat[, .(med_val = median(activity_value[!is.infinite(activity_value)], na.rm = T)), by = .()]
    p <- ggplot(plotdat, aes(x = min_after_exp_start_time, y = get(y_col))) +
      geom_line(aes(group = well, color = buffer))+
      geom_point(aes(color = buffer))+
      scale_y_continuous(limits = c(0,yrange[2]), expand = expansion(mult = c(0.01,0.01)))+
      ylab(y_col)+
      geom_ribbon(aes(xmin = 20, xmax = 60), fill = 'blue', alpha = 0.35)+
      geom_ribbon(aes(xmin = 80, xmax = 120), fill = 'green', alpha = 0.35)+
      scale_color_manual(values = c('10uM HEPES' = 'orange',
                                    'none' = 'black'))+
      ggtitle(paste0(sub('CCTE_Shafer_MEA_acute_','',unique(plotdat$acnm)),
                     '\n',unique(plotdat$experiment.date),', ',unique(plotdat$plate.id),', ',unique(plotdat$maestro_type_foldername),
                     '\nblue=baseline, green=treated'))+
      theme_bw()+
      theme(legend.position = 'top',
            legend.direction = 'horizontal',
            legend.margin = margin(0, 0.1, 0, 0.1, unit = 'pt'),
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 7))

    print_plot <- print_plot +
      draw_plot(p, x = ((i-1)%%3)*(0.33), y = 0, width = 0.33, height = 1)
    if (i%%3 == 0) {
      print(print_plot)
      print_plot <- ggdraw()
    }
  }
}

make_and_save_da_plots <- function(acnmi, y_col = 'activity_value', filename_suffix = '', yrange_consistent = TRUE) {
  pdf(file = paste0('investigations/baseline_decline/figs/',acnmi,'_by_plate_',y_col,filename_suffix,'.pdf'), width = 12, height = 5)
  make_da_plots(acnmi, y_col, yrange_consistent)
  graphics.off()
}


# Make more plots ---------------------------------------------------------
dat1[, recording_rank := frank(sec_after_exp_start_time, ties.method = 'dense'), by = .(experiment.date, plate.id, maestro_type_foldername)]
dat1[, activity_value_start := activity_value[recording_rank == 1], by = .(experiment.date, plate.id, maestro_type_foldername, well, acnm)]
dat1[, activity_value_normalized_start := (activity_value-activity_value_start)/activity_value_start*100]
dat1[, activity_value_normalized_start := (activity_value)/activity_value_start*100]

# Does it make sense to have a minimum of 0 for every endpoint?
dat1[, .(min(activity_value, na.rm = T)), by = .(acnm)][order(V1)]
# the maximum minimum value is 5
# I think this occurs where the minimum number of spikes in a burst is 5


make_da_plots(acnmi = 'CCTE_Shafer_MEA_acute_burst_number', y_col = 'activity_value', yrange_consistent = F)

# deprecated:
# make_and_save_da_plots(acnmi = 'CCTE_Shafer_MEA_acute_burst_number', y_col = 'activity_value')
# make_and_save_da_plots(acnmi = 'CCTE_Shafer_MEA_acute_burst_duration_mean', y_col = 'activity_value')
# make_and_save_da_plots(acnmi = 'CCTE_Shafer_MEA_acute_synchrony_index', y_col = 'activity_value')
# make_and_save_da_plots(acnmi = 'CCTE_Shafer_MEA_acute_network_burst_number', y_col = 'activity_value')
# 
# make_and_save_da_plots(acnmi = 'CCTE_Shafer_MEA_acute_firing_rate_mean_weighted', y_col = 'activity_value_normalized_start')
# make_and_save_da_plots(acnmi = 'CCTE_Shafer_MEA_acute_burst_number', y_col = 'activity_value_normalized_start')
# make_and_save_da_plots(acnmi = 'CCTE_Shafer_MEA_acute_burst_duration_mean', y_col = 'activity_value_normalized_start')
# make_and_save_da_plots(acnmi = 'CCTE_Shafer_MEA_acute_synchrony_index', y_col = 'activity_value_normalized_start')
# make_and_save_da_plots(acnmi = 'CCTE_Shafer_MEA_acute_network_burst_number', y_col = 'activity_value_normalized_start')


# For all the endpoints!!
for (acnmi in unique(dat1$acnm)) {
  make_and_save_da_plots(acnmi, y_col = 'activity_value', yrange_consistent = F)
}

# all endpoints, normalized!
for (acnmi in unique(dat1$acnm)) {
  make_and_save_da_plots(acnmi, y_col = 'activity_value_normalized_start', yrange_consistent = F)
}

# remaking these with patch fix to skip missing data in maestro
# make_and_save_da_plots(acnmi = 'CCTE_Shafer_MEA_acute_per_network_burst_interspike_interval_cv', y_col = 'activity_value', yrange_consistent = F)
# make_and_save_da_plots(acnmi = 'CCTE_Shafer_MEA_acute_per_network_burst_interspike_interval_cv', y_col = 'activity_value_normalized_start', yrange_consistent = F)


# Idea: visually show the duration of the recording? ----------------------

# Could add points at the tail end of the recording, so that there is a horizontal indicating the duration of the recording
# help with visualizing the recording block
# but, meh.

# Specific things I could check out:
# 
# - Just view a set of these plots for eveyr plate, for 1 - 3 endpoints
# - If I were to calculate the rval, using just time, given no treatment, what would the rval's be? (show how much they decrease)
# - How many of the activity values decrease from acceptable to not acceptable ranges over the time period (i.e. for MFR or nAE?)

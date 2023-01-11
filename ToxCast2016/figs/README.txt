check_if_outlier plots were made to evaluate the reason that these wells were removed in source_to_lvl0_nheerl_mea_acute.R based on the document "ToxCast MEA data Outliers and data check.docx" (which I have not been able to find).
Plots made with the script data_wllq_checks.R

In each plot, the red points show the points that were removed in source_to_lvl0_nheerl_mea_acute.R
4 endpoints were chosen: firing rate, synchrony index, number of actively bursting electrodes, and mean number of spikes per network bursts
The first 4 plots show the activity in the baseline recording for all wells in the ToxCast data set.
The next 4 plots show the activity in the baseline recording for only wells in the plate of interest. 
The next 4 plots show the percent change in activity (treated - baseline)*100/baseline for all wells in the ToxCast data set.
The next 4 plots show the percent change in activity (treated - baseline)*100/baseline for only wells in the plate of interest. 

**Note that in each plot, the points are black are only where the wllq == 1 (based on my filter of nAE > 10). The points in red for the wells of interest include both wllq=1 and wllq=0, because I wanted to see if the wllq=0 wells would explain why these were removed.
Anyhow, just note that for the red points, some of the wells have wllq=0, some have wllq=1
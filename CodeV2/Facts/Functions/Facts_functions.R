#File contains some useful functions to construct the facts.


#Create custom estimation and plotting functions using mgcv package 
#(generalized partially additive model). This is to deal with nonparametrics.

#Estimates given formula, returns GGPlot object. Requires mgcv, gratia, patchwork loaded
flexibleEstimation <- function(Dataframe_list,              #Pass named list with two dataframes -- a "Superstar" and "Non-Superstar" sample (by name)
                               formula,                     #GAM formula you want to pass to the object, must include 
                               smooth_N = 1000,             #Default smoothing for plotting 
                               weights = FALSE,             #if true, use citywt as weights
                               interceptDifference = FALSE, #if true, allow intercept to vary by sample. 
                               SampleNames = list("Superstar" = "Superstar", #list of names to put into ggplot legend
                                                  "nonSuperstar" = "nonSuperstar"),
                               CBD = FALSE, #replace density with distance to CBD
                               density_quantiles = TRUE, #use quantiled density vs no density
                               gamma = 2.5) { #additional smoothing parameter         
  #List of regression objects
  RegObjects <- list()
  
  #Regressions
  if (weights == "city") { #cities weighted evenly
    for (sample in c("Superstar", "nonSuperstar")) {
          RegObjects[[paste0(sample,"_reg")]] <- mgcv::gam(formula = formula,
                                                           data = Dataframe_list[[sample]],
                                                           weights = citywt,
                                                           gamma = gamma) #Set large smoothing parameter to
    }
  }
  
  if (weights == "household") { #block groups weighted by number of households
    for (sample in c("Superstar", "nonSuperstar")) {
      RegObjects[[paste0(sample,"_reg")]] <- mgcv::gam(formula = formula,
                                                       data = Dataframe_list[[sample]],
                                                       weights = household_wt,
                                                       gamma = gamma) #Set large smoothing parameter to
    }
  }
  
  if (weights == FALSE) {
    
    for (sample in c("Superstar", "nonSuperstar")) {
      RegObjects[[paste0(sample,"_reg")]] <- mgcv::gam(formula = formula,
                                                       data = Dataframe_list[[sample]],
                                                       gamma = gamma)

    }
  }
  
  
  
  #Smoothing with point estimates for plot. Note: smooth_estimates automatically takes flexible rank_density_CBSA term.
  for (sample in c("Superstar", "nonSuperstar")) {
   RegObjects[[paste0(sample, "_smooth")]] <- gratia::smooth_estimates(object = RegObjects[[paste0(sample, "_reg")]], 
                                                                       n = smooth_N) %>% add_confint()
  }
  
  #If intercept difference, allow estimates to vary by specified variable
  if (interceptDifference != FALSE) {
    RegObjects[["Superstar_smooth"]]$lower_ci <- RegObjects[["Superstar_smooth"]]$lower_ci + mean(Dataframe_list[["Superstar"]][[interceptDifference]], na.rm = TRUE) - 
                                                                                             mean(Dataframe_list[["nonSuperstar"]][[interceptDifference]], na.rm = TRUE) #add differences in intercept
    
    RegObjects[["Superstar_smooth"]]$upper_ci <- RegObjects[["Superstar_smooth"]]$upper_ci + mean(Dataframe_list[["Superstar"]][[interceptDifference]], na.rm = TRUE) - 
                                                                                             mean(Dataframe_list[["nonSuperstar"]][[interceptDifference]], na.rm = TRUE) 
    
    RegObjects[["Superstar_smooth"]]$est <- RegObjects[["Superstar_smooth"]]$est + mean(Dataframe_list[["Superstar"]][[interceptDifference]], na.rm = TRUE) - 
                                                                                   mean(Dataframe_list[["nonSuperstar"]][[interceptDifference]], na.rm = TRUE) #add differences in intercept
  }
  
  
  if (CBD == FALSE) {
    
    if (density_quantiles == TRUE) {
      ReturnPlot <-  ggplot() +
                          geom_ribbon(data = RegObjects[["Superstar_smooth"]], aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
                          geom_line(data =  RegObjects[["Superstar_smooth"]], aes(x = rank_density_CBSA, y = est, color = SampleNames[["Superstar"]])) + 
                          geom_ribbon(data = RegObjects[["nonSuperstar_smooth"]], aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
                          geom_line(data = RegObjects[["nonSuperstar_smooth"]], aes(x = rank_density_CBSA, y = est, color = SampleNames[["nonSuperstar"]])) + 
                          scale_colour_manual(name="Sample", values = c("blue","red")) #Change colors here
  
      
    }else{ #else, regress on log density quantiles
      ReturnPlot <-  ggplot() +
                         geom_ribbon(data = RegObjects[["Superstar_smooth"]], aes(ymin = lower_ci, ymax = upper_ci, x = demeaned_log_Housing_density), alpha = 0.2) +
                         geom_line(data =  RegObjects[["Superstar_smooth"]], aes(x = demeaned_log_Housing_density, y = est, color = SampleNames[["Superstar"]])) + 
                         geom_ribbon(data = RegObjects[["nonSuperstar_smooth"]], aes(ymin = lower_ci, ymax = upper_ci, x = demeaned_log_Housing_density), alpha = 0.2) +
                         geom_line(data = RegObjects[["nonSuperstar_smooth"]], aes(x = demeaned_log_Housing_density, y = est, color = SampleNames[["nonSuperstar"]])) + 
                         scale_colour_manual(name="Sample", values = c("blue","red")) #Change colors here
    }

  } #End check if CBD is false
  
  if (CBD == TRUE) {
    ReturnPlot <-  ggplot() +
                      geom_ribbon(data = RegObjects[["Superstar_smooth"]], aes(ymin = lower_ci, ymax = upper_ci, x = rank_inv_D2CBD), alpha = 0.2) +
                      geom_line(data =  RegObjects[["Superstar_smooth"]], aes(x = rank_inv_D2CBD, y = est, color = SampleNames[["Superstar"]])) + 
                      geom_ribbon(data = RegObjects[["nonSuperstar_smooth"]], aes(ymin = lower_ci, ymax = upper_ci, x = rank_inv_D2CBD), alpha = 0.2) +
                      geom_line(data = RegObjects[["nonSuperstar_smooth"]], aes(x = rank_inv_D2CBD, y = est, color = SampleNames[["nonSuperstar"]])) + 
                      scale_colour_manual(name="Sample", values = c("blue","red")) #Change colors here
    
    
  }
  
  #Returning plot
  return(ReturnPlot)
} 




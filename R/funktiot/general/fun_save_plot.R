# Define a helperfunction to make saving plots easier and more readable

save2 <- function(obj, path, wid = 18, hei = 13, unit = "cm") {
  
  # Save image of the plot
  if ("ggplot" %in% class(obj))
    ggsave(paste0(path,".png"), obj, width = wid, height = hei, units = unit)
  else png(paste0(path,".png"), wid, hei, unit, res = 300)
  # Save the plot data object
  save(obj, file = paste0(path,".RData"))
  
}


  
  
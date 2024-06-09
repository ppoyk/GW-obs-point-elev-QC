# Function to draw the monitoring stations from the provided table with
# lat and lon coordinates in CRS EPSG:3067


# First, just a function to draw points on the area of Finland (versatile use)
points_on_fin <- function(lon, lat, main, col = "red") {
  
  # Haetaan Suomen alueen kartta
  kartta <- rnaturalearth::ne_countries(
    scale = "large",
    returnclass = "sf",
    country = c("finland", "aland"))
  
  # Muutetaan koordinaattireferenssi oikeaksi
  kartta <- sf::st_transform(kartta, crs = crs)
  
  # Luodaan plotti jossa kaikkien asemien pisteet. Tarvittaessa rajataan karttaa
  pisteet_kartta <- ggplot() +
    # Plot the base map
    geom_sf(data = kartta) +
    #ggplot2::coord_sf(xlim=c(70000, 755000), ylim=c(6600000, 7800000), expand=FALSE) +
    # Add the coordinate points
    geom_point(data = data.frame(lon, lat),
               mapping = aes(x = lon, y = lat),
               shape = "circle",
               colour = col,
               size = 3,
               alpha = 0.35) +
    ggtitle(main) + # Add provided title text
    theme(plot.title = element_text(hjust = 0.5, size = 11), #Justify title to center
          axis.title.x=element_blank(), axis.title.y=element_blank()) #No axis labs
  
  return(pisteet_kartta)
  }

# Helper function to draw the monitoring stations
piirra_asemakartta <- function(asemataulu, file = NULL, main, col = "red"){
  
  # Handle main title
  if (rlang::is_empty(main)) # Display "by default"
    main <- paste("Hydrologisen seurannan pohjavesiasemat, n =",length(asemataulu[,1]))
  if (is.na(main)) # Option to omit title
    main = NULL
  
  # Plot the station points on map
  asemat_kartalla <- points_on_fin(lon = asemataulu$lon,
                                   lat = asemataulu$lat,
                                   main = main,
                                   col = col)
  
  if (!is.null(file)) {
    # Save to designated file path
    ggsave(file.path(file), asemat_kartalla,
           height = 13, width = 8, units = "cm")
  }
  
  return(asemat_kartalla) # Return for further editing
}

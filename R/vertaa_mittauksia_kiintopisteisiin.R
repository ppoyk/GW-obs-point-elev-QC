# This script compares GPS measurements done on national coordinate-system-
# defining reference points for this research to the reference point values
# stored in the MML database.


# Import measured control point data 
ref_kp <- readxl::read_excel(
  file.path(D$data,"ref_mittaukset/ref_mittaukset.xlsx"),
  sheet = "kp", skip = 1)


yhdista_kiintopisteet <-
  function(tunnus, link_data, point_data) {
    
    # Ota mittauspisteiden linkityksen rivi tunnuksen mukaan
    row <- link_data[link_data[1] == tunnus, ]
    
    point_name <- row$point_name # Ota mittauspisteen tekstiosa
    point_seq <- row$point_start_no:row$point_end_no # Pisteiden numero-osat
    data <-
      point_data[point_data$Point %in% paste0(point_name, point_seq), ]
    
    # Insert data of points in place of point name and point indexes
    data <- cbind(kiintopistetunnus = rep(tunnus, nrow(data)),
                  data)
    return(data)
  }
# Join control point measurements with related kiintopistetunnus
ref_kp <- apply(ref_kp[,"kiintopistetunnus"], 1, yhdista_kiintopisteet,
                link_data = ref_kp, point_data = ref_raw) # Use fun defined above
ref_kp <- do.call(rbind, ref_kp) # Transform from list into df


# Select control points from a database table where own measurements were taken
db_kp <- kp1lk[kp1lk$kiintopistetunnus %in% ref_kp$kiintopistetunnus, ]
# INSERT TRANSFORMED N2000 ELEVATION VALUES
# (https://kartta.paikkatietoikkuna.fi/ coord. transformation tool) (values from
# cols EUREF_FIN_GRS80_lon/_lat/h)(65.224356231,25.367352328,37.142 -> 19.4077)
db_kp[["GRS80h_to_N2000"]] <- NA
db_kp[db_kp$kiintopistetunnus == "5403", "GRS80h_to_N2000"]   <- 19.4077
db_kp[db_kp$kiintopistetunnus == "53327", "GRS80h_to_N2000"]  <- 118.7129
db_kp[db_kp$kiintopistetunnus == "91M3247","GRS80h_to_N2000"] <- 107.3791
db_kp[db_kp$kiintopistetunnus == "982616", "GRS80h_to_N2000"] <- 68.5768

# Join database kp values to own measurements
ref_kp <- dplyr::left_join(ref_kp, db_kp, by = "kiintopistetunnus")




# Plot measurement differences to db values
# Calculate horizontal difference to reference point
ref_kp[["hz_ero"]] <- terra::distance(
  cbind(ref_kp$East, ref_kp$North),
  cbind(as.numeric(ref_kp$ETRS_TM35FIN_E), as.numeric(ref_kp$ETRS_TM35FIN_N)),
  lonlat = F, pairwise = T)

ref_kp[["lon_ero"]] <- ref_kp$East - as.numeric(ref_kp$ETRS_TM35FIN_E)
ref_kp[["lat_ero"]] <- ref_kp$North - as.numeric(ref_kp$ETRS_TM35FIN_N)

# Vertical difference to reference point (no need to adjust based on the measurement
#  surface type, as measured directly from the solid reference point marker)
ref_kp[["vt_ero"]] <- ref_kp$Elev - as.numeric(ref_kp$N2000_korkeus)
# Vertical difference to reference point EUREF-FIN-GRS80h -> N2000 elevations
ref_kp[["vt_transf_ero"]] <- ref_kp$Elev - ref_kp$GRS80h_to_N2000


# Plot performance compared to reported accuracy values
# Elevations
vt_err_v_prec <-
  ggplot(ref_kp, aes(x=Vt_Prec, y=vt_ero, color=kiintopistetunnus)) +
  geom_point(shape=1, size=3) +
  geom_abline(slope=c(1,-1),intercept=0, linetype=2) +
  geom_hline(yintercept=0, linewidth=1) + coord_cartesian(ylim=c(-0.05, 0.05)) +
  labs(y="Vt. Error (N2000) [m]",x="Vt. Precision [m]") +
  theme_classic() + theme(legend.position="none")
vt_err_v_prec_transf <-
  ggplot(ref_kp,aes(x=Vt_Prec, y=vt_transf_ero, color=kiintopistetunnus)) +
  geom_point(shape=1, size=3) +
  geom_abline(slope=c(1,-1),intercept=0, linetype=2) +
  geom_hline(yintercept=0, linewidth=1) + coord_cartesian(ylim=c(-0.05, 0.05)) +
  labs(y="Vt. Error (GRS80h to N2000) [m]",x="Vt. Precision [m]") +
  theme_classic() + theme(legend.position="none")
# Lon Lat coordinates
lon_err_v_prec <-
  ggplot(ref_kp, aes(x=Hz_Prec, y=lon_ero, color=kiintopistetunnus)) +
geom_point(shape=1, size=3) +
  geom_abline(slope=c(1,-1),intercept=0, linetype=2) + 
  geom_hline(yintercept=0, linewidth=1) + coord_cartesian(ylim=c(-0.1, 0.1)) +
  labs(y="Easting Error (ETRS-TM35FIN) [m]",x="Hz. Precision [m]") +
  theme_classic() + theme(legend.position="none")
lat_err_v_prec <-
  ggplot(ref_kp, aes(x=Hz_Prec, y=lat_ero, color=kiintopistetunnus)) +
  geom_point(shape=1, size=3) +
  geom_abline(slope=c(1,-1),intercept=0, linetype=2) +
  geom_hline(yintercept=0, linewidth=1) + coord_cartesian(ylim=c(-0.1, 0.1)) +
  labs(y="Northing Error (ETRS-TM35FIN) [m]",x="Hz. Precision [m]") +
  theme_classic() + theme(legend.position="none")
save2(vt_err_v_prec,
      f.path(D$tulokset,"ref_mittaukset",substitute(vt_err_v_prec)),
      wid=9, hei=9)
save2(vt_err_v_prec_transf,
      f.path(D$tulokset,"ref_mittaukset",substitute(vt_err_v_prec_transf)),
      wid=9, hei=9)
save2(lon_err_v_prec,
      f.path(D$tulokset,"ref_mittaukset",substitute(lon_err_v_prec)),
      wid=9, hei=9)
save2(lat_err_v_prec,
      f.path(D$tulokset,"ref_mittaukset",substitute(lat_err_v_prec)),
      wid=9, hei=9)
rm(vt_err_v_prec, vt_err_v_prec_transf, lon_err_v_prec, lat_err_v_prec)

# Plot boxplot on abs error vs the reported precision
ref_kp_plotdata <- data.frame(
  Vt_N2000 = abs(ref_kp$vt_ero) - ref_kp$Vt_Prec,
  Vt_GRS80h_to_N2000 = abs(ref_kp$vt_transf_ero) - ref_kp$Vt_Prec,
  #Hz = abs(ref_kp$hz_ero) - ref_kp$Hz_Prec,
  lon = abs(ref_kp$lon_ero) - ref_kp$Hz_Prec,
  lat = abs(ref_kp$lat_ero) - ref_kp$Hz_Prec,
  kiintopistetunnus = ref_kp$kiintopistetunnus
  )
                    
ref_kp_plotdata <- tidyr::pivot_longer(ref_kp_plotdata, cols=!c("kiintopistetunnus"),
                                       names_to="type", values_to="error")
ref_kp_prec_acc <- ggplot(ref_kp_plotdata, aes(x=type, y=error)) + geom_boxplot() +
  geom_hline(yintercept=0, linetype=5) +
  scale_x_discrete(labels = c(lon="Easting", lat="Northing",
    Vt_N2000="Elevation (N2000)",Vt_GRS80h_to_N2000="Elevation\n(GRS80h to N2000)")) +
  labs(y="Abs. measurement error – dev. reported precision [m]",x=NULL) +
  theme_classic()
save2(ref_kp_prec_acc, f.path(D$tulokset,"ref_mittaukset","error_vs_prec"),
      wid=14, hei=10)
rm(ref_kp_plotdata, ref_kp_prec_acc)

# Get best measurements for each kp
ref_kp_best <- data.frame(ref_kp[0, ]) # Init df with right cols
for (kpt in unique(ref_kp$kiintopistetunnus)) {
  kp_points <- ref_kp[ref_kp$kiintopistetunnus == kpt ,]
  # Append best point info to end of df
  ref_kp_best <- rbind(ref_kp_best, kp_points[which.min(kp_points$Vt_Prec), ])
}
rm(kpt, kp_points)

# Print results of best measurements
cat("GPS device vs. reference points:")
print(ref_kp_best[,c("kiintopistetunnus","Hz_Prec","hz_ero",
                     "Vt_Prec","vt_ero","vt_transf_ero")])

rm(db_kp, ref_kp)


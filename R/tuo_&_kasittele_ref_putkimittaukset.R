

# Import measured pipe info and point linking
ref_mittaukset <- readxl::read_excel(
  file.path(D$data,"ref_mittaukset/ref_mittaukset.xlsx"),
  sheet = "pipes", skip = 1)
# Trim empty lines used to help in data entry
ref_mittaukset <- ref_mittaukset[!is.na(ref_mittaukset$tunnus), ]
# Adjust visible pipe length to be in meters
ref_mittaukset$putki_ref_raw <- ref_mittaukset$putki_ref_raw / 100
# Adjust "tunnus" to be in line with the project (prevent filepath errors)
ref_mittaukset$tunnus <- sub("/", "-", ref_mittaukset$tunnus)

# Define a fun to consolidate raw points into a single observation for 1 location
yhdista_mittauspisteet <-
  function(tunnus, link_data, point_data) {
    # Ota mittauspisteiden linkityksen rivi tunnuksen mukaan
    row <- link_data[link_data[1] == tunnus, ]
    
    point_name <- row$point_name # Ota mittauspisteen tekstiosa
    point_seq <- row$point_start_no:row$point_end_no # Pisteiden numero-osat
    data <-
      point_data[point_data$Point %in% paste0(point_name, point_seq), ]
    
    # Observation combination/compaction method:
    data <- data[which.min(data$Vt_Prec), ] #Select obs. w/ best vt.prec. (1st match)
    
    # Insert compacted data in place of point name and point indexes
    data <- cbind(row[ ,1:(match("point_name",names(row)) - 1)],
                  data,
                  if (ncol(row) > match("point_end_no",names(row))) { #if not last col
                    row[ ,(match("point_end_no",names(row)) + 1):ncol(row)]}
                  else NA)
    
    return(data)
  }

# Consolidate measured points into one for each pipe with custom fun
ref_mittaukset <- apply(ref_mittaukset[,"tunnus"], 1, yhdista_mittauspisteet,
                        link_data = ref_mittaukset, point_data = ref_raw)
ref_mittaukset <- do.call(rbind, ref_mittaukset) # Transform from list into df

if (anyDuplicated(ref_mittaukset$paikka_id)!=0 | anyNA(ref_mittaukset$paikka_id)) stop()


# Transform surface into a factor
ref_mittaukset$mittausalusta <- as.factor(ref_mittaukset$mittausalusta)
if (length(levels(ref_mittaukset$mittausalusta)) > 3) # Check for typos etc.
  stop("Too many factor levels in reference measurement data")

# Adjust measured pipe lengths according to the measurement surface
ref_mittaukset[["putki_ref"]] <- NA
for (i in 1:nrow(ref_mittaukset)) {
  ref_mittaukset$putki_ref[i] <- 
    switch(as.character(ref_mittaukset$mittausalusta[i]),
           "moss" = ref_mittaukset$putki_ref_raw[i] + 0.02,
           "sand" = ref_mittaukset$putki_ref_raw[i],
           "solid"= ref_mittaukset$putki_ref_raw[i])
}
rm(i)


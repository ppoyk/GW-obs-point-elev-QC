# Function to retrieve the mapping table for corresponding manual and automatic
# observation locations.
# (Ids which correspond to the same physical pipe where manual measurements are
# stored (col 2) and where an automatic device sensors stores the values (col 1))

hae_PaikkaAutomaattiManuaali <- function() {
  
  # Connect to db
  yhteys <- .connect_db(D$secrets)
  on.exit(DBI::dbDisconnect(yhteys))
  
  # Get the linking table
  PaikkaAutomaattiManuaali <- DBI::dbGetQuery(
    yhteys, 
    readLines(f.path(D$secrets,"SQL",
                     p0(as.character(match.call()[[1]]),".txt")))
    )
    
  return(PaikkaAutomaattiManuaali)
}


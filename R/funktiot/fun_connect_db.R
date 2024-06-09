# Funktio, jolla avataan yhteys SQL-tietokantaan
# Kysyy käytäjältä tietokannan identifioivat tiedot, jos vastaavaa tiedostoa ei ole
# Kun tiedot ovat tiedostossa, funktio lukee arvot jatkossa sieltä
# Tiedoston kansiota ei saa seurata versionhallinnassa

.connect_db <- function(dir_db_info) {
  # Annetun polun tulee viedä kansioon jossa db_info niminen yaml sijaitsee
  # (Pääskriptissä määritelty muuttujaan dir_secrets)
  # Muutetaan annettu polku varalta uudelleen poluksi
  dir_db_info <- file.path(dir_db_info)
  
  # Aseta vaatimukset paketeista
  if (!requireNamespace(c("DBI", "obcd", "yaml"), quietly = TRUE)) {
    stop(
      "Packages \"DBI\", \"odbc\" and \"yaml\" must be installed to use this function.",
      call. = FALSE
    )}
  
  # Tarkista tallennuskansion olemassaolo.
  if (!dir.exists(dir_db_info)) {
    stop("Tietokannan asetustiedoston kansiota ei ole!\nTarkista ", dir_db_info, call. = FALSE)
  }
  
  # Määritä polku tiedostonimeä myöten
  dir_db_info <- file.path(dir_db_info, "db_info.yaml")
  
  # Tarkista tietokannan osoitetiedoston olemassaolo
  if (!file.exists(dir_db_info)) {
    # Kysy db tietoja, ja kirjoita vastaukset tiedostoon, jota jatkossa käytetään
    writeLines(c("\n##### Yhteyttä ei voitu muodostaa",
                 paste("##### Tiedot puuttuvat kohteesta", dir_db_info)))
    yaml::write_yaml(data.frame(
      palvelin = {cat("\n----- Syötä palvelimen nimi:"); readline()},
      tietokanta = {cat("\n----- Syötä tietokannan nimi:"); readline()}
    ),
    file = file(dir_db_info, open = "w"))
  }
  
  # Siivous
  on.exit(expr = {rm(dir_db_info)})
  
  # Muodosta yhteys lukemalla rivit tiedostosta, ja
  # vie yhteys funktiota kutsuneeseen ympäristöön
  return(
    DBI::dbConnect(odbc::odbc(),
                   .connection_string = paste0(
                     "driver={SQL Server};server=",
                     yaml::yaml.load_file(dir_db_info)[["palvelin"]],
                     ";database=",
                     yaml::yaml.load_file(dir_db_info)[["tietokanta"]],
                     ";trusted_connection=true"
                   )
    )
  )
}


# Sisältää funktiot erilaisia skriptin vaatimien yhteyksien tarkistamista varten

# Tarkistaa tietokannan tiedot sisältävän tiedoston olemassaolon
# Tarvittaessa luo uuden ,kysyen sisällön käyttäjältä.
check_db_info <- function(...) {
  file <- file.path(...)
  if (file.exists(file)){
    message("Tiedosto tietokantaan yhdistämistä varten löytyy ✅")
  } else {
    warning("##### Tiedot pohjavesitietokantaan yhdistämistä varten puuttuvat kohteesta",file)
    cat("\n#### Syötä tallennettavat tiedot ⏬\n")
    yaml::write_yaml(data.frame(
      palvelin = {cat("\n----- Syötä palvelimen nimi:"); readline()},
      tietokanta = {cat("\n----- Syötä tietokannan nimi:"); readline()}
    ),
    file = file(file, open = "w"))
  }
}


# Tarkistaa Maanmittauslaitoksen API-avaimen tiedoston olemassaolon
check_key <- function(..., verbose = F) {
  file <- file.path(...)
  if (file.exists(file)) {
    if(verbose) message("Avain MML:n aineiston lataamista varten löytyy ✅")
    return(invisible(readLines(file))) # Return API-key value invisibly
  } else {
    warning("Tiedostoa Maanmittauslaitoksen tiedostonlatauspalvelun API-avaimelle ei löydy")
    cat("\n https://www.maanmittauslaitos.fi/rajapinnat/api-avaimen-ohje")
    writeLines(readline("Syötä käytettävä API-avain ▶️"), con = file)
  }
}

# Funktiota ei käytetä tässä projektissa, koska tässä tapauksessa ei voida taata
# kirjoitusoikeuksia LAS-aineiston levylle (CSC:n Laseralustan käyttö), ja
# LAX:ien pitäisi olla alkuperäisten tiedostojen rinnalla.
# Funktio hyödyllinen, jos käytettävä LAS-aineisto on itsellä vapaasti muokattavissa


# Luodaan lax-tiedostot katalogin laz-tiedostoille
# (https://rapidlasso.de/lasindex-spatial-indexing-of-lidar-data/)
# Pitäisi nopeuttaa tiedon rajaamista tiedostoista, jos järjestelmä osaa käyttää lax:eja oikein

# Vaihtoehtoinen toteutustapa, mutta ei ole löytynyt toimivaa ratkaisua:
# lidR::opt_output_files(ktlg) <- file.path(D$data, "5p_aineisto")
# ktlg@output_options$drivers$LAS$param = list(index = TRUE)
# lidR::opt_output_files(ktlg) <- ""
# Ei ainakaan suoraan luo LAXeja, ehkä jatkoprosessoinnin myötä muodostaisi?


# Myös mahdollisuus pakottaa lax:ien muodostaminen kaikille laz-tiedostoille
# kehitystarkoituksessa (RASKAS!)

luo_katalogin_lax <- function(las_katalogi, force = FALSE) {
  # Las-katalogi voidaan tarjota valmiina katalogina, tai katalogin muodostavien
  # tiedostojen vektorina
  
  # Jos on annettu katalogi, luetaan käytetyt tiedostot sen rakenteesta
  if(class(las_katalogi) == "LAScatalog") {
    # (tiedostopolut muutetaan tarvittaessa standardimuotoon)
    if (any(grepl("\\\\", las_katalogi@data$filename))) {
      files <- gsub("\\\\", "/", las_katalogi@data$filename)
    } else files <- las_katalogi@data$filename # Tämän pitäisi olla vakio vain ei-Windows ympäristössä
    
  # Jos on annettu vektori tiedostopolkuja, otetaan sellaisenaan
  } else if (class(las_katalogi) == "character") {
    files <- las_katalogi
  } else stop("Katalogin tiedot syötetty virheellisessä muodossa")
  
  
  if (force) { # Jos halutaan luoda lax-tiedostot uudestaan kaikille
    message("Katalogin kaikkia LAX-tiedostoja luodaan uudelleen...")
    sapply(files, FUN = rlas::writelax)
  } else { # Muuten luodaan vain niille joilta puuttuu
    # Muutetaan kaikki katalogin tiedostojen polut lax-päätteisiksi, ja tarkistetaan olemassaolo
    puuttuvat_lax <- gsub(".laz$", ".lax", files)
    puuttuvat_lax <- !file.exists(puuttuvat_lax)

    if (all(!puuttuvat_lax)) { # Tehdään vain jos yhtään tiedostoa jäljellä käsiteltäväksi
      message(paste0("Ei puuttuvia LAX-tiedostoja luotavaksi annetulle katalogille '",
                    substitute(las_katalogi), "'"))
    } else {
      # Ajetaan lax:ien muodostaminen niille joilta se puuttuu
      message("Katalogin puuttuvia LAX-tiedostoja luodaan...")
      sapply(files[puuttuvat_lax], FUN = rlas::writelax)
    }
  }
}




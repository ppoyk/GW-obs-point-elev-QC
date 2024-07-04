# ENIMMÄKSEEN VANHENTUNUT !
# Funktion tuloksia ei käytetä pääskriptissä 00-base.R mihinkään.
# Pääasiassa käytä funktiota fun_hae_paikat.R, mistä voi suodattaa pois muut kuin havaintoputket

# Hakee putkien tiedot asemilta, joiden ID on annettu listassa. (Pitää ehkä olla data.frame rivi/sarake)

# Jos savepath annettu, tallentaa hakutuloksen tähän sijaintiin

hae_putket <- function(asema_id_lista, savepath = NULL) {
  # Yhdistetään tietokantaan
  yhteys <- .connect_db(D$secrets)
  on.exit(DBI::dbDisconnect(yhteys)) # Suljetaan yhteys funktiosta poistuttaessa
  
  asema_id_lista <- paste(asema_id_lista, collapse = ",") # Muutetaan kirjain vektoriksi
  
  haetut_putket <- DBI::dbGetQuery(
    yhteys,
          # Rajaus aktiivisessa seurannassa oleviin asemiin tehty hae_asematiedot.R -tiedostossa, jonka asema ID:itä käytetään paikkojen/putkien haun perusteena.
          # Rajataan pois AM-putket, koska läht.koht. tietojen pitäisi täsmätä man.putkien kanssa
    stringr::str_glue(
      p(readLines(f.path(D$secrets,"SQL",
                             p0(as.character(match.call()[[1]]),".txt"))),#Query from file
        collapse = " "),
      asema_id_lista = asema_id_lista)) # Query supplied with ids to insert

  
  # Korvataan kautta-merkit tekstistä viivalla, jotta tallennuspolut eivät sekoa
  for (sarake in names(haetut_putket)) {
    if (is.character(haetut_putket[[sarake]])) {
      haetut_putket[[sarake]] <- sub("/", "-", haetut_putket[[sarake]])
      # Trimmataan turhat välilyönnit tms. joita on taulussa.
      # Varoitus! trimws() Muuttaa luvut "tekstiksi" jos sovelletaan kaikkiin sarakkeisiin
      haetut_putket[[sarake]] <- trimws(haetut_putket[[sarake]])
    }
  }
  
  # Muutetaan PaikkaTyyppi factor-muuttujaksi. Id<->Selite tietoina käytetty PaikkaTyyppi-taulua
  haetut_putket$PaikkaTyyppi_Id <- factor(haetut_putket$PaikkaTyyppi_Id,
                              levels = c("1", "2", "3", "4", "5",
                                         "6", "7", "8", "9", "10",
                                         "12", "13"), #HUOM 11 puuttuu (myös kannassa)
                              labels = c("Havaintoputki", "Lähde",
                                    "Pohjavesilammikko", "Kaivo",
                                    "Routaputki", "Maankosteusmittari",
                                    "Lumikeppi", "Lysimetri", "Luminäyte",
                                    "Ottamon hana", "Talousvesihana",
                                    "Virtaamahavaintopaikka"))
  
  # Tallennetaan levylle jos halutaan
  if (!is.null(savepath)) {
    data.table::fwrite(haetut_putket,
                       file = file.path(savepath),
                       na = NA, bom = T)
    if (file.exists(savepath)) {
      message("Haetut paikkojen tiedot tallennettu:", savepath)
    } else stop("Paikkojen tietojen tallennusyritys epäonnistui:", savepath, call. = F)
  }
  
  return(haetut_putket)
}

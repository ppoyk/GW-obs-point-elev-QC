# Funktio, jolla haetaan pohjavesitietokannasta haluttujen havaintopaikkojen
# pinnankorkeuden mittaustulokset paikka_id:n perusteella.
# Pitäisi palauttaa tietoja vain putkien ja kaivojen ID:illä.
# g = groundwater

# Haettu taulu voidaan tallentaa haluttuun sijaintiin.
# tiedot haetuilta havaintopaikoilta.
hae_g <- function(paikka_id_lista, savepath = NULL) {
  # Yhdistetään tietokantaan
  yhteys <- .connect_db(D$secrets)
  on.exit(DBI::dbDisconnect(yhteys))
  
  
  paikka_id_lista <- paste(paikka_id_lista, collapse = ",") # Muutetaan kirjain vektoriksi
  
  haetut_pinnkor <- DBI::dbGetQuery(
    yhteys,
    stringr::str_glue(
      p(readLines(f.path(D$secrets,"SQL","hae_g.txt")),#Query from file
        collapse = " "),
      paikka_id_lista = paikka_id_lista)
  )
  
  # Muutetaan tyhjät liput NA'sta "":ksi
  haetut_pinnkor$lippu[is.na(haetut_pinnkor$lippu)] <- ""
  
  # Muutetaan lippu_id factoriksi. Järjestetty luotettavuuden perusteella.
  # Nimeämisessä käytetty tietokannan taulua SiirtoLippu
  # (ei haeta mäppäyksiä suoraan tietokannasta, koska selitteet liian pitkäsanaisia)
  haetut_pinnkor$lippu <- factor(
    haetut_pinnkor$lippu,
    levels = c("O","M","","A","L","K","J"),
    labels = c("Oikea","Mitattu","(tyhjä)",
               "Arvioitu","Perustuu kenttäkeskiarvoihin",
               "Kuiva","Jäässä"),
    ordered = TRUE)
  
  # Sanitize "/" and remove whitespace
  for (sarake in names(haetut_pinnkor)) {
    if (is.character(haetut_pinnkor[[sarake]])) {
      haetut_pinnkor[[sarake]] <- sub("/", "-", haetut_pinnkor[[sarake]])
      # Trimmataan turhat välilyönnit tms. joita on taulussa.
      # Varoitus! trimws() Muuttaa luvut "tekstiksi" jos ajetaan lukusarakkeeseen
      haetut_pinnkor[[sarake]] <- trimws(haetut_pinnkor[[sarake]])
    }
  }
  
  # Change date to correct datatype
  haetut_pinnkor$pvm <- base::as.Date(haetut_pinnkor$pvm, format = "%Y-%m-%d")
  
  # Tallennetaan haluttuun sijaintiin (jos halutaan)
  if (!is.null(savepath)) {
    write.csv2(haetut_pinnkor,
               file = file.path(savepath))
    if (file.exists(savepath)) {
      message("Haettu pinnankorkeusdata tallennettu:", savepath)
    } else stop("Pinnankorkeuden tallennusyritys epäonnistui:", savepath, call. = F)
  }
  
  return(haetut_pinnkor)
}

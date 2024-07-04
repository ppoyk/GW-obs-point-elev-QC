# Haetaan lista halutuista hydrologisen pohjavesiseurannan
# asemista, ja niiden koordinaatit

# Jos savepath annettu, tallentaa hakutuloksen tähän sijaintiin

hae_asematiedot <- function(as_idt = NULL, savepath = NULL, tstamp = FALSE) {
  # Yhdistetään tietokantaan
  yhteys <- .connect_db(D$secrets)
  on.exit(DBI::dbDisconnect(yhteys)) # Suljetaan yhteys funktion lopussa
  
  
  if (!is.null(as_idt)) as_idt <- paste(as_idt, collapse=",") #Convert to string for SQL
  
  # SQL-lause asemien hakuun
  asemien_hakulause <- paste(
    paste(readLines(f.path(D$secrets,"SQL",
                           p0(as.character(match.call()[[1]]),".txt"))), #Query from file
          collapse = " "),
    if (!is.null(as_idt)){paste("AND PohjavesiAsema_Id IN (",as_idt,")")}) #ID search


  # Suoritetaan haku
  asemataulu <- DBI::dbGetQuery(yhteys, asemien_hakulause)
  
  # Korvataan kautta-merkit tekstistä viivalla, jotta tallennuspolut eivät sekoa
  for (sarake in names(asemataulu)) {
    if (is.character(asemataulu[[sarake]])) {
      asemataulu[[sarake]] <- sub("/", "-", asemataulu[[sarake]])
      # Trimmataan turhat välilyönnit tms. joita on taulussa.
      # Varoitus! trimws() Muuttaa luvut "tekstiksi" jos sovelletaan kaikkiin sarakkeisiin
      asemataulu[[sarake]] <- trimws(asemataulu[[sarake]])
    }
  }
  
  # Tallennetaan levylle
  if (!is.null(savepath)) {
    data.table::fwrite(asemataulu,
                       file = file.path(savepath),
                       na = NA, bom = T)
  }
  
  # Dokumentoidaan myös tieto, milloin aineisto ladattiin tietokannasta
  if (tstamp == TRUE) {
    writeLines(
      c("Tämä tiedosto päivitetään automaattisesti, kun pohjavesiasemien tiedot tallennetaan tietokannasta levylle", "",
        paste("Analyysissä käytetyt pohjavesiasemien tiedot ladattu Ympäristöhallinnon pohjavesitietokannasta:",Sys.time()),"",
        paste("Hakulauseen muotoutuminen kuvattu funktiossa",
              gsub(root,"",this.path::this.path()))),
      con = file.path(D$doc, "asemien_tietokantahaun_tiedot.txt"))
  }
  
  return(asemataulu)
}

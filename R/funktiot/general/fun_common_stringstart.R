# Funktio, joka löytää pisimmän yhteisen kirjainjonon kaikista tarjotuista jonoista
# From: https://stackoverflow.com/questions/28273716/r-implementation-for-finding-the-longest-common-starting-substrings-in-a-set-of 
common_stringstart <- function(x) {
  x <- sort(x)
  # split the first and last element by character
  d_x <- strsplit(x[c(1, length(x))], "")
  # search for the first not common element and so, get the last matching one
  # Varoitukset vaimennettu elementtien eri pituuksista
  suppressWarnings(der_com <- match(FALSE, do.call("==", d_x)) - 1)
  # if there is no matching element, return an empty vector, else return the common part
  ifelse(der_com == 0,
         return(character(0)),
         return(substr(x[1], 1, der_com)))
}


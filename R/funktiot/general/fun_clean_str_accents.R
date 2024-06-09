# Function to replace accented characters with ASCII counterparts

clean_str_accent <- function(s) {
  # Map replaceable letters to ASCII counterparts
  # (Not comprehensive)
  map <- list(
    a = "áàâäãåæ",
    A = "ÁÀÂÄÃÅÆ",
    c = "ç",
    C = "Ç",
    e = "éèêë",
    E = "ÉÈÊË",
    i = "íìîï",
    I = "ÍÌÎÏ",
    n = "ñ",
    N = "Ñ",
    o = "óòôöõøœ",
    O = "ÓÒÔÖÕØŒ",
    s = "śŝşšß",
    S = "ŚŜŞŠẞ",
    u = "úùûü",
    U = "ÚÙÛÜ"
  )

  # Go through the mapping, replacing accented letters
  for (l in 1:length(map)) {
    s <- chartr(map[[l]], # String of replaced, accented letters
                paste0(
                  rep(names(map[l]), nchar(map[[l]])),
                  collapse = ""), # Replacement stretched to same length as old str
                s)
  }

  return(s)
}


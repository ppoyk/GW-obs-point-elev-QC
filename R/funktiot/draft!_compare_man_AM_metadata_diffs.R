# An example of a function body to check discrepancies in metadata between
# linked manual and automatic GW monitoring places 

# Not universal, meaning that appropriate variables and data must be available
# beforehand.
# Could be developed into a self-contained function.

if (FALSE) {
  
koord_vrt <- apply(link_am_man, 1, function(x) {
  
  man <- kaikki_paikat[kaikki_paikat$paikka_id %in% x[[2]], c("lon", "lat")]
  am  <- kaikki_paikat[kaikki_paikat$paikka_id %in% x[[1]], c("lon", "lat")]
  
  if (all(!is.na(kaikki_paikat[kaikki_paikat$paikka_id %in% x[[2]], c("yhd_lon", "yhd_lat")])) &&
      all(!is.na(kaikki_paikat[kaikki_paikat$paikka_id %in% x[[1]], c("yhd_lon", "yhd_lat")]))) {
    if (isTRUE(all.equal(as.num(man), as.num(am)))) {
      return(NULL) 
    } else {
      warning(paste("Db koord eroavat:", x[[1]],x[[2]],"\n"))
    }
  } else {
    return(NULL) # Yhd koordit löytyvät, ei haittaa vaikka db:ssä erit
  }
}
)

}

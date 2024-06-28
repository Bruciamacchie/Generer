#' Fonction permettant de générer des placettes
#' @description Fonction permettant de générer n placettes sur un périmètre. Leur répartition
#' peut être régulière ou aléatoire
#'
#' @return Une liste da placettes sous forme d'un objet sf.
#'
#' @param shp = Périmètre sous forme d'un objet sf
#' @param ecart = écart à la bordure
#' @param NbPlac = nombre de placettes. Par défault NbPlac = 10.
#' @param type = type de répartition. Par défault type = "regular".
#' Le type peut également être "random" ou "hexagonal".
#' Dans le cas du type "random" le nombre exact de placettes sera tiré au hasard.
#' Pour les autres types, le nombre de placettes sera approché.
#'
#' @author Bruciamacchie Max
#' @import sf
#' @import tidyverse
#'
#' @examples
#' data(perim)
#' res <- Generer::GenererPla(perim)
#'
#' @export

CreatePlac <- function (shp, NbPlac = 10, ecart=30, type="regular") {

  zoneEchan  <- st_buffer(shp, dist = - ecart)
  placettes <- st_sample(zoneEchan, NbPlac, type=type, exact=T)
  placettes <- placettes |>
    st_sf() |>
    mutate(NumPlac = 1:length(placettes))

  return(placettes)
}

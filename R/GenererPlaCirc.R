#' Fonction permettant de générer des placettes circulaires
#' @description Fonction permettant de générer n placettes circulaires sur un périmètre. Leur répartition
#' peut être régulière ou aléatoire
#'
#' @return Une liste da placettes sous forme d'un objet sf.
#'
#' @param shp = Périmètre sous forme d'un objet sf
#' @param arbres = fichier obtenu avec la fonction GenererArbres.
#' @param rayon = rayon de la placette en mètre. Par défaut rayon = 15 m.
#' @param NbPlac = nombre de placettes. Par défault NbPlac = 10.
#' @param type = type de répartition. Par défault type = "regular".
#' Le type peut également être "random" ou "hexagonal".
#' Dans le cas du type "random" le nombre exact de placettes sera tiré au hasard.
#' Pour les autres types, le nombre de placettes sera approché.
#'
#' @author Bruciamacchie Max
#' @import sf
#' @import tidyverse
#' @import units
#'
#' @examples
#' data(perim)
#' res <- Generer::GenererPlaCirc(perim, arbres)
#' res$Estim
#' res$graph
#'
#' @export

GenererPlaCirc <- function (shp, placettes, arbres, rayon = 15) {

  placettes  <- placettes |>
    st_buffer(dist=rayon)

  # zoneEchan  <- st_buffer(shp, dist=-rayon)
  # centrePlac <- st_sample(zoneEchan, NbPlac, type=type, exact=T)
  # placettes  <- st_buffer(centrePlac, dist=rayon)
  # placettes <- st_sf(placettes) %>%
  #   mutate(NumPlac = 1:length(centrePlac))

  echan <- st_intersection(arbres, placettes)

  tabCir <- echan %>%
    group_by(NumPlac) %>%
    summarise(Gha = sum(G)*10000/pi/rayon^2)
  EstimCir <- sum(tabCir$Gha)/length(centrePlac)



  out <- list(EstimCir,  echan)
  names(out) <- c("Estim", "echan")
  return(out)

}

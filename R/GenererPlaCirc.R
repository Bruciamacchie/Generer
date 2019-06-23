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

GenererPlaCirc <- function (shp, arbres, rayon = 15, NbPlac = 10, type="regular") {

  zoneEchan  <- st_buffer(shp, dist=-rayon)
  centrePlac <- st_sample(zoneEchan, NbPlac, type=type, exact=T)
  placettes  <- st_buffer(centrePlac, dist=rayon)
  placettes <- st_sf(placettes) %>%
    mutate(NumPlac = 1:length(centrePlac))
  echan <- st_intersection(arbres, placettes)

  tabCir <- echan %>%
    group_by(NumPlac) %>%
    summarise(Gha = sum(G)*10000/pi/rayon^2)
  EstimCir <- sum(tabCir$Gha)/length(centrePlac)

  g <- ggplot() +
    geom_sf(data=shp, fill=NA) +
    geom_sf(data=zoneEchan, fill=NA, color='red', linetype = "12") +
    geom_sf(data=placettes, fill='red', alpha=0.2) +
    geom_sf(data=centrePlac, color='red', shape=3) +
    geom_sf(data=arbres, aes(size=Diam), shape=1, color='blue', alpha=0.5) +
    guides(size=F) +
    geom_sf(data=echan,  aes(size=Diam), color='blue', alpha=0.5) +
    guides(size=F)

  out <- list(EstimCir, g)
  names(out) <- c("Estim", "graph")
  return(out)

}

#' Fonction permettant de générer des placettes à angle fixe
#' @description Fonction permettant de générer n placettes à angle fixe sur un périmètre.
#' Leur répartition peut être régulière ou aléatoire
#'
#' @return Une liste da placettes sous forme d'un objet sf.
#'
#' @param shp = Périmètre sous forme d'un objet sf
#' @param arbres = fichier obtenu avec la fonction GenererArbres.
#' @param alpha = angle retenu. Par défaut alpha = 0.02.
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
#' res <- Generer::GenererPlaAngle(perim, arbres)
#' res$Estim
#' res$graph
#'
#' @export

GenererPlaAngle <- function (shp, arbres, alpha=0.02, NbPlac = 10, type="regular") {

  # --------- centre des placettes
  Dmax <- max(arbres$Diam)
  zoneEchan <- st_buffer(shp, dist=-Dmax/alpha/100)
  centrePlac <- st_sample(zoneEchan, NbPlac, type=type, exact=T)
  centrePlac <- st_sf(centrePlac) %>%
    mutate(NumPlac = 1:length(centrePlac))
  # --------- Echantillon
  arbresBuf <- st_buffer(arbres, dist= arbres$Diam/alpha/100) %>%
    mutate(Rayon = Diam/alpha/100)
  # st_intersects permet de compter les arbres qui sont sur plusieurs placettes
  mat <- as.data.frame(st_intersects(arbresBuf, centrePlac, sparse = F))
  names(mat) <- centrePlac$NumPlac
  mat$ID = arbresBuf$ID

  arbresEch <- mat %>%
    gather(NumPlac, value, -ID) %>%
    filter(value) %>%
    left_join(arbres, by = "ID")

  arbresBufEch <- st_intersection(arbresBuf, centrePlac)

  tab <- arbresEch %>%
    group_by(NumPlac) %>%
    summarise(Nb = n(),
              Gha = Nb*2500*alpha^2)

  EstimAng <- mean(tab$Gha)

  g <- ggplot() +
    geom_sf(data=shp, fill=NA) +
    geom_sf(data=zoneEchan, fill=NA, color='red', linetype = "12") +
    geom_sf(data=arbres, aes(size=Diam), shape=1, color='blue', alpha=0.1) +
    guides(size=F) +
    geom_sf(data=arbresEch, aes(size=Diam), color='blue', alpha=0.6) +
    guides(size=F) +
    geom_sf(data=centrePlac, color='red', shape=3) +
    theme_bw()

  out <- list(EstimAng, g)
  names(out) <- c("Estim", "graph")
  return(out)
}

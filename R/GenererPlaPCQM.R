#' Fonction permettant de générer des placettes PCQM
#' @description Fonction permettant de générer n placettes PCQM.
#' Leur répartition peut être régulière ou aléatoire
#'
#' @return Une liste da placettes sous forme d'un objet sf.
#'
#' @param shp = Périmètre sous forme d'un objet sf
#' @param arbres = fichier obtenu avec la fonction GenererArbres.
#' @param rayonPCQM = rayon maximal de recherche des arbres. Par défault rayonPCQM = 25m.
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
#' res <- Generer::GenererPlaPCQM(perim, arbres)
#' res$Estim
#' res$graph
#'
#' @export

GenererPlaPCQM <- function (shp, arbres, rayonPCQM=25, NbPlac = 10, type="regular") {

  zoneEchan  <- st_buffer(shp, dist=-rayonPCQM)
  centrePlac <- st_sample(zoneEchan, NbPlac, type=type, exact=T) %>%
    st_sf() %>%
    mutate(NumPlac = 1:dim(.)[1])

  placettes  <- centrePlac %>%
    st_buffer(dist=rayonPCQM)

  # centrePlac <- st_sample(zoneEchan, NbPlac, type=type, exact=T)
  # placettes  <- st_buffer(centrePlac, dist=rayonPCQM)
  # placettes <- st_sf(placettes) %>%
  #   mutate(NumPlac = 1:length(centrePlac))

  echan <- st_intersection(arbres, placettes)

  coordCentre <- do.call(rbind, st_geometry(centrePlac)) %>%
    as_tibble() %>%
    setNames(c("Xcentre","Ycentre"))

  tabCentre <- centrePlac %>%
    bind_cols(coordCentre) %>%
    st_set_geometry(NULL)

  echan <- echan %>%
    left_join(tabCentre, by = "NumPlac")

  coordEchan <- do.call(rbind, st_geometry(echan)) %>%
    as_tibble() %>%
    setNames(c("Xarbre","Yarbre"))

  arbresEch <- echan %>%
    bind_cols(coordEchan) %>%
    st_set_geometry(NULL) %>%
    mutate(Quart = NA,
           Quart = ifelse((Xarbre-Xcentre) >0 & (Yarbre-Ycentre) >0, 1, Quart),
           Quart = ifelse((Xarbre-Xcentre) >0 & (Yarbre-Ycentre) <0, 2, Quart),
           Quart = ifelse((Xarbre-Xcentre) <0 & (Yarbre-Ycentre) <0, 3, Quart),
           Quart = ifelse((Xarbre-Xcentre) <0 & (Yarbre-Ycentre) >0, 4, Quart)) %>%
    mutate(rayon = ((Xarbre-Xcentre)^2 + (Yarbre-Ycentre)^2)^0.5) %>%
    group_by(NumPlac, Quart) %>%
    slice(which.min(rayon)) %>%
    ungroup() %>%
    dplyr::select(ID, NumPlac, Quart, Diam, G, rayon) %>%
    left_join(arbres[,c("ID","geometry")], by = "ID")

  tab <- arbresEch %>%
    group_by(NumPlac) %>%
    mutate(Poids = 10000*3/pi/sum(rayon^2),
           Gha = G*Poids) %>%
    summarise(nb = n(),
              Gha = sum(Gha))

  EstimPCQM <- mean(tab$Gha)

  # arbresEchCor <- arbresEch %>%
  # filter(rayon <= rayonPCQM)

  arbresEchCor <- expand.grid(NumPlac=1:dim(centrePlac)[1], Quart=1:4) %>%
    arrange(NumPlac) %>%
    left_join(arbresEch, by = c("NumPlac", "Quart")) %>%
    dplyr::select(-geometry) %>%
    mutate(rayon = ifelse(is.na(rayon),
                          quantile(arbresEch$rayon, probs=0.95),
                          rayon))

  t1 <- arbresEchCor %>%
    group_by(NumPlac) %>%
    mutate(Poids = 10000*3/pi/sum(rayon^2),
           Gha = G*Poids) %>%
    summarise(nb = n(),
              Gha = sum(Gha, na.rm=T))

  EstimPCQMCor <- mean(t1$Gha)

  EstimPCQMmitchell <- EstimPCQMCor

  g <- ggplot() +
    geom_sf(data=shp, fill=NA) +
    geom_sf(data=zoneEchan, fill=NA, color='red', linetype = "12") +
    geom_sf(data=arbres, aes(size=Diam), shape=1, color='blue', alpha=0.1) +
    guides(size=F) +
    geom_sf(data=arbresEch, aes(size=Diam), color='blue', alpha=0.6) +
    guides(size=F) +
    geom_sf(data=centrePlac, color='red', shape=3) +
    theme_bw()

  out <- list(EstimPCQM, EstimPCQMCor, g)
  names(out) <- c("Estim", "EstimCor", "graph")
  return(out)

}

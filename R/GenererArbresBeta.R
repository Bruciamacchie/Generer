#' Fonction permettant de générer des arbres.
#' @description Fonction permettant de générer des arbres à partir d'une surface terrière objectif
#' et d'une distribution des diamètres selon une loi Béta définie par les paramètres beta1 et beta2.
#'
#' @return Une liste d'arbres sous forme d'un objet sf
#'
#' @param shp = Périmètre sous forme d'un objet sf
#' @param gha = Surfacec terrière à l'hectare
#' @param SseuilDist = distance mini acceptable entre tiges
#' @param beta1 = premier paramètre d'un loi béta
#' @param beta2 = second paramètre d'un loi béta
#' @param dmax = diam max
#' @param dmin = diam min
#'
#' @author Bruciamacchie Max
#' @import sf
#' @import tidyverse
#' @import units
#'
#' @examples
#' library(sf, quietly =T, warn.conflicts =F)
#' library(tidyverse, quietly =T, warn.conflicts =F)
#' library(units)
#' library(Generer)
#' # ----------------- Données
#' SeuilDist  <- set_units(1, m)    # Seuil minimum de distance entre 2 tiges
#' Beta1      <- 1    # coefft loi beta
#' Beta2      <- 4    # coefft loi beta
#' Gha        <- 17   # surface terriere objectif (m2/ha)
#' Dmax       <- 80
#' Dmin       <- 18
#' # ----------------- creation arbres
#' data(perim)
#' surf <- as.vector(st_area(perim))/10000
#' arbres <- GenererArbresGD(perim, Gha, SeuilDist, Beta1, Beta2, Dmax, Dmin)
#' sum(arbres$G)/surf
#'
#' @export

GenererArbresBeta <- function(shp, gha, seuilDist, beta1, beta2, dmax, dmin) {
  surf <- st_area(shp)
  surf <- as.vector(set_units(surf, "ha"))
  dmoy <- beta1/(beta1+beta2)*100 + 7.5
  nb <- gha*40000/dmoy^2/pi*surf
  nb <- round(nb * 1.2, 0)
  centres <- st_sample(shp, nb)
  centres <- st_sf(centres)
  # ------------ generation diametres
  nb = dim(centres)[1]
  arbres <- centres %>%
    rename(geometry = centres) %>%
    mutate(Diam = round(Dmin + rbeta(nb, beta1, beta2)*100,0)) %>%
    filter(Diam >=dmin & Diam <= dmax)
  # ------------ suppression des tiges trop proches
  d <- st_distance(centres)
  diag(d) <- NA
  close <- (d <= seuilDist)
  if (length(close)>0) {
    closePts <- which(close, arr.ind=T)
    centres <- centres[-closePts[,1],]
  }
  # ------------ niveau Gha
  arbres <- arbres %>%
    mutate(G = pi/40000*Diam^2,
           Cumul = cumsum(G)) %>%
    filter(Cumul <= gha*surf) %>%
    dplyr::select(-Cumul) %>%
    mutate(ID = 1:dim(.)[1])

  # ------------ Histograme
  histo <- arbres %>%
    st_set_geometry(NULL) %>%
    mutate(Classe = floor(Diam/5+0.5)*5) %>%
    group_by(Classe) %>%
    summarise(Nb = n()) %>%
    mutate(Nb = Nb/as.vector(set_units(st_area(shp), "ha"))) %>%
    ggplot(aes(x=Classe, y=Nb)) +
    geom_bar(stat = 'identity', fill="grey") +
    labs(y="N/ha") + theme_bw()

  out <- list(arbres, histo)
  names(out) <- c("arbres", "histo")
  return(out)

}


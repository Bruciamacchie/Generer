#' Calcul placettes PCQM
#' @description Calcul de la surface terrière et de la densité à partir n placettes PCQM.
#'
#' @return Surface terrière et densité moyennes.
#'
#' @param tab = tableau
#'
#' @details Le tableau en entrée doit contenir 4 colonnes dénommées "NumPlac","Quart","rayon","Diam".
#'
#' @author Bruciamacchie Max
#'
#' @import sf
#' @import tidyverse
#'
#' @examples
#' data(perim)
#' res <- Generer::GenererPlaPCQM(perim, arbres)
#' res$Estim
#' res$graph
#'
#' @export

CalculPCQM <- function(tab, pla) {
  if (sum(c("NumPlac","Quart","rayon","Diam") %in% names(tab)) == 4) {
    if (class(pla) %in% c("integer", "numeric")) {

      if("geometry" %in% names(tab)) {
        tab <- tab  %>% dplyr::select(-geometry)
      }
      Placettes <- data.frame(NumPlac = pla)
      Quarts    <- data.frame(Quart = 1:4)
      nb             <- dim(Placettes)[1]
      listPlacEch    <- unique(tab$NumPlac)
      NbPlacEch      <- length(listPlacEch)

      tabComplet <- merge(Placettes, Quarts, by=NULL) %>%
        arrange(NumPlac) %>%
        left_join(tab, by = c("NumPlac", "Quart"))

      tabCor <- tabComplet %>%
        mutate(rayon = ifelse(is.na(rayon),
                              quantile(arbresEch$rayon, probs=0.95),
                              rayon))
      # ------- méthode par placette
      t1 <- tabCor %>%
        group_by(NumPlac) %>%
        mutate(Poids = 10000*3/pi/sum(rayon^2),
               Gha = G*Poids) %>%
        summarise(nb = n(),
                  Nha = sum(Poids),
                  Gha = sum(Gha, na.rm=T))
      # ------- méthode globale
      data(CoefftPCQM)
      vides <- nb*4 - dim(tab)[1]
      if (vides >0) {
        Pourc <- floor(vides/nb/4*100)
        Coef <- as.numeric(CoefftPCQM[Pourc, 2])
      } else{Coef=1}

      nha <- 10000*4*(4*nb-1)/pi/sum(tab$rayon^2)*Coef
      gmoy <- mean(tab$G)
      t2 <- tibble(Nha = nha,
                   Gha = Nha*gmoy)

      # ------- Sortie
      out = list(t1, t2)
      names(out) <- c("PCQMpla", "PCQMglob")
      return(out)

    } else {print("La liste des placettes doit être fournie sous forme d'integer ou numeric.")}

  } else {print("Le tableau en entrée doit contenir 4 colonnes dénommées NumPlac, Quart, rayon, Diam.")}
}

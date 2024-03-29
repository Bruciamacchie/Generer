% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GenererPlaAngle.R
\name{GenererPlaAngle}
\alias{GenererPlaAngle}
\title{Fonction permettant de générer des placettes à angle fixe}
\usage{
GenererPlaAngle(shp, arbres, alpha = 0.02, NbPlac = 10, type = "regular")
}
\arguments{
\item{shp}{= Périmètre sous forme d'un objet sf}

\item{arbres}{= fichier obtenu avec la fonction GenererArbres.}

\item{alpha}{= angle retenu. Par défaut alpha = 0.02.}

\item{NbPlac}{= nombre de placettes. Par défault NbPlac = 10.}

\item{type}{= type de répartition. Par défault type = "regular".
Le type peut également être "random" ou "hexagonal".
Dans le cas du type "random" le nombre exact de placettes sera tiré au hasard.
Pour les autres types, le nombre de placettes sera approché.}
}
\value{
Une liste da placettes sous forme d'un objet sf.
}
\description{
Fonction permettant de générer n placettes à angle fixe sur un périmètre.
Leur répartition peut être régulière ou aléatoire
}
\examples{
data(perim)
res <- Generer::GenererPlaAngle(perim, arbres)
res$Estim
res$graph

}
\author{
Bruciamacchie Max
}

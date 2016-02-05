\name{WikipediaR-package}
\alias{WikipediaR-package}
\alias{WikipediaR}
\docType{package}
\title{
R-Based Wikipedia Client
}
\description{

Provides an interface to the Wikipedia web API.
 
Three functions provide details for a specific Wikipedia page ; the \code{links} function lists all links that are present, the \code{backLinks} function lists all pages that link to, the \code{contribs} function lists all the contributions (revisions for main pages, and discussions for talk pages). 
The page can be defined by the parameter "page", as the title, i.e. a character string, or the page ID, a numeric value. The character string title can include spaces and special characters, and lower/upper case letters are taking in account.
 
Two functions provides details for a specific user ; the \code{userContribs} function lists all contributions, and the \code{userInfo} function provides general information (as name, gender, rights or groups).
The user is defined by his or her name. Lower/upper case letters are taking in account.
 
The domain can be specified in all the functions, by the parameter "domain". the default domain is "en" for the \url{https://en.wikipedia.org}.
 
It provides additional information compared to others packages, as WikipediR. It does not need login.
 
The multiplex network that can be constructed from the results of the functions of WikipediaR can be modeled as Stochastic Block Model as in  Barbillon P. et al.
}
\details{
\tabular{ll}{
Package: \tab WikipediaR\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2016-01-25\cr
License: \tab GPL (>2)\cr
Depends: \tab R (>= 2.10.0), XML (>=2.6.3), httr (>=1.0.0)\cr
URL: \tab http;//www.r-project.org\cr
Repository: \tab CRAN\cr
Packaged: \tab 2016-02-05 09:57:22 UTC; Louise\cr
Built: \tab R 3.2.3; ; 2016-02-05 09:57:22 UTC; windows\cr
}

As part of a PRES Sorbonne Paris Cite project, Paris Descartes statisticians, computer scientists and sociologists Paris Diderot Sciences Po are working on the problem of multi-level networks. One part of the project is to analyze data extracted from Wikipedia with the free software R.

Like the \code{twitteR} package that provides an interface to the Twitter web API, the objective of the \code{WikipediaR} package is to provide a way to access to data extracted from Wikipedia, and return it in a exploitable format in R. API means application programming interface.

A package already exists with a similar objective : \code{WikipediR}. This package is under progress, and as it permits modifications on the Wikipedia base, it needs a connexion, with rights. To see more details about this package, you can visit \url{http://ironholds.org/blog/introducing-wikipedir/}.

Our package use the \code{XML} package to interact with wikipedia, via MediaWiki API syntax. You can see this syntax here: \url{http://en.wikipedia.org/w/api.php}.

Which others packages interact with MediaWiki API ?
The Tiki Wiki CMS/Groupware framework has an R plugin ( \code{PluginR} ) to run R code from wiki pages, and use data from their own collected web databases (trackers). A demo: \url{http://r.tiki.org}

The \code{wikibooks} package provides functions and datasets of the german WikiBook "GNU R".


Remark1: "fr" and "en" domains have been tested, but others can lead to not anticipated problem. Trying domain="gu" is at your peril... The encoding is UTF-8 for most of the output.

Remark2: as the functions get informations on internet in real time, the execution time depends on your internet connection !

Index:
\preformatted{
backLinks               lists pages that link to the Wikipedia page
contribs                lists contributions for a specific wikipedia page
links                   links on a Wikipedia page
testWikiPage            internal function testWikiPage
testWikiUser            internal function testWikiPage
userContribs            list of contributions for a specific user
userInfo                General information for a Wikipedia user
}

}
\author{
Avner Bar-Hen <Avner.Bar-Hen@mi.parisdescartes.fr>, with contributions of Louise Baschet, Francois-Xavier Jollois, Jeremie Riou

Maintainer: Avner Bar-Hen <Avner.Bar-Hen@mi.parisdescartes.fr>
}
\references{Barbillon P., Donnet, S., Lazega E., and Bar-Hen A. : Stochastic Block Models for Multiplex networks: an application to networks of researchers, ArXiv 1501.06444, \url{http://arxiv.org/abs/1501.06444}.}

\keyword{ package }
\seealso{

\code{\link[WikipediR:WikipediR-package]{WikipediR}}
}
\examples{
 \dontrun{
## wikipedia links and external links that are present 
links("Louis Pasteur") # default domain : en
## if the specified page needs to be redirected
links(page ="Structure baschet",domain = "fr")
# warning message

## wikipedia pages that link to the page about Luxor Temple
bl.Luxor <- backLinks(domain ="en", page = "Luxor Temple")
# in how many main pages and discussions this page is linked ?
table(bl.Luxor$backLinks$nscat)

## contributions on a page specified by its page ID
contrib.5636 <- contribs(5636, domain = "en")
# get the last non anonymous contributor
last.contrib.5636 <- contrib.5636$contribs[which(contrib.5636$contribs$userid!=0),]$user[1]
last.contrib.5636

## number of contributions for this user
nrow(userContribs(user.name = last.contrib.5636, domain = "en")$contribs)
## and informations about this user
userInfo(user.name = last.contrib.5636, domain = "en",
  usprop="groups|rights|editcount|gender" )
  }
}

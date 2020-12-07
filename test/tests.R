# Page : NoSQL | id : 23968131
# Page: Wikipedia | id : 5043734

source("backLinks.R")
res_backLinks1 = backLinks("NoSQL")
dim(res_backLinks1$backLinks)
res_backLinks2 = backLinks(23968131)
dim(res_backLinks2$backLinks)

source("contribs.R")
res_contribs1 = contribs("NoSQL")
dim(res_contribs1$contribs)
res_contribs2 = contribs(23968131)
dim(res_contribs2$contribs)

source("links.R")
res_links1 = links("Wikipedia")
dim(res_links1$links)
length(res_links1$extlinks)
res_links2 = links(5043734)
dim(res_links2$links)
length(res_links2$extlinks)

source("userContribs.R")
res2 = userContribs("Philip_Cross")
dim(res2$contribs)


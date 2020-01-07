## ---- echo=FALSE, eval=TRUE, fig.align="center"-------------------------------
library(visNetwork)
nodes <- data.frame(id = c("W", "A", "Y"))
nodes$label <- nodes$id
edges <- data.frame(from = c("W", "W", "A"), to = c("A", "Y", "Y"))
network <- visNetwork(nodes, edges, height = "300px", width = "200px") %>%
  visEdges(arrows = list(to = TRUE)) %>%
  visLayout(randomSeed = 25)
network


## ----cv_fig, fig.show="hold"--------------------------------------------------
knitr::include_graphics("img/misc/vs.pdf")


## ----cv_fig2, fig.show="hold"-------------------------------------------------
knitr::include_graphics("img/misc/SLKaiserNew.pdf")


## ----cv_fig3, fig.show="hold"-------------------------------------------------
knitr::include_graphics("img/misc/ericSL.pdf")


## ----cv_fig4, fig.show="hold"-------------------------------------------------
knitr::include_graphics("img/misc/TMLEimage.pdf")


## ----load_washb_data_intro, message=FALSE, warning=FALSE----------------------
library(tidyverse)

# read in data
dat <- read_csv("https://raw.githubusercontent.com/tlverse/tlverse-data/master/wash-benefits/washb_data.csv")
dat


## ----skim_washb_data, message=FALSE, warning=FALSE----------------------------
library(skimr)
skim(dat)


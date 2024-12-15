library(tidyverse)
library(magrittr)
library(farver)
library(colorspace)
library(DescTools)

hsv_scaled <- readRDS("data/hsv_scaled.rds") %>%
  filter(v>=.8)

base_colours <- hsv(0,0,0:1)
newcol <- NULL
colopts <- NULL
tdif <- 30

range(hsv_scaled$di)
d <- 0
while (d <= 100) {
  a <- hsv_scaled %>%
    filter(d==di)
  if (nrow(a)>0) {
    
    collist <- c(base_colours,newcol,a$col)
    dif_base <-  collist %>%
      decode_colour() %>%
      compare_colour(from_space="rgb",method=c("cie2000","cie94")[2])
    # dif_deut <- collist %>%
    #   deutan() %>%
    #   decode_colour() %>%
    #   compare_colour(from_space="rgb",method=c("cie2000","cie94")[2])
    # dif_prot <- collist %>%
    #   protan() %>%
    #   decode_colour() %>%
    #   compare_colour(from_space="rgb",method=c("cie2000","cie94")[2])
    # dif_trit <- collist %>%
    #   tritan() %>%
    #   decode_colour() %>%
    #   compare_colour(from_space="rgb",method=c("cie2000","cie94")[2])
    # dif_grey <- collist %>%
    #   ColToGrey() %>%
    #   decode_colour() %>%
    #   compare_colour(from_space="rgb",method=c("cie2000","cie94")[2])
    # dif <- pmin(dif_base,dif_deut,dif_prot,dif_trit,dif_grey)
    dif <- dif_base
    
    dif <- dif+t(dif)
    diag(dif) <- 100
    
    nc <- length(c(base_colours,newcol))
    colopts <- dif[-(1:nc),1:nc,drop=FALSE] %>%
      apply(1,min) %>%
      data.frame(mindiff=.,i=1:nrow(a)) %>%
      filter(mindiff>=tdif) %>%
      # filter(mindiff==min(mindiff)) %>%
      filter(mindiff<=min(mindiff)*1.2) %>%
      pull(i)
    
    if (length(colopts)>0) {
      nc <- a[colopts,] %>%
        arrange(-s) %>%
        pull(col) %>%
        {.[1]}
      newcol <- c(newcol,nc) 
    } else {
      d <- d + 1
    }
    colopts <- NULL
  } else {d <- d + 1}
  print(d)
}

colnom <- newcol
names(colnom) <- newcol
expand.grid(ii=1:length(newcol),t=1:5) %>%
  left_join(data.frame(ii=1:length(newcol),col=newcol)) %>%
  mutate(y=runif(n())) %>%
  ggplot(aes(x=t,y=y,col=col)) +
  geom_line(show.legend=FALSE,size=2) +
  scale_colour_manual(values=colnom) +
  labs(title=length(newcol))



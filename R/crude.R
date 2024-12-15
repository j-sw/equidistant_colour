library(tidyverse)
library(magrittr)
library(farver)
library(colorspace)
library(DescTools)

base_colours <- hsv(0,0,0:1)
newcol <- NULL
colopts <- NULL
tdif <- 20

vistep <- .05
vistart <- .6

vi <- vistart

while (vi < 1) {
n = 30
a = expand.grid(h=seq(0,1,length.out=n+1)[-1],s=seq(0,1,length.out=n),v=vi) %>%
  mutate(col=hsv(h,s,v))

collist <- c(base_colours,newcol,a$col)
dif_base <-  collist %>%
  decode_colour() %>%
  compare_colour(from_space="rgb",method=c("cie2000","cie94")[2])
dif_deut <- collist %>%
  deutan() %>%
  decode_colour() %>%
  compare_colour(from_space="rgb",method=c("cie2000","cie94")[2])
dif_prot <- collist %>%
  protan() %>%
  decode_colour() %>%
  compare_colour(from_space="rgb",method=c("cie2000","cie94")[2])
dif_trit <- collist %>%
  tritan() %>%
  decode_colour() %>%
  compare_colour(from_space="rgb",method=c("cie2000","cie94")[2])
dif_grey <- collist %>%
  ColToGrey() %>%
  decode_colour() %>%
  compare_colour(from_space="rgb",method=c("cie2000","cie94")[2])
dif <- pmin(dif_base,dif_deut,dif_prot,dif_trit,dif_grey)
# dif <- dif_base

dif <- dif+t(dif)
diag(dif) <- 100

nc <- length(c(base_colours,newcol))
colopts <- dif[-(1:nc),1:nc] %>%
  apply(1,min) %>%
  data.frame(mindiff=.,i=1:n^2) %>%
  filter(mindiff>=tdif) %>%
  filter(mindiff==min(mindiff)) %>%
  pull(i)

if (length(colopts)>0) {
  newcol <- c(newcol,a$col[colopts[1]]) 
  } else {
    vi <- vi + vistep
  }
  colopts <- NULL
  print(vi)
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



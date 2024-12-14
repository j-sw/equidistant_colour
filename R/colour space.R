library(tidyverse)
library(magrittr)
library(farver)
library(colorspace)

seqdiff <- function(a) {
  n <- length(a)
  dif <- compare_colour(decode_colour(a),from_space="rgb",method=c("cie2000","cie94")[2])
  diag(dif[1:(length(a)-1),2:length(a)])
}

# Spacing of hue
hn <- 100
hvd <- NULL
hs <- seq(0,1,length.out=hn)
for (vi in seq(.05,1,.01)) {
  di <- hsv(h=hs,s=1,v=vi) %>%
    seqdiff() %>%
    cumsum() %>%
    {c(0,./max(.))} %>%
    data.frame(vi,hs,d=.)
  hvd <- bind_rows(hvd,di)
}
  
hvd %>%
  filter(abs(vi-.6)<.01) %>%
  ggplot(aes(x=hs,y=d,group=vi)) +
  geom_line()

# Spacing of v
vn <- 100
vvd <- NULL
vs <- seq(0,1,length.out=hn)
for (hi in seq(.05,1,.01)) {
  di <- hsv(h=hi,s=1,v=vs) %>%
    seqdiff() %>%
    cumsum() %>%
    # {c(0,./max(.))} %>%
    {c(0,.)} %>%
    data.frame(vs,hi,d=.)
  vvd <- bind_rows(vvd,di)
}

vvd %>%
  # filter(abs(vi-.6)<.01) %>%
  ggplot(aes(x=vs,y=d,group=hi,col=hi)) +
  geom_line()

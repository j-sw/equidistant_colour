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
hn <- 101
hvd <- NULL
hs <- seq(0,1,length.out=hn)
for (vi in seq(.05,1,.01)) {
  di <- hsv(h=hs,s=.8,v=vi) %>%
    seqdiff() %>%
    cumsum() %>%
    {c(0,./max(.))} %>%
    data.frame(vi,hs,d=.)
  hvd <- bind_rows(hvd,di)
}
  
hvd %>%
  # filter(abs(vi-.6)<.01) %>%
  ggplot(aes(x=hs,y=d,group=vi)) +
  geom_line()

# Sequence of hues that is more evenly spread perceptually - depends on saturation chosen previously also, higher saturation shows larger diffs
hout <- seq(0,1,length.out=21)
hueseq_adj <- hvd %>%
  group_by(hs) %>%
  summarise(d=mean(d)) %$%
  approx(x=d,y=hs,xout=hout)$y

hsv(hs,s=.8,v=.8) %>%
  seqdiff() %>%
  quantile()
hsv(hueseq_adj,s=.8,v=.8) %>%
  seqdiff() %>%
  quantile()

# Spacing of v
vn <- 100
vvd <- NULL
vs <- seq(0,1,length.out=vn)
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

# Spacing of s
sn <- 100
svd <- NULL
ss <- seq(0,1,length.out=sn)
for (hi in seq(.05,1,.01)) {
  di <- hsv(h=hi,s=ss,v=.5) %>%
    seqdiff() %>%
    cumsum() %>%
    # {c(0,./max(.))} %>%
    {c(0,.)} %>%
    data.frame(h=hi,s=ss,d=.)
  svd <- bind_rows(svd,di)
}

svd %>%
  # filter(abs(vi-.6)<.01) %>%
  ggplot(aes(x=s,y=d,group=h,col=h)) +
  geom_line()

# Scale along V for different combinations of hue and sat
hsv_scaled <- NULL
vs <- seq(0,1,length.out=21)
di <- seq(0,100,length.out=101)
for (hi in hueseq_adj) {
  for (si in seq(0,1,length.out=21)) {
    vd <- hsv(hi,si,vs) %>%
      seqdiff() %>%
      cumsum() %>%
      {c(0,.)}
    newscaled <- approx(x=vd,y=vs,xout=di)$y %>%
      data.frame(h=hi,s=si,v=.,di=di)
    hsv_scaled <- bind_rows(hsv_scaled,newscaled)
  }
  print(hi)
}
hsv_fin <- hsv_scaled %>%
  filter(!is.na(v)) %>%
  mutate(col=hsv(h,s,v)) %>%
  filter(h!=1) 

hsv_fin %>% # drop duplicate
  saveRDS("data/hsv_scaled.rds")




library(tidyverse)
library(magrittr)
library(farver)
library(colorspace)

acs <- acses_data(2024) %>%
  filter(source=="SIP")


pp <- acs %>%
  group_by(profession_of_post) %>%
  summarise(n=n())

pp %>%
  ggplot(aes(x=profession_of_post,y=n,fill=profession_of_post)) +
  geom_col() +
  coord_flip() +
  scale_fill_discrete_af("main6")

afcol = decode_colour(af_colour_palettes$main6)
afnom = make.names(rownames(afcol))

d = compare_colour(afcol,from_space="rgb",method="cie2000") # regular vision
dd = compare_colour(deutan(afcol),from_space="rgb",method="cie2000") # deutan
dp = compare_colour(protan(afcol),from_space="rgb",method="cie2000") # protan
dt = compare_colour(tritan(afcol),from_space="rgb",method="cie2000") # tritan

difs <- map2(list(d,dd,dp,dt),c("reg","deut","prot","trit"),~{
  .x %>%
    {data.frame(.,fromcol=rownames(.))} %>%
    pivot_longer(any_of(afnom),names_to="tocol") %>%
    mutate(type=.y)
}) %>%
  bind_rows()

difs %>%
  filter(value>0) %>%
  mutate(fromcol=make.names(fromcol)) %>%
  group_by(fromcol,tocol) %>%
  summarise(n=n(),mindif=min(value)) %>%
  arrange(mindif)

hv_ratio=1
n = 10
satu=.6

colspiral <- function(hv_ratio=1,n=10,satu=.6) hsv(h=(seq(0,1,length.out=n)*hv_ratio)%%1,s=satu,v=seq(0,1,length.out=n))
seqdiff <- function(a,n) compare_colour(decode_colour(a),from_space="rgb",method=c("cie2000","cie94")[2])[1:(n-1),2:n]

hseq = (seq(0,1,length.out=n)*hv_ratio)
vseq = seq(0,1,length.out=n)^.5
cols = hsv(h=hseq%%1,s=satu,v=vseq)
hdifs <- basedifs <- seqdiff(cols,n) %>%
  diag()
for (i in 1:10) {
  hdifcum <- cumsum(hdifs)
  hseq <- approx(x=c(0,hdifcum),y=hseq,xout=seq(0,max(hdifcum),length.out=n))$y
  cols = hsv(h=hseq%%1,s=satu,v=vseq)
  hdifs <- seqdiff(cols,n) %>%
    diag()
}
data.frame(basedifs,hdifs)

pp %>%
  arrange(-n) %>%
  {.[1:10,]} %>%
  ggplot(aes(x=profession_of_post,y=n,fill=profession_of_post)) +
  geom_col() +
  coord_flip() +
  scale_fill_discrete(type=cols)
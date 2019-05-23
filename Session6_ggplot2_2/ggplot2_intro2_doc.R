

## ------------------------------------------------------------------------
library(tidyverse)
library(RColorBrewer) # to create color scales
library(ggrepel)      # to crate nice labels for gene names
library(scales)       # to look at color we created


## ------------------------------------------------------------------------
genes.Spleen.Xray <- read_csv("data/results_miceSpleen_xray.csv")
head(genes.Spleen.Xray)


## ------------------------------------------------------------------------
pdat <- genes.Spleen.Xray %>%
  mutate(logFDR = -log10(FDR),
         threshold = ifelse(FDR < 0.1 & abs(logFC) > .4, TRUE, FALSE) )

#subset top 10 genes to add labels later to our plot
data.label <- pdat %>%
  filter(threshold == TRUE) %>%
  top_n(n = 10, wt = logFDR)
data.label


## ------------------------------------------------------------------------
?pch


## ------------------------------------------------------------------------
vp <- ggplot(pdat, aes(x = logFC, y = logFDR)) +
  geom_point(data = filter(pdat, threshold == FALSE),
             size=.35, alpha = .55, colour = "grey80") +
  geom_point(data = filter(pdat, threshold == TRUE),
             aes(size = logFDR, fill=logFDR),
             pch = 21, stroke = .25, alpha = .65, color = "grey59")
vp


## ------------------------------------------------------------------------
?brewer_pal
brewer.pal(6, "PuRd")
display.brewer.pal(6, "PuRd")


myPallete <- colorRampPalette(brewer.pal(8,"PuRd"))
myPallete(10)

show_col(myPallete(10))
show_col(myPallete(100), labels = FALSE)


## ------------------------------------------------------------------------
vp <- vp +
  scale_fill_gradientn(guide="colourbar", colours = myPallete(100)) +
  guides(size=FALSE,
         color=FALSE,
         fill=guide_colourbar(title = expression(-log[10](FDR)))) +
  labs(x = "Fold Change",
       y = expression(-log[10](FDR))) +
  geom_hline(yintercept=1, color="black", size=1) +
  annotate(geom = "text", x=-6, y=0.75, label="italic(FDR) == 0.1",parse=TRUE) +
    ggtitle("A Volcano Plot!")

vp


## ------------------------------------------------------------------------
vp <- vp  +
  geom_label_repel(data = data.label,
                   aes(label = GENE), alpha = .95,
                   size = 4, color = "black",
                   box.padding = 0.5, segment.size = .5,
                   arrow = arrow(length = unit(0.015, 'npc')))


vp


## ------------------------------------------------------------------------
vp <- vp  +
  theme_minimal() +
  theme(plot.title = element_text(lineheight = 1,face = "bold",size = 20),
           axis.text = element_text(size = 12),
           axis.title = element_text(size = 16, face = "bold"),
           legend.text=element_text(size=13),
           legend.position="bottom",
           legend.title = element_text(size = 12,face = "bold"),
           legend.key.size = unit(0.035, "npc"),
           strip.text.x = element_text(size = 16, colour = "black"),
           strip.text.y = element_text(size = 16, colour = "black"))

vp




## ------------------------------------------------------------------------
?mpg
head(mpg,3)
mpg$class[1:20]


## ------------------------------------------------------------------------
ggplot(mpg, aes(manufacturer)) +
  geom_bar() +
    theme(axis.text.x = element_text(size = 10, angle = 45))


## ------------------------------------------------------------------------
ggplot(mpg, aes(manufacturer)) +
 geom_bar(aes(fill = class)) +
  theme(axis.text.x = element_text(size = 10, angle = 45))


## ------------------------------------------------------------------------
ggplot(mpg, aes(manufacturer)) +
 geom_bar(aes(fill = class), position = "dodge")

ggplot(mpg, aes(manufacturer)) +
  geom_bar(aes(fill = class),position = position_dodge(width = 0.4))


## ------------------------------------------------------------------------
?stat_summary_bin

ggplot(mpg, aes(class)) +
  stat_summary(aes(y = displ), fun.y = "mean", geom = "bar")



## ------------------------------------------------------------------------
df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
df


## ------------------------------------------------------------------------
ggplot(df, aes(trt, outcome)) +
  geom_col()


## ------------------------------------------------------------------------
ggplot(df, aes(trt, outcome)) +
  geom_col(fill="steelblue")



## ------------------------------------------------------------------------
ggplot(df, aes(trt, outcome)) +
  geom_col(aes(fill=trt))


## ------------------------------------------------------------------------
mpg %>%
  group_by(class) %>%
  summarise(cty_mean = mean(cty)) %>%
  ungroup() %>%
  ggplot(aes(class, cty_mean)) +
  geom_col()



## ------------------------------------------------------------------------
ggplot(mpg, aes(displ)) + geom_bar()


## ------------------------------------------------------------------------
ggplot(iris, aes(Sepal.Length)) + geom_histogram(binwidth = 0.2)


## ------------------------------------------------------------------------
ggplot(iris, aes(Sepal.Length, fill=Species)) +
  geom_histogram(binwidth = 0.5)


## ------------------------------------------------------------------------
ggplot(iris, aes(Sepal.Length, fill=Species)) +
  geom_histogram(binwidth = 0.5, alpha=0.4)


## ------------------------------------------------------------------------
ggplot(iris, aes(Sepal.Length, fill=Species)) +
  geom_histogram(binwidth = 0.5, position = "identity", alpha=0.4)


## ------------------------------------------------------------------------
head(ToothGrowth)
?ToothGrowth


## ------------------------------------------------------------------------
gb <- ToothGrowth %>%
      unite(regimen, -len, sep= "_") %>%
      ggplot(data=., aes(regimen, len)) +
      geom_boxplot(aes(color=regimen))


## ------------------------------------------------------------------------
tg.stats <- ToothGrowth %>%
  group_by(supp, dose) %>% # group by
  summarise(.,
            mean.len = mean(len),  # mean
            sd.len = sd(len),      # sd
            n = length(len)        # count
            ) %>% 
  ungroup() %>%
  mutate(se.len = sd.len/sqrt(n))



## ------------------------------------------------------------------------
tg.stats


## ------------------------------------------------------------------------
ggplot(tg.stats, aes(x=dose, y=mean.len, colour=supp)) +
    geom_point(aes(x=dose, y= mean.len), size=4, shape=18)


## ------------------------------------------------------------------------
ggplot(tg.stats, aes(x=dose, y=mean.len, colour=supp)) +
  geom_point(aes(x=dose, y= mean.len), size=4, shape=18) +
  geom_errorbar(aes(ymin=mean.len-sd.len, ymax=mean.len+sd.len), width=.1)


## ------------------------------------------------------------------------
ggplot(tg.stats, aes(x=dose, y=mean.len, colour=supp)) +
  geom_point(aes(x=dose, y= mean.len), size=4, shape=18) +
  geom_errorbar(aes(ymin=mean.len-sd.len, ymax=mean.len+sd.len), width=.1) +
  geom_line()


## ------------------------------------------------------------------------
ggplot(tg.stats, aes(x=dose, y=mean.len, colour=supp)) +
  geom_point(aes(x=dose, y= mean.len), size=4, shape=18) +
  geom_errorbar(aes(ymin=mean.len-sd.len, ymax=mean.len+sd.len), width=.1) +
  geom_line() +
  # here I add the original data points!
  geom_point(data=ToothGrowth, aes(x=dose, y=len, fill=supp), alpha=0.4, size=1.5)


## ------------------------------------------------------------------------
ggplot(tg.stats, aes(x=dose, y=mean.len, colour=supp)) +
  geom_point(aes(x=dose, y= mean.len), size=4, shape=18) +
  geom_errorbar(aes(ymin=mean.len-sd.len, ymax=mean.len+sd.len), width=.1) +
  geom_line() +
  geom_point(data=ToothGrowth, aes(x=dose, y=len, fill=supp), alpha=0.3, size=1.5) +

# Here I add the titles I want
  labs(x="Supplement Dose",
       y="Tooth Length",
       title= "The Effect of Vitamin C on Tooth Growth in Guinea Pigs",
       subtitle= "Comparing delivery methods: Orange Juice vs Ascorbic Acid",
       caption = "Based on the data from R")


## ------------------------------------------------------------------------
ggplot(tg.stats, aes(x=dose, y=mean.len, colour=supp)) +
  geom_point(aes(x=dose, y= mean.len), size=4, shape=18) +
  geom_errorbar(aes(ymin=mean.len-sd.len, ymax=mean.len+sd.len), width=.1) +
  geom_line() +
  geom_point(data=ToothGrowth, aes(x=dose, y=len, fill=supp), alpha=0.3, size=1.5) +

# Here I add the titles I want
  labs(x="Supplement Dose",
       y="Tooth Length",
       title= "The Effect of Vitamin C on Tooth Growth in Guinea Pigs",
       subtitle= "Comparing delivery methods: Orange Juice vs Ascorbic Acid",
       caption = "Based on the data from R") +

# Change x and y axes limits
  xlim(0, 3) +
  ylim(0, 60)


## ------------------------------------------------------------------------
ggplot(tg.stats, aes(x=dose, y=mean.len, colour=supp)) +
  geom_point(aes(x=dose, y= mean.len), size=4, shape=18) +
  geom_errorbar(aes(ymin=mean.len-sd.len, ymax=mean.len+sd.len), width=.1) +
  geom_line() +
  geom_point(data=ToothGrowth, aes(x=dose, y=len, fill=supp), alpha=0.3, size=1.5) +

# Here I add the titles I want
  labs(x="Supplement Dose",
       y="Tooth Length",
       title= "The Effect of Vitamin C on Tooth Growth in Guinea Pigs",
       subtitle= "Comparing delivery methods: Orange Juice vs Ascorbic Acid",
       caption = "Based on the data from R") +

# Change the legend
  guides(colour=guide_legend("Supplement"), fill=guide_legend("Supplement"))


## ------------------------------------------------------------------------
pd <- position_dodge(0.1)

# my plot is getting very long, I'll assign it
tgp <- ggplot(tg.stats, aes(x=dose, y=mean.len, colour=supp)) +
  geom_point(aes(x=dose, y= mean.len), size=4, shape=18, position=pd) +
  geom_errorbar(aes(ymin=mean.len-sd.len, ymax=mean.len+sd.len), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(data=ToothGrowth, aes(x=dose, y=len, fill=supp), alpha=0.3, size=1.5, position=pd) +


# Here I add the titles I want
  labs(x="Supplement Dose",
       y="Tooth Length",
       title= "The Effect of Vitamin C on Tooth Growth in Guinea Pigs",
       subtitle= "Comparing delivery methods: Orange Juice vs Ascorbic Acid",
       caption = "Based on the data from R") +

# Change the legend
  guides(colour=guide_legend("Supplement"), fill=guide_legend("Supplement"))

tgp


## ------------------------------------------------------------------------
euStock <- read_csv("data/EUstock_1992-1997.csv")
head(euStock)


## ------------------------------------------------------------------------
euStock <- euStock %>%
  gather(Stock, Closing, -date) 
head(euStock)


## ------------------------------------------------------------------------
p <- euStock %>%
ggplot(aes(x=date, y=Closing, color = Stock)) +
  geom_line()
p


## ------------------------------------------------------------------------
p+scale_x_date(date_labels = "%b")
p+scale_x_date(date_labels = "%Y %b %d")
p+scale_x_date(date_labels = "%m-%Y")


## ------------------------------------------------------------------------
p<- p + theme(axis.text.x=element_text(angle=60, hjust=1, size=8))

p + scale_x_date(date_breaks = "1 month", date_labels = "%b")
p + scale_x_date(date_breaks = "3 month", date_labels = "%b %y")
p + scale_x_date(date_breaks = "3 month", date_labels = "%b %y",
                 date_minor_breaks = "2 week") 


## ---- warning=FALSE------------------------------------------------------
p + scale_x_date(limit=c(as.Date("1994-01-01"),as.Date("1996-01-31")))


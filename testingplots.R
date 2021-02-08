#load packages

library(shiny)
library(ggplot2)
library(here)
library(grid)
library(gridExtra)
library(ggpubr)
library(jpeg)
library(ggimage)


# Download and read sample image (readJPEG doesn't work with urls)
url <- "http://mathworld.wolfram.com/images/gifs/rabbduck.jpg"
download.file(url, destfile = "rabbduck.jpg")
img <- "rabbduck.jpg"

img3 = "http://phylopic.org/assets/images/submissions/bf5fe2c5-1247-4ed9-93e2-d5af255ec462.512.png"

img2 = "background2.png"

img2 <- readJPEG("rabbduck.jpg")

birdman <- ggplot(iris, aes(Species, Sepal.Length)) +
  background_image(img) +
  geom_boxplot(aes(fill = Species))

birdman2 <- ggplot(iris, aes(Species, Sepal.Length)) +
  geom_boxplot(aes(fill = Species))


birdman3 <- ggbackground(birdman2, img2)

birdman3


theme_meg <- function () {
  theme_bw(base_family="Times") %+replace%
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.title=element_blank(), legend.text=element_text(size=9),
          axis.text.x=element_text(size=10, face="bold"),axis.ticks.length.y=unit(-10, "pt"),
          axis.text.y.left=element_text(margin= margin(r=12), size=10, face="bold", color="black"),
          axis.text.y.right = element_blank(),
          plot.margin=grid::unit(c(0,8,0,5),"mm"))
}


reds <- c("blue", "yellow")
g <- rasterGrob(reds, width = unit(1, "npc"), height = unit(1, "npc"),
                interpolate = TRUE)
economics <- economics
plot <- ggplot(data = economics, aes(x = date, y = unemploy)) +
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_line( alpha=1, color = "white", size = 0.5 ) +
  xlab("Years") +
  ylab("Unemployed [thousands]")
plot




fig <- plot_ly(z = volcano, type = "heatmap")

fig

fig2 <- plot_ly()

test <- data.frame(volcano)

plot_ly(type)

d=expand.grid(r=seq(0,1,0.1), g=seq(0,1,0.1), b=seq(0,1,0.1))
d2 <- d
d2 <- d2[-c(1:2)]
d2 <- d2[order(-d2$b),]

d <- cbind(d,d2)
d <- d[-c(3)]

##COLORS IS CURRENTLY THE BEST PLOT
colors<- ggplot() +
  scale_x_continuous(name="red", breaks=seq(0.05, 1.05, 0.16667), labels=seq(6, 9, 0.5)) +
  scale_y_continuous(name="green", breaks=seq(0.05, 1.05, 0.2), labels=seq(0, 1, 0.2)) +
  scale_fill_identity() +
  geom_rect(data=d, mapping=aes(xmin=r, xmax=r+resolution(r), ymin=g, ymax=g+resolution(g),
                                fill=rgb(r,d2,g)), size=0.1, alpha=.1)+
  theme_meg()


colors

colors2<- ggplot() +
  scale_x_continuous(name="red", breaks=seq(0.05, 1.05, 0.2), labels=seq(0, 1, 0.2)) +
  scale_y_continuous(name="green", breaks=seq(0.05, 1.05, 0.2), labels=seq(0, 1, 0.2)) +
  scale_fill_identity() +
  geom_rect(data=d, mapping=aes(xmin=r, xmax=r+resolution(r), ymin=g, ymax=g+resolution(g),
                                fill=rgb(r,d2,g)), size=0.1, alpha=.1)+
  theme_meg()


colors2

#try out a basic plot

plot <- ggplot()+
  geom_point(aes(x=7, y=100), shape=23, size=3, fill="blue")+
  scale_y_continuous(limits=c(0,150),breaks = seq(0, 150, 25))+
  scale_x_continuous(limits=c(6,9), breaks=seq(6,9,.5))+
  labs(x="pH", y="DIC (mg C/L)")+
  theme_meg()

plot

#ok now let's get the diffeent boxes on correctly

#top left
plot2 <- plot+
  geom_rect(aes(xmin=-Inf, xmax=7.5, ymin=50, ymax=Inf), fill="orange",
          alpha=.5)+
  geom_text(aes(x=6.5, y=140, label="Potentially High\nCopper Levels", family="Times"))

plot2

#bottom left

plot3 <- plot+
  geom_rect(aes(xmin=-Inf, xmax=7.5, ymin=-Inf, ymax=50), fill="green",
            alpha=.5)

plot3

#top right

plot4 <- plot+
  geom_rect(aes(xmin=7.5, xmax=Inf, ymin=50, ymax=Inf), fill="green",
            alpha=.5)

plot4

#bottom right

plot5 <- plot+
  geom_rect(aes(xmin=7.5, xmax=Inf, ymin=-Inf, ymax=50), fill="orange",
            alpha=.5)+
  geom_text(aes(x=8.5, y=10, label="Copper Pitting Likely", family="Times"))

plot5


#all together

plot6 <- plot+
  geom_rect(aes(xmin=-Inf, xmax=7.5, ymin=50, ymax=Inf), fill="orange",
            alpha=.5)+
  geom_text(aes(x=6.5, y=140, label="Potentially High\nCopper Levels", family="Times"))+
  geom_rect(aes(xmin=-Inf, xmax=7.5, ymin=-Inf, ymax=50), fill="green",
            alpha=.5)+
  geom_rect(aes(xmin=7.5, xmax=Inf, ymin=50, ymax=Inf), fill="green",
            alpha=.5)+
  geom_rect(aes(xmin=7.5, xmax=Inf, ymin=-Inf, ymax=50), fill="orange",
            alpha=.5)+
  geom_text(aes(x=8.5, y=10, label="Copper Pitting Likely", family="Times"))


plot6

ggsave("example.png", plot = plot6, scale = 3, width=5, height=5, dpi=400,units=c("cm"))
ggsave("example2.png", plot = plot2, scale = 3, width=5, height=5, dpi=400,units=c("cm"))


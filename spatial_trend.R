#required libraries
library(ncdf4)
library(raster)
library(rasterVis)
library(wql)
library(tidyverse)
library(fields)
library(maps)
library(rnaturalearth)
library(lubridate)
library(metR)
library(RColorBrewer)
library(ggpubr)
library(egg)
library(oce)
#get data
bob <- brick('Data/noaa_oisst_v2_merged_1982_2020.nc')

#kel_cel <- function(temp){temp - 273.15 }
#bob_c <- calc(bob,kel_cel)
####~~~~~~~~~~~~~~~~~~~~~Regression~~~~~~~~~
tt <- 1:nlayers(bob)
# run the regression
fun <- function(x) { if (is.na(x[1])){ NA } else {lm(x ~ tt)$coefficients[2] }} 

bob.lm <- calc(bob, fun)
bob.lm.df <- as.data.frame(bob.lm, xy=TRUE) %>% drop_na()
bob.lm.df <- bob.lm.df %>% filter(layer > 0)
bob.lm.df <- bob.lm.df %>% mutate(layer1 = layer*120)
###
mytheme <- rasterTheme(region=oce.colorsJet(50))
windows(width = 5,height = 5)
levelplot(bob.lm,par.settings=mytheme,margin = F)
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~Mann-Kendall~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bob.sen <- calc(bob, function(x) {mannKen(x)$sen.slope})

bob.df <- as.data.frame(bob.sen, xy=TRUE) %>% drop_na()
bob.df <- bob.df %>% filter(layer > 0)
bob.df <- bob.df %>% mutate(layer1 = layer*120)
mycol <- oce::oce.colorsJet(100)
##base map
coast <- ne_countries(scale = "medium", returnclass = "sf")


ggplot(data = bob.df) + metR::geom_contour_fill(aes(x=x,y=y,z=layer1))+ scale_fill_gradientn(name ="°C/decade",colours = rev(brewer.pal(10,"RdBu")),limits=c(0.03945823,0.25694017),breaks= seq(0.03945823,0.25694017,length.out = 6),labels=c(0.04,0.083,0.13,0.17,0.21,0.26),guide = guide_colorbar(nbin = 12,raster = FALSE,ticks = TRUE, ticks.colour = "black",barheight = unit(3.5,units = "in"),barwidth = 0.7,frame.colour = c("black"),frame.linewidth = 1,label.hjust = 0.5,title.vjust = 0.1,title.position = "right",direction = "vertical")) + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+theme_bw()+theme(legend.title = element_text(angle = 90,hjust = 0.5))
ggplot(data = bob.df) + metR::geom_contour_fill(aes(x=x,y=y,z=layer1))+ scale_fill_gradientn(name ="°C/decade",colours = oce::oceColorsJet(100),limits=c(0.03945823,0.25694017),breaks= seq(0.03945823,0.25694017,length.out = 6),labels=c(0.04,0.083,0.13,0.17,0.21,0.26),guide = guide_colorbar(nbin = 12,raster = FALSE,ticks = TRUE, ticks.colour = "black",barheight = unit(3.5,units = "in"),barwidth = 0.7,frame.colour = c("black"),frame.linewidth = 1,label.hjust = 0.5,title.vjust = 0.1,title.position = "right",direction = "vertical")) + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+theme_bw()+theme(legend.title = element_text(angle = 90,hjust = 0.5))


ggsave("sstTrend1981-2019.png",width = 5,height = 5,units = "in", dpi = 300)

###################Seasonal SST trend#############################
#####                   ~~~winter~~~

i <- c(11,12,1,2)

win.sen <- which(layer_names %in% i) 
win.sen <- names(bob)[win.sen]
win.sen <- bob[[win.sen]]

years <- names(win.sen)
years <- gsub('X', '', years)
years <- gsub('\\.', '-', years)
years <- as.Date(years)
years <- year(years)

win.sen <- stackApply(win.sen, years, fun = mean)

win.sen <- calc(win.sen, function(x) {mannKen(x)$sen.slope})

win.sen <- as.data.frame(win.sen, xy=TRUE) %>% drop_na()
colnames(win.sen) <- c("lon","lat","slope")
win.sen <- win.sen %>% mutate(slopee = slope*10)


#####        Spring  
i <- c(3,4,5)
spr.sen <- which(layer_names %in% i) 
spr.sen <- names(bob)[spr.sen]
spr.sen <- bob[[spr.sen]]

years <- names(spr.sen)
years <- gsub('X', '', years)
years <- gsub('\\.', '-', years)
years <- as.Date(years)
years <- year(years)

spr.sen <- stackApply(spr.sen, years, fun = mean)

spr.sen <- calc(spr.sen, function(x) {mannKen(x)$sen.slope})

spr.sen <- as.data.frame(spr.sen, xy=TRUE) %>% drop_na()
colnames(spr.sen) <- c("lon","lat","slope")
spr.sen <- spr.sen %>% mutate(slopee = slope*10)

#####           Summer
i <- c(6,7,8)
sum.sen <- which(layer_names %in% i) 
sum.sen <- names(bob)[sum.sen]
sum.sen <- bob[[sum.sen]]

years <- names(sum.sen)
years <- gsub('X', '', years)
years <- gsub('\\.', '-', years)
years <- as.Date(years)
years <- year(years)

sum.sen <- stackApply(sum.sen, years, fun = mean)

sum.sen <- calc(sum.sen, function(x) {mannKen(x)$sen.slope})

sum.sen <- as.data.frame(sum.sen, xy=TRUE) %>% drop_na()
colnames(sum.sen) <- c("lon","lat","slope")
sum.sen <- sum.sen %>% mutate(slopee = slope*10)

##               FAll

i <- c(9,10)
fall.sen <- which(layer_names %in% i) 
fall.sen <- names(bob)[fall.sen]
fall.sen <- bob[[fall.sen]]

years <- names(fall.sen)
years <- gsub('X', '', years)
years <- gsub('\\.', '-', years)
years <- as.Date(years)
years <- year(years)

fall.sen <- stackApply(fall.sen, years, fun = mean)

fall.sen <- calc(fall.sen, function(x) {mannKen(x)$sen.slope})

fall.sen <- as.data.frame(fall.sen, xy=TRUE) %>% drop_na()
colnames(fall.sen) <- c("lon","lat","slope")
fall.sen <- fall.sen %>% mutate(slopee = slope*10)

###
######################################################
#             plotting
######################################################
#coastline
coast <- ne_countries(scale = "medium", returnclass = "sf")

#range of sst for all seasons
sl_range <- range(lapply(list(win.sen,spr.sen,sum.sen,fall.sen), function(s) s$slopee))
sl_range <- c(-0.4,0.4)

#s1 <- ggplot(data = sum.sen) + geom_contour_fill(aes(x=lon,y=lat,z=slopee)) +scale_fill_gradientn(name ="°C/decade",colours = oce::oce.colorsJet(100),limits = sl_range,breaks = seq(-0.3,0.4,length.out = 8),guide = guide_colorbar(nbin = 15,raster = FALSE,ticks = TRUE, ticks.colour = "black",barheight = 1,barwidth = 15,frame.colour = c("black"),frame.linewidth = 1,label.hjust = 0.5,title.vjust = 0.8,title.position = "left",direction = "horizontal")) + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Winter",size=3)+theme_bw()+theme(legend.title = element_text(angle = ,hjust = 0.5))

s1 <- ggplot(data = sum.sen) + geom_contour_fill(aes(x=lon,y=lat,z=slopee)) +scale_fill_gradientn(name ="°C/decade",colours = ncol,limits = c(-0.4,0.4),breaks = seq(-0.3,0.4,length.out = 8),guide = guide_colorbar(nbin = 15,raster = FALSE,ticks = TRUE, ticks.colour = "black",barheight = 1,barwidth = 15,frame.colour = c("black"),frame.linewidth = 1,label.hjust = 0.5,title.vjust = 0.8,title.position = "left",direction = "horizontal")) + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Winter",size=3)+theme_bw()+theme(legend.title = element_text(angle = ,hjust = 0.5))

ggplot(data = sum.sen) + geom_contour(aes(x=lon,y=lat,z=slopee)) +geom_contour_fill(aes(x=lon,y=lat,z=slopee)) +scale_fill_gradientn(name ="°C/decade",colours = ncol,limits = c(-0.4,0.4),breaks = seq(-0.3,0.4,length.out = 8),guide = guide_colorbar(nbin = 15,raster = FALSE,ticks = TRUE, ticks.colour = "black",barheight = 1,barwidth = 15,frame.colour = c("black"),frame.linewidth = 1,label.hjust = 0.5,title.vjust = 0.8,title.position = "left",direction = "horizontal")) + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Winter",size=3)+theme_bw()+theme(legend.title = element_text(angle = ,hjust = 0.5))

##
ncol <- c("#00007F","#0000C8","#0012FF","#005AFF","#00A3FF","#00ECFF","#36FFC8","#ffffff","#C8FF36","#FFEC00","#FFA300","#FF5A00","#FF1200", "#C80000" ,"#7F0000")
#ncol <- c("#00007F","#0000C8","#0012FF","#005AFF","#00A3FF","#00ECFF","#36FFC8","#ffffff","#2ef207","#FFEC00","#FFA300","#FF5A00","#FF1200", "#C80000" ,"#7F0000")


#get legend

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#save legend
legend <- get_legend(s1)
gridExtra::grid.arrange(legend)

#now plot others season 
s1 <- ggplot(data = win.sen) + geom_contour_fill(aes(x=lon,y=lat,z=slopee)) +scale_fill_gradientn(name ="°C/decade",colours = oce::oce.colorsJet(100),limits = sl_range,breaks = seq(-0.3,0.4,length.out = 8),guide = guide_colorbar(nbin = 15,raster = FALSE,ticks = TRUE, ticks.colour = "black",barheight = 1,barwidth = 15,frame.colour = c("black"),frame.linewidth = 1,label.hjust = 0.5,title.vjust = 0.1,title.position = "top",direction = "horizontal")) + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Winter",size=4)+theme_bw()+theme(legend.title = element_text(angle = 90,hjust = 0.5),plot.margin = margin(5.5,4,4,5.5),axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),legend.position = 'none' )
s2 <-  ggplot(data = spr.sen) + geom_contour_fill(aes(x=lon,y=lat,z=slopee))+ scale_fill_gradientn(name ="°C/decade",colours = oce::oce.colorsJet(100),limits = sl_range,breaks = seq(-0.3,0.4,length.out = 8),guide = guide_colorbar(nbin = 15,raster = FALSE,ticks = TRUE, ticks.colour = "black",barheight = 1,barwidth = 15,frame.colour = c("black"),frame.linewidth = 1,label.hjust = 0.5,title.vjust = 0.1,title.position = "top",direction = "horizontal")) + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Spring",size=4)+theme_bw()+theme(legend.title = element_text(angle = 90,hjust = 0.5),plot.margin = margin(5.5,5.5,4,4),axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),legend.position = 'none')
s3 <-  ggplot(data = sum.sen) + geom_contour_fill(aes(x=lon,y=lat,z=slopee))+ scale_fill_gradientn(name ="°C/decade",colours = oce::oce.colorsJet(100),limits = sl_range,breaks = seq(-0.3,0.4,length.out = 8),guide = guide_colorbar(nbin = 15,raster = FALSE,ticks = TRUE, ticks.colour = "black",barheight = 1,barwidth = 15,frame.colour = c("black"),frame.linewidth = 1,label.hjust = 0.5,title.vjust = 0.1,title.position = "top",direction = "horizontal")) + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Summer",size=4)+theme_bw()+theme(legend.title = element_text(angle = 90,hjust = 0.5),plot.margin = margin(4,4,18.5,5.5),legend.position = 'none')
s4 <-  ggplot(data = fall.sen) + geom_contour_fill(aes(x=lon,y=lat,z=slopee))+ scale_fill_gradientn(name ="°C/decade",colours = oce::oce.colorsJet(100),limits = sl_range,breaks = seq(-0.3,0.4,length.out = 8),guide = guide_colorbar(nbin = 15,raster = FALSE,ticks = TRUE, ticks.colour = "black",barheight = 1,barwidth = 15,frame.colour = c("black"),frame.linewidth = 1,label.hjust = 0.5,title.vjust = 0.1,title.position = "top",direction = "horizontal")) + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Fall",size=4)+theme_bw()+theme(legend.title = element_text(angle = 90,hjust = 0.5),plot.margin = margin(4,5.5,18.5,4),legend.position = 'none',axis.title.y = element_blank(),axis.text.y = element_blank())

s1 <- ggplot(data = win.sen) + geom_contour_fill(aes(x=lon,y=lat,z=slopee)) +scale_fill_gradientn(name ="°C/decade",colours = ncol,limits = sl_range,breaks = seq(-0.3,0.4,length.out = 8),guide = guide_colorbar(nbin = 15,raster = FALSE,ticks = TRUE, ticks.colour = "black",barheight = 1,barwidth = 15,frame.colour = c("black"),frame.linewidth = 1,label.hjust = 0.5,title.vjust = 0.1,title.position = "top",direction = "horizontal")) + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Winter",size=4)+theme_bw()+theme(legend.title = element_text(angle = 90,hjust = 0.5),plot.margin = margin(5.5,4,4,5.5),axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),legend.position = 'none' )
s2 <-  ggplot(data = spr.sen) + geom_contour_fill(aes(x=lon,y=lat,z=slopee))+ scale_fill_gradientn(name ="°C/decade",colours = ncol,limits = sl_range,breaks = seq(-0.3,0.4,length.out = 8),guide = guide_colorbar(nbin = 15,raster = FALSE,ticks = TRUE, ticks.colour = "black",barheight = 1,barwidth = 15,frame.colour = c("black"),frame.linewidth = 1,label.hjust = 0.5,title.vjust = 0.1,title.position = "top",direction = "horizontal")) + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Spring",size=4)+theme_bw()+theme(legend.title = element_text(angle = 90,hjust = 0.5),plot.margin = margin(5.5,5.5,4,4),axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),legend.position = 'none')
s3 <-  ggplot(data = sum.sen) + geom_contour_fill(aes(x=lon,y=lat,z=slopee))+ scale_fill_gradientn(name ="°C/decade",colours = ncol,limits = sl_range,breaks = seq(-0.3,0.4,length.out = 8),guide = guide_colorbar(nbin = 15,raster = FALSE,ticks = TRUE, ticks.colour = "black",barheight = 1,barwidth = 15,frame.colour = c("black"),frame.linewidth = 1,label.hjust = 0.5,title.vjust = 0.1,title.position = "top",direction = "horizontal")) + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Summer",size=4)+theme_bw()+theme(legend.title = element_text(angle = 90,hjust = 0.5),plot.margin = margin(4,4,18.5,5.5),legend.position = 'none')
s4 <-  ggplot(data = fall.sen) + geom_contour_fill(aes(x=lon,y=lat,z=slopee))+ scale_fill_gradientn(name ="°C/decade",colours = ncol,limits = sl_range,breaks = seq(-0.3,0.4,length.out = 8),guide = guide_colorbar(nbin = 15,raster = FALSE,ticks = TRUE, ticks.colour = "black",barheight = 1,barwidth = 15,frame.colour = c("black"),frame.linewidth = 1,label.hjust = 0.5,title.vjust = 0.1,title.position = "top",direction = "horizontal")) + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Fall",size=4)+theme_bw()+theme(legend.title = element_text(angle = 90,hjust = 0.5),plot.margin = margin(4,5.5,18.5,4),legend.position = 'none',axis.title.y = element_blank(),axis.text.y = element_blank())


png('seasonal_TREND_1981-2019.png',width = 5.5,height = 5.5,units = 'in',res = 300)
#windows(width = 10.5,height = 10.5)
ggarrange(s1,s2,s3,s4,ncol = 2) %>% gridExtra::grid.arrange(legend,heights = unit(c(4.5,0.1),'in'))
dev.off()


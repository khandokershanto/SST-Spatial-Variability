#  libraries
library(ncdf4)
library(raster)
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
#

# data
bob <- brick('Data/noaa_oisst_v2_merged_1982_2020.nc')
# Temporal Average of SST from 1982-2020

bob_m <-  calc(bob, fun = mean,na.rm = TRUE)

#data frame
bob_m <- as.data.frame(bob_m, xy=TRUE) %>% drop_na()
colnames(bob_m) <- c('lon','lat','sst')
#bob_m$sst <- bob_m$sst-273.15

range(bob_m$sst)
summary(bob_m$sst)
##base map
coast <- ne_countries(scale = "medium", returnclass = "sf")

#plot
windows(width = 5,height = 5)
ggplot(data = bob_m) + metR::geom_contour_fill(aes(x=lon,y=lat,z=sst))+ scale_fill_gradientn(name ="SST (°C)",colours = rev(brewer.pal(10,"RdBu")),limits=c(26.94570,29.64404),breaks = seq(26,30,0.5),guide = guide_colorbar(nbin = 10,raster = FALSE,ticks = TRUE, ticks.colour = "black",barheight = 18,barwidth = 0.7,frame.colour = c("black"),frame.linewidth = 1,label.hjust = 0.5,title.vjust = 0.1,title.position = "right",direction = "vertical")) + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+theme_bw()+theme(legend.title = element_text(angle = 90,hjust = 0.5))

ggsave("sstClimatology1981-2019.png",width = 5,height = 5,units = "in", dpi = 300)

#------------------------------------------------------------
#                        SEasonal SST
#--------------------------------------------------------------

#######################
layer_names <- names(bob)
layer_names <- gsub('X', '', layer_names)
layer_names <- gsub('\\.', '-', layer_names)
layer_names <- as.Date(layer_names)
layer_names <- month(layer_names)

bob_m <-  stackApply(bob, layer_names, fun = mean)

###############################################################

##                           ~Winter~

winter <- bob_m[[c("index_11","index_12","index_1","index_2")]]

winter <- stackApply(winter,4,fun = mean)

winter <- as.data.frame(winter, xy=TRUE) %>% drop_na()
colnames(winter) <- c("lon","lat","sst")
#winter$sst <- winter$sst-273.15

range(winter$sst)
summary(winter$sst)



##                        ~Spring~

spring <- bob_m[[c("index_3","index_4","index_5")]]

spring <- stackApply(spring,3,fun = mean)

spring <- as.data.frame(spring, xy=TRUE) %>% drop_na()
colnames(spring) <- c("lon","lat","sst")
#spring$sst <- spring$sst-273.15
summary(spring$sst)

##                      ~Summer~

summer <- bob_m[[c("index_6","index_7","index_8")]]

summer <- stackApply(summer,3,fun = mean)

summer <- as.data.frame(summer, xy=TRUE) %>% drop_na()
colnames(summer) <- c("lon","lat","sst")
#summer$sst <- summer$sst-273.15
summary(summer$sst)

##                        ~FAll~~


fall <- bob_m[[c("index_9","index_10")]]

fall <- stackApply(fall,2,fun = mean)

fall <- as.data.frame(fall, xy=TRUE) %>% drop_na()
colnames(fall) <- c("lon","lat","sst")
#fall$sst <- fall$sst-273.15
summary(fall$sst)

######################################################
#             plotting
######################################################
#coastline
coast <- ne_countries(scale = "medium", returnclass = "sf")

#range of sst for all seasons
sl_range <- range(lapply(list(winter,spring,summer,fall  ), function(s) s$sst))
breaks <- seq(23,31,1)
#for legend
s1 <- ggplot(data = winter) + geom_contour_fill(aes(x=lon,y=lat,z=sst)) +scale_fill_gradientn(name ="SST (°C)",colours = rev(brewer.pal(11,"RdBu")),limits = sl_range,breaks = breaks,guide = guide_colorbar(nbin = 11,raster = FALSE,ticks = TRUE, ticks.colour = "black",barheight = 1,barwidth = 15,frame.colour = c("black"),frame.linewidth = 1,label.hjust = 0.5,title.vjust = 0.8,title.position = "left",direction = "horizontal")) + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Winter",size=3)+theme_bw()+theme(legend.title = element_text(angle = ,hjust = 0.5))

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
s1 <- ggplot(data = winter) + geom_contour_fill(aes(x=lon,y=lat,z=sst)) +scale_fill_gradientn(name ="SST (°C)",colours = rev(brewer.pal(11,"RdBu")),limits = sl_range,breaks=breaks,guide = guide_colorbar(nbin = 11,raster = FALSE,ticks = TRUE, ticks.colour = "black",barheight = 1,barwidth = 15,frame.colour = c("black"),frame.linewidth = 1,label.hjust = 0.5,title.vjust = 0.1,title.position = "top",direction = "horizontal")) + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Winter",size=4)+theme_bw()+theme(legend.title = element_text(angle = 90,hjust = 0.5),plot.margin = margin(5.5,4,4,5.5),axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),legend.position = 'none' )
s2 <-  ggplot(data = spring) + geom_contour_fill(aes(x=lon,y=lat,z=sst))+ scale_fill_gradientn(name ="SST (°C)",colours = rev(brewer.pal(11,"RdBu")),limits = sl_range,breaks=breaks,guide = guide_colorbar(nbin = 11,raster = FALSE,ticks = TRUE, ticks.colour = "black",barheight = 1,barwidth = 15,frame.colour = c("black"),frame.linewidth = 1,label.hjust = 0.5,title.vjust = 0.1,title.position = "top",direction = "horizontal")) + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Spring",size=4)+theme_bw()+theme(legend.title = element_text(angle = 90,hjust = 0.5),plot.margin = margin(5.5,5.5,4,4),axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),legend.position = 'none')
s3 <-  ggplot(data = summer) + geom_contour_fill(aes(x=lon,y=lat,z=sst))+ scale_fill_gradientn(name ="SST (°C)",colours = rev(brewer.pal(11,"RdBu")),limits = sl_range,breaks=breaks,guide = guide_colorbar(nbin = 11,raster = FALSE,ticks = TRUE, ticks.colour = "black",barheight = 1,barwidth = 15,frame.colour = c("black"),frame.linewidth = 1,label.hjust = 0.5,title.vjust = 0.1,title.position = "top",direction = "horizontal")) + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Summer",size=4)+theme_bw()+theme(legend.title = element_text(angle = 90,hjust = 0.5),plot.margin = margin(4,4,18.5,5.5),legend.position = 'none')
s4 <-  ggplot(data = fall) + geom_contour_fill(aes(x=lon,y=lat,z=sst))+ scale_fill_gradientn(name ="SST (°C)",colours = rev(brewer.pal(11,"RdBu")),limits = sl_range,breaks=breaks,guide = guide_colorbar(nbin = 11,raster = FALSE,ticks = TRUE, ticks.colour = "black",barheight = 1,barwidth = 15,frame.colour = c("black"),frame.linewidth = 1,label.hjust = 0.5,title.vjust = 0.1,title.position = "top",direction = "horizontal")) + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Fall",size=4)+theme_bw()+theme(legend.title = element_text(angle = 90,hjust = 0.5),plot.margin = margin(4,5.5,18.5,4),legend.position = 'none',axis.title.y = element_blank(),axis.text.y = element_blank())

png('seasonalClimatology1981-2019.png',width = 5.5,height = 5.5,units = 'in',res = 300)
ggarrange(s1,s2,s3,s4,ncol = 2) %>% gridExtra::grid.arrange(legend,heights = unit(c(4.5,0.1),'in'))
dev.off()






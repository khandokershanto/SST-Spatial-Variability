###############################################################
#                         SST monthly Climatology
###############################################################
######################
# data
bob <- brick('Data/noaa_oisst_v2_merged_1982_2020.nc')


layer_names <- names(bob)
layer_names <- gsub('X', '', layer_names)
layer_names <- gsub('\\.', '-', layer_names)
layer_names <- as.Date(layer_names)
layer_names <- month(layer_names)

bob_m <-  stackApply(bob, layer_names, fun = mean)

jan <- bob_m[["index_1"]]
feb <- bob_m[["index_2"]]
mar <- bob_m[["index_3"]]
apr <- bob_m[["index_4"]]
may <- bob_m[["index_5"]]
jun <- bob_m[["index_6"]]
july <- bob_m[["index_7"]]
aug <- bob_m[["index_8"]]
sep <- bob_m[["index_9"]]
oct <- bob_m[["index_10"]]
nov <- bob_m[["index_11"]] 
dec <- bob_m[["index_12"]]


jan <- as.data.frame(jan, xy=TRUE) %>% drop_na()
feb <- as.data.frame(feb, xy=TRUE) %>% drop_na()  
mar <- as.data.frame(mar, xy=TRUE) %>% drop_na()  
apr <- as.data.frame(apr, xy=TRUE) %>% drop_na()  
may <- as.data.frame(may, xy=TRUE) %>% drop_na()  
jun <- as.data.frame(jun, xy=TRUE) %>% drop_na()  
july <- as.data.frame(july, xy=TRUE) %>% drop_na()  
aug <- as.data.frame(aug, xy=TRUE) %>% drop_na()  
sep <- as.data.frame(sep, xy=TRUE) %>% drop_na()  
oct <- as.data.frame(oct, xy=TRUE) %>% drop_na()  
nov <- as.data.frame(nov, xy=TRUE) %>% drop_na()  
dec <- as.data.frame(dec, xy=TRUE) %>% drop_na()  

#changing column names
colnames(jan) <- c('lon','lat','sst')
colnames(feb) <- c('lon','lat','sst')
colnames(mar) <- c('lon','lat','sst')
colnames(apr) <- c('lon','lat','sst')
colnames(may) <- c('lon','lat','sst')
colnames(jun) <- c('lon','lat','sst')
colnames(july) <- c('lon','lat','sst')
colnames(aug) <- c('lon','lat','sst')
colnames(sep) <- c('lon','lat','sst')
colnames(oct) <- c('lon','lat','sst')
colnames(nov) <- c('lon','lat','sst')
colnames(dec) <- c('lon','lat','sst')

# kelvin to celsius
#dec$sst <- dec$sst - 273.15

#lapply(list(jan,feb,mar,apr,may,jun,july,aug,sep,oct,nov,dec),setNames,nm = c('lon','lat','sst'))

sl_range <- range(lapply(list(jan,feb,mar,apr,may,jun,july,aug,sep,oct,nov,dec), function(s) s$sst))
breaks <- seq(21,31, 1)
##base map
coast <- ne_countries(scale = "medium", returnclass = "sf")

##plotting
bar <- scale_fill_gradientn(name ="SST (°C)",colours = rev(brewer.pal(11,"RdBu")),limits = c(21.1,30.8),breaks = breaks ,guide = guide_colorbar(nbin = 11,raster = FALSE,ticks = TRUE, ticks.colour = "black",barheight = 1,barwidth = 15,frame.colour = c("black"),frame.linewidth = 1,label.hjust = 0.5,title.vjust = 0.8,title.position = "left",direction = "horizontal"))
m1 <- ggplot(data = jan) + geom_contour_fill(aes(x=lon,y=lat,z=sst))+ bar + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="January")+theme_bw()+theme(legend.title = element_text(hjust = 0.5))

#get legend

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#save legend
legend <- get_legend(m1)
gridExtra::grid.arrange(legend)

m1 <- ggplot(data = jan) + geom_contour_fill(aes(x=lon,y=lat,z=sst))+ bar + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Jan",size = 5)+theme_bw()+theme(legend.title = element_text(hjust = 0.5),plot.margin = margin(5.5,4,4,5.5),axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),legend.position = 'none' )
m2 <- ggplot(data = feb) + geom_contour_fill(aes(x=lon,y=lat,z=sst))+ bar + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Feb",size = 5)+theme_bw()+theme(legend.title = element_text(hjust = 0.5),plot.margin = margin(5.5,4,4,4),axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_blank(),legend.position = 'none' )
m3 <- ggplot(data = mar) + geom_contour_fill(aes(x=lon,y=lat,z=sst))+ bar + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Mar",size = 5)+theme_bw()+theme(legend.title = element_text(hjust = 0.5),plot.margin = margin(5.5,4,4,4),axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_blank(),legend.position = 'none' )
m4 <- ggplot(data = apr) + geom_contour_fill(aes(x=lon,y=lat,z=sst))+ bar + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Apr",size = 5)+theme_bw()+theme(legend.title = element_text(hjust = 0.5),plot.margin = margin(5.5,4,4,4),axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_blank(),legend.position = 'none' )

m5 <- ggplot(data = may) + geom_contour_fill(aes(x=lon,y=lat,z=sst))+ bar + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="May",size = 5)+theme_bw()+theme(legend.title = element_text(hjust = 0.5),plot.margin = margin(5.5,4,4,5.5),axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),legend.position = 'none' )
m6 <- ggplot(data = jun) + geom_contour_fill(aes(x=lon,y=lat,z=sst))+ bar + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="June",size = 5)+theme_bw()+theme(legend.title = element_text(hjust = 0.5),plot.margin = margin(5.5,4,4,4),axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_blank(),legend.position = 'none' )
m7 <- ggplot(data = july) + geom_contour_fill(aes(x=lon,y=lat,z=sst))+ bar + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="July",size = 5)+theme_bw()+theme(legend.title = element_text(hjust = 0.5),plot.margin = margin(5.5,4,4,4),axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_blank(),legend.position = 'none' )
m8 <- ggplot(data = aug) + geom_contour_fill(aes(x=lon,y=lat,z=sst))+ bar + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Aug",size = 5)+theme_bw()+theme(legend.title = element_text(hjust = 0.5),plot.margin = margin(5.5,4,4,4),axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_blank(),legend.position = 'none' )

m9 <- ggplot(data = sep) + geom_contour_fill(aes(x=lon,y=lat,z=sst))+ bar + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Sept",size = 5)+theme_bw()+theme(legend.title = element_text(hjust = 0.5),plot.margin = margin(5.5,4,4,5.5),legend.position = 'none' )
m10 <- ggplot(data = oct) + geom_contour_fill(aes(x=lon,y=lat,z=sst))+ bar + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Oct",size = 5)+theme_bw()+theme(legend.title = element_text(hjust = 0.5),plot.margin = margin(5.5,4,4,4),axis.title.y = element_blank(),axis.text.y = element_blank(),legend.position = 'none' )
m11 <- ggplot(data = nov) + geom_contour_fill(aes(x=lon,y=lat,z=sst))+ bar + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Nov",size = 5)+theme_bw()+theme(legend.title = element_text(hjust = 0.5),plot.margin = margin(5.5,4,4,4),axis.title.y = element_blank(),axis.text.y = element_blank(),legend.position = 'none' )
m12 <- ggplot(data = dec) + geom_contour_fill(aes(x=lon,y=lat,z=sst))+ bar + geom_sf(data = coast,fill="grey70",col='black')+coord_sf(xlim = c(80.5,99.5),ylim = c(5,25),expand = FALSE)+ xlab("Longitude")+ylab("Latitude")+annotate('text',x=84,y=23.5,label ="Dec",size = 5)+theme_bw()+theme(legend.title = element_text(hjust = 0.5),plot.margin = margin(5.5,4,4,4),axis.title.y = element_blank(),axis.text.y = element_blank(),legend.position = 'none' )

png('monthlyClimatology1981-2019.png',width = 7.5,height = 7.5,units = 'in',res = 300)
#windows(width = 7.5,height = 7.5)
ggarrange(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,ncol = 4) %>% gridExtra::grid.arrange(legend,heights = unit(c(6.5,0.2),'in'))
dev.off()
##############################################

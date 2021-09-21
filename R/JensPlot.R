rm(list=ls(all=TRUE))
gc()

NCELL <- 67420


datadir <- paste0(getwd(),"/data/")
ausgabe <- paste0(getwd(),"/Figure_1/")

load(paste(ausgabe,"figure_1_preprocessed.RData",sep=""))

nclass <- dim(table.pop_in_classes_hist)[1]

library(RColorBrewer)
library(maptools)

world_borders <- readShapePoly("/Users/heinke/Work/data/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

load(paste0(datadir,"landmask_hires.RData"))

halfdeg_to_hires <- function(map)
{
  nlon_halfdeg <- dim(map)[1]
  nlat_halfdeg <- dim(map)[2]
  nlon_hires <- dim(map.landmask_hires)[1]
  nlat_hires <- dim(map.landmask_hires)[2]
  bylon <- nlon_hires/nlon_halfdeg
  bylat <- nlat_hires/nlat_halfdeg
  if(bylon%%1!=0 | bylat%%1!=0) stop("ERROR: Grid mismatch")
  map_hires <- map.landmask_hires
  for(ilon in 1:nlon_halfdeg) for(ilat in 1:nlat_halfdeg) map_hires[(ilon-1)*bylon+1:bylon,(ilat-1)*bylat+1:bylat] <- map_hires[(ilon-1)*bylon+1:bylon,(ilat-1)*bylat+1:bylat] + map[ilon,ilat]
  return(map_hires)
}

plot_legend_box <- function(x,y,width,height,col,label)
{
  polygon(c(x,x+width,x+width,x),c(y,y,y+height,y+height),col=col,xpd=NA)
  text(x+1.4*width,y+0.5*height,labels=label,xpd=NA,adj=c(0,0.5))
}


vlon_hires <- as.numeric(dimnames(map.landmask_hires)[[1]])
vlat_hires <- as.numeric(dimnames(map.landmask_hires)[[2]])

latrange <- c(-50,90)
lwd_boundaries <- 0.3

#cols <- brewer.pal(5,"Spectral")[c(1,2,3,4)]
cols <- c("#9e0142","#f46d43","#ffffbf","#66c2a5","#3288bd")
h <- 13

bitmap(paste(ausgabe,"/figure_1.png",sep=""),type="png16m",height=h,width=22,res=200,pointsize=20)
par(oma=c(0,0,0,0))
nplots <- length(popscens)+1
layout(matrix(c(rep(1,7),rep(2,7),rep(2+1:nplots,each=2),rep(2+2*nplots+2,2),rep(2+nplots+1:nplots,each=2),rep(2+2*nplots+1,2)),3,2*(nplots+1),byrow=T),widths=1,heights=c(1.4,1,1))


par(mar=c(3,0,2,0))
plot(world_borders,col="white",border="white",ylim=latrange)
image(halfdeg_to_hires(maps.crowding[,,"2010"]),x=vlon_hires,y=vlat_hires,xlab="",ylab="",xaxt="n",yaxt="n",breaks=c(-2,0,100,600,1000,2000,max(maps.crowding,na.rm=T)),col=c(grey(0.7),rev(cols)),add=T)
plot(world_borders,lwd=lwd_boundaries,add=T)
text(x=0,y=100,"(a) 2010",cex=1.5,font=2,xpd=NA)

plot(world_borders,col="white",border="white",ylim=latrange)
image(halfdeg_to_hires(maps.crowding[,,"SSP2"]),x=vlon_hires,y=vlat_hires,xlab="",ylab="",xaxt="n",yaxt="n",breaks=c(-2,0,100,600,1000,2000,max(maps.crowding,na.rm=T)),col=c(grey(0.7),rev(cols)),add=T)
plot(world_borders,lwd=lwd_boundaries,add=T)
text(x=0,y=100,"(b) SSP2 2100",cex=1.5,font=2,xpd=NA)

par(mar=c(3,2,2,3))
plot(NA,xlim=c(1920,2010),ylim=c(0,14e9),type="l",xlab="",ylab="",axes=F,main="          Historical",cex.main=1.1)
for(ic in nclass:2) polygon(c(min(popyears_hist),popyears_hist,max(popyears_hist)),c(0,apply(table.pop_in_classes_hist[1:ic,],2,sum),0),col=cols[ic])
polygon(c(min(popyears_hist),popyears_hist,max(popyears_hist)),c(0,table.pop_in_classes_hist[1,],0),col=cols[1])
axis(1,at=c(1950,1960,1970,1980,1990,2000,2010),labels=c("1950","1960","1970","1980","1990","2000","2010"))
axis(2,at=c(0,2,4,6,8,10,12,14)*1e9,labels=c("0","2","4","6","8","10","12","14"),pos=1945.5)
mtext("Total Population in Billion",2,line=-0.2,cex=0.7,xpd=NA)
text(x=1920,y=17e9,"(c)",cex=2,font=2,xpd=NA)

for(pscen in popscens)
{
  plot(NA,xlim=c(2010,2100),ylim=c(0,14e9),type="l",xlab="",ylab="",axes=F,main=pscen,cex.main=1.1)
  for(ic in nclass:2) polygon(c(min(popyears_scen),popyears_scen,max(popyears_scen)),c(0,apply(table.pop_in_classes_scen[1:ic,pscen,],2,sum),0),col=cols[ic])
  polygon(c(min(popyears_scen),popyears_scen,max(popyears_scen)),c(0,table.pop_in_classes_scen[1,pscen,],0),col=cols[1])
  axis(1,at=c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100),labels=c("2010","2020","2030","2040","2050","2060","2070","2080","2090","2100"))
  axis(2,at=c(0,2,4,6,8,10,12,14)*1e9,labels=c("0","2","4","6","8","10","12","14"))
  mtext("Total Population in Billion",2,line=2.6,cex=0.7,xpd=NA)
}


plot(NA,xlim=c(1920,2010),ylim=c(0,1),type="l",xlab="",ylab="",axes=F,main="          Historical",cex.main=1.1)
for(ic in nclass:2) polygon(c(min(popyears_hist),popyears_hist,max(popyears_hist)),c(0,apply(table.share_in_classes_hist[1:ic,],2,sum),0),col=cols[ic])
polygon(c(min(popyears_hist),popyears_hist,max(popyears_hist)),c(0,table.share_in_classes_hist[1,],0),col=cols[1])
axis(1,at=c(1950,1960,1970,1980,1990,2000,2010),labels=c("1950","1960","1970","1980","1990","2000","2010"))
axis(2,at=c(0,0.2,0.4,0.6,0.8,1.0),labels=c("0","20","40","60","80","100"),pos=1945.5)
mtext("Share of Population in %",2,line=-0.2,cex=0.7,xpd=NA)
text(x=1920,y=17/14,"(d)",cex=2,font=2,xpd=NA)

for(pscen in popscens)
{
  plot(NA,xlim=c(2010,2100),ylim=c(0,1),type="l",xlab="",ylab="",axes=F,main=pscen,cex.main=1.1)
  for(ic in nclass:2) polygon(c(min(popyears_scen),popyears_scen,max(popyears_scen)),c(0,apply(table.share_in_classes_scen[1:ic,pscen,],2,sum),0),col=cols[ic])
  polygon(c(min(popyears_scen),popyears_scen,max(popyears_scen)),c(0,table.share_in_classes_scen[1,pscen,],0),col=cols[1])
  axis(1,at=c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100),labels=c("2010","2020","2030","2040","2050","2060","2070","2080","2090","2100"))
  axis(2,at=c(0,0.2,0.4,0.6,0.8,1.0),labels=c("0","20","40","60","80","100"))
  mtext("Share of Population in %",2,line=2.6,cex=0.7,xpd=NA)
}



plot(NA,xlim=c(2010,2100),ylim=c(0,1),xlab="",ylab="",axes=F)

xll <- 1980
yll <- 0.95
ygap <- 0.02
h <- 0.07
w <- 20

offset <- 0
plot_legend_box(x=xll,y=yll+offset+3*(h+ygap),width=w,height=h,col=cols[1],label="> 2000 p/fu:")
text(xll+1.4*w,yll+offset+2*(h+ygap)+0.5*h,labels="absolute water scarcity,",adj=c(0,0.5),xpd=NA)
text(xll+1.4*w,yll+offset+1*(h+ygap)+0.5*h,labels="beyond water boundary",adj=c(0,0.5),xpd=NA)
offset <- offset + 3*(h+ygap) + 3*ygap
plot_legend_box(x=xll,y=yll+offset+3*(h+ygap),width=w,height=h,col=cols[2],label="1000 - 2000 p/fu:")
text(xll+1.4*w,yll+offset+2*(h+ygap)+0.5*h,labels="absolute water scarcity,",adj=c(0,0.5),xpd=NA)
text(xll+1.4*w,yll+offset+1*(h+ygap)+0.5*h,labels="below water boundary",adj=c(0,0.5),xpd=NA)
offset <- offset + 3*(h+ygap) + 3*ygap
plot_legend_box(x=xll,y=yll+offset+2*(h+ygap),width=w,height=h,col=cols[3],label="600 - 1000 p/fu:")
text(xll+1.4*w,yll+offset+1*(h+ygap)+0.5*h,labels="water stress",adj=c(0,0.5),xpd=NA)
offset <- offset + 2*(h+ygap) + 3*ygap
plot_legend_box(x=xll,y=yll+offset+2*(h+ygap),width=w,height=h,col=cols[4],label="100 - 600 p/fu:")
text(xll+1.4*w,yll+offset+1*(h+ygap)+0.5*h,labels="quality and dry-season problems",adj=c(0,0.5),xpd=NA)
offset <- offset + 2*(h+ygap) + 3*ygap
plot_legend_box(x=xll,y=yll+offset+2*(h+ygap),width=w,height=h,col=cols[5],label="< 100 p/fu:")
text(xll+1.4*w,yll+offset+1*(h+ygap)+0.5*h,labels="no water-related problems",adj=c(0,0.5),xpd=NA)
offset <- offset + 2*(h+ygap) + 3*ygap

text(xll,yll+offset+0.32,labels="Water crowding classes",xpd=NA,adj=c(0,0.5),font=2)
text(xll,yll+offset+0.22,labels=bquote(bold("(p/fu: people per 10"^6*" m"^3*" yr"^-1*")")),xpd=NA,adj=c(0,0.5))


dev.off()

########################################################################################################


bitmap(paste(ausgabe,"/figure_S1.png",sep=""),type="png16m",height=15,width=22,res=200,pointsize=20)

par(mar=c(3,0,2,0),oma=c(0,0,0,0))
nplots <- length(popscens)+1
layout(matrix(1:nplots,3,nplots/3,byrow=T))

plot(world_borders,col="white",border="white",ylim=latrange)
image(halfdeg_to_hires(maps.crowding[,,"2010"]),x=vlon_hires,y=vlat_hires,xlab="",ylab="",xaxt="n",yaxt="n",breaks=c(-2,0,100,600,1000,2000,max(maps.crowding,na.rm=T)),col=c(grey(0.7),rev(cols)),add=T)
plot(world_borders,lwd=lwd_boundaries,add=T)
text(x=0,y=100,"2010",cex=1.5,font=2,xpd=NA)

for(pscen in popscens)
{
  plot(world_borders,col="white",border="white",ylim=latrange)
  image(halfdeg_to_hires(maps.crowding[,,pscen]),x=vlon_hires,y=vlat_hires,xlab="",ylab="",xaxt="n",yaxt="n",breaks=c(-2,0,100,600,1000,2000,max(maps.crowding,na.rm=T)),col=c(grey(0.7),rev(cols)),add=T)
  plot(world_borders,lwd=lwd_boundaries,add=T)
  text(x=0,y=100,paste(pscen,"2100"),cex=1.5,font=2,xpd=NA)
}

dev.off()

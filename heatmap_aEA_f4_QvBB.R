args=commandArgs(TRUE)

STATFILE = as.character(args[1])

print(STATFILE)

print(paste(STATFILE,'.map',sep=''))
library(fields)
library(RColorBrewer)

padding = 3

data=read.table(paste(STATFILE,'.map',sep=''))
#print (data)

datapos=subset(data,V4>0)
dataneg=subset(data,V4<0)

statpos=as.numeric(datapos[,4])
statneg=as.numeric(dataneg[,4])
latpos=as.numeric(datapos[,2])
latneg=as.numeric(dataneg[,2])
lonpos<-as.numeric(datapos[,3])
lonneg<-as.numeric(dataneg[,3])
namespos=as.character(datapos[,1])
namesneg=as.character(dataneg[,1])

minLong = 80
maxLong = 145.5
minLat = 5
maxLat = 55

rgbpos=colorRampPalette(c("white","darkgreen"))
rgbneg=colorRampPalette(c("darkblue","white"))
colpos=rgbpos(24) # thousandth position times 4
colneg=rgbneg(8) # thousandth position times 4
colors=c(colneg,colpos)
maxstat =max(statpos)
minstat = min(statneg)

pdf(paste(STATFILE,'.newmap.pdf',sep=''),width=15,height=15) #,res=300)
par(mar=c(5,6,4,1)+.1)

library(mapdata)
library(maps)
library(maptools)
map(database='worldHires', fill=TRUE,xlim=c(minLong,maxLong),ylim=c(minLat,maxLat),col='lightgray',border="lightgray") 

par(new=TRUE)

colspos <- as.character(cut(statpos, breaks = length(colpos),labels =colpos))
colsneg <- as.character(cut(statneg, breaks = length(colneg),labels =colneg))
plot(c(lonpos,lonneg),c(latpos,latneg),type="n",main=expression(paste(f[3])),xlim=c(minLong,maxLong),ylim=c(minLat,maxLat),xlab="Longitude", ylab="Latitude",cex.lab=2,cex.axis=2)




#CASPIAN <- read.table('Caspian.txt')
#polygon(CASPIAN[,2:1],col='white',border='white')

PCHpos <- vector(length=length(lonpos))
PCHneg <- vector(length=length(lonneg))
PCHpos[1:length(PCHpos)] <- 22
PCHneg[1:length(PCHneg)] <- 22

points(lonpos,latpos,pch=PCHpos,bg=colspos,col="black",cex=8)
points(lonneg,latneg,pch=PCHneg,bg=colsneg,col="black",cex=8)
box()


###legend
tempcolours=rev(colors)
legendposx= maxLong-2*padding
legendwidth=1.5*padding
legendtop = maxLat-(maxLat-minLat)/2
legendbottom= minLat+(maxLat-minLat)/10
legendspan=legendtop-legendbottom
legendincrement=legendspan/length(colors)


for(i in 1:length(tempcolours)) {
  rect(legendposx,legendtop-(legendincrement*i),(legendposx+legendwidth),legendtop-(legendincrement*i-legendincrement),col=tempcolours[i],border=NA)
}
text(legendposx+legendwidth/2,legendtop+(maxLat-minLat)/30,round(maxstat,3),cex=3.5)
text(legendposx+legendwidth/2,legendbottom-(maxLat-minLat)/30,round(minstat,3),cex=3.5)
rect(legendposx,legendtop,(legendposx+legendwidth),legendbottom,col=NA)

 
dev.off()

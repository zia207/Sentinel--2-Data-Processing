# Data Processing: Image Classification using Deep Neural Network
# Author: "Zia Ahmed, PhD, University at Buffalo"

### Load R packages 
library(rgdal)  # spatial data processing
library(raster) # raster processing
library(plyr)   # data manipulation 
library(dplyr)  # data manipulation 
library(RStoolbox) # ploting spatial data with ggplot2 
library(RColorBrewer)
library(sp)
library(ggplot2)

### Set working directory
setwd("F:\\My_GitHub\\DNN_H20_R")
dsn<-("F:\\My_GitHub\\DNN_H20_R\\Data")

### Import training polygon layer 
poly <- readOGR(dsn=dsn, "Training_layer") 
ID<-read.csv("Data\\Landuse_ID.csv", header=T)

### Convert polygon to raster
#### Raster extent (we will use any band of Sentinel-2 to set raster extent, we use B2 for define extent)
b2<-raster("Data\\B2.tif") # 
crs(b2) <- "+proj=utm +zone=17N +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
extent=extent(b2)

#### Convert to raster (2.5 m grid size)
r<-raster(extent, resolution=2.5,
            crs = '+proj=utm +zone=17N +ellps=WGS84 +datum=WGS84 +units=m +no_defs ')
extent(r) <- extent(poly)
rp <- rasterize(poly, r, 'Class_ID')
plot(rp)

#### Convert raster to data.frame and rename colum to "layer"" to Class_ID
rp.df <- as.data.frame(rasterToPoints(rp))
colnames(rp.df)[3] <- 'Class_ID'

#### Create a Spatial point Data frame
xy <- rp.df[,c(1,2)]
point.SPDF <- SpatialPointsDataFrame(coords = xy,
                                data=rp.df,
                               proj4string = CRS("+proj=utm +zone=17N +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
#### Load multi-band raster 
multi=stack("Data\\multi_bands.tif")
# rename the bands
names(multi) <- c("B2", "B3","B4","B5","B6","B7","B8","B8A","B11","B12")

### Plot map, to check everything working well

# Natural Color
p1<-ggRGB(multi, r=3, g=2, b=1, stretch = "lin")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle("Natural Color\n(R= Red, G= Green, B= Blue)")
# False Color image
p2<-ggRGB(multi, r=7, g=3, b=2, stretch = "lin")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle("False Color Infrared\n(R= NIR, G= Red,  B=Green)")

source("multi_plot_function.r") # We need this function to plot multiple figures in one page
#(http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/)
multiplot(p1, p2, cols=2)

#### Extract raster values to point file
point.df <- extract(multi, point.SPDF, df=TRUE, method='simple')

#### Combine with data frame
point.mf<-cbind(rp.df,point.df)
head(point.mf)

#### Keep values belong only to 5 classes
point.train<-point.mf %>% 
  select(x,y,Class_ID, B2, B3, B4, B5, B6,B7,B8,B8A,B11,B12) %>% 
  filter(Class_ID >0) 

#### Add class ID and save as a CSV file 
point<-join(point.train, ID, by="Class_ID", type="inner")
write.csv(point, "point_data.csv", row.names=F)

### Convert raster stack to CSV file
#### First creat XY data-frame, will use Band B2
grid.point <- data.frame(rasterToPoints(b2))
# Remove B2 column
grid.point$B2<-NULL
names(grid.point)

#### Convert to sptial point data frame and define projection
coordinates(grid.point) <- ~x + y
projection(grid.point) <- CRS("+proj=utm +zone=17N +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

#### Extract all bands values to grird.point
df.grid<- extract(multi, grid.point, df=TRUE, method='simple')

#### Combine with grid.df (we need to add xy coordinated for mapping) and write as a CSV file
grid<-cbind(as.data.frame(grid.point),df.grid)
head(grid)
write.csv(grid, "grid_data.csv", row.names=F)


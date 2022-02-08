#library("sp")
library("raster")
library("rgdal")


## read-in raster and polygon
meacham <- raster("gis_data/be_fp_tiff/be_fp_clip1.tif")
fp_poly <- readOGR("gis_data/meacham_gis_data/umatilla_fp_clip.shp")

## plot both
plot(fp_poly)
plot(meacham)

## create a masked raster using the polygon
masked <- mask(x = meacham, mask = fp_poly)
plot(masked)

## clip the masked raster using the polygon's extent
cropped <- crop(x = masked, y = extent(fp_poly))
plot(cropped)

## save raster
writeRaster(cropped, filename = "gis_data/meacham_shade_floodplain.tif", overwrite = TRUE)
writeRaster(cropped, filename = "gis_data/meacham_shade_floodplain.asc", overwrite = TRUE)
writeRaster(meacham, filename = "gis_data/meacham_fp_full.asc")

makesure <- raster("gis_data/meacham_shade_floodplain.asc")
plot(makesure)
makesure
summary(cropped)

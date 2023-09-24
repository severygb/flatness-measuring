library("tidyverse")
library("plot3D")
library("plot3Drgl")

lines <- read.table("distortion-correction-try1.txt")
head(lines)

distortion_map <- select(lines, V4, V5, V6) %>%
  rename(X = V4,
         Y = V5,
         Z = V6) %>%
  mutate(X = stringr::str_sub(X, 2, length(X))) %>%
  mutate(Y = stringr::str_sub(Y, 2, length(Y))) %>%
  mutate(Z = stringr::str_sub(Z, 2, length(Z)))

glimpse(distortion_map)

distortion_map %<>%
  mutate(X = as.double(X)) %>%
  mutate(Y = as.double(Y)) %>%
  mutate(Z = as.double(Z)) %>%
  glimpse()


#turn tidy data into the sequences needed for persp
x <-  unique(sort(distortion_map$X))
y <-  unique(sort(distortion_map$Y))

#turn z from list to matrix (actually data frame)
z <-  pivot_wider(distortion_map, names_from = Y, values_from = Z) %>%
  glimpse()
as.matrix(z)
z_subset <- as.matrix(z)[,2:length(z)]
colnames(z_subset) <- NULL

#persp (base R function) creats 3D plot in the plot window
persp(x, y, z_subset, theta=-30, phi=40, r=2, shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
      ticktype="detailed", col="cyan", xlab="X position", 
      ylab="Y position", zlab="Height")

#persp3D (plot3D package) extends this, but the additional functionality is realized with the plot3Drgl package
# plot = F suppresses plot window output, but allows the rgl function to act on it
persp3D(x, y, z_subset, theta=-30, phi=40, r=2, shade=0.4, axes=TRUE,scale=F, box=TRUE,
       ticktype="detailed", xlab="X position", col = "cyan", ploygon_offset = 1,
       ylab="Y position", zlab="Height", plot = FALSE, expand = 10)
#persp3D(x,y,z_subset, front = "lines", back = "lines", add = T, plot = FALSE)
plotrgl(lighting = F, new = TRUE, add = FALSE, smooth = F)

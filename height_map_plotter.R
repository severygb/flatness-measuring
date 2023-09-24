library("tidyverse")
library("plot3D")
library("plot3Drgl")

# Read in log file and parse out Z-probe lines
raw_log <- read.delim("data/insulated2.log", header = F, sep = "\n") %>%
  unlist()


probes <- filter(as.data.frame(raw_log), grepl("Z-probe", raw_log)) %>%
  unlist() %>%
  glimpse()

probes2 <- str_sub(probes, start = 26) %>%
  str_split(pattern = " ")

z_data <- data.frame(matrix(unlist(probes2), nrow=length(probes2), byrow=T), stringsAsFactors = F) %>%
  glimpse()
#data is now in a near-tidy form

z_data <- select(z_data, X1,X2,X3) %>%
  rename(Z_travel = X1,
         X = X2,
         Y = X3) %>%
  glimpse()

z_data %<>%
  mutate(X = as.numeric(str_sub(X, 3)), #coerce chr to double
         Y = as.numeric(str_sub(Y, 3)),
         Z = 5 - as.numeric(Z_travel), #transform Z-probe movement to absolute Z height, ref height = 5mm
         Z_travel = NULL, #remove obsolete variable
         X = round(X, digits = 1), #round probing locations to 0.1mm. There were some slight variations
         Y = round(Y, digits = 1)) %>%
  glimpse()
#at this point data is tidy in X, Y, and Z coordinates

#---------------------------------------------------
# Plot the result in an interactive form

#' To do on this plot:
#'  |~| color gradient for z height
#'  |~| compute regression plane
#'  |~| compare regression plate to data for variance
#'  |~| plot variance
#'  | | compare heated and unheated bed


height_map <- z_data #allows more variability later with less name changing in code
#check validity - jk you can't see the tiny variations...
# p <- ggplot(height_map, mapping = aes(x = X, y = Y)) +
#   geom_point()
# p

#turn tidy data into the sequences needed for persp
#needs x and y to be a vector (type double) sorted in ascending order
#z needs to be in matrix (tabular) form
x_plot <-  unique(sort(height_map$X))

y_plot <-  unique(sort(height_map$Y))



#turn z from list to matrix (actually data frame)
z_plot <-  pivot_wider(height_map, names_from = Y, values_from = Z) %>%
  glimpse()

z_plot <- as.matrix(z_plot)[,2:length(z_plot)]
colnames(z_plot) <- NULL


#persp3D (plot3D package) extends this, but the additional functionality is realized with the plot3Drgl package
persp3D(x_plot, y_plot, z_plot, shade=0.5, axes=TRUE, box=TRUE,
        main = "Bed height map with regression plane",
       ticktype="detailed", xlab="X position", ylab="Y position", zlab="Height",
       plot = FALSE, #suppresses plot, allows rgl function to act on it later
       scale=F, #coordinates are scales to preserve aspect ratios
       expand = 20, #multiplier to emphasize Z variation
       colkey = list(size=1, length = 0.5) #colors along z height
)
plotrgl(lighting = FALSE, new = TRUE, add = FALSE, smooth = FALSE)


#-------------------------------------------------------------------------------
#Plot regression plane with true bed shape

# ## apply linear regression model
m1 <- lm(Z ~ X + Y, data = z_data)
# 
# # in package rockchalk. Automatically plots points and regression plane together, can pass parameters through to persp
# rockchalk::plotPlane(m1, plotx1 = "X", plotx2 = "Y",
#           #additional arguments go through to persp
#           scale=F, #coordinates are scales to preserve aspect ratios
#           expand = 30, #multiplier to emphasize Z variation
# )
#'I can't think of a way to pass plotPlane to persp3d to persp to plotrgl. Yuck. 
#'Try plotting the plane as facets and appending to existing plot with add = T in plotrgl

#extract fitted values from linear model list to matrix
temp_reg_data <- data.frame(X = m1[["model"]]$X, Y = m1[["model"]]$Y, Z_plane = m1[["fitted.values"]])

reg_plane <-  pivot_wider(temp_reg_data, names_from = Y, values_from = Z_plane) %>%
  glimpse()

reg_plane <- as.matrix(reg_plane)[,2:length(reg_plane)]
colnames(reg_plane) <- NULL

#add regression plane to existing plot
persp3D(x_plot, y_plot, reg_plane, shade=0.5, axes=TRUE, box=TRUE,
        plot = FALSE, #suppresses plot, allows rgl function to act on it later
        scale=F, #coordinates are scales to preserve aspect ratios
        expand = 1, #multiplier to emphasize Z variation
        col = "gray" #colors along z height
)
plotrgl(lighting = FALSE, add = T, smooth = FALSE)

#-------------------------------------------------------------------------------
#Plot residuals!

#extract residual values from linear model list to matrix
temp_reg_data <- data.frame(X = m1[["model"]]$X, Y = m1[["model"]]$Y, resi = m1[["residuals"]])

resi_map <-  pivot_wider(temp_reg_data, names_from = Y, values_from = resi) %>%
  glimpse()

resi_map <- as.matrix(resi_map)[,2:length(resi_map)]
colnames(resi_map) <- NULL

#coloring try 1: colors on z height, but from min-max with breaks by number of data
nbcol = 100

color_neg = hcl.colors(nbcol/2, palette = "Reds3") #palette for negative
color_pos = hcl.colors(nbcol/2, palette = "Blues3", rev = T) #palette for positive
color <-c(color_neg, color_pos) #concatenate palettes

breaks_neg <- seq(from = min(resi_map), to = 0, length.out = nbcol/2)
breaks_pos <- seq(from = 0, to = max(resi_map), length.out = nbcol/2)
zcol  = cut(sort(resi_map), breaks = c(breaks_neg, breaks_pos[-1]))
#in persp3d: col=color[zcol]


#add regression plane to existing plot
persp3D(x_plot, y_plot, resi_map, shade=0.5, axes=TRUE, box=TRUE,
        ticktype="detailed", xlab="X position", ylab="Y position",
        main = "Bed residual map from regression plane",
        plot = FALSE, #suppresses plot, allows rgl function to act on it later
        scale=F, #coordinates are scales to preserve aspect ratios
        expand = 1, #multiplier to emphasize Z variation
        col=color[zcol]
)
plotrgl(lighting = FALSE, new = T, add = F, smooth = FALSE)

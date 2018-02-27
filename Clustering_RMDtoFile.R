library(EBImage)
library(dplyr)

# list.files is from the EBImage package
filenames <- list.files("/Users/austingrosel/Desktop/A&F/PARKINSON_HW/hw_drawings/Dynamic Spiral Test", pattern="*.png", full.names=TRUE)

par(mfrow=c(1,2))
plot(readImage(filenames[1]))
plot(readImage(filenames[23]))
par(mfrow=c(1,1))

# 0 assigned to non-Parkinson's, 1 assigned to Parkinson's
observed_set = data.frame(name = gsub("/", "", substr(filenames, nchar(filenames) - 6, nchar(filenames))),  class = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1))

remove_axes = function(df, x_vec, y_vec) {
  return(df[df$y > (as.numeric(y_vec[1]) + 5) & df$y < (as.numeric(y_vec[2]) - 4) & df$x > (as.numeric(x_vec[1]) + 4) & df$x < (as.numeric(x_vec[2]) - 5), ])
}

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

initialize_image_df = function(filename) {
  # Read in the image file
  x = readImage(filename)
  
  # Get the dimensions of the data
  dimension = dim(x)
  
  # Get the x,y coordinates and the RGB values into a dataframe
  # Each row represents a pixel
  x_rgb <- data.frame(
    x = rep(1:dimension[2], each = dimension[1]),
    y = rep(dimension[1]:1, dimension[2]),
    R = as.vector(x[,,1]),
    G = as.vector(x[,,2]),
    B = as.vector(x[,,3])
  )
  return(x_rgb)
}

populate_image_df = function(filenames, rows) {
  # Create an empty dataframe with one row per pixel and 25 columns
  d = data.frame(matrix(NA, nrow = rows, ncol = length(filenames)))
  # This will be transposed later on to 25 rows with many columns
  
  i = 1
  for(f in filenames) {
    x1_rgb = initialize_image_df(f)
    
    # Create an all "black" variable and "only_blue" variable for pixels that are all black or only blue
    x1_rgb$only_blue = ifelse(x1_rgb$B == 1 & x1_rgb$G == 0 & x1_rgb$R == 0, 1, 0)
    
    # This removes the axes and tick marks based on which x,y coordinates have the longest black lines in the image.
    x1_rgb = remove_axes(x1_rgb, x_vec, y_vec)
    
    # Paste the blue pixel values of the drawings into each column
    d[,i] = x1_rgb$only_blue
    
    # Change the column name to the image's name
    colnames(d)[i] = substr(filenames[i], nchar(filenames[i]) - 6, nchar(filenames[i]))
    colnames(d)[i] = gsub("/", "", colnames(d)[i])
    i = i + 1
  }
  
  return(d)
}

# Initialize the first image
x1_rgb = initialize_image_df(filenames[1])

# Find where the pixels are strictly black
x1_rgb$black = ifelse(x1_rgb$B == 0 & x1_rgb$G == 0 & x1_rgb$R == 0, 1, 0)

# Get and sort the x and y values that contain the most frequent black pixel value.
x1_black = x1_rgb[x1_rgb$black == 1,]
x_vec = as.data.frame(table(x1_black$x))
x_vec = as.numeric.factor(x_vec[order(-x_vec$Freq),]$Var1)
y_vec = as.data.frame(table(x1_black$y))
y_vec = as.numeric.factor(y_vec[order(-y_vec$Freq),]$Var1)

# Chop off the axes borders and take note of how many pixels are left.
x1_rgb = remove_axes(x1_rgb, x_vec, y_vec)
rows = nrow(x1_rgb)

image_df = populate_image_df(filenames, rows)

# I chose to vary the method between Ward's method, single linkage, and complete linkage.
for(method in c("ward.D", "complete", "single")) {
  # Transposed and converted the data into a dataframe
  t_image_df = t(image_df)
  t_image_df = data.frame(t_image_df)
  
  # Since we're dealing with 1's and 0's, the distance between each of the images was set to binary
  distance = dist(t_image_df, method = "binary")
  
  # Here's where I started the hierachical clustering.
  fit <- hclust(distance, method = method) 
  # The fit was plotted and a border assigning cluster boundary was drawn
  plot(fit, main = paste("Clustering Method:", toupper(method)))
  groups <- cutree(fit, k=2)
  rect.hclust(fit, k=2, border="red")
  
  # This line creates a dataframe of the predicted groups and the "theoretical" (or observed) classes for each image.
  # The class column holds the values I assigned manually from the observed_set data frame.
  groups_df = data.frame(groups, class = observed_set$class)
  
  print(groups_df %>% group_by(groups) %>% summarise(mean = mean(class), amt = n()))
  
  # Wrtie the unsupervised output to a CSV
  df = data.frame(groups, Drawing = gsub(".png", "", rownames(data.frame(groups))))
  df$Drawing = as.numeric(gsub("d", "", df$Drawing))
  df = df[order(df$Drawing),]
  df$Drawing = paste0("d", df$Drawing)
  df$Unsupervised_Group = ifelse(df$groups == 1, "A", "B")
  rownames(df) = NULL
  df$groups = NULL
  write.csv(df, paste0("ClusterOutput_", method))
}

# Read in the static drawing folder files
filenames2 <- list.files("/Users/austingrosel/Desktop/A&F/PARKINSON_HW/hw_drawings/Static Spiral Test", pattern="*.png", full.names=TRUE)

# Append the new file names to the filenames vector
filenames = c(filenames, filenames2)

# Populate the image matrix
image_df = populate_image_df(filenames, rows)

# Transpose the data
t_image_df = t(image_df)
t_image_df = data.frame(t_image_df)

# Calculate the distance for the clusters on each other
distance = dist(t_image_df, method = "binary")

# Create the clustering algorithm
fit <- hclust(distance, method = "ward.D") 
plot(fit)
groups <- cutree(fit, k=2)
rect.hclust(fit, k=2, border="red")

# Test to see if there's a significant difference between the clusters.
# An 's' is labeled if the drawing is static, and 'd' for dynamic.
# Ideally, we'd like one class to have 25 's', 0 'd' and vise versa for the other cluster.
# The worst outcome would to have 50% of s and d in one cluster, and 50% of each in the other cluster.
test = data.frame(file = rownames(t_image_df), class = substr(rownames(t_image_df), 0, 1),pred = groups-1)

test %>% group_by(pred, class) %>% summarise(n())

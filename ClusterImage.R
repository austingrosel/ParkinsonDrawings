library(png)

f = system.file("images","sample.png", package = "EBImage")

filenames <- list.files("/Users/austingrosel/Desktop/A&F/PARKINSON_HW/hw_drawings/Dynamic Spiral Test", pattern="*.png", full.names=TRUE)

d = data.frame(matrix(NA, nrow = 141100, ncol = length(filenames)))
i = 1
for(f in filenames) {
  print(f)
  x1 = readImage(f)
  dimension = dim(x1)
  x1_rgb <- data.frame(
    x = rep(1:dimension[2], each = dimension[1]),
    y = rep(dimension[1]:1, dimension[2]),
    R = as.vector(x1[,,1]), #slicing our array into three
    G = as.vector(x1[,,2]),
    B = as.vector(x1[,,3])
  )
  x1_rgb$black = ifelse(x1_rgb$B == 0 & x1_rgb$G == 0 & x1_rgb$R == 0, 1, 0)
  x1_rgb$only_blue = ifelse(x1_rgb$B == 1 & x1_rgb$G == 0 & x1_rgb$R == 0, 1, 0)
  x1_rgb$gray = (x1_rgb$R + x1_rgb$G + x1_rgb$B)/3
  x1_rgb$gray = ifelse(x1_rgb$gray != 0 & x1_rgb$gray != 1, 0.5, 0)
  x1_rgb = x1_rgb[x1_rgb$y > 58 & x1_rgb$y < 484 & x1_rgb$x > 37 & x1_rgb$x < 370, ]
  d[,i] = x1_rgb$only_blue
  colnames(d)[i] = substr(filenames[i], nchar(filenames[i]) - 6, nchar(filenames[i]))
  colnames(d)[i] = gsub("/", "", colnames(d)[i])
  i = i + 1
}

#d$d7.png = NULL

#d$row_sums = rowSums(d)
#d = d[d$row_sums != 0, ]
#d$row_sums = NULL

td = t(d)
td = data.frame(td)

distance = dist(td, method = "binary")

fit <- hclust(distance, method = "complete") 
plot(fit)
groups <- cutree(fit, k=2)
rect.hclust(fit, k=2, border="red")



x1 = readImage("d1.png")
dimension = dim(x1)
x1_rgb <- data.frame(
  x = rep(1:dimension[2], each = dimension[1]),
  y = rep(dimension[1]:1, dimension[2]),
  R = as.vector(x1[,,1]), #slicing our array into three
  G = as.vector(x1[,,2]),
  B = as.vector(x1[,,3])
)



x2 = readImage("d2.png")
dimension = dim(x2)
x2_rgb <- data.frame(
  x = rep(1:dimension[2], each = dimension[1]),
  y = rep(dimension[1]:1, dimension[2]),
  R = as.vector(x2[,,1]), #slicing our array into three
  G = as.vector(x2[,,2]),
  B = as.vector(x2[,,3])
)

x2_rgb$gray = (x2_rgb$R + x2_rgb$G + x2_rgb$B)/3

d = data.frame(x2_rgb$gray)
d = t(d)
d = data.frame(d)

par(mfrow=c(1,2))
plot(readImage(filenames[1]))
plot(readImage(filenames[23]))
par(mfrow=c(1,1))

x9 = readImage("/Users/austingrosel/Desktop/A&F/PARKINSON_HW/hw_drawings/Dynamic Spiral Test/d1.png")
m2 = imageData(x9)
img2 = Image(m2)
display(img2)
plot(img2)



dimension = dim(x9)
x9_rgb <- data.frame(
  x = rep(1:dimension[2], each = dimension[1]),
  y = rep(dimension[1]:1, dimension[2]),
  R = as.vector(x9[,,1]), #slicing our array into three
  G = as.vector(x9[,,2]),
  B = as.vector(x9[,,3])
)


download.file("https://andreacirilloac.github.io/dataviz/images/sacra_famiglia_canigiani.jpg", "image.jpg")
painting <- readJPEG("image.jpg")

dimension = dim(painting)

painting_rgb <- data.frame(
  x = rep(1:dimension[2], each = dimension[1]),
  y = rep(dimension[1]:1, dimension[2]),
  R = as.vector(painting[,,1]), #slicing our array into three
  G = as.vector(painting[,,2]),
  B = as.vector(painting[,,3])
)







print(f)
x1 = readImage(f)
dimension = dim(x1)
x1_rgb <- data.frame(
  x = rep(1:dimension[2], each = dimension[1]),
  y = rep(dimension[1]:1, dimension[2]),
  R = as.vector(x1[,,1]), #slicing our array into three
  G = as.vector(x1[,,2]),
  B = as.vector(x1[,,3])
)
x1_rgb$black = ifelse(x1_rgb$B == 0 & x1_rgb$G == 0 & x1_rgb$R == 0, 1, 0)
x1_rgb$only_blue = ifelse(x1_rgb$B == 1 & x1_rgb$G == 0 & x1_rgb$R == 0, 1, 0)
#d[,i] = x1_rgb$black
i = i + 1

x1_black = x1_rgb[x1_rgb$black == 1,]

x2 = readImage('/Users/austingrosel/Desktop/A&F/PARKINSON_HW/hw_drawings/Dynamic Spiral Test/d9.png')
dimension = dim(x2)
x2_rgb <- data.frame(
  x = rep(1:dimension[2], each = dimension[1]),
  y = rep(dimension[1]:1, dimension[2]),
  R = as.vector(x2[,,1]), #slicing our array into three
  G = as.vector(x2[,,2]),
  B = as.vector(x2[,,3])
)
x2_rgb$black = ifelse(x2_rgb$B == 0 & x2_rgb$G == 0 & x2_rgb$R == 0, 1, 0)
x2_rgb$only_blue = ifelse(x2_rgb$B == 1 & x2_rgb$G == 0 & x2_rgb$R == 0, 1, 0)

black = x2_rgb[x2_rgb$black == 1,]




x2 = readImage('/Users/austingrosel/Desktop/A&F/PARKINSON_HW/hw_drawings/Dynamic Spiral Test/d11.png')
dimension = dim(x2)
x2_rgb <- data.frame(
  x = rep(1:dimension[2], each = dimension[1]),
  y = rep(dimension[1]:1, dimension[2]),
  R = as.vector(x2[,,1]), #slicing our array into three
  G = as.vector(x2[,,2]),
  B = as.vector(x2[,,3])
)
x2_rgb$black = ifelse(x2_rgb$B == 0 & x2_rgb$G == 0 & x2_rgb$R == 0, 1, 0)
x2_rgb$only_blue = ifelse(x2_rgb$B == 1 & x2_rgb$G == 0 & x2_rgb$R == 0, 1, 0)

black = x2_rgb[x2_rgb$black == 1,]
table(black$x)
table(black$y)

border = x2_rgb[x2_rgb$y > 53 & x2_rgb$y < 488 & x2_rgb$x > 33 & x2_rgb$x < 375, ]




table(d$d1.png)
table(d$d2.png)

e = data.frame(matrix(NA, nrow = 141100, ncol = 4))
i = 1
for(f in c('d1.png', 'd2.png')) {
  print(f)
  x1 = readImage(f)
  dimension = dim(x1)
  x1_rgb <- data.frame(
    x = rep(1:dimension[2], each = dimension[1]),
    y = rep(dimension[1]:1, dimension[2]),
    R = as.vector(x1[,,1]), #slicing our array into three
    G = as.vector(x1[,,2]),
    B = as.vector(x1[,,3])
  )
  x1_rgb$black = ifelse(x1_rgb$B == 0 & x1_rgb$G == 0 & x1_rgb$R == 0, 1, 0)
  x1_rgb$only_blue = ifelse(x1_rgb$B == 1 & x1_rgb$G == 0 & x1_rgb$R == 0, 1, 0)
  x1_rgb$gray = (x1_rgb$R + x1_rgb$G + x1_rgb$B)/3
  x1_rgb = x1_rgb[x1_rgb$y > 58 & x1_rgb$y < 484 & x1_rgb$x > 37 & x1_rgb$x < 370, ]
  e[3] = x1_rgb$x
  colnames(e)[3] = "X"
  e[4] = x1_rgb$y
  colnames(e)[4] = "Y"
  e[,i] = x1_rgb$gray
  colnames(e)[i] = paste0("d", i)
  i = i + 1
}

e$diff = e$d1 - e$d2

e = t(e)
te = data.frame(e)
rm(e)



x1 = readImage(f)
dimension = dim(x1)
x1_rgb <- data.frame(
  x = rep(1:dimension[2], each = dimension[1]),
  y = rep(dimension[1]:1, dimension[2]),
  R = as.vector(x1[,,1]), #slicing our array into three
  G = as.vector(x1[,,2]),
  B = as.vector(x1[,,3])
)
x1_rgb$black = ifelse(x1_rgb$B == 0 & x1_rgb$G == 0 & x1_rgb$R == 0, 1, 0)
x1_rgb$only_blue = ifelse(x1_rgb$B == 1 & x1_rgb$G == 0 & x1_rgb$R == 0, 1, 0)
x1_rgb$gray = (x1_rgb$R + x1_rgb$G + x1_rgb$B)/3
x1_rgb$gray = ifelse(x1_rgb$gray != 0 & x1_rgb$gray != 1, 0.5, 0)


x1_rgb = x1_rgb[x1_rgb$y > (as.numeric(y_tbl[1]) + 5) & x1_rgb$y < (as.numeric(y_tbl[2]) - 4) & x1_rgb$x > (as.numeric(x_tbl[1]) + 4) & x1_rgb$x < (as.numeric(x_tbl[2]) - 5), ]



# Read in the image file
x1 = readImage(filenames[1])

# Get the dimensions of the data
dimension = dim(x1)

# Get the x,y coordinates and the RGB values into a dataframe
# Each row represents a pixel
x1_rgb <- data.frame(
  x = rep(1:dimension[2], each = dimension[1]),
  y = rep(dimension[1]:1, dimension[2]),
  R = as.vector(x1[,,1]),
  G = as.vector(x1[,,2]),
  B = as.vector(x1[,,3])
)
x1_rgb$black = ifelse(x1_rgb$B == 0 & x1_rgb$G == 0 & x1_rgb$R == 0, 1, 0)
x1_black = x1_rgb[x1_rgb$black == 1,]
x_tbl = as.data.frame(table(x1_black$x))
x_tbl = as.numeric.factor(x_tbl[order(-x_tbl$Freq),]$Var1)
y_tbl = as.data.frame(table(x1_black$y))
y_tbl = as.numeric.factor(y_tbl[order(-y_tbl$Freq),]$Var1)









as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
# Read in the image file
x1 = readImage(filenames[1])

# Get the dimensions of the data
dimension = dim(x1)

# Get the x,y coordinates and the RGB values into a dataframe
# Each row represents a pixel
x1_rgb <- data.frame(
  x = rep(1:dimension[2], each = dimension[1]),
  y = rep(dimension[1]:1, dimension[2]),
  R = as.vector(x1[,,1]),
  G = as.vector(x1[,,2]),
  B = as.vector(x1[,,3])
)
x1_rgb$black = ifelse(x1_rgb$B == 0 & x1_rgb$G == 0 & x1_rgb$R == 0, 1, 0)
x1_black = x1_black[x1_black$black == 1,]
x_tbl = as.data.frame(table(x1_black$x))
x_tbl = as.numeric.factor(x_tbl[order(-x_tbl$Freq),]$Var1)
y_tbl = as.data.frame(table(x1_black$y))
y_tbl = as.numeric.factor(y_tbl[order(-y_tbl$Freq),]$Var1)

x1_rgb = x1_rgb[x1_rgb$y > (as.numeric(y_tbl[1]) + 5) & x1_rgb$y < (as.numeric(y_tbl[2]) - 4) & x1_rgb$x > (as.numeric(x_tbl[1]) + 5) & x1_rgb$x < (as.numeric(x_tbl[2]) - 5), ]

# Create an empty dataframe with one row per pixel and 25 columns
d = data.frame(matrix(NA, nrow = nrow(x1_rgb), ncol = length(filenames)))
# This will be transposed later on to 25 rows with many columns


i = 1
for(f in filenames) {
  # Read in each image
  x1 = readImage(f)
  dimension = dim(x1)
  # Create a dataframe of each pixel's coordinates and data
  x1_rgb <- data.frame(
    x = rep(1:dimension[2], each = dimension[1]),
    y = rep(dimension[1]:1, dimension[2]),
    R = as.vector(x1[,,1]),
    G = as.vector(x1[,,2]),
    B = as.vector(x1[,,3])
  )
  # Create an all "black" variable and "only_blue" variable for pixels that are all black or only blue
  x1_rgb$only_blue = ifelse(x1_rgb$B == 1 & x1_rgb$G == 0 & x1_rgb$R == 0, 1, 0)
  
  # This removes the axes and tick marks based on which x,y coordinates have the longest black lines in the image.
  x1_rgb = x1_rgb[x1_rgb$y > (as.numeric(y_tbl[1]) + 5) & x1_rgb$y < (as.numeric(y_tbl[2]) - 4) & x1_rgb$x > (as.numeric(x_tbl[1]) + 5) & x1_rgb$x < (as.numeric(x_tbl[2]) - 5), ]
  
  # Paste the blue pixel values of the drawings into each column
  d[,i] = x1_rgb$only_blue
  
  # Change the column name to the image's name
  colnames(d)[i] = substr(filenames[i], nchar(filenames[i]) - 6, nchar(filenames[i]))
  colnames(d)[i] = gsub("/", "", colnames(d)[i])
  i = i + 1
}

td = t(d)
td = data.frame(td)

distance = dist(td, method = "binary")

fit <- hclust(distance, method = "ward.D") 
plot(fit)
groups <- cutree(fit, k=2)
rect.hclust(fit, k=2, border="red")

this_names = substr(filenames, nchar(filenames) - 6, nchar(filenames))
this_names = gsub("/", "", this_names)

groups_df = data.frame(groups, class = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1))

#library(dplyr)
groups_df %>% group_by(groups) %>% summarise(mean(class), n())






x10_rgb = create_image_df("d2.png")
x10_rgb$only_blue = ifelse(x10_rgb$B == 1 & x10_rgb$G == 0 & x10_rgb$R == 0, 1, 0)
x10_rgb = remove_axes(x10_rgb, x_tbl, y_tbl)

remove_axes = function(df, x_tbl, y_tbl) {
  return(df[df$y > (as.numeric(y_tbl[1]) + 5) & df$y < (as.numeric(y_tbl[2]) - 4) & df$x > (as.numeric(x_tbl[1]) + 4) & df$x < (as.numeric(x_tbl[2]) - 5), ])
}

View(x1_rgb)

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

create_image_df = function(filename) {
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







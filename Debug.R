# PART 2
filenames2 <- list.files("/Users/austingrosel/Desktop/A&F/PARKINSON_HW/hw_drawings/Static Spiral Test", pattern="*.png", full.names=TRUE)

filenames = c(filenames, filenames2)

d = data.frame(matrix(NA, nrow = 141100, ncol = length(filenames)))
i = 1
for(f in filenames) {
  print(f)
  x1_rgb = create_image_df(f)
  
  # Create an all "black" variable and "only_blue" variable for pixels that are all black or only blue
  x1_rgb$only_blue = ifelse(x1_rgb$B == 1 & x1_rgb$G == 0 & x1_rgb$R == 0, 1, 0)
  
  # This removes the axes and tick marks based on which x,y coordinates have the longest black lines in the image.
  x1_rgb = remove_axes(x1_rgb, x_tbl, y_tbl)
  
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

test = data.frame(file = rownames(td), class = substr(rownames(td), 0, 1),pred = groups-1)

test %>% group_by(pred, class) %>% summarise(n())
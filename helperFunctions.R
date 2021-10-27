# helper functions


#color for plots
load("color_list_2.Rdata")
color.list = color_list_2
convert360ToColorVal = function(deg){
  deg = deg%%360+1
  color = color.list[[deg]]
  rgb(color[1],color[2],color[3], maxColorValue = 255)
} 


coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}


circDist = function(end,start){
  a = end - start
 (a+ 180) %% 360 - 180
}





perspControl = function(theta = 1, phi = 30, r = sqrt(3), d = 1, scale = TRUE, expand = 1, col = "white", 
                        border = NULL, ltheta = -135, lphi = 0, shade = 0.5, box = TRUE, axes = TRUE, nticks = 5,
                        ticktype = "simple", xlab = NULL, ylab = NULL, zlab = NULL, main = NULL) {
  
  
  list(theta = theta, phi = phi, r = r, d=d, scale = scale, expand = expand, col = col, 
       border = border, ltheta = ltheta, lphi = lphi, shade = shade, box = box, axes = axes, nticks = nticks, 
       ticktype= ticktype, xlab = xlab, ylab = ylab, zlab = zlab, main = main)
  
}

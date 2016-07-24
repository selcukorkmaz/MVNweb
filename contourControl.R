contourControl = function(nlevels=20, labels = NULL, xlab = NULL, ylab = NULL,
                            labcex = 0.6, drawlabels = TRUE, method = c("simple", "edge", "flattest"), axes = TRUE,
                            frame.plot = axes, col = par("fg"), lty = par("lty"), lwd = par("lwd")) {
                                
    
    list(nlevels = nlevels, labels = labels, xlab=xlab, ylab = ylab, labcex = labcex, drawlabels = drawlabels,
    method = method, axes = axes, frame.plot = frame.plot, col = col, lty = lty, lwd = lwd)
    
}

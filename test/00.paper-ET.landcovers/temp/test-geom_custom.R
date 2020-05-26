custom_grob <- function(data, x=0.5,y=0.5){
    # browser()
    grob(data=data,x=x,y=y, cl="custom")
}
preDrawDetails.custom <- function(x){
    # browser()
    pushViewport(viewport(x=x$x,y=x$y))
}
postDrawDetails.custom <- function(x){
    upViewport()
}

# f         data
# 1 a c(0.4, 0....
#       2 b c(0.4, 0....
#             3 c c(0.4, 0....
#                   4 d c(0.4, 0....
#
# 这是如何确定空间大小的
drawDetails.custom <- function(x, recording=FALSE, ...){
    # browser()
    grid.rect(gp = gpar(col = "red", fill = "transparent"))
    # grid.rect(mean(x$data$x), mean(x$data$y),
    #           width=diff(range(x$data$x)),
    #           height=diff(range(x$data$y)))
    # grid.lines(x$data$x, x$data$y, gp=gpar(col=x$data$col,lwd=2), default.units = "native")
}
d <- data.frame(x=rep(1:3, 4), f=rep(letters[1:4], each=3))
gl <- lapply(1:4, function(ii){
    data.frame(x=seq(0.4,0.6,length=10),
               y = runif(10,0.45,0.55),
               col = hcl(h = seq(0,300,length=nrow(d)))[ii],
               stringsAsFactors = FALSE)
})
subplots <- data.frame(f=letters[1:4], data = I(gl))
str(subplots)

ggplot(d, aes(f,x)) +
    facet_wrap(~f, nrow=1)+
    # coord_polar() +
    geom_point()+
    geom_custom(data = subplots, aes(data = data, x = f, y = 2),
                grob_fun = custom_grob)


figure2 = FALSE
if (figure2) {
    library(grid)
    d <- data.frame(x=rep(1:3, 4), f=rep(letters[1:4], each=3))
    gl <- replicate(4, matrix(sample(palette(), 9, TRUE), 3, 3), FALSE)
    dummy <- data.frame(f = letters[1:4], data = I(gl))
    ggplot(d, aes(f,x)) +
        facet_wrap(~f) +
        theme_bw() +
        geom_point() +
        geom_custom(data = dummy, aes(data = data, y = 2),
                    grob_fun = function(x) rasterGrob(x, interpolate = FALSE))
}

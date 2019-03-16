library(sp)

addNAemptyRowsCols = function(obj) {
  # accept gridded; return SpatialPointsDataFrame with NA records on empty row/cols
  fullgrid(obj) = FALSE
  nfull = obj@grid@cells.dim[1] * obj@grid@cells.dim[2]
  missingpatt = rep(TRUE, nfull)
  missingpatt[obj@grid.index] = FALSE
  missingpatt = matrix(missingpatt,
                       obj@grid@cells.dim[1], obj@grid@cells.dim[2], byrow = FALSE)
  missing.x = which(apply(missingpatt, 1, all))
  missing.y = which(apply(missingpatt, 2, all))
  
  xy = coordinates(obj)[,1:2,drop=FALSE]
  coordvals = coordinatevalues(obj@grid)
  missing.x = coordvals[[1]][missing.x]
  missing.y = coordvals[[2]][missing.y]
  n = length(missing.x) + length(missing.y)
  if (n > 0) {
    if (length(missing.x) > 0)
      xy = rbind(xy, cbind(missing.x, rep(xy[1,2], length(missing.x))))
    if (length(missing.y) > 0)
      xy = rbind(xy, cbind(rep(xy[1,1], length(missing.y)), missing.y))
    newatt = data.frame(lapply(obj@data, function(x) c(x, rep(NA, n))))
    row.names(xy) = seq_len(nrow(xy)) 
    obj = SpatialPointsDataFrame(xy, newatt, obj@coords.nrs, obj@proj4string, FALSE)
  } else
    obj = as(obj, "SpatialPointsDataFrame")
  obj
}

getFormulaLevelplot = function(sdf, zcol) {
# if (length(zcol) > 1)
# 	as.formula(paste("z~", paste(dimnames(coordinates(sdf))[[2]], 
# 		collapse = "+"), "|name"))
# else {
# 	if (!is.character(zcol)) 
# 		zcol = names(sdf)[zcol]
# 	as.formula(paste(zcol, "~", paste(dimnames(coordinates(sdf))[[2]],
# 		collapse = "+")))
# }
  as.formula(paste("z~", paste(dimnames(coordinates(sdf))[[2]], 
                               collapse = "+"), "|name"))
}

colorkey.factor = function(f, colorkey = list(), doColorkey = TRUE) {
	lf = levels(f)
	at = seq(0.5, nlevels(f)+0.501)
	at.labels = seq(1, nlevels(f))
	if (doColorkey) {
		colorkey=append(colorkey, list(labels=list(at=at.labels, labels=lf), 
			height=min(1, .05 * length(lf))))
		list(at = at, colorkey = colorkey)
	} else
		list(at = at)
}

spplot.grid = function(obj, zcol = names(obj), ..., names.attr, 
		scales = list(draw = FALSE), xlab = NULL, ylab = NULL, 
		aspect = mapasp(obj,xlim,ylim), panel = panel.gridplot, sp.layout = NULL, formula, 
		xlim = bbox(obj)[1,], ylim = bbox(obj)[2,], checkEmptyRC = TRUE,
		col.regions = get_col_regions()) {
	if (is.null(zcol)) stop("no names method for object")
	if (checkEmptyRC)
		sdf = addNAemptyRowsCols(obj) # returns SpatialPointsDataFrame
	else
		sdf = as(obj, "SpatialPointsDataFrame")
	if (missing(formula))
		formula = getFormulaLevelplot(sdf, zcol)
	
# if (length(zcol) > 1) {
# 	sdf = spmap.to.lev(sdf, zcol = zcol, names.attr = names.attr)
# 	zcol2 = "z"
# } else
# 	zcol2 = zcol
	
	# modified by Dongdong Kong, 20170321
	sdf = spmap.to.lev(sdf, zcol = zcol, names.attr = names.attr)
	zcol2 = "z"
	
    if (exists("panel.levelplot.raster")) {
        opan <- lattice.options("panel.levelplot")[[1]]
        lattice.options("panel.levelplot"="panel.levelplot.raster")
#       cat("using raster panel\n")
    }
	scales = longlat.scales(obj, scales, xlim, ylim)
	args = append(list(formula, data = as(sdf, "data.frame"), 
		aspect = aspect, panel = panel, xlab = xlab, ylab = ylab, scales = scales,
		sp.layout = sp.layout, xlim = xlim, ylim = ylim, col.regions = col.regions), 
		list(...))
	# deal with factor variables:
	if (all(unlist(lapply(obj@data[zcol], is.factor)))) {
		#if (!is.null(args$col.regions) &&
		#		nlevels(obj@data[[zcol[1]]]) != length(args$col.regions))
		#	stop("length of col.regions should match number of factor levels")
		args$data[[zcol2]] = as.numeric(args$data[[zcol2]])
		if (is.null(args$colorkey) || (is.logical(args$colorkey) && args$colorkey)
				|| (is.list(args$colorkey) && is.null(args$colorkey$at) && 
					is.null(args$colorkey$labels))) {
			if (!is.list(args$colorkey))
				args$colorkey = list()
			ck = args$colorkey
			args$colorkey = NULL
			args = append(args, colorkey.factor(obj[[zcol[1]]], ck))
		} else
			args = append(args, colorkey.factor(obj[[zcol[1]]], ck, FALSE))
	}
	ret = do.call(levelplot, args)
    if (exists("panel.levelplot.raster"))
        lattice.options("panel.levelplot" = opan)
	ret 
}

setMethod("spplot", signature("SpatialPixelsDataFrame"), spplot.grid)
setMethod("spplot", signature("SpatialGridDataFrame"),
	function(obj, ...) spplot.grid(as(obj, "SpatialPixelsDataFrame"), ...))
environment(spplot.grid) <- environment(sp:::spplot.grid)

# environment(spplot) <- environment(sp:::spplot.grid)
panel.levelplot.raster2 <-
    function(x, y, z, 
             subscripts,
             at = pretty(z),
             ...,
             col.regions = regions$col,
             alpha.regions = regions$alpha,
             interpolate = FALSE,
             identifier = "levelplot")
{
    if (length(subscripts) == 0) return()
    regions <- trellis.par.get("regions")
    x.is.factor <- is.factor(x)
    y.is.factor <- is.factor(y)
    x <- as.numeric(x)
    y <- as.numeric(y)
    z <- as.numeric(z)
    zcol <- level.colors(z, at, col.regions, colors = TRUE)
    x <- x[subscripts]
    y <- y[subscripts]
    z <- z[subscripts]
    zcol <- zcol[subscripts]

    if (hasGroupNumber())
        group <- list(...)$group.number
    else
        group <- 0

    if (x.is.factor)
    {
        ## unique values (we want to keep missing levels in between)
        ux <- seq(from = min(x, na.rm = TRUE), to = max(x, na.rm = TRUE))
        xwid <- 1L
    }
    else
    {
        ## sorted unique values of x 
        ux <- sort(unique(x[!is.na(x)]))
        ## complain if all ux are not equidistant
        ## if             (length(unique(diff(ux))) != 1) -- too strict
        if (!isTRUE(all.equal(diff(range(diff(ux))), 0)))
            warning("'x' values are not equispaced; output may be wrong")
        xwid <- mean(diff(ux))
    }
    ## same things for y
    if (y.is.factor)
    {
        ux <- seq(from = min(y, na.rm = TRUE), to = max(y, na.rm = TRUE))
        ywid <- 1L
    }
    else
    {
        uy <- sort(unique(y[!is.na(y)]))
        if (!isTRUE(all.equal(diff(range(diff(uy))), 0)))
            warning("'y' values are not equispaced; output may be wrong")
        ywid <- mean(diff(uy))
    }
    ncolumns <- length(ux)
    nrows <- length(uy)
    xlow <- ux[1] - 0.5 * xwid
    xhigh <- ux[ncolumns] + 0.5 * xwid
    ylow <- uy[1] - 0.5 * ywid
    yhigh <- uy[nrows] + 0.5 * ywid
    ## create a suitable matrix of colors
    zmat <- rep("transparent", ncolumns * nrows)
    idx <- match(x, ux)
    idy <- match(y, rev(uy)) # image goes top to bottom
    id <- idy + nrows * (idx-1L)
    zmat[id] <- zcol
    dim(zmat) <- c(nrows, ncolumns)
    rasterGrob(as.raster(zmat), interpolate = interpolate,
                x = xlow, y = ylow,
                width = xhigh - xlow, height = yhigh - ylow,
                just = c("left", "bottom"),
                default.units = "native",
                name = trellis.grobname(paste(identifier, "raster", sep="."),
                                        type = "panel", group = group))
}
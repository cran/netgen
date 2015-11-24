getSummaryOfInstances = function(path) {
  # in the first level we expect only directories, each directory containing the
  # corresponding instances
  files = list.files(path, full.names = TRUE)
  source.dirs = Filter(function(x) testDirectory(x, access = "r"), files)
  print(source.dirs)
  x = lapply(source.dirs, function(source.dir) {
    catf("Working on directory: '%s'", source.dir)
    meta = getInstanceOverview(source.dir)
    if (is.null(meta)) {
      return(NULL)
    }
    meta$type = basename(source.dir)
    #FIXME: eventually filter functions?
    return(meta)
  })
  x = Filter(function(y) !is.null(y), x)

  nns = getCommonNames(x)
  #print(do.call(rbind, lapply(x, function(x) x[nns])))
  #stop()

  res = do.call(rbind, lapply(x, function(x) x[nns]))
  return(res)
}

getCommonNames = function(x) {
  # get properties
  ns = lapply(x, names)

  # reduce to minimal common set of names
  nns = ns[[1]]
  for (i in 1:length(ns)) {
    nns = intersect(nns, ns[[i]])
  }
  return(nns)
}

getInstanceOverview = function(path) {
  assertDirectory(path, access = "r")
  source.files = list.files(path, full.names = TRUE, pattern = "*.tsp$")
  if (length(source.files) == 0L) {
    return(NULL)
  }
  #FIXME: ability to filter
  x = lapply(source.files, function(source.file) {
    fh = file(source.file, "r")
    specs = readSpecificationPart(fh, list())
    specs = lapply(specs, function(x) {
      if (length(x) > 1L) return(collapse(x)) else return(x)
    })
    close(fh)
    return(specs)
  })

  nns = getCommonNames(x)

  # extract common specs and build up nice data frame
  x = as.data.frame(do.call(rbind, lapply(x, function(x) unlist(x[nns]))))

  return(x)
}


#res = getInstanceOverview("../../../instances/reduced_instances")
# library(ggplot2)
# pl = ggplot(res, aes(x = edge_weight_type, y = as.numeric(dimension))) + geom_boxplot()


#res = getSummaryOfInstances("../../../instances")


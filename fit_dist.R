compute_fits = function (infiles) {
    dists = c("gamma", "exp", "weibull")
    for (i in 1:length(infiles)) {
        print(infiles[i])
        data = read.table(infiles[i])$V1
        #descdist(data,boot=100)
        fits = list()
        for (j in 1:length(dists)) {
            dist = dists[j]
            if (dist == "weibull") {
                f = fitdist(data, dist, lower=0, upper=1, start=0.1)
            } else {
                f = fitdist(data, dist)
            }
            fits[[dist]] = f
        }
        #print(fits)
        #cdfcomp(fits, legendtext = names(fits))
        print(gofstat(fits, fitnames=names(fits)))[['aic']]
    }
}
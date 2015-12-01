rm(list=ls())
files = c("mass_postrb.txt", "pd_postrb.txt", "tdt_postrb.txt")
data = read.table(files[2])$V1
descdist(data,boot=100)
dists = list("gamma", "exp", "weibull")
fits = list()
for (i in 1:length(dists)) {
    dist = dists[[i]]
    if (dist == "weibull") {
        f = fitdist(data, dist, lower=0, upper=1, start=0.1)
    } else {
        f = fitdist(data, dist)
    }
    fits[[dist]] = f
}
cdfcomp(fits, legendtext = names(fits))
print(gofstat(fits, fitnames=names(fits)))
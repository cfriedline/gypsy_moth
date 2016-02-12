### gypsy moth phenotypes

dat <- read.delim("/Users/professor/Desktop/papers/gypsy_moth/phenotypes_gypsy_moth.txt")

means <- matrix(nrow = length(unique(dat$Population)), ncol = 3)

gmean <- c(mean(dat[,3]),mean(dat[,4]),mean(dat[,5]))

sems <- matrix(nrow = length(unique(dat$Population)), ncol = 3)

pops <- unique(dat$Population)

for(i in 1:6) {
	
	dat_temp <- dat[which(dat$Population == pops[i]),]
	
	means[i,1] <- mean(dat_temp[,3])
	means[i,2] <- mean(dat_temp[,4])
	means[i,3] <- mean(dat_temp[,5])
	
	sems[i,1] <- sd(dat_temp[,3])/sqrt(nrow(dat_temp))
	sems[i,2] <- sd(dat_temp[,4])/sqrt(nrow(dat_temp))
	sems[i,3] <- sd(dat_temp[,5])/sqrt(nrow(dat_temp))
	
	}

or <- c("NC","VA1","VA2","NY","QC93","QC32") ### this is south to north

coor_trim <- matrix(nrow = 6,ncol = 2)

for(i in 1:6) {
    
    coor_trim[i,1] <- dat[which(dat$Population == or[i]),c("lon")][1]
    
    coor_trim[i,2] <- dat[which(dat$Population == or[i]),c("lat")][1]
    
    }

means <- means[c(1,5,6,2,4,3),]

sems <- sems[c(1,5,6,2,4,3),]

phen <- dat[,c(3:5)]

phen[,1] <- scale(phen[,1])
phen[,2] <- scale(phen[,2])
phen[,3] <- scale(phen[,3])

phen2 <- as.matrix(phen)

out <- manova(phen2 ~ as.factor(dat$Population))

res <- summary(out, test="Wilks")

res.aov <- summary.aov(out)

### lme4 analysis to get VC for Pst

library(lme4)

mod1 <- lmer(phen[,1] ~ (1|dat$Population))

mod2 <- lmer(phen[,2] ~ (1|dat$Population))

mod3 <- lmer(phen[,3] ~ (1|dat$Population))

pst_mass <- VarCorr(mod1)[[1]][1]/(VarCorr(mod1)[[1]][1]+attr(VarCorr(mod1), "sc")^2)
pst_pdt <- VarCorr(mod2)[[1]][1]/(VarCorr(mod2)[[1]][1]+attr(VarCorr(mod2), "sc")^2)
pst_tdt <- VarCorr(mod3)[[1]][1]/(VarCorr(mod3)[[1]][1]+attr(VarCorr(mod3), "sc")^2)

### Get parametric bs_cis for pst values

pst_mass_sim <- numeric(1000)

pst_pdt_sim <- numeric(1000)

pst_tdt_sim <- numeric(1000)

for(i in 1:1000) {
    
    mass <- simulate(mod1)
    
    pdt <- simulate(mod2)
    
    tdt <- simulate(mod3)
    
    fit_mass <- lmer(mass[,1] ~ (1|dat$Population))
    
    fit_pdt <- lmer(pdt[,1] ~ (1|dat$Population))
    
    fit_tdt <- lmer(tdt[,1] ~ (1|dat$Population))
    
    pst_mass_sim[i] <- VarCorr(fit_mass)[[1]][1]/(VarCorr(fit_mass)[[1]][1]+attr(VarCorr(fit_mass), "sc")^2)
    
    pst_pdt_sim[i] <- VarCorr(fit_pdt)[[1]][1]/(VarCorr(fit_pdt)[[1]][1]+attr(VarCorr(fit_pdt), "sc")^2)
    
    pst_tdt_sim[i] <- VarCorr(fit_tdt)[[1]][1]/(VarCorr(fit_tdt)[[1]][1]+attr(VarCorr(fit_tdt), "sc")^2)
    
    #print(i) ### I use print statements to figure out where issues arise then turn them off once debugging is done.
    
    }

pst_mass_ci <- quantile(pst_mass_sim, probs = c(0.025, 0.975))

pst_pdt_ci <- quantile(pst_pdt_sim, probs = c(0.025, 0.975))

pst_tdt_ci <- quantile(pst_tdt_sim, probs = c(0.025, 0.975))

### make plots about diversity and junk

par(mfrow=c(1,3))

plot(x = means[,1], y = c(1:6), xlim = c(0.20,0.35), pch = 19, cex = 1.5, xlab = "Pupal Mass", ylab = "Population", yaxt = "n"); axis(side = 2, at = c(1:6), labels = or)

for(i in 1:6) {
	
	segments(x0=means[i,1] - 1.96*sems[i,1],y0=i,x1 = means[i,1] + 1.96*sems[i,1],y1=i)
	
	}

abline(v = gmean[1], lty = "dashed")

text(x = 0.225, y = 6.0, label = expression('P'[ST]*' = 0.0707'), cex = 1.5)

text(x = 0.225, y = 5.75, label = expression('F'['5,164']*' = 3.1200'), cex = 1.5)

text(x = 0.225, y = 5.50, label = "P = 0.0102", cex = 1.5)

plot(x = means[,2], y = c(1:6), pch = 19, xlim = c(8,12), cex = 1.5, xlab = "Pupal Development Time", ylab = "Population", yaxt = "n"); axis(side = 2, at = c(1:6), labels = or)

for(i in 1:6) {
	
	segments(x0=means[i,2] - 1.96*sems[i,2],y0=i,x1 = means[i,2] + 1.96*sems[i,2],y1=i)
	
	}

abline(v = gmean[2], lty = "dashed")

text(x = 8.5, y = 6.00, label = expression('P'[ST]*' = 0.0324'), cex = 1.5)

text(x = 8.5, y = 5.75, label = expression('F'['5,164']*' = 1.9480'), cex = 1.5)

text(x = 8.5, y = 5.50, label = "P = 0.0891", cex = 1.5)

plot(x = means[,3], y = c(1:6), pch = 19, xlim = c(65,80), cex = 1.5, xlab = "Total Development Time", ylab = "Population", yaxt = "n"); axis(side = 2, at = c(1:6), labels = or)

for(i in 1:6) {
	
	segments(x0=means[i,3] - 1.96*sems[i,3],y0=i,x1 = means[i,3] + 1.96*sems[i,3],y1=i)
	
	}

abline(v = gmean[3], lty = "dashed")

text(x = 67.5, y = 6.00, label = expression('P'[ST]*' = 0.2288'), cex = 1.5)

text(x = 67.5, y = 5.75, label = expression('F'['5,164']*' = 8.3543'), cex = 1.5)

text(x = 67.5, y = 5.50, label = "P = 4.6580e-07", cex = 1.5)

### Plot of TDT v MASS

plot(means[,1], means[,3], pch = 19, cex = 1.25, xlim = c(0.22,0.33), ylim = c(67,80), xlab = "Pupal Mass", ylab = "Total Development Time")

for(i in 1:6) {
    
    segments(x0=means[i,1] - 1.96*sems[i,1], y0 = means[i,3], x1 = means[i,1] + 1.96*sems[i,1], y1 = means[i,3])
    
    segments(x0 = means[i,1], y0 = means[i,3] - 1.96*sems[i,3], x1 = means[i,1], y1 = means[i,3] + 1.96*sems[i,3])
    
    }

abline(lm(means[,3]~means[,1]), lty = "dashed")

text(x = 0.23, y = 80, label = expression('R'^2*' = 0.612'))

text(x = 0.23, y = 79, label = expression('F'['1,4']*' = 6.303'))

text(x = 0.23, y = 78, label = "P = 0.066")

### RDA, use only lat and lon

library(vegan)

env <- dat[,8:26]

for(i in 1:ncol(env)) {env[,i] <- scale(env[,i])}

geo <- dat[,6:7]

for(i in 1:ncol(geo)) {geo[,i] <- scale(geo[,i])}

gen <- dat[,27:ncol(dat)]

for(i in 1:ncol(gen)) {gen[,i] <- scale(gen[,i])}

env_pc <- prcomp(env)$x[,1:2] ### These two have eigenvalues > 1 and acct for ~95% of the variance.

all <- cbind(env_pc, gen, geo)

colnames(all) <- c("ePC1", "ePC2", "gPC1", "gPC2", "gPC3", "gPC4", "gPC5", "gPC6", "gPC7","gPC8", "gPC9", "gPC10", "gPC11", "gPC12", "gPC13", "gPC14", "gPC15", "lon", "lat")

rda_full <- rda(X = phen, Y = all)

indivs <- summary(rda_full)[[2]][,1:2]

traits <- summary(rda_full)[[1]][,1:2]

sc <- 4.745172

vars <- summary(rda_full)[[4]][,1:2] * sc

cols <- character(nrow(phen))

library(RColorBrewer)

#colsg <- brewer.pal(n = 9, "Greens")

#cols[which(dat$Population == "NC")] <- colsg[3]

#cols[which(dat$Population == "VA1")] <- colsg[4]

#cols[which(dat$Population == "VA2")] <- colsg[5]

#cols[which(dat$Population == "NY")] <- colsg[6]

#cols[which(dat$Population == "QC93")] <- colsg[7]

#cols[which(dat$Population == "QC32")] <- colsg[8]

plot(indivs, pch = 19, col = "light gray", xlim =c(-4, 3), ylim = c(-3.5,3), cex = 0.75, xlab = "RDA1 (PVE = 62.56)", ylab = "RDA2 (PVE = 24.05)")

abline(v = 0, lty = "dashed")

abline(h = 0, lty = "dashed")
    
#points(traits, pch = 3, cex = 2, col = "red", lwd = 1.5)

text(traits[1,1], traits[1,2], "Mass", col = "red")

text(traits[2,1], traits[2,2], "PD", col = "red")

text(traits[3,1], traits[3,2], "TDT", col = "red")

cols <- c("blue1", "blue1", rep("black", 15), "burlywood3", "burlywood3")

for(i in 1:nrow(vars)) {
    
    arrows(x0 = 0, y0 = 0, x1 = vars[i,1], y1 = vars[i,2], length = 0.10, col = cols[i])
    
    }

for(i in 1:nrow(vars)) {
    
    tx <- row.names(vars)[i]
    
    fac <- vars[i,2]/vars[i,1]
    
    text(vars[i,1] + (0.15*vars[i,1]), vars[i,2] + ((0.15*vars[i,1])*fac), tx, col = cols[i], cex = 0.85)
    
    }

text(x = -3.25, y = 3, expression('F'['19,150']*' = 2.369'))

text(x = -3.25, y = 2.75, expression('P = 1.0e-04'))

text(x = -3.25, y = 2.5, label = expression('R'^2*' = 0.231'))

as.matrix(gen) -> gen

as.matrix(geo) -> geo

### Partial effect combo 1: gen and (env_pc & geo)

### Four models in combo 1:

c1_1 <- rda(phen ~ gen + Condition(geo + env_pc))
c1_2 <- rda(phen ~ geo + env_pc)
c1_3 <- rda(phen ~ geo + env_pc + Condition(gen))
c1_4 <- rda(phen ~ gen)

### Partial effect combo 2: geo and (env_pc & gen)

### Four models in combo 2:

c2_1 <- rda(phen ~ geo + Condition(gen + env_pc))
c2_2 <- rda(phen ~ gen + env_pc)
c2_3 <- rda(phen ~ gen + env_pc + Condition(geo))
c2_4 <- rda(phen ~ geo)

### Partial effect combo 3: env_pc and (geog & soil)

c3_1 <- rda(phen ~ env_pc + Condition(geo + gen))
c3_2 <- rda(phen ~ geo + gen)
c3_3 <- rda(phen ~ geo + gen + Condition(env_pc))
c3_4 <- rda(phen ~ env_pc)

### Pure effects

env_pure <- RsquareAdj(c3_1)$r.squared

gen_pure <- RsquareAdj(c1_1)$r.squared

geo_pure <- RsquareAdj(c2_1)$r.squared

### Pairwise joint effects

gen_geo <- RsquareAdj(c3_3)$r.squared - gen_pure - geo_pure

gen_env <- RsquareAdj(c2_3)$r.squared - gen_pure - env_pure

geo_env <- RsquareAdj(c1_3)$r.squared - geo_pure - env_pure

### Three-way effects

gen_env_geo <- (RsquareAdj(c1_2)$r.squared - RsquareAdj(c1_3)$r.squared) - gen_geo - gen_env

env_pure + gen_pure + geo_pure + gen_geo + gen_env + geo_env + gen_env_geo -> test

pve <- (c(env_pure, gen_pure, geo_pure, gen_geo, gen_env, geo_env, gen_env_geo)/test)*100

nams <- c("Env|(Gen & Geo)", "Gen|(Env & Geo)", "Geo|(Gen & Env)", "Joint Gen/Geo", "Joint Gen/Env", "Joint Env/Geo", "Joint all")

barplot(pve, ylim = c(-3, 80), names.arg = nams, ylab = "Portion of Total Effect (%)"); box(); abline(h = 0)

barplot(pve, plot = F) -> bill

text(x = bill[1,1], y = pve[1] + 2.5, expression('P = 0.219'))

text(x = bill[2,1], y = pve[2] + 2.5, expression('P = 6.0e-04'))

text(x = bill[3,1], y = pve[3] + 2.5, expression('P = 0.006'))



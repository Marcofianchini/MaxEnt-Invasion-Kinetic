bestopt
m <- e.mx@models[[bestopt$tune.args]]
###  Extract the models from results and save them temporally to parallelize the prediction process smoothly
mnms <- names(e.mx@models)
mdls <- e.mx@models
foreach(i = 1:length(mnms)) %do% {
  #if(!file.exists(paste0('temp/',names(mdls)[i],'.rds'))){
    saveRDS(mdls[[i]], paste0('temp/',names(mdls)[i],'.rds'))
  #} else{ print('model already saved')}
  
}
envs.df<-as.data.frame(envs.final,na.rm=TRUE)
no_cores <- 5  # 
registerDoParallel(cores=no_cores)
rm(e.mx)
rm(mdls) # we don't need them now and are big files
###  if you have fitted the models with spatRaster data, you may not need this. You can extract present predictions from e.mx@predictions 
rp <- foreach(i = 1:length(mnms), .combine='cbind') %dopar% {
  m <- readRDS(paste0('temp/',mnms[i],'.rds'))
  p <- dismo::predict(m, envs.df, args = 'outputformat=cloglog')
  return(p)
}
closeAllConnections()
names(rp) <- mnms
df<-as.data.frame(rp)
colnames(df) <- mnms
df <- na.omit(df)
saveRDS(df, file = paste0('results/',expname,'_','present_predictions.rds'))
bestopt.pdf.p <- df[,bestopt$tune.args]

lgth <- length(names(envs.final))-2

# compute for the future 
fut.cond <- stack(paste0('rasters/','future3050_raster.tif'))

fut.cond.df <- as.data.frame(fut.cond, na.rm=TRUE)
no_cores <- 5  # 
registerDoParallel(cores=no_cores)
rf <- foreach(i = 1:length(mnms), .combine='cbind') %dopar% {
  m <- readRDS(paste0('temp/',mnms[i],'.rds'))
  p <- dismo::predict(m, fut.cond.df, args = 'outputformat=cloglog')
  return(p)
}
closeAllConnections()
colnames(rf) <- mnms
rf<-as.data.frame(rf)
saveRDS(rf, file = paste0('results/',expname,'_','future_predictions.rds'))
bestopt.pdf.f <- rf[,bestopt$tune.args]

# load the classification matrices 
int_mx <- readRDS(paste0('results/',expname,'_','int_mx.rds'))
int_mx_lower <- readRDS(paste0('results/',expname,'_','int_mx_lower.rds'))
int_mx_upper <- readRDS(paste0('results/',expname,'_','int_mx_upper.rds'))

png(paste0('plots/','present_suitability_pdf.png'), width = 1600, height = 1200)
#leave a little margin for axis 
par(mar=c(10,10,2,2), mgp = c(6, 2.5, 0))
plot(density(bestopt.pdf.p, from = 0, to = 1), col = 'darkblue', lwd = 2, main = NA, xlab = 'Suitability score', ylab = 'Density', ylim = c(0,20), cex.lab=4.5, cex.axis=4)

# if want to add a bg color for contrast
#rect(par("usr")[1], par("usr")[3],
#     par("usr")[2], par("usr")[4],
#     col = grey(0.9,0.5))
for(i in 1:length(mnms)){
  p <- df[,i]
  lines(density(na.omit(p), from = 0, to = 1), col = alpha('blue',0.02), lwd = 0.01)
}

lines(density(na.omit(bestopt.pdf.p),from = 0, to = 1), col = 'darkblue', lwd = 5)


abline(v = th,lty=1, lwd=2)
abline(v= int_mx[1,2],lty=2)
abline(v= int_mx[2,2],lty=3)
# add a ribbon for the lower-upper bounds of each line
rect(int_mx_upper[2,1],0,int_mx_lower[2,1],par('usr')[4], col = alpha('darkgrey',0.1), border = NA)
rect(int_mx_upper[3,1],0,int_mx_lower[3,1],par('usr')[4], col = alpha('darkgrey',0.1), border = NA)
rect(th_upper,0,th_lower,par('usr')[4], col = alpha('darkgrey',0.1), border = NA)
text(x=mean(int_mx[1,1:2]),y= 20,'no', cex = 4.5, font= 3)
text(x=mean(int_mx[2,1:2]),y= 20,'low',cex= 4.5, font=3)
text(x=mean(int_mx[3,1:2]),y= 20,'mid',cex= 4.5, font=3)
text(x=mean(int_mx[4,1:2]), y=20,'high suitability',cex=4.5, font=3)
legend("right", legend=c("Present", 'maxSSS threshold'), col=c("darkblue", 'black'),lwd=3, lty=1, cex=3,text.font=2)
dev.off()

png(paste0('plots/','future_suitability_pdf.png'), width = 1600, height = 1200)
#leave a little margin for axis 
par(mar=c(10,10,2,2), mgp = c(6, 2.5, 0))
plot(density(bestopt.pdf.f,from = 0, to = 1), col = 'darkred', lwd = 2, main = NA, xlab = 'Suitability score', ylab = 'Density', ylim = c(0,20), cex.lab=4.5, cex.axis=4)
# if want to add a bg color for contrast
#rect(par("usr")[1], par("usr")[3],
#     par("usr")[2], par("usr")[4],
#     col = grey(0.99,0.1))
#
for(i in 1:length(mnms)){
  p <- rf[,i]
  lines(density(na.omit(p[]),from= 0, to = 1), col = alpha('red',0.015), lwd = 1)
}
lines(density(bestopt.pdf.f,from = 0, to = 1), col = 'darkred',lwd=5)


abline(v = th,lty=1, lwd=2)
abline(v= int_mx[1,2],lty=2)
abline(v= int_mx[2,2],lty=3)
# add a ribbon for the lower-upper bounds of each line
rect(int_mx_upper[2,1],0,int_mx_lower[2,1],par('usr')[4], col = alpha('darkgrey',0.1), border = NA)
rect(int_mx_upper[3,1],0,int_mx_lower[3,1],par('usr')[4], col = alpha('darkgrey',0.1), border = NA)
rect(th_upper,0,th_lower,par('usr')[4], col = alpha('darkgrey',0.1), border = NA)
text(x=mean(int_mx[1,1:2]),y= 20,'no', cex = 4.5, font= 3)
text(x=mean(int_mx[2,1:2]),y= 20,'low',cex= 4.5, font=3)
text(x=mean(int_mx[3,1:2]),y= 20,'mid',cex= 4.5, font=3)
text(x=mean(int_mx[4,1:2]), y=20,'high suitability',cex=4.5, font=3)
legend("right", legend=c("Future",'maxSSS threshold'), col=c("darkred",'black'),lwd=3, lty=1, cex=3,text.font=2)
dev.off()






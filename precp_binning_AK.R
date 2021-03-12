library(ncdf4)

#=====================
# import data

pr = list.files("/home/terraces/projects/LPJ_lightning/ymm/", pattern = 'prc', 
                full.names = TRUE)
model_names = gsub('/home/terraces/projects/LPJ_lightning/ymm//prc_Amon_','',pr)
model_names = gsub('_201001-201912_ymm.nc', '', model_names)

wglc = list.files("/home/terraces/projects/LPJ_lightning/wglc_remap/",
                  pattern = paste0(model_names, collapse = '|'), 
                  full.names = TRUE)
landmask = list.files("/home/terraces/projects/LPJ_lightning/landmask/", 
                      pattern = paste0(model_names, collapse = '|'), 
                      full.names = TRUE)

GetNCvars = function(x, varname){
  x = nc_open(x)
  x.var = ncvar_get(x, varname)
  nc_close(x)
  return(x.var)
}

pr.nc =lapply(pr, FUN = GetNCvars, varname = 'prc')
wglc.nc =lapply(wglc, FUN = GetNCvars, varname = 'stroke_density')
landmask.nc =lapply(landmask, FUN = GetNCvars, varname = 'sftlf')


# masking
mask_land = function(mask){
  mask[mask < 100] = 0
  mask=mask /100
  return(mask)
}

landmask.nc = lapply(landmask.nc, FUN = mask_land)

tlen = 12

for (i in 1:length(pr.nc)){
  pr_item=pr.nc[[i]]
  wglc_item=wglc.nc[[i]]
  mask_item= landmask.nc[[i]]
  
  for(t in seq(1:tlen)) {
    print(t)
    pr_item[,,t] = pr_item[,,t] * mask_item
    wglc_item[,,t] = wglc_item[,,t] * mask_item
    
  }
  pr.nc[[i]] = pr_item
  wglc.nc[[i]]= wglc_item
}


# # for loop convert second to monthly
# 
# days = c(31,28,31,30,31,30,31,31,30,31,30,31)
# 
# for (i in 1:length(pr.nc)){
# 
#   for(t in seq(1:tlen)) {
#     print(t)
#     pr.nc[[i]][,,t] = pr.nc[[i]][,,t] *3600*24*days[t]
#     
#   }
#   
# }


# create dataframe

precp_land_df <- list()
wglcland_df <- list()
combined_df <- list()
data_sub <- list()

for (i in 1:length(pr.nc)){
  
  precp_land_df[[i]] <- matrix(NA,nrow=length(pr.nc[[i]]),ncol=1)
  precp_land_df[[i]][,1] <- pr.nc[[i]]
  
  wglcland_df[[i]] <- matrix(NA,nrow=length(wglc.nc[[i]]),ncol=1)
  wglcland_df[[i]][,1] <- wglc.nc[[i]]
  
  #combined_df[[i]] <- matrix(NA,nrow=length(pr.nc[[i]]), ncol=2)
  combined_df[[i]] <- data.frame(wglcland_df[[i]],precp_land_df[[i]]) 
  
  data_sub[[i]] = subset(combined_df[[i]],wglcland_df[[i]] > 0 & precp_land_df[[i]]  > 0)
  
}


# binning


df$bin = cut(df[,1], breaks = 100, labels = FALSE)
df.agg = aggregate.data.frame(df, by = list(bin=df$bin), FUN = mean)
plot(df.agg[,3], df.agg[,2], 'l')


pr_mean <- list()
light_mean <- list()

for (i in 1:length(pr.nc)){
  
    qw <- quantile(data_sub[[i]][,2],prob= seq(0,0.99,0.01))
    pr_mean[[i]] <- tapply(data_sub[[i]][,2], findInterval(data_sub[[i]][,2], qw), mean)
    light_mean[[i]] <- tapply(data_sub[[i]][,1], findInterval(data_sub[[i]][,2], qw), mean)

}

#=====================

# plotting

color_names <- c("navy","slategrey","darkgreen","darkred","purple4","royalblue","cyan4","blueviolet","coral3","darkgoldenrod2",
                 "deeppink3","darkolivegreen3","deepskyblue1","burlywood4")


  plot(light_mean[[1]]~pr_mean[[1]],col="navy",pch=16,type="l",lwd=2,ylim=c(0,0.3),xlab="mean of  precipitation (kg m-2 day-1) ",ylab="mean of WGLC lightning (strokes km-2 month-1)")

  for (i in 1:length(pr.nc)){
  
  points(light_mean[[i]]~pr_mean[[i]],pch=16,col=color_names[i],type="l",lty=1,lwd=2)
  
  }   
    
  par(ps=8);legend(400,0.15,title="models",model_names,col=color_names, pch=19,y.intersp=0.8)
  
  
  
  
  
  
  

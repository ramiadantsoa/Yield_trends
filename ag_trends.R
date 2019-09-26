source('/Users/gola/Box Sync/projects/ACES_ag_trends/code/source_function.R')
setwd("/Users/gola/Box Sync/projects/ACES_ag_trends/data/clean/")
###########################################################
##################### SPATIAL ANALYSES  #####################
###########################################################


####################################################
##################  FIGURE 1   #####################
####################################################

# fit arima model to the mean value 

filename <- paste("all_mean.csv",sep = "")
dat <- read.table(filename, header = FALSE, sep = ",")

out <- data.frame(matrix(, nrow = 9, ncol = 3))
colnames(out) <- c("c0","c1","c2")
rownames(out) <- dat[,1]

for(i in 1:9){
	yval <- as.numeric(dat[i,-1])
	xt  <- 1:48
	xt2 <- xt^2
	m1 <- arima(yval, order = c(1,0,0), xreg = xt, method = "ML")
	m2 <- arima(yval, order = c(1,0,0), xreg = cbind(xt, xt2), method = "ML")

	m21 <- sig(m1,m2, 3.841)
	if(m21 == 1 ){
	param  = c(m2$coef[2:4])
	}else{
	param = c(m1$coef[2:3],0)
	}
	out[i,] <- param
}
write.csv(out,"fit_mean.csv")


####################################################
################# FIGURE 2, 4, 5  ##################
####################################################

CROP <- c("corn","soy","wheat")
CAT <-  c("_unc", "_irr", "_non")

for(cr in CROP){
	for(ct in CAT){ 
		filename <- paste("clean_", cr,ct,sep = "")
		print(filename)
		ag_analysis(filename)
	}
}

####################################################
####################  FIGURE 8  ####################
####################################################

CROP <- c("corn","soy","wheat")
CAT <-  c("_unc", "_irr", "_non")

# dT is for the last dT year
for(dT in c(15,20,25)){
	for(cr in CROP){
		for(ct in CAT){ 
			filename <- paste("clean_", cr,ct,sep = "")
			print(filename)
			ag_analysis_sub(filename, dT)
		}
	}
}


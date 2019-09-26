#### library needed

##### utility functions

# test if complex model (mc) is significantly better than simple model (ms), note that the simple model is inputed first
sig <- function(ms,mc,val){
	temp <- 2*(mc$loglik - ms$loglik)
	result <- 0
	if(temp > val) result <- 1
	return(result)
} 


# returns the opposite of the value that is highest in absolute value. The goal is to allow a symmetric range when plotting a histogram in qgis
maxAbs <- function(vect){
	nv <- as.numeric(vect[which(vect!= "NA")])
	if(length(nv)== 0){
		result <- "NA"
	}
	else{
		x1 <- min(nv)
		x2 <- max(nv)
		if(abs(x1) < abs(x2)) result <- -x2 else result <- -x1
	}
	return(result)
}

# fit a line using arima estimated coefficients 
fit_lin <- function(t,m2){
	coeff <- m2$coef
	result <- coeff[2] + coeff[3]*t
	return (result)
}

# fit a parabola using arima estimated coefficients
fit_quad <- function(t,m3){
	coeff <- m3$coef
	result <- coeff[2] + coeff[3]*t + coeff[4]*t^2
	return (result)
}

# fit the time series with linear and quadratic arima model, and also add the fitted coefficient to a
ag_analysis <- function(filename){
	# filename <- "clean_soy_irr"
	name <- paste(filename,".csv",sep = "")
	dat <- read.table(name, header = TRUE, sep = ",")
	ll <- dim(dat)[1]
	nc <- dim(dat)[2] # this is used for predicting the values

	pdat_lin <- dat # stores the predicted values from the linear regression
	pdat_quad <- dat # stores the predicted values from the quadratic regression 
	res_lin <- dat # stores the residuals from the linear regression 
	res_quad <- dat # stores the residuals from the quadratic regression 
	res_lin_s <- dat # stores the residuals from the linear regression 
	res_quad_s <- dat # stores the residuals from the quadratic regression 

	# for each county, the list of the significant models parameters and set NA for the non-siginficant
	cn <- c("FIPS","sig","c1","sigc1","c2","sd_res","m_resp","m_resn","sd_sres","m_sresp","m_sresn")	

	# stores the coefficients and std of the residual
	ndat <- matrix(NA, nrow = ll, ncol = (length(cn)-1) ) # FIPS will be appended later hence the  -1 
	for (i in 1:ll) {
		# i <- 6
		# print(i)
		vect <- as.numeric(dat[i,2:nc]) # the first column is  for FIPS ID
		pos <- which(is.na(vect) == FALSE)
		
		# pos is used as predictor instead of year because the arima below will become unstable if say x = 2017^2
		xt <- pos
		xt2 <- xt^2
		yval <- as.numeric(vect[pos])
 
		## fit all models ###
		m1 <- arima(yval, order = c(1,0,0), xreg = xt, method = "ML") # fit the linear model
		m2 <- arima(yval, order = c(1,0,0), xreg = cbind(xt, xt2), method = "ML") # fit the quadratic model

		# extract the residuals from the model
		res_l <- m1$residuals
		res_q <- m2$residuals

		# residiuals from the fitted model
		res_lin[i,pos+1] <- res_l
		res_quad[i,pos+1] <- res_q

		# for prediction, minus 1 because the first column is  FIPS id
		val_lin <- fit_lin(1:(nc-1),m1)
		val_quad <- fit_quad(1:(nc-1),m2)

		# predicted values from the fitted model
		pdat_lin[i,2:nc] <- val_lin 
		pdat_quad[i,2:nc] <- val_quad

		# return 1 if the complicated model is significantly better otherwise return 0
		m21 <-sig(m1,m2, 3.841) 

		# stores the significant parameters
		c1 <- m1$coef[3]
		sigc1 <- if(m21 == 0) m1$coef[3] else "NA"
		c2 <- if(m21 == 1) m2$coef[4] else "NA"
		

		# scale the residuals (%) i.e. divide by the predicited values 
		res_l_s <- 100*res_l/val_lin[pos]
		res_q_s <- 100*res_q/val_quad[pos]

		# stores scaled residuals
		res_lin_s[i,pos+1] <- res_l_s
		res_quad_s[i,pos+1] <- res_q_s

		# stores the significant parameters
		sd_res <- if(m21 == 1) sd(res_q, na.rm = TRUE) else sd(res_l, na.rm = TRUE)
		sd_sres <- if(m21 == 1) sd(res_q_s, na.rm = TRUE) else sd(res_l_s, na.rm = TRUE)

		lp <- res_l[which(res_l > 0)]
		ln <- res_l[which(res_l <= 0)]
		qp <- res_q[which(res_q > 0)]
		qn <- res_q[which(res_q <= 0)]

		slp <- res_l_s[which(res_l_s > 0)]
		sln <- res_l_s[which(res_l_s <= 0)]
		sqp <- res_q_s[which(res_q_s > 0)]
		sqn <- res_q_s[which(res_q_s <= 0)]

		# stores the significant parameters
		m_res_p <- if(m21 == 1) mean(qp, na.rm = TRUE) else mean(lp, na.rm = TRUE)
		m_res_n <- if(m21 == 1) mean(qn, na.rm = TRUE) else mean(ln, na.rm = TRUE)
		m_sres_p <- if(m21 == 1) mean(sqp, na.rm = TRUE) else mean(slp, na.rm = TRUE)
		m_sres_n <- if(m21 == 1) mean(sqn, na.rm = TRUE) else mean(sln, na.rm = TRUE)

		# coefficients from significant models
		res <- c(m21, c1, sigc1, c2, sd_res, m_res_p, m_res_n, sd_sres, m_sres_p, m_sres_n)
		
		ndat[i,] <- res
	}

	# used to have symmetric extreme values when plotting the map i.e. 
	max_lin <- maxAbs(ndat[,2])
	max_sig_lin <- maxAbs(ndat[,3])
	max_sig_quad<- maxAbs(ndat[,4])

	frow <- c("NA", max_lin, max_sig_lin, max_sig_quad,"NA","NA","NA", "NA","NA","NA")

	# add the extreme values 
	ndat <- rbind(ndat,frow)
	col <- c(dat[,1],2185) # assign Alsakan FIPS to extreme values (North Slope)

	# add a row to the df (fitted coeff data) to make it easy to plot ranges in qgis
	df <- ndat
	df <- cbind(col,df)
	
	colnames(df) <- cn

	outname1<- paste(filename, "_fit_coeff.csv", sep = "")
	outname2<- paste(filename, "_pred_lin.csv", sep = "")
	outname3<- paste(filename, "_pred_quad.csv", sep = "")
	outname4<- paste(filename, "_res_lin.csv", sep = "")
	outname5<- paste(filename, "_res_quad.csv", sep = "")
	outname6<- paste(filename, "_res_lin_s.csv", sep = "")
	outname7<- paste(filename, "_res_quad_s.csv", sep = "")

	write.table(df,outname1, row.names = FALSE, col.names = TRUE, sep = ",")
	write.table(pdat_lin,outname2, row.names = FALSE, col.names = FALSE, sep = ",")
	write.table(pdat_quad,outname3, row.names = FALSE, col.names = FALSE, sep = ",")
	write.table(res_lin,outname4, row.names = FALSE, col.names = FALSE, sep = ",")
	write.table(res_quad,outname5, row.names = FALSE, col.names = FALSE, sep = ",")
	write.table(res_lin_s,outname6, row.names = FALSE, col.names = FALSE, sep = ",")
	write.table(res_quad_s,outname7, row.names = FALSE, col.names = FALSE, sep = ",")
}

# fit the time series for last 20  years with linear model (last figure) and export mean yield, standard devitation of the residuals, and slope. The differences with the code above are:
# - select the last "duration" year (duration is an interger)
# - the FIPS are attached directly to vector, not appended to the matrix at the end
# - fit only a linear model
# - fitted values are only on the non NA data points
ag_analysis_sub <- function(filename, dT){
	# filename <- "clean_soy_irr"
	# dT <- 20
	name <- paste(filename,".csv",sep = "")
	dat <- read.table(name, header = TRUE, sep = ",")
	ll <- dim(dat)[1]
	nc <- dim(dat)[2] # this is used for predicting the values

	# for each county, the list of the significant models parameters and set NA for the non-siginficant
	cn <- c("FIPS","mean","sd","slope","ssd")	

	# stores the coefficients and std of the residual
	ndat <- c() 
	for (i in 1:ll) {
		# i <- 3
		# print(i)
		fips <- dat[i,1]
		nc0 <- (nc - dT + 1)
		vect <- as.numeric(dat[i,nc0:nc]) # the first column is  for FIPS ID
		pos <- which(is.na(vect) == FALSE)
		
		# only use sites when there are at least 15 points
		if(length(pos) > 9){
			# pos is used as predictor instead of year because the arima below will become unstable if say x = 2017^2
			xt <- pos
			yval <- as.numeric(vect[pos])

			## calculate the mean ##
			me = mean(yval,na.rm = TRUE)
	 
			## fit all models ##
			m1 <- arima(yval, order = c(1,0,0), xreg = xt, method = "ML") # fit the linear model

			slope <- as.numeric(m1$coef[3])

			## calculate the standard deviation of the scaled residuals ##
			# extract the residuals from the model
			res_l <- m1$residuals
			# fit values
			val_lin <- fit_lin(xt,m1)
			# calculated scaled residuals (%)
			res_l_s <- 100*res_l/val_lin

			sd <- sd(res_l, na.rm = TRUE)
			ssd <- sd(res_l_s, na.rm = TRUE)

			res <- c(fips, me, sd, slope, ssd)
			
			# append to the data
			ndat <- rbind(ndat, res)
		}
	}
	colnames(ndat) <- cn

	outname1<- paste(filename,"_",dT, "_sum.csv", sep = "")

	write.table(ndat,outname1, row.names = FALSE, col.names = TRUE, sep = ",")
}

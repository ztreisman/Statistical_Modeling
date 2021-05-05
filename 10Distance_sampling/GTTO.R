setwd("E:/IMBCR")

install.packages("Distance")
install.packages("readxl")

library(Distance)
library(readxl)

GTTO<- data.frame(read_excel("IMBCR_GUFO_master (for R).xlsx", 
                             sheet = "GTTO", col_types = c("date", 
                                                           "numeric", "text", "skip", "skip", 
                                                           "text", "numeric", "numeric", "text", "text", 
                                                           "text", "numeric", "numeric", "numeric","skip", "skip")))
names(GTTO)<-sub("Observer", "obs", names(GTTO))
names(GTTO)<-sub("BirdCode", "detection", names(GTTO))
names(GTTO)<-sub("radialDistance", "distance", names (GTTO))
names(GTTO)<-sub("CL_Count", "groupsize", names(GTTO))
names(GTTO)<-sub("Full_ID", "siteID", names(GTTO))
names(GTTO)<-sub("RealID", "Sample.Label", names(GTTO))
names(GTTO)<-sub("gridID", "Region.Label", names(GTTO))
names(GTTO)<-sub("effort", "Effort", names(GTTO))
View(GTTO)

#flat files
GTTOflatgrid<-(data.frame(read_excel("IMBCR_GUFO_master (for R).xlsx", 
                                     sheet = "GTTO", col_types = c("skip", 
                                                                   "skip", "text", "skip", "skip", "skip", 
                                                                   "numeric", "skip", "text", "skip", "text", "numeric", "skip", "numeric","skip", "skip"))))

names(GTTOflatgrid)<-sub("radialDistance", "distance", names (GTTOflatgrid))
names(GTTOflatgrid)<-sub("RealID", "Sample.Label", names (GTTOflatgrid))
names(GTTOflatgrid)<-sub("gridID", "Region.Label", names (GTTOflatgrid))
names(GTTOflatgrid)<-sub("effort", "Effort", names (GTTOflatgrid))
names(GTTOflatgrid)<-sub("gridArea", "Area", names (GTTOflatgrid))
names(GTTOflatgrid)<-sub("Observer", "obs", names (GTTOflatgrid))

GTTOflatpt<-(data.frame(read_excel("IMBCR_GUFO_master (for R).xlsx", 
                                   sheet = "GTTO", col_types = c("skip", 
                                                                 "skip", "text", "skip", "skip", "skip", 
                                                                 "numeric", "skip", "text", "skip", "text", "numeric", "numeric", "skip","skip", "skip"))))

names(GTTOflatpt)<-sub("radialDistance", "distance", names (GTTOflatpt))
names(GTTOflatpt)<-sub("RealID", "Sample.Label", names (GTTOflatpt))
names(GTTOflatpt)<-sub("gridID", "Region.Label", names (GTTOflatpt))
names(GTTOflatpt)<-sub("effort", "Effort", names (GTTOflatpt))
names(GTTOflatpt)<-sub("ptArea", "Area", names (GTTOflatpt))
names(GTTOflatpt)<-sub("Observer", "obs", names (GTTOflatpt))

# What does data look like
head(GTTO, n=1351)
conversion.factor <- convert_units("meter", NULL, "hectare")

# Fit half-normal detection function, no truncation
GTTO.hn <- ds(data=GTTO, transect="point", key="hn",
              convert.units=conversion.factor)
plot(GTTO.hn, pdf=TRUE)


# Try truncation of 100m based on preliminary fit

# Half normal, no adjustments
GTTO.hn.t100m <- ds(data=GTTO, transect="point", key="hn", truncation=100,
                    convert.units=conversion.factor)
plot(GTTO.hn.t100m, main = "Half-normal, no adjustments", pdf=TRUE)

# Half normal, cos
GTTO.hn.cos.t100m <- ds(data=GTTO, transect="point", key="hn", adjustment = "cos", truncation=100,
                        convert.units=conversion.factor)
plot(GTTO.hn.cos.t100m, main = "Half-normal, cosine", pdf=TRUE)

# Half normal, herm
GTTO.hn.herm.t100m <- ds(data=GTTO, transect="point", key="hn", adjustment = "herm", truncation=100,
                         convert.units=conversion.factor)
plot(GTTO.hn.herm.t100m, main = "Half-normal, hermite", pdf=TRUE)

# Half normal, poly
GTTO.hn.poly.t100m <- ds(data=GTTO, transect="point", key="hn", adjustment = "poly", truncation=100,
                         convert.units=conversion.factor)
plot(GTTO.hn.poly.t100m, main = "Half-normal, polynomial", pdf=TRUE)

# Hazard rate, no adjustments
GTTO.hr.t100m <- ds(data=GTTO, transect="point", key="hr", truncation=100,
                    convert.units=conversion.factor)
plot(GTTO.hr.t100m, main = "Hazard Rate, no adjustments", pdf=TRUE)

# Hazard rate, cos
GTTO.hr.cos.t100m <- ds(data=GTTO, transect="point", key="hr", adjustment = "cos", truncation=100,
                        convert.units=conversion.factor)
plot(GTTO.hr.cos.t100m, main = "Hazard Rate, cosine", pdf=TRUE)

# Hazard rate, herm
GTTO.hr.herm.t100m <- ds(data=GTTO, transect="point", key="hr", adjustment = "herm", truncation=100,
                         convert.units=conversion.factor)
plot(GTTO.hr.herm.t100m, main = "Hazard Rate, hermite", pdf=TRUE)

# Hazard rate, poly
GTTO.hr.poly.t100m <- ds(data=GTTO, transect="point", key="hr", adjustment = "poly", truncation=100,
                         convert.units=conversion.factor)
plot(GTTO.hr.poly.t100m, main = "Hazard Rate, polynomial", pdf=TRUE)


GTTO.tab <- data.frame(DetectionFunction=c("Half-normal","Half-normal",
                                           "Half-normal","Half-normal",
                                           "Hazard Rate","Hazard Rate"
                                           ,"Hazard Rate","Hazard Rate"), 
                       Adjustments=c("None","Cosine","Hermite","Polynomial",
                                     "None","Cosine","Hermite","Polynomial"), Truncation=c(100,100,100,100,100,100,100,100), 
                       AIC=rep(NA,8), Density=rep(NA,8), D.CV=rep(NA,8), Lower.CI=rep(NA,8), Upper.CI=rep(NA,8))

get.results.f <- function(fit.model) {
        list(AIC=summary(fit.model$ddf)$aic,
             D=summary(fit.model$ddf)$average.p,
             D.CV=summary(fit.model$ddf)$average.p.se/summary(fit.model$ddf)$average.p,
             lCL=summary(fit.model$ddf)$average.p - 1.96*(summary(fit.model$ddf)$average.p.se),
             uCL=summary(fit.model$ddf)$average.p + 1.96*(summary(fit.model$ddf)$average.p.se))
}

GTTO.tab[1,4:8] <- get.results.f(GTTO.hn.t100m)
GTTO.tab[2,4:8] <- get.results.f(GTTO.hn.cos.t100m)
GTTO.tab[3,4:8] <- get.results.f(GTTO.hn.herm.t100m)
GTTO.tab[4,4:8] <- get.results.f(GTTO.hn.poly.t100m)
GTTO.tab[5,4:8] <- get.results.f(GTTO.hr.t100m)
GTTO.tab[6,4:8] <- get.results.f(GTTO.hr.cos.t100m)
GTTO.tab[7,4:8] <- get.results.f(GTTO.hr.herm.t100m)
GTTO.tab[8,4:8] <- get.results.f(GTTO.hr.poly.t100m)


# Print results
knitr::kable(GTTO.tab, caption="Results from simulated point transect data.", digits=3)

# Plot detection functions
par(mfrow=c(2,2))
plot(GTTO.hn, main="Half normal, no truncation")
plot(GTTO.hn.t100m, main="Half normal, truncation 100m")
plot(GTTO.hr.t100m, main="Hazard rate, truncation 100m")



# Plot probability density functions
par(mfrow=c(2,2))
plot(GTTO.hn, main="Half normal, no truncation", pdf=TRUE)
plot(GTTO.hn.t100m, main="Half normal, truncation 100m", pdf=TRUE)
plot(GTTO.hr.t100m, main="Hazard rate, truncation 100m", pdf=TRUE)


#Observer as covariate
par(mfrow=c(1,1))
GTTO$fyear<-factor(GTTO$year)

GTTO.hn.obs <- ds(data=GTTO, transect="point", formula = ~obs, key="hn", truncation = 100,  
                  convert.units=conversion.factor)
plot(GTTO.hn.obs, main = "half-normal: obs covariate, 100m trunc", showpoints = FALSE, pdf=TRUE)
add_df_covar_line(GTTO.hn.obs, data.frame(obs= "KROSS"),col = "green", pdf = TRUE)
add_df_covar_line(GTTO.hn.obs, data.frame(obs= "PMAGE"), col = "blue", pdf = TRUE)
add_df_covar_line(GTTO.hn.obs, data.frame(obs= "KBROD"), col = "red", pdf =TRUE)

GTTO.hr.obs <- ds(data=GTTO, transect="point", formula = ~obs, key="hr", truncation = 100,   
                  convert.units=conversion.factor)
plot(GTTO.hr.obs, main = "hazard rate: obs covariate, 100m trunc", showpoints = FALSE, pdf=TRUE)
add_df_covar_line(GTTO.hr.obs, data.frame(obs= "KROSS"),col = "green", pdf = TRUE)
add_df_covar_line(GTTO.hr.obs, data.frame(obs= "PMAGE"), col = "blue", pdf = TRUE)
add_df_covar_line(GTTO.hr.obs, data.frame(obs= "KBROD"), col = "red", pdf =TRUE)
Year
GTTO.hn.yr <- ds(data=GTTO, transect="point", formula = ~fyear, key="hn", truncation = 100,  
                 convert.units=conversion.factor)
plot(GTTO.hn.yr, main = "half-normal: year covariate, 100m trunc", showpoints = FALSE, pdf=TRUE)
add_df_covar_line(GTTO.hn.yr, data.frame(fyear = "2018"),col = "purple", pdf = TRUE)
add_df_covar_line(GTTO.hn.yr, data.frame(fyear = "2019"), col = "orange", pdf = TRUE)
add_df_covar_line(GTTO.hn.yr, data.frame(fyear = "2020"), col = "navy", pdf =TRUE)

GTTO.hr.yr <- ds(data=GTTO, transect="point", formula = ~fyear, key="hr", truncation = 100,   
                 convert.units=conversion.factor)
plot(GTTO.hr.yr, main = "hazard rate: year covariate, 100m trunc", showpoints = FALSE, pdf=TRUE)
add_df_covar_line(GTTO.hr.yr, data.frame(fyear = "2018"),col = "purple", pdf = TRUE)
add_df_covar_line(GTTO.hr.yr, data.frame(fyear = "2019"), col = "orange", pdf = TRUE)
add_df_covar_line(GTTO.hr.yr, data.frame(fyear = "2020"), col = "navy", pdf =TRUE)



# Observer and Year as covariates
GTTO.hn.t100m <- ds(data=GTTO, transect="point", formula = ~obs+fyear, key="hn", truncation = 100,  
                    convert.units=conversion.factor)
plot(GTTO.hn.t100m, main = "half-normal, 100m trunc", showpoints = FALSE, pdf=TRUE)
add_df_covar_line(GTTO.hn.t100m, data.frame(obs= "KROSS"),col = "green", pdf = TRUE)
add_df_covar_line(GTTO.hn.t100m, data.frame(obs= "PMAGE"), col = "blue", pdf = TRUE)
add_df_covar_line(GTTO.hn.t100m, data.frame(obs= "KBROD"), col = "red", pdf =TRUE)
add_df_covar_line(GTTO.hn.t100m, data.frame(fyear = "2018"),col = "purple", pdf = TRUE)
add_df_covar_line(GTTO.hn.t100m, data.frame(fyear = "2019"), col = "orange", pdf = TRUE)
add_df_covar_line(GTTO.hn.t100m, data.frame(fyear = "2020"), col = "navy", pdf =TRUE)


GTTO.hr.t100m <- ds(data=GTTO, transect="point", formula = ~obs+fyear, key="hr", truncation=100, 
                    convert.units=conversion.factor)
plot(GTTO.hr.t100m, main = "hazard rate, 100m trunc", showpoints = FALSE, pdf=TRUE)
add_df_covar_line(GTTO.hr.t100m, data.frame(obs= "KROSS"),col = "green", pdf = TRUE)
add_df_covar_line(GTTO.hr.t100m, data.frame(obs= "PMAGE"), col = "blue", pdf = TRUE)
add_df_covar_line(GTTO.hr.t100m, data.frame(obs= "KBROD"), col = "red", pdf =TRUE)
add_df_covar_line(GTTO.hr.t100m, data.frame(fyear = "2018"),col = "purple", pdf = TRUE)
add_df_covar_line(GTTO.hr.t100m, data.frame(fyear = "2019"), col = "orange", pdf = TRUE)
add_df_covar_line(GTTO.hr.t100m, data.frame(fyear = "2020"), col = "navy", pdf =TRUE)


#table of results with covar
GTTO.tab <- data.frame(DetectionFunction=c("Half-normal","Half-normal","Half-normal", "Hazard Rate", "Hazard Rate", "Hazard Rate"), 
                       Covariate=c("Obs","Year","Obs+Year","Obs","Year","Obs+Year"), Truncation=c(100, 100, 100, 100, 100, 100), 
                       AIC=rep(NA,6), Density=rep(NA,6), D.CV=rep(NA,6), Lower.CI=rep(NA,6), Upper.CI=rep(NA,6))

get.results.f <- function(fit.model) {
        list(AIC=summary(fit.model$ddf)$aic,
             D=summary(fit.model$ddf)$average.p,
             D.CV=summary(fit.model$ddf)$average.p.se/summary(fit.model$ddf)$average.p,
             lCL=summary(fit.model$ddf)$average.p - 1.96*(summary(fit.model$ddf)$average.p.se),
             uCL=summary(fit.model$ddf)$average.p + 1.96*(summary(fit.model$ddf)$average.p.se))
}
GTTO.tab[1,4:8]<-get.results.f(GTTO.hn.obs) 
GTTO.tab[2,4:8]<-get.results.f(GTTO.hn.yr) 
GTTO.tab[3,4:8]<-get.results.f(GTTO.hn.t100m) 
GTTO.tab[4,4:8]<-get.results.f(GTTO.hr.obs) 
GTTO.tab[5,4:8]<-get.results.f(GTTO.hr.yr) 
GTTO.tab[6,4:8]<-get.results.f(GTTO.hr.t100m)

# Print results
knitr::kable(GTTO.tab, caption="Results from simulated point transect data with covariates (year, observer).", digits=3)

#change truncation
#Observer as covariate
par(mfrow=c(1,1))
GTTO$fyear<-factor(GTTO$year)
GTTO.hn.obs.t125 <- ds(data=GTTO, transect="point", formula = ~obs, key="hn", truncation = 125,  
                  convert.units=conversion.factor)
plot(GTTO.hn.obs.t125, main = "half-normal: obs covariate, 125m trunc", showpoints = FALSE, pdf=TRUE)
add_df_covar_line(GTTO.hn.obs.t125, data.frame(obs= "KROSS"),col = "green", pdf = TRUE)
add_df_covar_line(GTTO.hn.obs.t125, data.frame(obs= "PMAGE"), col = "blue", pdf = TRUE)
add_df_covar_line(GTTO.hn.obs.t125, data.frame(obs= "KBROD"), col = "red", pdf =TRUE)

GTTO.hr.obs.t125 <- ds(data=GTTO, transect="point", formula = ~obs, key="hr", truncation = 125,   
                  convert.units=conversion.factor)
plot(GTTO.hr.obs.t125, main = "hazard rate: obs covariate, 125m trunc", showpoints = FALSE, pdf=TRUE)
add_df_covar_line(GTTO.hr.obs.t125, data.frame(obs= "KROSS"),col = "green", pdf = TRUE)
add_df_covar_line(GTTO.hr.obs.t125, data.frame(obs= "PMAGE"), col = "blue", pdf = TRUE)
add_df_covar_line(GTTO.hr.obs.t125, data.frame(obs= "KBROD"), col = "red", pdf =TRUE)
Year
GTTO.hn.yr.t125 <- ds(data=GTTO, transect="point", formula = ~fyear, key="hn", truncation = 125,  
                 convert.units=conversion.factor)
plot(GTTO.hn.yr.t125, main = "half-normal: year covariate, 125m trunc", showpoints = FALSE, pdf=TRUE)
add_df_covar_line(GTTO.hn.yr.t125, data.frame(fyear = "2018"),col = "purple", pdf = TRUE)
add_df_covar_line(GTTO.hn.yr.t125, data.frame(fyear = "2019"), col = "orange", pdf = TRUE)
add_df_covar_line(GTTO.hn.yr.t125, data.frame(fyear = "2020"), col = "navy", pdf =TRUE)

GTTO.hr.yr.t125 <- ds(data=GTTO, transect="point", formula = ~fyear, key="hr", truncation = 125,   
                 convert.units=conversion.factor)
plot(GTTO.hr.yr.t125, main = "hazard rate: year covariate, 125m trunc", showpoints = FALSE, pdf=TRUE)
add_df_covar_line(GTTO.hr.yr.t125, data.frame(fyear = "2018"),col = "purple", pdf = TRUE)
add_df_covar_line(GTTO.hr.yr.t125, data.frame(fyear = "2019"), col = "orange", pdf = TRUE)
add_df_covar_line(GTTO.hr.yr.t125, data.frame(fyear = "2020"), col = "navy", pdf =TRUE)



# Observer and Year as covariates
GTTO.hn.t125m <- ds(data=GTTO, transect="point", formula = ~obs+fyear, key="hn", truncation = 125,  
                    convert.units=conversion.factor)
plot(GTTO.hn.t125m, main = "half-normal, 125m trunc", showpoints = FALSE, pdf=TRUE)
add_df_covar_line(GTTO.hn.t125m.t125m, data.frame(obs= "KROSS"),col = "green", pdf = TRUE)
add_df_covar_line(GTTO.hn.t125m, data.frame(obs= "PMAGE"), col = "blue", pdf = TRUE)
add_df_covar_line(GTTO.hn.t125m, data.frame(obs= "KBROD"), col = "red", pdf =TRUE)
add_df_covar_line(GTTO.hn.t125m, data.frame(fyear = "2018"),col = "purple", pdf = TRUE)
add_df_covar_line(GTTO.hn.t125m, data.frame(fyear = "2019"), col = "orange", pdf = TRUE)
add_df_covar_line(GTTO.hn.t125m, data.frame(fyear = "2020"), col = "navy", pdf =TRUE)


GTTO.hr.t125m <- ds(data=GTTO, transect="point", formula = ~obs+fyear, key="hr", truncation=125, 
                    convert.units=conversion.factor)
plot(GTTO.hr.t125m, main = "hazard rate, 125m trunc", showpoints = FALSE, pdf=TRUE)
add_df_covar_line(GTTO.hr.t125m, data.frame(obs= "KROSS"),col = "green", pdf = TRUE)
add_df_covar_line(GTTO.hr.t125m, data.frame(obs= "PMAGE"), col = "blue", pdf = TRUE)
add_df_covar_line(GTTO.hr.t125m, data.frame(obs= "KBROD"), col = "red", pdf =TRUE)
add_df_covar_line(GTTO.hr.t125m, data.frame(fyear = "2018"),col = "purple", pdf = TRUE)
add_df_covar_line(GTTO.hr.t125m, data.frame(fyear = "2019"), col = "orange", pdf = TRUE)
add_df_covar_line(GTTO.hr.t125m, data.frame(fyear = "2020"), col = "navy", pdf =TRUE)


#table of results with covar
GTTO.tab <- data.frame(DetectionFunction=c("Half-normal","Half-normal","Half-normal", "Hazard Rate", "Hazard Rate", "Hazard Rate"), 
                       Covariate=c("Obs","Year","Obs+Year","Obs","Year","Obs+Year"), Truncation=c(125, 125, 125, 125, 125, 125), 
                       AIC=rep(NA,6), Density=rep(NA,6), D.CV=rep(NA,6), Lower.CI=rep(NA,6), Upper.CI=rep(NA,6))

get.results.f <- function(fit.model) {
        list(AIC=summary(fit.model$ddf)$aic,
             D=summary(fit.model$ddf)$average.p,
             D.CV=summary(fit.model$ddf)$average.p.se/summary(fit.model$ddf)$average.p,
             lCL=summary(fit.model$ddf)$average.p - 1.96*(summary(fit.model$ddf)$average.p.se),
             uCL=summary(fit.model$ddf)$average.p + 1.96*(summary(fit.model$ddf)$average.p.se))
}
GTTO.tab[1,4:8]<-get.results.f(GTTO.hn.obs.t125) 
GTTO.tab[2,4:8]<-get.results.f(GTTO.hn.yr.t125) 
GTTO.tab[3,4:8]<-get.results.f(GTTO.hn.t125m) 
GTTO.tab[4,4:8]<-get.results.f(GTTO.hr.obs.t125) 
GTTO.tab[5,4:8]<-get.results.f(GTTO.hr.yr.t125) 
GTTO.tab[6,4:8]<-get.results.f(GTTO.hr.t125m)

# Print results
knitr::kable(GTTO.tab, caption="Results from simulated point transect data with covariates (year, observer).", digits=3)

#change left trunc
GTTO.hn.obs.lt10 <- ds(data=GTTO, transect="point", formula = ~obs, key="hn", truncation = list(left=10,right=125),  
                  convert.units=conversion.factor)
plot(GTTO.hn.obs.lt10, main = "half-normal: obs covariate, 125m trunc", showpoints = FALSE, pdf=TRUE)
add_df_covar_line(GTTO.hn.obs.lt10, data.frame(obs= "KROSS"),col = "green", pdf = TRUE)
add_df_covar_line(GTTO.hn.obs.lt10, data.frame(obs= "PMAGE"), col = "blue", pdf = TRUE)
add_df_covar_line(GTTO.hn.obs.lt10, data.frame(obs= "KBROD"), col = "red", pdf =TRUE)

GTTO.hr.obs.lt10 <- ds(data=GTTO, transect="point", formula = ~obs, key="hr", truncation = list(left=10,right=125),   
                  convert.units=conversion.factor)
plot(GTTO.hr.obs.lt10, main = "hazard rate: obs covariate, 125m trunc", showpoints = FALSE, pdf=TRUE)
add_df_covar_line(GTTO.hr.obs.lt10, data.frame(obs= "KROSS"),col = "green", pdf = TRUE)
add_df_covar_line(GTTO.hr.obs.lt10, data.frame(obs= "PMAGE"), col = "blue", pdf = TRUE)
add_df_covar_line(GTTO.hr.obs.lt10, data.frame(obs= "KBROD"), col = "red", pdf =TRUE)
#Year
GTTO.hn.yr.lt10 <- ds(data=GTTO, transect="point", formula = ~fyear, key="hn", truncation = list(left=10,right=125),  
                 convert.units=conversion.factor)
plot(GTTO.hn.yr.lt10, main = "half-normal: year covariate, 125m trunc", showpoints = FALSE, pdf=TRUE)
add_df_covar_line(GTTO.hn.yr.lt10, data.frame(fyear = "2018"),col = "purple", pdf = TRUE)
add_df_covar_line(GTTO.hn.yr.lt10, data.frame(fyear = "2019"), col = "orange", pdf = TRUE)
add_df_covar_line(GTTO.hn.yr.lt10, data.frame(fyear = "2020"), col = "navy", pdf =TRUE)

GTTO.hr.yr.lt10 <- ds(data=GTTO, transect="point", formula = ~fyear, key="hr", truncation = list(left=10,right=125),   
                 convert.units=conversion.factor)
plot(GTTO.hr.yr.lt10, main = "hazard rate: year covariate, 125m trunc", showpoints = FALSE, pdf=TRUE)
add_df_covar_line(GTTO.hr.yr.lt10, data.frame(fyear = "2018"),col = "purple", pdf = TRUE)
add_df_covar_line(GTTO.hr.yr.lt10, data.frame(fyear = "2019"), col = "orange", pdf = TRUE)
add_df_covar_line(GTTO.hr.yr.lt10, data.frame(fyear = "2020"), col = "navy", pdf =TRUE)



# Observer and Year as covariates
GTTO.hn.lt10 <- ds(data=GTTO, transect="point", formula = ~obs+fyear, key="hn", truncation = list(left=10,right=125),  
                    convert.units=conversion.factor)
plot(GTTO.hn.lt10, main = "half-normal, 125m trunc", showpoints = FALSE, pdf=TRUE)
add_df_covar_line(GTTO.hn.lt10, data.frame(obs= "KROSS"),col = "green", pdf = TRUE)
add_df_covar_line(GTTO.hn.lt10, data.frame(obs= "PMAGE"), col = "blue", pdf = TRUE)
add_df_covar_line(GTTO.hn.lt10, data.frame(obs= "KBROD"), col = "red", pdf =TRUE)
add_df_covar_line(GTTO.hn.lt10, data.frame(fyear = "2018"),col = "purple", pdf = TRUE)
add_df_covar_line(GTTO.hn.lt10, data.frame(fyear = "2019"), col = "orange", pdf = TRUE)
add_df_covar_line(GTTO.hn.lt10, data.frame(fyear = "2020"), col = "navy", pdf =TRUE)


GTTO.hr.lt10 <- ds(data=GTTO, transect="point", formula = ~obs+fyear, key="hr", truncation=list(left=10,right=125), 
                    convert.units=conversion.factor)
plot(GTTO.hr.lt10, main = "hazard rate, 125m trunc", showpoints = FALSE, pdf=TRUE)
add_df_covar_line(GTTO.hr.lt10, data.frame(obs= "KROSS"),col = "green", pdf = TRUE)
add_df_covar_line(GTTO.hr.lt10, data.frame(obs= "PMAGE"), col = "blue", pdf = TRUE)
add_df_covar_line(GTTO.hr.lt10, data.frame(obs= "KBROD"), col = "red", pdf =TRUE)
add_df_covar_line(GTTO.hr.lt10, data.frame(fyear = "2018"),col = "purple", pdf = TRUE)
add_df_covar_line(GTTO.hr.lt10, data.frame(fyear = "2019"), col = "orange", pdf = TRUE)
add_df_covar_line(GTTO.hr.lt10, data.frame(fyear = "2020"), col = "navy", pdf =TRUE)


#table of results with covar
GTTO.tab <- data.frame(DetectionFunction=c("Half-normal","Half-normal","Half-normal", "Hazard Rate", "Hazard Rate", "Hazard Rate"), 
                       Covariate=c("Obs","Year","Obs+Year","Obs","Year","Obs+Year"), Truncation=c(10-125, 10-125, 10-125, 10-125, 10-125, 10-125), 
                       AIC=rep(NA,6), Density=rep(NA,6), D.CV=rep(NA,6), Lower.CI=rep(NA,6), Upper.CI=rep(NA,6))

get.results.f <- function(fit.model) {
        list(AIC=summary(fit.model$ddf)$aic,
             D=summary(fit.model$ddf)$average.p,
             D.CV=summary(fit.model$ddf)$average.p.se/summary(fit.model$ddf)$average.p,
             lCL=summary(fit.model$ddf)$average.p - 1.96*(summary(fit.model$ddf)$average.p.se),
             uCL=summary(fit.model$ddf)$average.p + 1.96*(summary(fit.model$ddf)$average.p.se))
}
GTTO.tab[1,4:8]<-get.results.f(GTTO.hn.obs.lt10) 
GTTO.tab[2,4:8]<-get.results.f(GTTO.hn.yr.lt10) 
GTTO.tab[3,4:8]<-get.results.f(GTTO.hn.lt10) 
GTTO.tab[4,4:8]<-get.results.f(GTTO.hr.obs.lt10) 
GTTO.tab[5,4:8]<-get.results.f(GTTO.hr.yr.lt10) 
GTTO.tab[6,4:8]<-get.results.f(GTTO.hr.lt10)

# Print results
knitr::kable(GTTO.tab, caption="Results from simulated point transect data with covariates (year, observer).", digits=3)

#Goodness of Fit
par(mfrow=c(1,1))
##Cramer-von Mises goodness of fit test

#half-normal, no adjustments - CvM
GTTO.hn.obs.CvM<-gof_ds(GTTO.hn.obs, plot = TRUE, nboot = 1000, ks = FALSE)
GTTO.hn.yr.CvM<-gof_ds(GTTO.hn.yr, plot = TRUE, nboot = 1000, ks = FALSE)
#hazard rate, no adjustments - CvM
GTTO.hr.obs.CvM<-gof_ds(GTTO.hr.obs, plot = TRUE, nboot = 1000, ks = FALSE)
GTTO.hr.yr.CvM<-gof_ds(GTTO.hr.yr, plot = TRUE, nboot = 1000, ks = FALSE)

GTTO.hn.obs.t125.CvM<-gof_ds(GTTO.hn.obs.t125, plot = TRUE, nboot = 1000, ks = FALSE)
GTTO.hn.yr.t125.CvM<-gof_ds(GTTO.hn.yr.t125, plot = TRUE, nboot = 1000, ks = FALSE)
GTTO.hr.obs.t125.CvM<-gof_ds(GTTO.hr.obs.t125, plot = TRUE, nboot = 1000, ks = FALSE)
GTTO.hr.yr.t125.CvM<-gof_ds(GTTO.hr.yr.t125, plot = TRUE, nboot = 1000, ks = FALSE)

GTTO.hn.obs.lt10.CvM<-gof_ds(GTTO.hn.obs.lt10, plot = TRUE, nboot = 1000, ks = FALSE)
GTTO.hn.yr.lt10.CvM<-gof_ds(GTTO.hn.yr.lt10, plot = TRUE, nboot = 1000, ks = FALSE)
GTTO.hr.obs.lt10.CvM<-gof_ds(GTTO.hr.obs.lt10, plot = TRUE, nboot = 1000, ks = FALSE)
GTTO.hr.yr.lt10.CvM<-gof_ds(GTTO.hr.yr.lt10, plot = TRUE, nboot = 1000, ks = FALSE)

#explore GOF metrics (looking for p value over 0.05)
GTTO.hn.obs.CvM
#Cramer test stat = 2.13378 , p-vaule = 6.4085e-06
GTTO.hn.yr.CvM
#Cramer test stat = 2.20647, p-vaule = 6.4085e-06
GTTO.hr.obs.CvM
#Cramer test stat = 0.620713, p-vaule = 0.0198978
GTTO.hr.yr.CvM
#Cramer test stat = 0.661378, p-vaule = 0.0158162
GTTO.hn.obs.t125.CvM
GTTO.hn.yr.t125.CvM
GTTO.hr.obs.t125.CvM
GTTO.hr.yr.t125.CvM

GTTO.hn.obs.lt10.CvM
GTTO.hn.yr.lt10.CvM
GTTO.hr.obs.lt10.CvM
GTTO.hr.yr.lt10.CvM

#density by grid
GTTOgrid<-dht2(GTTO.hr.yr.lt10, flatfile = GTTOflatgrid,
               strat_formula = ~Region.Label, convert_units = conversion.factor)
print(GTTOgrid, report = "both", groups =FALSE)
write.csv(GTTOgrid, "E:/IMBCR/GTTO.grid.csv")

#density by point
GTTOpt<-dht2(GTTO.hr.yr.lt10, flatfile = GTTOflatpt,
             strat_formula = ~Sample.Label, convert_units = conversion.factor)
options(max.print = 99999) ##need otherwise analysis will omit 100+ rows
print(GTTOpt, report = "both", groups =FALSE)
write.csv(GTTOpt, "E:/IMBCR/GTTO.pt.csv")

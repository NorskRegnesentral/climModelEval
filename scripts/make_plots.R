source("~/Desktop/EURO-CORDEXevaluationPrecip/code2019/load_necessary.R")

# --- needed data ---
colsAndNames()
load("~/postclim/Results/2018/lon_lat_date_19802005.dat")
parVec <- c(1, 0, 2, 6)
phens <- c("dryDays", "wetDays")
s <- 1
coast <- map(database="world",interior=F,xlim=c(3,35),ylim=c(54.7,72.6),plot=FALSE)
p <- "~/Desktop/EURO-CORDEXevaluationPrecip/code2019/Plots/final/"
prepDataPath <- "~/postclim/PreparedData/2018/"

# ------------------------------- DIFFERENCE -------------------------------
# LSCE with all models, against NGCD type 2
name <- type_names[3]
methods <- c(method_names[2], method_names[4])
var_name_obs <- load(paste(prepDataPath, name, "/precipitation_19802005.dat", sep = ""))                                         
lim <- list()
lim[[seasons[1]]] <- c(-2.5, 2.5)
lim[[seasons[2]]] <- c(-4, 4)

for (season in seasons){
  a <- get(var_name_obs[grep(season, sapply(var_name_obs, tolower))])
  path <- paste(p, "LSCE_SMHI_bias_NGCD2_", season, ".png", sep = "")     # path to place plot
  png(path, units = 'in', height = 10, width = 25, res = 200, pointsize = 18)
  par(mfrow=c(2,5), mar = parVec)
  for (method in methods) {
    for (model in model_names) {
      f <- paste(prepDataPath, model, "/", method, "_precipitation_19802005.dat", sep = "")                    
      if(file.exists(f)) {                                                                                                     
        var_names_mod <- load(f)                                                                                                                     
        b <- get(var_names_mod[grep(season, sapply(var_names_mod, tolower))])
        d <- findDiff(a, b)
        if (model == model_names[1] & method == methods[1]) {
          plotMap(d, lonFenno, latFenno, model_plot_names[match(model, model_names)], paste("Method: ", method_plot_names[match(method, method_names)], sep = ""), seasonSize = s, legend = F, z = lim[[season]]) 
        } else if (method == methods[1] & model == model_names[length(model_names)]) {
          plotMap(d, lonFenno, latFenno, model_plot_names[match(model, model_names)], z = lim[[season]]) 
        } else if (model == model_names[1]) {
          plotMap(d, lonFenno, latFenno, "", paste("Method: ", method_plot_names[match(method, method_names)], sep = ""), seasonSize = s, legend = F, z = lim[[season]]) 
        } else if (method == methods[1]) {
          plotMap(d, lonFenno, latFenno, model_plot_names[match(model, model_names)], legend = F, z = lim[[season]])  
        } else if (model == model_names[length(model_names)] & method == methods[1]) {
          plotMap(d, lonFenno, latFenno, legend = T, lWidth = 4, z = lim[[season]]) 
        } else {
          plotMap(d, lonFenno, latFenno, legend = F, z = lim[[season]]) 
        }
      }
    }
  }
  dev.off()
}


# CCLM(MPI-ESM-LR) with all methods, against NGCD type 2
name <- type_names[3]
model <- model_names[4]
var_name_obs <- load(paste(prepDataPath, name, "/precipitation_19802005.dat", sep = ""))                                      
lim <- list()
lim[[seasons[1]]] <- c(-5, 1.5)
lim[[seasons[2]]] <- c(-3, 7)

path <- paste(p, model, "_bias_NGCD2.png", sep = "")     # path to place plot
png(path, units = 'in', height = 10, width = 20, res = 200, pointsize = 18)
par(mfrow=c(2,4), mar = parVec)
for (season in seasons) {
  a <- get(var_name_obs[grep(season, sapply(var_name_obs, tolower))])
  for (method in method_names) {
    f <- paste(prepDataPath, model, "/", method, "_precipitation_19802005.dat", sep = "")                                        
    if(file.exists(f)) {                                                                                                                             
      var_names_mod <- load(f)                                                                                                                 
      b <- get(var_names_mod[grep(season, sapply(var_names_mod, tolower))])
      d <- findDiff(a, b)
      if (season == seasons[1] & method == method_names[1]) {
        plotMap(d, lonFenno, latFenno, method_plot_names[match(method, method_names)], paste("Season: ", season, sep = ""), seasonSize = s, legend = F, z = lim[[season]]) 
      } else if (season == seasons[1] & method == method_names[length(method_names)]) {
        plotMap(d, lonFenno, latFenno, method_plot_names[match(method, method_names)], z = lim[[season]]) 
      } else if (season == seasons[1]) {
        plotMap(d, lonFenno, latFenno, method_plot_names[match(method, method_names)], legend = F, z = lim[[season]]) 
      } else if (method == method_names[1]) {
        plotMap(d, lonFenno, latFenno, "", paste("Season: ", season, sep = ""), seasonSize = s, legend = F, z = lim[[season]]) 
      } else if (method == method_names[length(method_names)]) {
        plotMap(d, lonFenno, latFenno, "", seasonSize = s, z = lim[[season]]) 
      } else {
        plotMap(d, lonFenno, latFenno, legend = F, z = lim[[season]]) 
      }
    }
  }
}
dev.off()


# ------------------------------- RELATIVE DIFFERENCE -------------------------------
# LSCE with all models, against NGCD type 2
name <- type_names[3]
methods <- c(method_names[2], method_names[4])
var_name_obs <- load(paste(prepDataPath, name, "/precipitation_19802005.dat", sep = ""))                                        
lim <- list()
lim[[seasons[1]]] <- c(-1.5, 0.5)
lim[[seasons[2]]] <- c(-2.5, 0.5)

for (season in seasons){
  a <- get(var_name_obs[grep(season, sapply(var_name_obs, tolower))])
  path <- paste(p, "relative_LSCE_SMHI_bias_NGCD2_", season, ".png", sep = "")    
  png(path, units = 'in', height = 10, width = 25, res = 200, pointsize = 18)
  par(mfrow=c(2,5), mar = parVec)
  for (method in methods) {
    for (model in model_names) {
      f <- paste(prepDataPath, model, "/", method, "_precipitation_19802005.dat", sep = "")                       
      if(file.exists(f)) {                                                                                                                    
        var_names_mod <- load(f)                                                                                                                     
        b <- get(var_names_mod[grep(season, sapply(var_names_mod, tolower))])
        d <- findDiff(a, b, relative = T)
        if (model == model_names[1] & method == methods[1]) {
          plotMap(d, lonFenno, latFenno, model_plot_names[match(model, model_names)], paste("Method: ", method_plot_names[match(method, method_names)], sep = ""), seasonSize = s, legend = F, z = lim[[season]]) 
        } else if (method == methods[1] & model == model_names[length(model_names)]) {
          plotMap(d, lonFenno, latFenno, model_plot_names[match(model, model_names)], z = lim[[season]]) 
        } else if (model == model_names[1]) {
          plotMap(d, lonFenno, latFenno, "", paste("Method: ", method_plot_names[match(method, method_names)], sep = ""), seasonSize = s, legend = F, z = lim[[season]]) 
        } else if (method == methods[1]) {
          plotMap(d, lonFenno, latFenno, model_plot_names[match(model, model_names)], legend = F, z = lim[[season]])  
        } else if (model == model_names[length(model_names)] & method == methods[1]) {
          plotMap(d, lonFenno, latFenno, legend = T, z = lim[[season]]) 
        } else {
          plotMap(d, lonFenno, latFenno, legend = F, z = lim[[season]]) 
        }
      }
    }
  }
  dev.off()
}


# CCLM(MPI-ESM-LR) with all methods, against NGCD type 2
name <- type_names[3]
model <- model_names[4]
var_name_obs <- load(paste(prepDataPath, name, "/precipitation_19802005.dat", sep = ""))                                         
lim <- list()
lim[[seasons[1]]] <- c(-4, 0.5)
lim[[seasons[2]]] <- c(-4, 0.5)

path <- paste(p, "relative_", model, "_bias_NGCD2.png", sep = "")     # path to place plot
png(path, units = 'in', height = 10, width = 20, res = 200, pointsize = 18)
par(mfrow=c(2,4), mar = parVec)
for (season in seasons) {
  a <- get(var_name_obs[grep(season, sapply(var_name_obs, tolower))])
  for (method in method_names) {
    f <- paste(prepDataPath, model, "/", method, "_precipitation_19802005.dat", sep = "")                                    
    if(file.exists(f)) {                                                                                                                           
      var_names_mod <- load(f)                                                                                                                      
      b <- get(var_names_mod[grep(season, sapply(var_names_mod, tolower))])
      d <- findDiff(a, b, relative = T)
      if (season == seasons[1] & method == method_names[1]) {
        plotMap(d, lonFenno, latFenno, method_plot_names[match(method, method_names)], paste("Season: ", season, sep = ""), seasonSize = s, legend = F, z = lim[[season]]) 
      } else if (season == seasons[1] & method == method_names[length(method_names)]) {
        plotMap(d, lonFenno, latFenno, method_plot_names[match(method, method_names)], z = lim[[season]]) 
      } else if (season == seasons[1]) {
        plotMap(d, lonFenno, latFenno, method_plot_names[match(method, method_names)], legend = F, z = lim[[season]]) 
      } else if (method == method_names[1]) {
        plotMap(d, lonFenno, latFenno, "", paste("Season: ", season, sep = ""), seasonSize = s, legend = F, z = lim[[season]]) 
      } else if (method == method_names[length(method_names)]) {
        plotMap(d, lonFenno, latFenno, "",  seasonSize = s, z = lim[[season]], legend = F) 
      } else {
        plotMap(d, lonFenno, latFenno, legend = F, z = lim[[season]]) 
      }
    }
  }
}
dev.off()


# ------------------------------- IQD PRECIPITATION -------------------------------
# LSCE with all models, against NGCD type 2
name <- type_names[3]
methods <- c(method_names[2], method_names[4])
lim <- list()
lim[[seasons[1]]] <- c(0, 0.7)
lim[[seasons[2]]] <- c(0, 1.4)

for (season in seasons){
  path <- paste(p, "LSCE_SMHI_IQD_precipitation_NGCD2_", season, ".png", sep = "")     
  png(path, units = 'in', height = 10, width = 25, res = 200, pointsize = 18)
  par(mfrow=c(2,5), mar = parVec)
  for (method in methods) {
    for (model in model_names) {
      f <- paste("~/postclim/Results/2019/precipitation/NGCD_type2/", model, "/", method, "_precipitation_IQD_", season, "_19802005.dat", sep = "")                                       
      if(file.exists(f)) {                                                                                                                        
        var_name <- load(f)                                                                                                                      
        d <- get(var_name)
        if (model == model_names[1] & method == methods[1]) {
          plotMap(d, lonFenno, latFenno, model_plot_names[match(model, model_names)], paste("Method: ", method_plot_names[match(method, method_names)], sep = ""), seasonSize = s, legend = F, z = lim[[season]]) 
        } else if (method == methods[1] & model == model_names[length(model_names)]) {
          plotMap(d, lonFenno, latFenno, model_plot_names[match(model, model_names)], z = lim[[season]]) 
        } else if (model == model_names[1]) {
          plotMap(d, lonFenno, latFenno, "", paste("Method: ", method_plot_names[match(method, method_names)], sep = ""), seasonSize = s, legend = F, z = lim[[season]]) 
        } else if (method == methods[1]) {
          plotMap(d, lonFenno, latFenno, model_plot_names[match(model, model_names)], legend = F, z = lim[[season]])  
        } else if (model == model_names[length(model_names)] & method == methods[1]) {
          plotMap(d, lonFenno, latFenno, legend = T, z = lim[[season]]) 
        } else {
          plotMap(d, lonFenno, latFenno, legend = F, z = lim[[season]]) 
        }
      }
    }
  }
  dev.off()
}


# ------------------------------- IQD DRY/WET DAYS -------------------------------
# LSCE with all models, against NGCD type 2
name <- type_names[3]
methods <- c(method_names[2], method_names[4])

lim <- list()
lim[[phens[1]]] <- list()
lim[[phens[2]]] <- list()
lim[[phens[1]]][[seasons[1]]] <- c(0, 0.11)
lim[[phens[1]]][[seasons[2]]] <- c(0, 0.11)
lim[[phens[2]]][[seasons[1]]] <- c(0, 0.11)
lim[[phens[2]]][[seasons[2]]] <- c(0, 0.11)

for (phen in phens) {
  for (season in seasons){
    path <- paste(p, "LSCE_SMHI_IQD_", tolower(phen), "_NGCD2_", season, ".png", sep = "")     # path to place plot
    png(path, units = 'in', height = 10, width = 25, res = 200, pointsize = 18)
    par(mfrow=c(2,5), mar = parVec)
    for (method in methods) {
      for (model in model_names) {
        f <- paste("~/Desktop/EURO-CORDEXevaluationPrecip/Results19/", phen, "/NGCD_type2/", season, "_", model, "(", method, ").dat", sep = "")                              
        if(file.exists(f)) {                                                                                                                       
          var_name <- load(f)                                                                                                                  
          d <- get(var_name)
          if (model == model_names[1] & method == methods[1]) {
            plotMap(d, lonFenno, latFenno, model_plot_names[match(model, model_names)], paste("Method: ", method_plot_names[match(method, method_names)], sep = ""), seasonSize = s, legend = F, z = lim[[phen]][[season]]) 
          } else if (method == methods[1] & model == model_names[length(model_names)]) {
            plotMap(d, lonFenno, latFenno, model_plot_names[match(model, model_names)], z = lim[[phen]][[season]]) 
          } else if (model == model_names[1]) {
            plotMap(d, lonFenno, latFenno, "", paste("Method: ", method_plot_names[match(method, method_names)], sep = ""), seasonSize = s, legend = F, z = lim[[phen]][[season]]) 
          } else if (method == methods[1]) {
            plotMap(d, lonFenno, latFenno, model_plot_names[match(model, model_names)], legend = F, z = lim[[phen]][[season]])  
          } else if (model == model_names[length(model_names)] & method == methods[1]) {
            plotMap(d, lonFenno, latFenno, legend = T, z = lim[[phen]][[season]]) 
          } else {
            plotMap(d, lonFenno, latFenno, legend = F, z = lim[[phen]][[season]]) 
          }
        }
      }
    }
    dev.off()
  }
}


# ------------------------------- IQD PRECIPITATION DATAPROD-------------------------------
# EOBS and NGCD type 1, against NGCD type 2
name <- type_names[3]
lim <- list()
lim[[seasons[1]]] <- c(0, 0.7)
lim[[seasons[2]]] <- c(0, 1.4)

for (season in seasons){
  path <- paste(p, "EOBS_NGCD1_IQD_precipitation_NGCD2_", season, ".png", sep = "")    
  png(path, units = 'in', height = 5, width = 10, res = 200, pointsize = 18)
  par(mfrow=c(1,2), mar = parVec)
  for (type in type_names[1:2]) {
    f <- paste("~/postclim/Results/2019/precipitation/NGCD_type2/", type, "/precipitation_IQD_", season, "_19802005.dat", sep = "")                                
    if(file.exists(f)) {                                                                                                                          
      var_name <- load(f)                                                                                                                      
      d <- get(var_name)
      if (type == type_names[2]) {
        plotMap(d, lonFenno, latFenno, type_plot_names[match(type, type_names)], legend = T, lWidth = 1, z = lim[[season]]) 
      } else {
        plotMap(d, lonFenno, latFenno, type_plot_names[match(type, type_names)], legend = F, lWidth = 1, z = lim[[season]]) 
      }
    }
  }
  dev.off()
}


# ------------------------------- IQD DRY/WET DAYS DATAPROD -------------------------------
# EOBS and NGCD type 1, against NGCD type 2
name <- type_names[3]
lim <- list()
lim[[phens[1]]] <- list()
lim[[phens[2]]] <- list()
lim[[phens[1]]][[seasons[1]]] <- c(0, 0.11)
lim[[phens[1]]][[seasons[2]]] <- c(0, 0.11)
lim[[phens[2]]][[seasons[1]]] <- c(0, 0.11)
lim[[phens[2]]][[seasons[2]]] <- c(0, 0.11)

for (phen in phens) {
  for (season in seasons){
    path <- paste(p, "EOBS_NGCD1_IQD_", tolower(phen), "_NGCD2_", season, ".png", sep = "")     # path to place plot
    png(path, units = 'in', height = 5, width = 10, res = 200, pointsize = 18)
    par(mfrow=c(1,2), mar = parVec)
    for (type in type_names[1:2]) {
      f <- paste("~/Desktop/EURO-CORDEXevaluationPrecip/Results19/", phen, "/NGCD_type2/", season, "_", type, ".dat", sep = "")                                   
      if(file.exists(f)) {                                                                                                                            
        var_name <- load(f)                                                                                                                     
        d <- get(var_name)
        if (type == type_names[2]) {
          plotMap(d, lonFenno, latFenno, type_plot_names[match(type, type_names)], legend = T, lWidth = 1, z = lim[[phen]][[season]]) 
        } else {
          plotMap(d, lonFenno, latFenno, type_plot_names[match(type, type_names)], legend = F, lWidth = 1, z = lim[[phen]][[season]]) 
        }
      }
    }
    dev.off()
  }
}


# ------------------------------- MEAN LENGTH WET/DRY PERIODS -------------------------------
lim <- list()
lim[[phens[1]]] <- list()
lim[[phens[2]]] <- list()
lim[[phens[1]]][[seasons[1]]] <- c(1.5, 4.5)
lim[[phens[1]]][[seasons[2]]] <- c(1.5, 4.5)
lim[[phens[2]]][[seasons[1]]] <- c(1.5, 8.5)
lim[[phens[2]]][[seasons[2]]] <- c(1.5, 8.5)

for (phen in phens) {
  if (phen == "dryDays") {
    dry <- T
  } else {
    dry <- F
  }
  path <- paste(p, "observed_", phen, "_periods_mean_length.png", sep = "")                      
  png(path, units = 'in', height = 10, width = 15, res = 300, pointsize = 18)
  par(mfrow=c(2,3), mar = parVec)
  for (season in seasons) {
    for(type in type_names) {
      var_names <- load(paste(prepDataPath, type, "/precipitation_19802005.dat", sep = ""))                                                          
      data <- get(var_names[grep(season, sapply(var_names, tolower))])                                                                                               
      d <- findMeanPeriodLength(data, d = dry)                                                                                                                                   
      if (season == seasons[1] & type == type_names[1]) {
        plotMap(d, lonFenno, latFenno, type_plot_names[match(type, type_names)], season, seasonSize = s, legend = F, z = lim[[phen]][[season]])  
      } else if (season == seasons[1] & type == type_names[3]) {
        plotMap(d, lonFenno, latFenno, type_plot_names[match(type, type_names)], seasonSize = s, legend = T, lWidth = 2, z = lim[[phen]][[season]])  
      } else if (season == seasons[2] & type == type_names[1]) {
        plotMap(d, lonFenno, latFenno, season = season, seasonSize = s, legend = F, z = lim[[phen]][[season]])  
      } else if (season == seasons[1]) {
        plotMap(d, lonFenno, latFenno, type_plot_names[match(type, type_names)], seasonSize = s, legend = F, z = lim[[phen]][[season]])  
      } else {
        plotMap(d, lonFenno, latFenno, seasonSize = s, legend = F, z = lim[[phen]][[season]])  
      }
    }
  }
  dev.off()
}


# ------------------------------- MEAN PRECEPITATION PER SEASON -------------------------------
# For all dataproducts
path <- paste(p, "observed_mean_precipitation.png", sep = "")                         
png(path, units = 'in', height = 10, width = 15, res = 300, pointsize = 18)
par(mfrow=c(2,3), mar = c(1, 0, 2, 6))
zl = c(0, 26*20)

for(season in seasons) {                                
  for(name in type_names) {
    var_names <- load(paste(prepDataPath, name, "/precipitation_19802005.dat", sep = ""))  
    a <- get(var_names[grep(season, sapply(var_names, tolower))])                                                                                  
    a <- apply(a, c(1, 2), mean)
    a <- a*26
    m <- round(mean(a, na.rm = T), 2)
    a[!keepIndex] <- NA                                                                                                                   
    if (season == seasons[1] & name == type_names[3]) {
      plotMap(a, lonFenno, latFenno, seasonSize = s, z = zl, colScale = "roma", title = type_plot_names[match(name, type_names)], legend = T, lWidth = 2, m = m)  
    } else if (season == seasons[1] & name == type_names[1]) {
      plotMap(a, lonFenno, latFenno, type_plot_names[match(name, type_names)], season, seasonSize = s, z = zl, colScale = "roma", legend = F, m = m)
    } else if (name == type_names[1]) {
      plotMap(a, lonFenno, latFenno, season = season, seasonSize = s, z = zl, colScale = "roma", legend = F, m = m)      
    } else if (season == seasons[1]) {
      plotMap(a, lonFenno, latFenno, title  = type_plot_names[match(name, type_names)], seasonSize = s, z = zl, colScale = "roma", legend = F, m = m)  
    } else {
      plotMap(a, lonFenno, latFenno, seasonSize = s, z = zl, colScale = "roma", legend = F, m = m)
    }
  }
}
dev.off()


# ------------------------------- FREQUENCE OF WET DAYS -------------------------------
path <- paste(p, "observed_freq_wet_days.png", sep = "")                          
png(path, units = 'in', height = 10, width = 15, res = 300, pointsize = 18)
par(mfrow=c(2,3), mar = c(1, 0, 2, 6))
zl = c(0, 1)

for(season in seasons) {
  for(name in type_names) {
    var_names <- load(paste(prepDataPath, name, "/precipitation_19802005.dat", sep = ""))                                           
    data <- get(var_names[grep(season, sapply(var_names, tolower))])                                                                                  
    a <- findFreq(data, threshold)                                                                                                                     
    if (season == seasons[1] & name == type_names[3]) {
      plotMap(a, lonFenno, latFenno, seasonSize = s, z = zl, colScale = "roma", title = type_plot_names[match(name, type_names)], legend = T, lWidth = 2)  
    } else if (season == seasons[1] & name == type_names[1]) {
      plotMap(a, lonFenno, latFenno, type_plot_names[match(name, type_names)], season, s, z = zl, colScale = "roma", legend = F)
    } else if (name == type_names[1]) {
      plotMap(a, lonFenno, latFenno, season = season, seasonSize = s, z = zl, colScale = "roma", legend = F)         
    } else if (season == seasons[1]) {
      plotMap(a, lonFenno, latFenno, title  = type_plot_names[match(name, type_names)], seasonSize = s, z = zl, colScale = "roma", legend = F)  
    } else {
      plotMap(a, lonFenno, latFenno, seasonSize = s, z = zl, colScale = "roma", legend = F)
    }                                                                                                                             
  }
}
dev.off()


# ------------------------------- MATRIX PATTERN PLOTS -------------------------------
depth = 2
seqVec = c("0-0-0", "0-0-1", "0-1-0", "0-1-1", "1-0-0", "1-0-1", "1-1-0", "1-1-1")
indexList = list(c(2:9))
colN <-  c("name", "0-0-0", "0-0-1", "0-1-0", "0-1-1", "1-0-0", "1-0-1", "1-1-0", "1-1-1")

season <- seasons[1]
plist <- makeMatrix(season, prepDataPath, depth, seqVec)
png(paste(p, "matrix_", season, ".png", sep = ""), units = 'in', height = 15, width = 10, res = 300)
plot_grid(plotlist = plist, ncol = length(model_names))
dev.off()

season <- seasons[2]
plist <- makeMatrix(season, prepDataPath, depth, seqVec)
png(paste(p, "matrix_", season, ".png", sep = ""), units = 'in', height = 15, width = 10, res = 300)
plot_grid(plotlist = plist, ncol = length(model_names))
dev.off()


# ------------------------------- IQD-SCATTER/RANK PLOTS WET/DRYDAYS -------------------------------
prec <- FALSE
guide_lim <- list(summer = c(0, 0.06), winter = c(0, 0.08))

for(phen in c("wetDays", "dryDays")) { 
  
  # Save all IQ-plots per season per type
  file_paths_in <- paste('~/Desktop/EURO-CORDEXevaluationPrecip/Results19/', phen, '/', type_names, '/IQD_all.dat', sep = '')
  li <- severalGgPlots(file_paths_in = file_paths_in, lon = lonFenno, lat = latFenno,
                       guide_lim = guide_lim, want_summ = TRUE, prec = prec, quantiles = c(0.1, 0.9),
                       do_bootstrap = TRUE, bootstrap_samples = 1000)
  all_plots <- li$all_plots
  quantile_vals <- li$quantile_vals
  for (season in seasons) {
    quantile_vals[[season]]$model <- model_plot_names[match(quantile_vals[[season]]$model, c(model_names, "data_sets"))]
    quantile_vals[[season]]$method <- method_plot_names[match(quantile_vals[[season]]$method, c(method_names, "EOBS", "NGCD_type1", "NGCD_type2"))]
    quantile_vals[[season]]$type <- type_plot_names[match(quantile_vals[[season]]$type, type_names)]
  }
  
  out_path <- p
  
  # Save scatter plots for the mean IQD of all models with methods for different type_names and seasons 
  plot_name_list <- paste(p, phen, '_scatters_', seasons, '.pdf', sep = '')
  gg_list <- makeScatterPlot(plot_name_list = plot_name_list, quantile_vals = quantile_vals, phen = phen, size = 5)
  
  png(paste(dirname(plot_name_list[1]), '/', phen, '_all_scatter_plots.png', sep = ''), width = 1600, height = 1000)
  temp <- cowplot::get_legend(gg_list[[1]]) 
  for(i in 1:length(gg_list)) {
    gg_list[[i]] <- gg_list[[i]] + theme(legend.position = "none")
  }
  gr <- plot_grid(plotlist = gg_list, ncol = 3)
  plot(plot_grid(gr, temp, ncol = 2, rel_widths = c(.25, .1)))
  dev.off()
  
  # Save ranking plots for mean IQD
  plot_name <- paste(p, '/', phen, '_rankings.pdf', sep = '')
  gg_list <- rankingPlots(quantile_vals, plot_name, size = 5.5, phen = phen)
  
  png(paste(dirname(plot_name), '/', phen, '_all_rank_plots.png', sep = ''), width = 1300, height = 800)
  temp <- cowplot::get_legend(gg_list[[1]]) 
  for(i in 1:length(gg_list)) {
    gg_list[[i]] <- gg_list[[i]] + theme(legend.position = "none")
  }
  gr <- plot_grid(plotlist = gg_list, ncol = 3)
  plot(plot_grid(gr, temp, ncol = 2, rel_widths = c(.25, .1)))
  dev.off()
}


# ------------------------------- IQD-SCATTER/RANK PLOTS PRECIP -------------------------------
prec <- TRUE
phen <- 'precipitation' 
guide_lim <- list(summer = c(0, 0.06), winter = c(0, 0.08))

# Save all IQ-plots per season per type
file_paths_in <- paste('~/Desktop/postclim/Results/2019/precipitation/', type_names, '/IQD_19802005.dat', sep = '')
li <- severalGgPlots(file_paths_in = file_paths_in, lon = lonFenno, lat = latFenno,
                     guide_lim = guide_lim, want_summ = TRUE, prec = prec, quantiles = c(0.1, 0.9),
                     do_bootstrap = TRUE, bootstrap_samples = 1000)
all_plots <- li$all_plots
quantile_vals <- li$quantile_vals
for (season in seasons) {
  quantile_vals[[season]]$model <- model_plot_names[match(quantile_vals[[season]]$model, c(model_names, "data_sets"))]
  quantile_vals[[season]]$method <- method_plot_names[match(quantile_vals[[season]]$method, c(method_names, "EOBS", "NGCD_type1", "NGCD_type2"))]
  quantile_vals[[season]]$type <- type_plot_names[match(quantile_vals[[season]]$type, type_names)]
}

out_path <- p

# Save scatter plots for the mean IQD of all models with methods for different type_names and seasons 
plot_name_list <- paste(p, 'precipitation_scatters_', seasons, '.pdf', sep = '')
gg_list <- makeScatterPlot(plot_name_list = plot_name_list, quantile_vals = quantile_vals, phen = phen, size = 5)

png(paste(dirname(plot_name_list[1]), '/precipitation_all_scatter_plots.png', sep = ''), width = 1600, height = 1000)
temp <- cowplot::get_legend(gg_list[[1]]) 
for(i in 1:length(gg_list)) {
  gg_list[[i]] <- gg_list[[i]] + theme(legend.position = "none")
}
gr <- plot_grid(plotlist = gg_list, ncol = 3)
plot_grid(gr, temp, ncol = 2, rel_widths = c(.25, .1))
dev.off()

# Save ranking plots for mean IQD
plot_name <- paste(p, '/precipitation_rankings.pdf', sep = '')
gg_list <- rankingPlots(quantile_vals, plot_name, size = 5.5, phen = phen)

png(paste(dirname(plot_name), '/precipitation_all_rank_plots.png', sep = ''), width = 1300, height = 800)
temp <- cowplot::get_legend(gg_list[[1]]) 
for(i in 1:length(gg_list)) {
  gg_list[[i]] <- gg_list[[i]] + theme(legend.position = "none")
}
gr <- plot_grid(plotlist = gg_list, ncol = 3)
plot_grid(gr, temp, ncol = 2, rel_widths = c(.25, .1))
dev.off()

#' Make a list of pattern plots
#'
#' This function makes a list of pattern plots to be ploted in a matrix
#' @param season What season data to use. 
#' @param prepDataPath Data path to prepered data. 
#' @param depth Depth argument sent to psttree. Defaults to 1.
#' @param seqVec sequence vector sent to predict function. Defaults to c("0-0-0", "0-0-1", "0-1-0", "0-1-1", "1-0-0", "1-0-1", "1-1-0", "1-1-1").
#' 
#' @keywords patters, pattern, plot, matrix, plotlist, list
#' @export
#' @examples
#' makeMatrix()

makeMatrix <- function(season, prepDataPath, depth = 2, seqVec = c("0-0-0", "0-0-1", "0-1-0", "0-1-1", "1-0-0", "1-0-1", "1-1-0", "1-1-1")) {
  
  if(!exists("colsAndNames")) {
    stop("colsAndNames function not defined")
  }
  colsAndNames()
  
  plist <- list()
  i <- 2
  for (name in type_names) {
    print(name)
    df <- data.frame(matrix(ncol = 9))
    colnames(df) <- colN
    var_names <- load(paste(prepDataPath, name, "/precipitation_19802005.dat", sep = ""))    # finding variable names
    data <- get(var_names[grep(season, sapply(var_names, tolower))])
    df <- rbind(df, c(name, 0, 0, 0, 0, 0, 0, 0, 0))                                                    # add a new row/observation
    df <- df[-1,]
    df[2:9] <- findDist(data, depth, seqVec)                 # find distributions
    plist[[i]] <- plotPatternplot(df, titleText = name, legend = F, xticks = F, yticks = F, showPanel = F)
    i <- i + 1
  }
  plist[[i]] <- plotPatternplot(df, legendOnly = T)
  i <- i + 1 # To have a blank plot
  for (method in method_names) {
    print(method)
    for (model in model_names) {
      f <- paste(prepDataPath, model, "/", method, "_precipitation_19802005.dat", sep = "")  # create filepath
      df <- data.frame(matrix(ncol = 9))
      colnames(df) <- colN
      if(file.exists(f)) {                                                                                      # if file exists...
        var_names <- load(f)                                                                                    # find variable names ... else continue to next method
        print(model)
        data <- get(var_names[grep(season, sapply(var_names, tolower))])                                      # get data from variable name
        df <- rbind(df, c(model, 0, 0, 0, 0, 0, 0, 0, 0))                                                    # add a new row/observation
        df <- df[-1,]
        df[2:9] <- findDist(data, depth, seqVec)                 # find distributions
        if (method == method_names[1] & model == model_names[1]) {
          plist[[i]] <- plotPatternplot(df, titleText = model_plot_names[match(model, model_names)], ylText = method_plot_names[match(method, method_names)], legend = F, xticks = F, yticks = F, showPanel = F)
        } else if (method == method_names[1]) {
          plist[[i]] <- plotPatternplot(df, titleText = model_plot_names[match(model, model_names)], legend = F, xticks = F, yticks = F, showPanel = F)
        } else if (model == model_names[1]) {
          plist[[i]] <- plotPatternplot(df, ylText = method_plot_names[match(method, method_names)], legend = F, xticks = F, yticks = F, showPanel = F)
        } else {
          plist[[i]] <- plotPatternplot(df, legend = F, xticks = F, yticks = F, showPanel = F)
        }
      }
      i <-  i + 1
    }
  }
  plist
}
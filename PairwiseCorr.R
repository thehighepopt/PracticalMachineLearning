#MICROSTRATEGY_BEGIN
#
#DESC PairwiseCorr measures the correlation between pairs of numeric variables to show how they behave with respect to each other.
#DESC The output 'Result' returns "Ok" when the correlations were calculated with no errors.
#DESC Revision A
#
#RVAR Labels -input -vector
#RVAR Vars -input -numeric -vector -repeat
#
#RVAR HasLabels -parameter BooleanParam1
#RVAR WindowSize -parameter NumericParam1
#RVAR ImageName -parameter StringParam8
#RVAR FileName -parameter StringParam9
#
#RVAR Result -output -string -vector
#
#Version 1.0 Metric Expression (uses the 27 Pre-defined Parameters):
#EXP-v1 Result: RScript<_RScriptFile="PairwiseCorr.R", _InputNames="Labels, Vars", BooleanParam1=TRUE, NumericParam1=0, StringParam8="PairwiseCorr", StringParam9="PairwiseCorr">(Labels, Vars)
#
#Version 2.0 Metric Expression (uses _Params for parameters):
#EXP-v2 Result: RScript<_RScriptFile="PairwiseCorr.R", _InputNames="Labels, Vars", _Params="HasLabels=TRUE, WindowSize=0, ImageName='PairwiseCorr', FileName='PairwiseCorr'">(Labels, Vars)
#
#MICROSTRATEGY_END

mstr.ErrMsg <- tryCatch({                                      #tryCatch for Exception Handling
  if(exists("mstr.WorkingDir")) setwd(mstr.WorkingDir)         #Working Directory if executed by MicroStrategy

  timer <- proc.time()                                         #Start a timer to measure how long it takes

  #Check to see if package(s) are installed, install if not and then load
  CheckInstallPackages <- function(pkgs) {                     #pkgs is a vector of strings with length >= 1
    x <- lapply(pkgs, function(pkg){                           #For each pkg in pkgs (attempt to load each package one at a time):
      if(!do.call("require", list(pkg))) {                     #  Load the package if available,
        try(install.packages(pkg, lib=.Library,
                             repos="http://cran.rstudio.com")) #    Silently attempt to install into the default library
        tryCatch(do.call("library", list(pkg)),                #    Now attempt to load the package, catch error if it wasn't installed
          error = function(err) {                              #    Catch if we're unable to install into the default library
            if(!interactive()) {                               #      If non-interactive, install into this user's personal library
              personalLibPath <- Sys.getenv("R_LIBS_USER")     #        Get the path to this user's personal library
              if(is.na(match(personalLibPath, .libPaths()))) { #        If the personal library is not in the list of libraries
                dir.create(personalLibPath, recursive = TRUE)  #          Then create the personal library
                .libPaths(personalLibPath)                     #          And add the personal library to the list of libraries
              }
              install.packages(pkg, lib=personalLibPath,       #        Attempt to install the package into the personal library
                              repos="http://cran.rstudio.com") #          if this fails, raise the error back to the report
              do.call("library", list(pkg))                    #        Finally, attempt to load the package
            }
          }
        )
      }
    })
  }
  
  ###Function for generating histogram
  panel.hist <- function (x, ...) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr=c(usr[1:2], 0, 1.5))
    h <- hist(x, plot=FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="grey90", ...)
  }
  ###Function for generating pairwise correlations
  panel.cor <- function (x, y, digits=2, prefix="", cex.cor, ...) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- (cor(x, y, use="complete"))
    rc <- round(((r+1)/2)*RES)
    ell <- ellipse(r, t = 0.43) + 0.5
    polygon(ell, col=clr[rc])
    lines(ell)
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    cex.cor <- 0.8/strwidth("-0.99")
    if(abs(r)<0.5) {
      tClr <- c("white", "black")
    } else {
      if(r>0.5) {
        tClr <- c(clr[RES], "white")
      } else {
        tClr <- c(clr[1], "white")
      }
    } 
    text(0.49, 0.51, txt, cex=cex.cor, col=tClr[1])
    text(0.51, 0.49, txt, cex=cex.cor, col=tClr[1])
    text(0.51, 0.51, txt, cex=cex.cor, col=tClr[1])
    text(0.49, 0.49, txt, cex=cex.cor, col=tClr[1])
    text(0.50, 0.50, txt, cex=cex.cor, col=tClr[2])
  }
  ###Function for generating correlation matrix plot
  GeneratePlot <- function(data) {
    pairs(data, labels=colnames(data),
          diag.panel=panel.hist, 
          cex.labels = 0.8*(par("fin")[1]/max(strwidth(colnames(data), units="inches"))),
          upper.panel=panel.smooth,
          lower.panel=panel.cor)
  }
  ###Function for generating a sorted vector of n random numbers 
  ###  that follow a particular statistical distribution
  GetVector <- function(type, n=100, pA=0, pB=1, pC=0, pD=FALSE) {
    sort(switch(tolower(substr(type, 1, 2)),
                be = rbeta(n, pA, pB),
                bi = rbinom(n, pA, pB), 
                ca = rcauchy(n, pA, pB),
                ch = rchisq(n, pA, pB),
                ex = rexp(n, a), 
                fd = rf(n, pA, pB, pC),
                ga = rgamma(n, pA, pB, pC),
                ge = rgeom(n, a), 
                hy = rhyper(n, pA, pB, pC), 
                ln = rlnorm(n, pA, pB), 
                mu = rmultinom(n, pA, pB),
                nb = rnbinom(n, pA, pB, pC),
                po = rpois(n, a), 
                st = rt(n, pA, pB),
                un = runif(n, pA, pB), 
                we = rweibull(n, pA, pB),
                rnorm(n, pA, pB)), pD)
  }
  
  #Get the data
  if(exists("mstr.ExFlag")) {                                  #If this is executed by MicroStrategy
    df <- data.frame(Labels, Vars)                             #  Create a data frame from the input variables
    colnames(df) <- mstr.InputNames                            #  Name these variables
    rowCount <- length(df[, 1])                                #  Get the number of records/observations
    varCount <- length(colnames(df))                           #  Get the number of variables
    if(HasLabels) varCount <- varCount-1                       #  If the first variable are labels, then don't count the first column of Labels
  } else {                                                     #If this is NOT via a MicroStrategy Report Execution
    set.seed(42)                                               #  Set random number seed for consistency
    varCount <- 10                                             #  Number of Variables
    rowCount <- 100                                            #  Number of Records
    HasLabels <- TRUE                                          #  Data has labels in Column 1
    WindowSize <- 50                                           #  Set the window size
    FileName <- "PairwiseCorr_console"                         #  Set the name for saving output
    ImageName <- FileName                                      #  Set the name for saving image
    #df <- read.csv("MyData.csv", header=TRUE)                 #  Optional: Read data from CSV (uncomment and comment out remaining)
    type <- c("Unif", "Norm", "Norm", "Norm", "Weibull", "Weibull", "Weibull", "Beta", "Beta", "Beta")
    pA <-    c(0,      0,      1,       -1,    0.5,        1.5,      3.0,       0.5,     2,     5)
    pB <-    c(1,      1,      2,      0.5,    2,          3,        4,         0.5,     2,     1)
    pC <-    c(0,      0,      0,      0,      0,          0,        0,         0,       0,     0)
    pD <-  c(TRUE,   FALSE,  TRUE,   FALSE,  TRUE,       FALSE,    TRUE,      FALSE,   TRUE, FALSE)
    df <- as.data.frame(matrix(nrow=rowCount, 
                               ncol=varCount+1))               #  Create the data frame
    df[, 1] <- seq(1, rowCount)                                #  Set the Labels in Column 1
    colnames(df)[1] <- "Label"                                 #  Set the column 1 name
    randomNoise <- 0.5                                         #  % of randonmess to add to the data as noise
    for (c in 1:varCount) {                                    #  For each variable
      df[, c+1] <- GetVector(type[c], 
                             rowCount, pA[c], pB[c], 
                             pC[c], pD[c])                     #    Get a random vector
      colnames(df)[c+1] <- paste(type[c], "(", pA[c], 
                                 ", ", pB[c], ")", 
                                 sep="")                       #    Set the variable name
      df[, c+1] <- df[, c+1] * 
        (randomNoise*runif(length(df[, c+1]), 
                           min(df[, c+1]), 
                           max(df[, c+1])) +
         (1-randomNoise))                                      #    Add some noise so the correlations aren't so perfect
    }
  }
  
  #Handle parameters
  if((WindowSize<2) || (WindowSize>rowCount)) {                #Make sure Window Size is value
    WindowSize = rowCount                                      #  No window -- all data is used once
  }
  numPairs <- (varCount*(varCount-1))/2                        #Number or pairs to be correlated
  pairRows <-  rowCount-WindowSize                             #Number of correlations to capture per pair
  if(HasLabels) {                                              #Make the label column is not used
    useCols <- -1                                              #  Ignore the first label column, use the rest
    rownames(df) <- df[, 1]
  } else {
    useCols <- 1:varCount                                      #  Use all the columns
  }
  #Generate the results table
  t <- as.data.frame(matrix(nrow=numPairs*pairRows, 
                            ncol=6))                           #Create flattened table for the pairwise correlations
  colnames(t) <- c("Index", "Label", "A", "B", "A-B", "Corr")  #Set column headers
  r <- 1                                                       #r is the current row, starting at 1\
  #Generate results for each window (if any)
  if(WindowSize<rowCount) {                                    #WindowSize is less than the data, so calculate each window
    for (q in (WindowSize):rowCount) {                         #  For each window, create a table of pair-wise correlations
      matCorr <- cor(df[(q-WindowSize):q, useCols])            #    Calculate the pairwise correlations
      for (i in 1:(length(rownames(matCorr))-1)) {             #    For each variable on rows
        for (j in (i+1):length(colnames(matCorr))) {           #      For each other variable on columns
          t[r, 1] <- q                                         #        Column 1 = the index of the last observation in the window
          t[r, 2] <- as.character(df[q, 1])                    #        Column 2 = the label of the last observation in the window
          t[r, 3] <- rownames(matCorr)[i]                      #        Column 3 = First Variable Name
          t[r, 4] <- rownames(matCorr)[j]                      #        Column 4 = Second Variable Name
          t[r, 5] <- paste(t[r, 3], t[r, 4], sep="-")          #        Column 5 = Pair Name
          t[r, 6] <- matCorr[i, j]                             #        Column 6 = Correlation Coefficient
          r <- r+1                                             #        Done with this row, increment for the next one
        }
      }
    }
  }
  #Generate the results table for all the data
  matCorr <- cor(df[, useCols])                                #Finally, compute the correlation for all the data
  for (i in 1:(length(rownames(matCorr))-1)) {                 #      For each variable on rows
    for (j in (i+1):length(colnames(matCorr))) {               #        For each other variable on columns
      t[r, 1] <- rowCount+1                                    #          Column 1 = 1 more than the last index
      t[r, 2] <- "ALL"                                         #          Column 2 = "ALL"
      t[r, 3] <- rownames(matCorr)[i]                          #          Column 3 = First Variable Name
      t[r, 4] <- rownames(matCorr)[j]                          #          Column 4 = Second Variable Name
      t[r, 5] <- paste(t[r, 3], t[r, 4], sep="-")              #          Column 5 = Pair Name
      t[r, 6] <- matCorr[i, j]                                 #          Column 6 = Correlation Coefficient
      r <- r+1                                                 #          Done with this row, increment for the next one
    }
  }
  #Save .Rdata, .csv, and .xml files  
  if(nchar(FileName)>0) {                                      #Output files if there's a file name
    timerXeq <-(proc.time() - timer)                           #  Capture Execution Timer for Rdata file
    save(list=c("df", "matCorr", "t", "timerXeq"),
         file=paste(FileName, "Rdata", sep = "."), version=2)  #  Persist objects to file
    write.csv(t, file=paste(FileName, "csv", sep="."))         #  Save flatted table to CSV
    CheckInstallPackages("XML")                                #  Load the XML package
    children <- apply(t, MARGIN=1, 
                      function(row) 
                        newXMLNode("Pair", 
                                   attrs=row))                 #  Create child nodes for pair
    root <- newXMLNode("Pairs", .children = children)          #  Add children to the XML root node
    saveXML(root, file=paste(FileName, "xml", sep="."))        #  Save the XML
  }
  #Correlation plotting
  CheckInstallPackages("ellipse")                              #Load the Ellipse package
  col1 <- c("red", "white", "blue")                            #Red-White-Blue (Light)      
  col2 <- c("#A50F15", "#DE2D26", "#FB6A4A", "#FCAE91",
            "#FEE5D9", "white", "#EFF3FF", "#BDD7E7", 
            "#6BAED6", "#3182BD", "#08519C")                   #Red-White-Blue (Dark)
  col3 <- c("#7F0000", "red", "#FF7F00", "yellow", 
            "white", "cyan", "#007FFF", "blue", "#00007F")     #Red-Orange-Yellow-White-Blue
  col4 <- c("#7F0000", "red", "#FF7F00", 
            "yellow", "#7FFF7F", "cyan", 
            "#007FFF", "blue", "#00007F")                      #Rainbow
  RES <- 200                                                   #Number of Colors in Color Palette
  clr <- colorRampPalette(col2, space="rgb", 
                          interpolate="linear", 
                          bias=1)(RES)                         #Create color palette
  
  if(nchar(ImageName)>0) {                                     #Output image if there's an image name
    jpeg(paste(ImageName, "jpg", sep="."), 
         width=2000, height=2000)                              #  Save Pairwise Correlation plot to a file      
    GeneratePlot(df[, useCols])                                #  Generate the plot
    dev.off()                                                  #  Close plot
  }                  
  Result <- rep("Ok", rowCount)                                #Return Ok to indicate execute was successful
  
  if(!exists("mstr.ExFlag")) {                                 #If executed from the console
    GeneratePlot(df[, useCols])                                #  Generate the plot
    timer <-(proc.time() - timer)                              #  Stop the overall timer
    try(print(paste0("Success! Elasped Time=", 
                     format(timer[3], nsmall=3), "sec")))      #  Print success message with overall time
  }

  mstr.ErrMsg <- ""                                            #If we made it here, no errors were caught
}, error = function(err) {                                     #Catch block to report an error
  try(print(err))                                              #  Print error message to console (using try to continue on any print error)
  return(err$message)                                          #  Return error Message
})


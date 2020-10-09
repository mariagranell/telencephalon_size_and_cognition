# summary.SE function -----------------------------------------------------

detach(plyr)
detach(dplr)

summarySE <- function(data = NULL, measurevar, groupvars = NULL, na.rm = FALSE,
                      conf.interval = .95, .drop = TRUE) {
  library(plyr)

  # New version of length which can handle NA's: if na.rm==T, don't count them

  length2 <- function(x, na.rm = FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }

  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop = .drop,
                 .fun = function(xx, col) {
                   c(N = length2(xx[[col]], na.rm = na.rm),
                     mean = mean(xx[[col]], na.rm = na.rm),
                     sd = sd(xx[[col]], na.rm = na.rm)
                   )
                 },
                 measurevar
  )

  # Rename the "mean" column
  datac <- rename(datac, c("mean" = measurevar))

  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval / 2 + .5, datac$N - 1)
  datac$ci <- datac$se * ciMult

  return(datac)
}


# CHECK IF THEY REACHED THEIR PERFORMANCE LEVELS --------------------------

#to check the performance levels you have to.
#First, in the bottom of the page there is the function summarySE, run it. Second,
#You trial by trial add it or remove it to the function until the leanring is stuck

aslearningA <- subset(aslearning, selection == "A")
aslearningA$trial <- as.integer(aslearningA$trial)

aslearningA <- subset(aslearningA, trial < 1)
aa <- summarySE(data = aslearningA, groupvars = c("selection", "colour"), conf.interval = 0.95, .drop = TRUE,
                measurevar = "choice", na.rm = T)
aa

#Now for fun I created a loop so I can easily compare how much is the difference between keep adding trials.
#So in each collum I will have the difference between performances. If the number has a - means that they are performing worst
#And i would like to have the number that approximates the most to just 0. Which mean that there is no difference between the numbers and that they have reached
#their top performance

##In order to run summarySE you have to: 1 restart the sesion (it dosent like plr-dplr)
##2 run the code of the function that is below

EndPerformance <- function(data) {

  m <- matrix(0, ncol = 27, nrow = 4)
  colnames(m) <- 2:28
  row.names(m) <- c("A-r", "A-y", "B-r", "B-y")


  for (i in 2:28) {
    ###SELECTION A
    #Keep the subsetiong of the data frame
    dataA <- subset(data, selection == "A")
    dataA$trial <- as.integer(dataA$trial)

    #collect the data for collum 1
    d1A <- subset(dataA, trial > i)
    aA <- summarySE(data = d1A, groupvars = c("selection", "colour"), conf.interval = 0.95, .drop = TRUE,
                    measurevar = "choice", na.rm = T)
    a1A <- aA[, 4]

    #collect the data for collum 2
    d2A <- subset(dataA, trial > i + 1)
    bA <- summarySE(data = d2A, groupvars = c("selection", "colour"), conf.interval = 0.95, .drop = TRUE,
                    measurevar = "choice", na.rm = T)
    b1A <- bA[, 4]

    #get the minus collum
    cA <- round((b1A - a1A) * 1000, digits = 2) #i used round() and *100 to make it more simple when comparing
    m[1:2, i - 1] <- cA

    ###SELECTION B
    #Keep the subsetiong of the data frame
    dataB <- subset(data, selection == "B")
    dataB$trial <- as.integer(dataB$trial)

    #collect the data for collum 1
    d1B <- subset(dataB, trial > i)
    aB <- summarySE(data = d1B, groupvars = c("selection", "colour"), conf.interval = 0.95, .drop = TRUE,
                    measurevar = "choice", na.rm = T)
    a1B <- aB[, 4]

    #collect the data for collum 2
    d2B <- subset(dataB, trial > i + 1)
    bB <- summarySE(data = d2B, groupvars = c("selection", "colour"), conf.interval = 0.95, .drop = TRUE,
                    measurevar = "choice", na.rm = T)
    b1B <- bB[, 4]

    #get the minus collum
    cB <- round((b1B - a1B) * 1000, digits = 2) #i used round() and *100 to make it more simple when comparing
    m[3:4, i - 1] <- cB
  }

  return(m)

}

#to combine two data sets for the long term memory experiment.
#the order will say it it's first or last. First=frist
 combina<-function (exp1,exp2) {
  compl <- as.data.frame(exp1$id)
  compl$selection <- exp1$selection
  compl$replicate <- exp1$replicate
  compl$time <- exp1$time
  compl$trial <- print("exp1")
  names(compl) <- c("id", "selection", "replicate", "time", "trial")

  compf <- as.data.frame(exp2$id)
  compf$selection <- exp2$selection
  compf$replicate <- exp2$replicate
  compf$time <- exp2$seconds
  compf$trial <- print("exp2")
  names(compf) <- c("id", "selection", "replicate", "time", "trial")

  comp <- rbind(compl, compf)
 return(comp)}

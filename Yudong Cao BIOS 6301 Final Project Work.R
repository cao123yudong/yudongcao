# Yudong Cao | BIOS 6301 Final Project |  work.R

# importProjection
# @files, vector of file names
# return: data.frame
# This function reads and combines projection data into a single data set.  A position column should be added.
# ...
# Remove players with duplicate names.  In these cases keep the player with the highest value in the `fpts` column.
importProjection <- function(files) {
  k <- read.csv(files['k'], header=TRUE, stringsAsFactors=FALSE)
  qb <- read.csv(files['qb'], header=TRUE, stringsAsFactors=FALSE)
  rb <- read.csv(files['rb'], header=TRUE, stringsAsFactors=FALSE)
  te <- read.csv(files['te'], header=TRUE, stringsAsFactors=FALSE)
  wr <- read.csv(files['wr'], header=TRUE, stringsAsFactors=FALSE)
  cols <- unique(c(names(k), names(qb), names(rb), names(te), names(wr)))
  k[,'position'] <- 'k'
  qb[,'position'] <- 'qb'
  rb[,'position'] <- 'rb'
  te[,'position'] <- 'te'
  wr[,'position'] <- 'wr'
  cols <- c(cols, 'position')
  k[,setdiff(cols, names(k))] <- 0
  qb[,setdiff(cols, names(qb))] <- 0
  rb[,setdiff(cols, names(rb))] <- 0
  te[,setdiff(cols, names(te))] <- 0
  wr[,setdiff(cols, names(wr))] <- 0
  x <- rbind(k[,cols], qb[,cols], rb[,cols], te[,cols], wr[,cols])
  x <- x[order(x[,'fpts'],decreasing=T),]
  x <- x[!duplicated(x[,'PlayerName']),]
  return(x)
}

# scaleByGamesPlayed
# @x, data.frame
# @def.gp, numeric, default number of games played
# @alt.gp, named numeric vector, number of games played for teams that haven't played def.gp
# return: data.frame
# This function scales the numeric columns of x by `number of games played / 16`.
# ...
# The NFL season is 17 weeks long, and 10 weeks have been completed.
# Each team plays 16 games and has one week off, called the bye week.
# Most teams have already had their bye week, which means they've played 9 games.
# The stats for a player on a team with 9 games played would be scaled by 9/16.
scaleByGamesPlayed <- function(x, def.gp, alt.gp) {
  match.gp <- alt.gp[match(x[,'Team'], names(alt.gp))]
  match.gp[is.na(match.gp)] <- def.gp
  x[,-c(1,2,19)] <- x[,-c(1,2,19)]*match.gp/16
  return(x)
}

# importObserved
# @file, file name
# return: data.frame
# This function reads the observed data.
importObserved <- function(file) {
    y<-read.csv(file, header=TRUE, stringsAsFactors=FALSE)
    return(y)
}

# topPicks
# @x, data.frame
# @pos, named numeric vector, the number of players to keep per position
# return: data.frame; using the default `pos` would return a data set with 160 rows
# This function should subset the data set by keeping the top players at each position.
# ...
# Define `top player` by ordering the data set by the `fpts` column descendingly.
# If k=20, that means only keep 20 rows where position=='k'.
topPicks <- function(x, pos=c(k=20, qb=20, rb=40, wr=60, te=20)) {
  x <- x[order(x[,'position'],x[,'fpts'],decreasing=T),]
  tp1 <- x[x$position==names(pos)[1],][1:pos[1],]
  tp2 <- x[x$position==names(pos)[2],][1:pos[2],]
  tp3 <- x[x$position==names(pos)[3],][1:pos[3],]
  tp4 <- x[x$position==names(pos)[4],][1:pos[4],]
  tp5 <- x[x$position==names(pos)[5],][1:pos[5],]
  x2 <- rbind(tp1,tp2,tp3,tp4,tp5)
  return(x2)
}

# mergeDat
# @x, data.frame, projection data
# @y, data.frame, observed data
# return: data.frame
# This function should merge the projected data with the observed data by the player's name.
# ...
# Keep all rows from the projection data. If observed data is missing, set it to zero.
mergeDat <- function(x2, y) {
  z <- merge(x2,y,all.x=T,by.x="PlayerName",by.y="Name")
  z[is.na(z)] <- 0
  return(z)
}

# residuals
# @x, data.frame
# return: list, contains matrix for each position
# This function computes the difference between the observed data and the projected data, then splits the data by position.
# ...
# There are 15 columns of interest to compare.  See `lookup` in `test_task1.R` for a description of the columns.
# The place-holder code found in this function should be completely removed, however it gives an approximation of the output.
residuals <- function(x) {
  r <- data.frame(matrix(ncol = 16, nrow = 160))
  pos <- c(k=20, qb=20, rb=40, wr=60, te=20)
  pcol <- c('fg','fga','xpt','pass_att','pass_cmp','pass_yds','pass_tds','pass_ints',
                  'rush_att','rush_yds','rush_tds','rec_att','rec_yds','rec_tds','fumbles')
  ocol <- c("FGM","FGA","XPM","Att.pass","Cmp.pass","Yds.pass","TD.pass","Int.pass",
                 "Att.rush","Yds.rush","TD.rush","Rec.catch","Yds.catch","TD.catch","Fmb")
  colnames(r) <- c('position', pcol)
  r[,'position'] <- x[,'position']
  for(j in 1:15){
    r[pcol[j]] <- x[ocol[j]] - x[pcol[j]]
  }
  r <- r[order(r[,'position']),]
  r[,'position'] <- NULL
  res <- list(r[1:20,],r[21:40,],r[41:80,],r[81:100,],r[101:160,])
  names(res) <- c('k','qb','rb','te','wr')
  return(res)
}

# calcPoints
# @l, list, league object
# return: list
# This function calculates fantasy points based on league attributes.
# ...
# Create a new column `points` inside `l$stats`.
# Multiply columns in `l$stats` by respective value in `points`, and sum
calcPoints <- function(l) {
  x <- l$stats
  p <- attr(l,'points')
  if (sum(names(p) %in% c('fg', 'xpt', 'pass_yds', 'pass_tds', 'pass_ints',
                               'rush_yds', 'rush_tds', 'fumbles', 'rec_yds', 'rec_tds')) < length(names(p))) {
    warning('illegal stat category')
  } else {
    for (i in 1:length(names(p))) {
    x[,paste('p_',names(p)[i],sep='')] <- x[,names(p)[i]]*p[i]
    x[which(is.na(x[,paste('p_',names(p)[i],sep='')])),paste('p_',names(p)[i],sep='')] <- 0
    }
  }
  l$stats[,'points'] <- rowSums(x[,grep("^p_", names(x))])
  return(l)
}

# buildValues
# @l, list, league object
# return: list
# This function calculates dollar values based on league attributes.
# ...
# Create a new column `value` inside `l$stats`.
# Order `l$stats` by `value` descendingly.
# As an example if a league has ten teams and requires one kicker, the tenth best kicker should be worth $1.
# All kickers with points less than the 10th kicker should be removed from `l$stats`.
buildValues <- function(l) {
  x <- l$stats
  pos <- attr(l,'pos')
  nteams <- attr(l,'nteams') 
  cap <- attr(l,'cap')
  x2 <- x[order(x[,'points'], decreasing=T),]
  for(i in names(pos)) {
    ix <- which(x2[,'position'] == i)
    baseline <- pos[i]*nteams
    if(baseline == 0) {
      x2[ix, 'marg'] <- -1
    } else {
      x2[ix, 'marg'] <- x2[ix,'points'] - x2[ix[baseline],'points']
    }
  }
  x2[which(is.na(x2[, 'marg'])), 'marg'] <- 0
  x3 <- x2[x2[,'marg'] >= 0,]
  x3[,'value'] <- x3[,'marg'] * (nteams * cap - nrow(x3)) / sum(x3[,'marg']) + 1
  diffpos <- setdiff(c('k','qb','rb','wr','te'), names(pos))
  x3[which(x3[,'position'] == diffpos), 'value'] <- 0
  l$stats <- x3[order(x3[,'value'], decreasing=T),]
  return(l)
}

# league
# @stats, data.frame
# @nteam, numeric, number of teams
# @cap, numeric, money available for each team
# @pos, named numeric vector, position requirements
# @points, named numeric vector, point values
# return: list, league object
# This function creates an object of type `league`.
# ...
# All arguments should remain attributes of the object.
# They define the league setup and will be needed to calculate points and dollar values.
# Call `calcPoints` and `buildValues` to add points and dollar values.
league <- function(stats, nteams, cap, pos, points) {
  l <- list()
  if (nteams > 32 | nteams < 2) {
    stop('not enough players')
  }
  if (cap<10) {
    stop('not enough money for each team')
  }
  if (sum(names(pos) %in% c('qb','rb','wr','te','k')) < length(names(pos))) {
    warning('illegal position')
  }
  attributes(l) <- list(nteams=nteams, cap=cap, pos=pos, points=points)
  l$stats <- stats
  class(l) <- 'league'
  l <- calcPoints(l)
  l <- buildValues(l)
  return(l)
}

# print.league
# @l, list, league object
# @..., optional additional arguments
# return: league object, invisibly
# This function prints that `stats` element of the league object.
# ...
# At a minimum print these columns: PlayerName,Team,position,points,value.
print.league <- function(l, ...) {
  invisible(l)
  x <- l$stats
  print(x[,c('PlayerName','Team','position','points','value')])
}

# plot.league
# @l, list, league object
# @..., optional additional arguments
# return: plot object
# This function should create a scatterplot of dollar values.
# ...
# Add minimal plotting decorations, such as axis labels.
plot.league <- function(l, ...) {
  x <- l$stats
  plot(x[,'value'], xlab = 'player index', ylab = 'dollar values', main = 'Scatterplot of Dollar Values')
}

# boxplot.league
# @l, list, league object
# @..., optional additional arguments
# return: plot object
# This function should create a boxplot of dollar values by position.
# ...
# Add minimal plotting decorations.
boxplot.league <- function(l, ...) {
  x <- l$stats
  boxplot(x[,'value'] ~ x[,'position'], data = x, xlab = 'position', ylab = 'dollar values')
}

# hist.league
# @l, list, league object
# @..., optional additional arguments
# return: plot object
# This function should create a histogram of dollar values.
# ...
# Add minimal plotting decorations.
hist.league <- function(l, ...) {
  x <- l$stats
  hist(x[,'value'], xlab = 'dollar values', main = 'Histogram of Dollar Values')
}

# addNoise
# @stats, data.frame
# @noise, list of matrix residuals by position
# return: data.frame
# This function simulates new stats by adding error.
# ...
# As an example assume you want to simulate new projections for quarterbacks.
# The residuals for quarterbacks is a 20x15 matrix.
# Each row from this matrix is no longer identified with a particular player, but rather it's potential error.
# Given the original projection for the first quarterback, sample a row number from the matrix (1 to 20).
# Add the 15 columns from the sampled row to the 15 columns for the first quarterback.
# Repeat the process for every quarterback (as well as every position).
# Note that stats can't be negative so replace any negative values with 0.
addNoise <- function(stats, noise) {
  for (i in 1:nrow(stats)) {
    p <- as.character(stats[i,'position'])
    randrow <- sample(nrow(noise[[p]]),1)
    for (j in names(noise[[p]][randrow,])) {
      stats[i,j] <- max(stats[i,j] + noise[[p]][randrow,][,j], 0)
    }
  }
  return(stats)
}

# simleague
# @n, numeric, number of simulations
# @noise, list of matrix residuals by position
# @stats, data.frame
# @nteam, numeric, number of teams
# @cap, numeric, money available for each team
# @pos, named numeric vector, position requirements
# @points, named numeric vector, point values
# return: list, simleague object
# This function creates an object of type `simleague`.
# ...
# A `simleague` is an extension of the `league` object.
# It should contain a new element, `sim`, which is a data.frame.
# It should call `addNoise` `n` times, creating a new projection data set.
# For each simulation, a new league object should be created and its dollar values stored.
# `sim` should have a column for PlayerName and position as well as `n` columns for dollar values.
# `sim` should have the same number of rows as the argument `stats`.
# If a player has no value for a particular simulation, his dollar value should be saved as $0.
simleague <- function(n, noise, stats, nteams, cap, pos, points) {
    sim <- data.frame(matrix(0, nrow = nrow(stats), ncol = n))
    for (i in 1:n) {
      simstats <- addNoise(stats, noise)
      l <- league(simstats, nteams, cap, pos, points)
      for (j in 1:nrow(stats)) {
        if (stats$PlayerName[j] %in% l$stats$PlayerName) {
          sim[j,i] <- l$stats[which(l$stats$PlayerName==stats$PlayerName[j]),'value']
        }
      }
    }  
    l$sim <- cbind(stats[,'PlayerName'], stats[,'position'], sim)
    colnames(l$sim)[1:2] <- c('PlayerName','position')
    class(l) <- 'simleague'
    return(l)
}

# quantile.simleague
# @l, list, simleague object
# @probs, numeric, probability for desired percentile
# return: matrix
# This function calls `quantile` on the simulated dollar values for each player.
# ...
# The number of rows should equal `nrow(l$sim)`.
# The number of columns should equal `length(probs)`.
# Rownames should be set to PlayerName.

quantile.simleague <- function(l, probs=c(0.25, 0.50, 0.75)) {
  qtls <- matrix(0,nrow=nrow(l$sim),ncol=length(probs))
  for (i in 1:nrow(l$sim)) {
      qtls[i,] <- quantile(as.numeric(l$sim[i,3:ncol(l$sim)]),probs)   
  }
  rownames(qtls) <- l$sim$PlayerName
  colnames(qtls) <- paste(as.character(probs*100),"%",sep='')
  return(qtls)
}

# ci
# This function is complete and should not be modified.
ci <- function(object, ...) {
    UseMethod("ci", object)
}

# ci.default
# This function is complete and should not be modified.
ci.default <- function(object, ...) {
}

# ci.simleague
# @l, list, simleague object
# @probs, numeric, probability for desired percentile
# return: list, ci.simleague object
# This function calculates the top players based on league settings and simulated dollar values.
# ...
# A `ci.simleague` object contains a list element for each position.
# Should call `quantile.simleague`.
# If `probs` has length > 1, use the highest to determine dollar value.
# Sort dollar values in descending order.
# Each list element is a matrix.
# For a given position, the number of rows should equal the number of required players at that position.
# For a given position, the number of columns should equal `length(probs)`.
# For a given position, the rownames should be set to PlayerName.
ci.simleague <- function(l, probs=c(0.25, 0.50, 0.75)) {
  cisim <- list(qb=0,rb=0,wr=0,te=0,k=0)
  simqtl <- as.data.frame(quantile.simleague(l,probs))
  row.names(simqtl) <- l$sim$PlayerName
  simqtl <- cbind(l$sim$position,simqtl)
  colnames(simqtl)[1] <- 'position'
  dval <- simqtl[order(simqtl[,ncol(simqtl)],decreasing = T),]
  df <- split(dval,f=dval$position)
  for (i in 1:length(attributes(l)$pos)) {
    cisim[[i]] <- df[[names(cisim)[i]]][1:(attributes(l)$pos[i]*attributes(l)$nteams),-1]
  }
  names(cisim) <- c('qb','rb','wr','te','k')
  class(cisim) <- 'ci.simleague'
  return(cisim)
}

# print.simleague
# @l, list, simleague object
# @probs, numeric, probability for desired percentile
# return: simleague object, invisibly
# This function prints that `sim` element of the simleague object.
# ...
# Should call `quantile.simleague`.
# At a minimum print these columns: PlayerName,position,value (for each percentile).
print.simleague <- function(l, probs=0.5, ...) {
  s <- quantile.simleague(l,probs)
  s <- cbind(l$sim[,2],s)
  colnames(s) <- c('position',paste(as.character(probs*100),'%',sep=''))
  print(s)
  invisible(s)
}

# plot.ci.simleague
# @l, list, ci.simleague object
# @pos, character, position
# @..., optional additional arguments
# return: plot object
# This function should create a plot of dollar values for a position.
# ...
# Each percentile defined in the `ci.simleague` object should be plotted on a line.
# Add minimal plotting decorations.
plot.ci.simleague <- function(cisim, pos='qb', ...) {
  if (pos %in% c('qb','rb','wr','te','k')) {
    a <- cisim[[pos]]
    plot(NULL,xlab='Ranking',ylab='Dollar Value',xlim=c(1,nrow(a)),ylim=c(0,max(a[,3])))
    lines(x=seq(nrow(a)),y=a[,3],lty='dotted')  
    lines(x=seq(nrow(a)),y=a[,2],lty='dashed')
    lines(x=seq(nrow(a)),y=a[,1],lty='solid')
    legend('topright', c("25%","50%","75%"),lty=c("solid","dashed","dotted")) 
  } else {
    stop('pos not valid')
  }
}
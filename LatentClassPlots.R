### Latent Class Plotting from Mplus ----

LatentClassPlots <- function(data,
  lines='b',panels=NULL,proportions=TRUE,items=NULL) 
{ #function start
  
  # Read Mplus Output
  MplusOut <- readLines(data)
  emptyLines <- grep('^\\s*$',MplusOut)

  # Generate Class-Size Table
  tmp <- grep('^FINAL CLASS COUNTS AND PROPORTIONS',MplusOut)
  classTable <- MplusOut[(tmp[1]+6):(tmp[2]-3)]
  classTable <- data.frame(t(sapply(strsplit(classTable,'\\s+'),cbind)))[,-1]
  classTable <- data.frame(sapply(classTable,function(x) as.numeric(levels(x))[x]))
  
  names(classTable) <- c('class','abs','prop')

  # Extract Thresholds
  tmp <- paste0('Latent Class [1-',nrow(classTable),']')
  begin <- grep(tmp,MplusOut)[grep(tmp,MplusOut)%in%(grep('Thresholds',MplusOut)-2)]+3
  end <- sapply(begin, function(x) emptyLines[which(emptyLines==(x-2))+1]-1)
  
  thresh <- as.list(rep(NA,nrow(classTable)))
  for (i in seq_along(begin)) {
    thresh[[i]] <- MplusOut[begin[i]:end[i]]
  }

  thresh <- lapply(thresh, function(x) data.frame(t(sapply(strsplit(x,'\\s+'),cbind)))[,-1])
  thresh <- lapply(thresh, function(y) data.frame(y[,1],sapply(y[,-1],function(x) as.numeric(levels(x))[x])))
  
  # Generating Item/Threshold Markers
  for (i in seq_along(thresh)) {
    names(thresh[[i]]) <- c('param','logit','se','t','p')
    thresh[[i]]$vari <- NA
    thresh[[i]]$cate <- NA
    
    for (j in 1:nrow(thresh[[i]])) {
      thresh[[i]]$vari[j] <- strsplit(as.character(thresh[[i]]$param[j]),'\\$')[[1]][1]
      thresh[[i]]$cate[j] <- as.numeric(strsplit(as.character(thresh[[i]]$param[j]),'\\$')[[1]][2])
    }
  }

  # Setting Panel options
  if (is.null(panels)) {
    tmp <- ceiling(sqrt(nrow(classTable)))
    panels <- matrix(1:(tmp^2),ncol=tmp,nrow=tmp)
  }
  
  layout(panels)

  # Plotting
  for (i in seq_along(thresh)) {
  
    # Number of items
    nItems <- length(unique(thresh[[i]]$vari))
    
    # Check / Provide Items names
    if (is.null(items)) {
      items <- unique(thresh[[i]]$vari)
    }
    
    if (length(items)!=nItems) {
      stop('Please provide the correct number of item labels.')
    }
    
    # Empty plot
    plot(NA,
      ylim=c(0,1),xlim=c(1,nItems),
      ylab='Probability',xlab='',xaxt='n')
    
    # Add class proportions
    if (proportions) {
      title(main=paste('Proportion =',round(classTable$prop[i],3)))
    }
    
    # Add Item names
    axis(1,at=1:length(items),labels=items,xlab='Items',las=2) 
    
    # Add help-lines
    if (lines%in%c('h','b')) {
      abline(h=seq(0,1,by=.1),col='grey90') 
    }
    if (lines%in%c('v','b')) {
      abline(v=1:nItems,lty=2,col='grey25') 
    }
    
    # Expand grid to include thresholds for unobserved categories
    plottable <- expand.grid(unique(thresh[[i]]$vari),unique(thresh[[i]]$cate))
    names(plottable) <- c('vari','cate')
    plottable$p <- NA
    
    for (j in 1:nrow(plottable)) {
      filter <- (thresh[[i]]$vari==plottable$vari[j] & thresh[[i]]$cate==plottable$cate[j]) 
      if (any(filter)) {
        plottable$p[j] <- plogis(thresh[[i]]$logit[filter])
      }
      else {
        plottable$p[j] <- 0
      }
    }
    
    # Draw plot
    for (j in sort(unique(plottable$cate),TRUE)) {
      polygon(c(1,1:nItems,nItems),c(0,plottable$p[plottable$cate==j],0),
        col=paste('grey',round((90/(max(unique(plottable$cate))+1))*(j+1)),sep=''))
    }
  }
    
} #end function

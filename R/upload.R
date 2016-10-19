#' upload
#'
#' stat
#' @param
#' @keywords
#' @export
#' @examples
#' upload()
#'
#'

upload = function(path){
  # path = "C:\\Users\\Sili Fan\\Desktop\\WORK\\WCMC\\projects\\Zijuan Lai\\ZijuanVisu\\data\\dataNew.xlsx"
  d<-openxlsx::read.xlsx(path,sheet=1,colNames=T)
  d2<-openxlsx::read.xlsx(path,sheet=2,colNames=T)
  total<-openxlsx::read.xlsx(path,sheet=3,colNames=T)
  total = total[total$`organ-Mump`%in%d$organ&total$`organ-mALA`%in%d2$organ&
                  total$species%in%d$species&total$species%in%d2$species,]

  system_color = substring(rainbow(length(unique(d$system))),2)
  names(system_color) = unique(d$system)

  keys = unlist(by(d$species,d$system,function(x){
    sort(unique(x))
  }))
  color = as.character(keys)
    i = 1
    while(i < (length(color)+1)){
      if(is.na(as.numeric(substr(names(keys)[i],(nchar(names(keys)[i])+1)-1,nchar(names(keys)[i]))))){
        color[i] = system_color[names(keys)[i]]
        i = i+1
      }else{
        realName = substr(names(keys)[i],1,nchar(names(keys)[i])-1)
        if(!is.na(as.numeric(substr(realName,(nchar(realName)+1)-1,nchar(realName))))){
          realName = substr(realName,1,nchar(realName)-1)
        }
        colorLength = sum(grepl(realName,names(keys)))

        rbPal <- colorRampPalette(c(paste0("#",system_color[realName]),'#FFFFFF'))
        color[i:(i+colorLength-1)] = substring(head(rbPal(colorLength+1),-1),2)
        i = i+colorLength
      }
    }


    d$organ = as.factor(d$organ)
    d$species = as.factor(d$species)

    d2$organ = as.factor(d2$organ)
    d2$species = as.factor(d2$species)


    csvIntensity1 = do.call("rbind",by(d,d$organ,function(x){
      # x = d[d$organ==d$organ[2],]
      value = by(x$intensity,x$species,mean)
      value[is.na(value)] = 0
      return(as.numeric(value))
    }))

    csvIntensity2 = do.call("rbind",by(d2,d2$organ,function(x){
      # x = d2[d2$organ==d2$organ[2],]
      value = by(x$intensity,x$species,mean)
      value[is.na(value)] = 0
      return(as.numeric(value))
    }))


    x = d2[d2$organ==d2$organ[2],]#temp
    value = by(x$intensity,x$species,mean)#temp
    csvIntensity = data.frame(csvIntensity1,csvIntensity2, check.names = F)


    for(i in 1:nrow(csvIntensity)){

    }

    colnames(csvIntensity) = rep(names(value),2)
    csvIntensity = data.frame(rows =rownames(csvIntensity),csvIntensity,check.names = F)
    rownames(csvIntensity) = 1:nrow(csvIntensity)

    csvIntensity[csvIntensity==0] = 1
        return(list(data=csvIntensity,color=rep(substring(color,1,6),2)))

}

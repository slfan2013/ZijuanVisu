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
  library(jsonlite)
  library(RColorBrewer)
  library(openxlsx)
  # path = "C:\\Users\\Sili Fan\\Desktop\\WORK\\WCMC\\projects\\Zijuan Lai\\ZijuanVisu\\data\\dataNew.xlsx"
  d<-openxlsx::read.xlsx(path,sheet=1,colNames=T)
  d2<-openxlsx::read.xlsx(path,sheet=2,colNames=T)
  total<-openxlsx::read.xlsx(path,sheet=3,colNames=T)
  total = total[total$`organ-Mump`%in%d$organ&total$`organ-mALA`%in%d2$organ&
                  total$species%in%d$species&total$species%in%d2$species,]


  colCdt = c("#56B4E9","#E69F00", "#0072B2","#009E73","#D55E00")
  system_color = substring(colCdt,2)
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


    colnames(csvIntensity) = rep(names(value),2)
    csvIntensity = data.frame(rows =rownames(csvIntensity),csvIntensity,check.names = F)
    rownames(csvIntensity) = 1:nrow(csvIntensity)

    csvIntensity[,-1] = round(csvIntensity[,-1]/1000)
    # csvIntensity[csvIntensity==0] = 1
    color_bar = color
##############################################################################

    by1 = by(d,d$organ,function(x){
      # x = d[d$organ=="Brain",]
      m = by(x$intensity,x$species,FUN=mean,na.rm=T)
      value = m/sum(m,na.rm = T)
      value[is.na(value)] = 0
      return(as.numeric(value))
    })
    ratio = as.numeric(by(d$intensity,d$organ,mean)/sum(by(d$intensity,d$organ,mean)))
    csvFrequency1 = do.call("rbind",by1)*ratio



    by2 = by(d2,d2$organ,function(x){
      # x = d2[d2$organ=="Brain",]
      m = by(x$intensity,x$species,FUN=mean,na.rm=T)
      value = m/sum(m,na.rm = T)
      value[is.na(value)] = 0
      return(as.numeric(value))
    })
    ratio = as.numeric(by(d2$intensity,d2$organ,mean)/sum(by(d2$intensity,d2$organ,mean)))
    csvFrequency2 = do.call("rbind",by2)*ratio


    x = d2[d2$organ==d2$organ[2],]#temp
    value = by(x$intensity,x$species,mean)#temp
    csvFrequency = data.frame(csvFrequency1,csvFrequency2, check.names = F)


    colnames(csvFrequency) = rep(names(value),2)
    csvFrequency = data.frame(rows =rownames(csvFrequency),csvFrequency,check.names = F)
    rownames(csvFrequency) = 1:nrow(csvFrequency)



    csvFrequency[,-1] = round(csvFrequency[,-1]*100)
    # csvFrequency[csvFrequency==0] = 1

##############################################################################
    path = "data\\dataNew.xlsx"
    #path = "C:\\Users\\Sili Fan\\Desktop\\WORK\\WCMC\\projects\\Zijuan Lai\\ZijuanVisu\\data\\dataNew.xlsx"
    library(jsonlite)
    library(RColorBrewer)

    path = gsub("dataNew", "dataSunburst", path)



    d<-openxlsx::read.xlsx(path,sheet=1,colNames=T)
    d$index = 1:nrow(d)
    d2<-openxlsx::read.xlsx(path,sheet=2,colNames=T)
    d2$index = 1:nrow(d2)


    color = substring(c(brewer.pal(11,"Spectral"),"#939A9A","#cfd2d2"),2)
    names(color) = c("Intestine and Fecal Matter","Serum and Plasma","Plant Organ",
                     "Brain","Heart","Lung","Liver","Kidney","Cancer Tissue","Cancer Cell","Cell","Others","Insect Organ")
#1
    organ_ids = d$organ
    j = 1
    for(i in unique(d$organ)){
      organ_ids[organ_ids==i] = j
      j=j+1
    }
    organ_ids = as.numeric(organ_ids)

    #organ1.
    root_children = list()
    for(i in unique(d[,"organ"])){
      root_children[[i]] = list(
        id = organ_ids[d[,"organ"]==i][1],
        name = i,
        color_hex_triplet = color[i],
        graph_order = organ_ids[d[,"organ"]==i][1],
        parent_structure_id = 0,
        children = list(

        )
      )
    }

    #system.
    temp = by(d$system,d$organ,function(x){
      # x = d$system[d$organ==d$organ[1]]
      return(unique(x))
    })
    temp = sapply(temp,as.character)
    total = length(unique(d$organ))
    for(i in 1:length(temp)){
      total = length(temp[[i]])+total
    }

    temp2 = data.frame(system_ids=(length(unique(d$organ))+1):total,organ = rep(names(temp),sapply(temp,length)),system = unlist(temp),stringsAsFactors = F)
    temp3 = merge(d,temp2,by=c("organ","system"),all.x = T,sort = F)
    temp3 = temp3[order(temp3$index),]
    system_ids = temp3$system_ids



    system_color = c("a6611a","dfc27d","80cdc1","b2abd2","f4a582")
    names(system_color) = c("Photoautotrophic model","Bacteria","Human","Yeast","Animal model")


    for(i in unique(d[,"organ"])){
      for(j in unique(d[d$organ==i,"system"])){
        root_children[[i]]$children[[j]] = list(
          id = system_ids[d$system==j&d$organ==i][1],
          name = j,
          color_hex_triplet = system_color[j],
          graph_order = system_ids[d$system==j&d$organ==i][1],
          parent_structure_id = organ_ids[d$organ==i][1],
          children = list()
        )
      }
    }


    # species
    temp = by(d$species,d[,c("organ","system")],function(x){
      # x = d$system[d$organ==d$organ[1]]
      return(unique(x))
    })
    temp = sapply(temp,as.character)

    temp.length=sapply(temp,length)
    total = max(system_ids) + sum(temp.length)
    temp.m = merge(sort(unique(d$organ)),sort(unique(d$system)))
    temp2 = data.frame(species_ids=(max(system_ids)+1):total,organ = rep(as.character(temp.m$x),temp.length),
                       system = rep(as.character(temp.m$y),temp.length),
                       species = unlist(temp),stringsAsFactors = F)
    temp3 = merge(d,temp2,by=c("organ","system","species"),all.x = T,sort = F)
    temp3 = temp3[order(temp3$index),]
    species_ids = temp3$species_ids



    species_color = vector()
    for(sys in unique(d[,"system"])){
      temp = d[d$system==sys,]
      rbPal <- colorRampPalette(c(paste0("#",system_color[temp[1,"system"]]),'#FFFFFF'))
      tempCol = substring(head(rbPal(length(unique(temp[,"species"]))+1),-1),2)

      temp.by = by(temp$intensity,temp$species,mean)
      temp.num = as.numeric(temp.by)
      names(temp.num)=names(temp.by)
      temp.col = sort(temp.num,decreasing = T)
      species_color[names(temp.col)] =tempCol
    }


    for(i in unique(d[,"organ"])){
      for(j in unique(d[d$organ==i,"system"])){
        for(k in unique(d[d$organ==i&d$system==j,"species"])){
          root_children[[i]]$children[[j]]$children[[k]] = list(
            id = species_ids[d$organ==i&d$system==j&d$species==k][1],
            name = k,
            color_hex_triplet = species_color[k],
            graph_order = species_ids[d$organ==i&d$system==j&d$species==k][1],
            parent_structure_id = system_ids[d$organ==i&d$system==j][1],
            children = list()
          )
        }
      }
    }


    # create long (structure).
    long1 = list(
      success = TRUE,
      id = 999999999,
      start_row = 0,
      num_rows = 1,
      total_rows = 1,
      msg = list(
        list(
          id = 0,
          name = "UMP",
          color_hex_triplet= "ffffff",
          graph_order = 0,
          children =
            root_children
        )
      )
    )


    #unname.
    names(long1$msg[[1]]$children) = NULL
    for(i in 1:length(long1$msg[[1]]$children)){
      names(long1$msg[[1]]$children[[i]][["children"]]) = NULL
    }
    for(i in 1:length(long1$msg[[1]]$children)){
      for(j in 1:length(long1$msg[[1]]$children[[i]][["children"]]))
        names(long1$msg[[1]]$children[[i]][["children"]][[j]][['children']]) = NULL
    }




    #short 1
    expression_energy = intensity  = vector()
    temp = by(d$intensity,d$organ,mean)
    organ_meanIntensity1 = as.numeric(temp);names(organ_meanIntensity1)=names(temp)
    organ_percent1 = organ_meanIntensity1/sum(organ_meanIntensity1)



    # temp = by(d$intensity,d[,c("organ","system")],mean)
    # temp.name = merge(sort(unique(d$organ)),sort(unique(d$system)),all=T)
    # colnames(temp.name) = c("organ","system")
    # temp = data.frame(value=as.numeric(temp),temp.name,stringsAsFactors = F)
    #
    # organ_meanIntensity1 = temp$value;
    # organ_percent1 = organ_meanIntensity1/sum(organ_meanIntensity1)



    temp_system = by(d,d$organ,function(x){
      # x = d[d$organ==d$organ[1],]
      intensity = by(x$intensity,x$species,mean)
      percent = intensity/sum(intensity)
      return(list(intensity=intensity,percent=percent))
    })

    temp_species = by(d,d$organ,function(x){
      # x = d[d$organ==d$organ[1]&d$system==d$system[1],]
      by(x,x$system,function(y){
        intensity = by(y$intensity,y$species,mean)
        percent = intensity/sum(intensity)
        return(list(intensity=intensity,percent=percent))
      })
    })


    msg_short1 = list()
    for(i in 1:max(species_ids)){
      if(i<(max(organ_ids)+1)){
        expression_energy=organ_percent1[d$organ[organ_ids==i][1]]
        intensity = organ_meanIntensity1[d$organ[organ_ids==i][1]]
      }else if(i<(max(system_ids)+1)){
        expression_energy=temp_system[[d$organ[system_ids==i][1]]]$percent[[d$species[system_ids==i][1]]]*organ_percent1[d$organ[system_ids==i][1]]
        intensity = temp_system[[d$organ[system_ids==i][1]]]$intensity[[d$species[system_ids==i][1]]]
      }else{
        expression_energy=temp_species[[d$organ[species_ids==i][1]]][[d$system[species_ids==i][1]]]$percent[[d$species[species_ids==i][1]]]*
          temp_system[[d$organ[species_ids==i][1]]]$percent[[d$species[species_ids==i][1]]]*organ_percent1[d$organ[species_ids==i][1]]
        intensity=temp_species[[d$organ[species_ids==i][1]]][[d$system[species_ids==i][1]]]$intensity[[d$species[species_ids==i][1]]]
      }
      msg_short1[[i]] = list()
      msg_short1[[i]]$expression_energy = expression_energy
      msg_short1[[i]][["structure_id"]] = i
      msg_short1[[i]]$intensity = intensity
    }


    # create short (expression and intensity)
    short1 = list(
      success = TRUE,
      id = 999999999,
      start_row=0,
      num_rows = max(organ_ids)+1,
      total_rows = max(organ_ids)+1,
      msg = msg_short1
    )




#2

    organ_ids2 = d2$organ
    j = 1
    for(i in unique(d2$organ)){
      organ_ids2[organ_ids2==i] = j
      j=j+1
    }
    organ_ids2 = as.numeric(organ_ids2)

    #organ1.
    root_children = list()
    for(i in unique(d2[,"organ"])){
      root_children[[i]] = list(
        id = organ_ids2[d2[,"organ"]==i][1],
        name = i,
        color_hex_triplet = color[i],
        graph_order = organ_ids2[d2[,"organ"]==i][1],
        parent_structure_id = 0,
        children = list(

        )
      )
    }

    #system.
    temp = by(d2$system,d2$organ,function(x){
      # x = d2$system[d2$organ==d2$organ[1]]
      return(unique(x))
    })
    temp = sapply(temp,as.character)
    total = length(unique(d2$organ))
    for(i in 1:length(temp)){
      total = length(temp[[i]])+total
    }

    temp2 = data.frame(system_ids=(length(unique(d2$organ))+1):total,organ = rep(names(temp),sapply(temp,length)),system = unlist(temp),stringsAsFactors = F)
    temp3 = merge(d2,temp2,by=c("organ","system"),all.x = T,sort = F)
    temp3 = temp3[order(temp3$index),]
    system_ids2 = temp3$system_ids



    system_color = c("a6611a","dfc27d","80cdc1","b2abd2","f4a582")
    names(system_color) = c("Photoautotrophic model","Bacteria","Human","Yeast","Animal model")


    for(i in unique(d2[,"organ"])){
      for(j in unique(d2[d2$organ==i,"system"])){
        root_children[[i]]$children[[j]] = list(
          id = system_ids2[d2$system==j&d2$organ==i][1],
          name = j,
          color_hex_triplet = system_color[j],
          graph_order = system_ids2[d2$system==j&d2$organ==i][1],
          parent_structure_id = organ_ids2[d2$organ==i][1],
          children = list()
        )
      }
    }


    # species
    temp = by(d2$species,d2[,c("organ","system")],function(x){
      # x = d$system[d$organ==d$organ[1]]
      return(unique(x))
    })
    temp = sapply(temp,as.character)

    temp.length=sapply(temp,length)
    total = max(system_ids2) + sum(temp.length)
    temp.m = merge(sort(unique(d2$organ)),sort(unique(d2$system)))
    temp2 = data.frame(species_ids=(max(system_ids2)+1):total,organ = rep(as.character(temp.m$x),temp.length),
                       system = rep(as.character(temp.m$y),temp.length),
                       species = unlist(temp),stringsAsFactors = F)
    temp3 = merge(d2,temp2,by=c("organ","system","species"),all.x = T,sort = F)
    temp3 = temp3[order(temp3$index),]
    species_ids2 = temp3$species_ids



    species_color = vector()
    for(sys in unique(d2[,"system"])){
      temp = d2[d2$system==sys,]
      rbPal <- colorRampPalette(c(paste0("#",system_color[temp[1,"system"]]),'#FFFFFF'))
      tempCol = substring(head(rbPal(length(unique(temp[,"species"]))+1),-1),2)
      temp.by = by(temp$intensity,temp$species,mean)
      temp.num = as.numeric(temp.by)
      names(temp.num)=names(temp.by)
      temp.col = sort(temp.num,decreasing = T)
      species_color[names(temp.col)] =tempCol
    }


    for(i in unique(d2[,"organ"])){
      for(j in unique(d2[d2$organ==i,"system"])){
        for(k in unique(d2[d2$organ==i&d2$system==j,"species"])){
          root_children[[i]]$children[[j]]$children[[k]] = list(
            id = species_ids2[d2$organ==i&d2$system==j&d2$species==k][1],
            name = k,
            color_hex_triplet = species_color[k],
            graph_order = species_ids2[d2$organ==i&d2$system==j&d2$species==k][1],
            parent_structure_id = system_ids2[d2$organ==i&d2$system==j][1],
            children = list()
          )
        }
      }
    }


    # create long (structure).
    long2 = list(
      success = TRUE,
      id = 999999999,
      start_row = 0,
      num_rows = 1,
      total_rows = 1,
      msg = list(
        list(
          id = 0,
          name = "UMP",
          color_hex_triplet= "ffffff",
          graph_order = 0,
          children =
            root_children
        )
      )
    )


    #unname.
    names(long2$msg[[1]]$children) = NULL
    for(i in 1:length(long2$msg[[1]]$children)){
      names(long2$msg[[1]]$children[[i]][["children"]]) = NULL
    }
    for(i in 1:length(long2$msg[[1]]$children)){
      for(j in 1:length(long2$msg[[1]]$children[[i]][["children"]]))
        names(long2$msg[[1]]$children[[i]][["children"]][[j]][['children']]) = NULL
    }




    #short 2
    expression_energy = intensity  = vector()
    temp = by(d2$intensity,d2$organ,mean)
    organ_meanIntensity2 = as.numeric(temp);names(organ_meanIntensity2)=names(temp)
    organ_percent2 = organ_meanIntensity2/sum(organ_meanIntensity2)



    # temp = by(d$intensity,d[,c("organ","system")],mean)
    # temp.name = merge(sort(unique(d$organ)),sort(unique(d$system)),all=T)
    # colnames(temp.name) = c("organ","system")
    # temp = data.frame(value=as.numeric(temp),temp.name,stringsAsFactors = F)
    #
    # organ_meanIntensity1 = temp$value;
    # organ_percent1 = organ_meanIntensity1/sum(organ_meanIntensity1)



    temp_system = by(d2,d2$organ,function(x){
      # x = d2[d2$organ=="Intestine and Fecal Matter",]
      intensity = by(x$intensity,x$species,mean)
      percent = intensity/sum(intensity)
      return(list(intensity=intensity,percent=percent))
    })

    temp_species = by(d2,d2$organ,function(x){
      # x = d2[d2$organ=="Intestine and Fecal Matter",]
      by(x,x$system,function(y){
        # y = x[x$system=="Animal model",]
        intensity = by(y$intensity,y$species,mean)
        percent = intensity/sum(intensity)
        return(list(intensity=intensity,percent=percent))
      })
    })


    msg_short2 = list()
    for(i in 1:max(species_ids2)){
      if(i<(max(organ_ids2)+1)){
        expression_energy=organ_percent2[d2$organ[organ_ids2==i][1]]
        intensity = organ_meanIntensity2[d2$organ[organ_ids2==i][1]]
      }else if(i<(max(system_ids2)+1)){
        expression_energy=temp_system[[d2$organ[system_ids2==i][1]]]$percent[[d2$species[system_ids2==i][1]]]*organ_percent2[d2$organ[system_ids2==i][1]]
        intensity = temp_system[[d2$organ[system_ids2==i][1]]]$intensity[[d2$species[system_ids2==i][1]]]
      }else{
        expression_energy=temp_species[[d2$organ[species_ids2==i][1]]][[d2$system[species_ids2==i][1]]]$percent[[d2$species[species_ids2==i][1]]]*
          temp_system[[d2$organ[species_ids2==i][1]]]$percent[[d2$species[species_ids2==i][1]]]*organ_percent2[d2$organ[species_ids2==i][1]]
        intensity=temp_species[[d2$organ[species_ids2==i][1]]][[d2$system[species_ids2==i][1]]]$intensity[[d2$species[species_ids2==i][1]]]
      }
      msg_short2[[i]] = list()
      msg_short2[[i]]$expression_energy = expression_energy
      msg_short2[[i]][["structure_id"]] = i
      msg_short2[[i]]$intensity = intensity
    }


    # create short (expression and intensity)
    short2 = list(
      success = TRUE,
      id = 999999999,
      start_row=0,
      num_rows = max(organ_ids2)+1,
      total_rows = max(organ_ids2)+1,
      msg = msg_short2
    )












    return(list(data=csvIntensity,color=rep(substring(color_bar,1,6),2),
                data2=csvFrequency,
                long1=toJSON(long1,auto_unbox=T),short1=toJSON(short1,auto_unbox=T),
                long2=toJSON(long2,auto_unbox=T),short2=toJSON(short2,auto_unbox=T)))

}

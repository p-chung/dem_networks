#Set your working directory
setwd("~/Dropbox/PAA2017_soc_sna/")

#install and load the igraph packages for network analysis
install.packages("igraph")
library(igraph)

#Make sure warnings don't stop loops
options(warn=0)


#Set up a container to house our results. We'll check to see if:
#nm - a given ego was never married,
#spouse.a - an ever married ego's spouse was alive, 
#mom.a and dad.a - an ego's mother or father are alive, 
#n.sib.bio - the number of siblings ego has had total
#n.sib.bio.a - the number of siblings ego has that are currently alive
#n.sib.all - the number of siblings ego has had total (including in-laws)
#n.sib.all.a - the number of siblings ego has that are currently alive (including in-laws)
#n.child.bio - the number of biological children to ego
#n.child.bio.a - the number of biological children currently alive to ego
#n.child.all - the number of children to ego (including in-laws)
#n.child.all.a - the number of children currently alive to ego (including in-laws)
#n.neph.bio - the number of nieces and nephews to ego
#n.neph.bio.a - the number of nieces and nephews currently alive to ego
#n.neph.all - the number of nieces and nephews to ego (including in-laws)
#n.neph.all.a - the number of nieces and nephews currently alive to ego (including in-laws)
#old.sib - if ego has a sibling
#old.sib.a - if ego has a sibing, is the oldest sibling alive
#old.sib.spouse - if ego has a sibing, is the oldest sibling married
#old.sib.spouse.a - if ego has a sibing, is the oldest sibling's spouse alive
#old.sib.child - if ego has a sibing, how many children has the oldest sibling had
#old.sib.child.a - if ego has a sibing, how many children has the oldest sibling had who are currently alive
#f.child - Has ego had a first child
#f.child.a - If ego has had a first child, is that child alive
#s.child - Has ego had a second child
#s.child.a - If ego has had a second child, is that child alive
#t.child - Has ego had a third child
#t.child.a - If ego has had a third child, is that child alive
#old.sib.1.child - Has ego's oldest sibling had a first child
#old.sib.1.child.a - If ego's oldest sibling has had a first child, is that child alive
#old.sib.2.child - Has ego's oldest sibling had a second child
#old.sib.2.child.a - If ego's oldest sibling has had a second child, is that child alive
#old.sib.3.child - Has ego's oldest sibling had a third child
#old.sib.3.child.a - If ego's oldest sibling has had a third child, is that child alive
#data - white or black population, year 2000
results <- data.frame(nm=NA, spouse.a=NA, mom.a=NA, dad.a=NA, n.sib.bio=NA, n.sib.bio.a=NA,n.sib.all=NA, n.sib.all.a=NA, n.child.bio=NA, n.child.bio.a=NA, n.child.all=NA, n.child.all.a=NA, n.neph.bio=NA, n.neph.bio.a=NA, n.neph.all=NA, n.neph.all.a=NA, old.sib=NA, old.sib.a=NA, old.sib.spouse=NA, old.sib.spouse.a = NA, old.sib.child=NA, old.sib.child.a=NA, f.child = NA, f.child.a = NA, s.child = NA, s.child.a = NA, t.child = NA, t.child.a = NA,old.sib.1.child=NA,old.sib.1.child.a=NA,old.sib.2.child=NA,old.sib.2.child.a=NA,old.sib.3.child=NA,old.sib.3.child.a=NA, data=NA)

#There are 2 datasets included, one for the black population in 2000, and one for the white population in 2000
#Note: populations are defined assuming a stable population based on the demographic rates observed in 2000
datasets <- c("2000_black", "2000_white")

#Start a for loop to populate our results
#loop over datasets
for(d in datasets){
  #load the data
  load(paste0(d,".RData"))
  #each dataset contains a number of simulations, all with the same rates, but with different random seeds
  #loop over each simulation
    for(j in 1:length(sims)){
      #take the data from the sim and put it into a data frame
      opop <- as.data.frame(sims[j])
      #pick a date - stable pop so the month doesn't actually matter
      year <- round(max(opop$dob)-80*12)
      #select alive people
      opop$alive <- as.numeric(opop$dod>year&opop$dob<year)
      #calculate everyone's age
      opop$age <- as.numeric((year - opop$dob) / 12)
      #create a dummy for alive or not
      opop.a <- opop[opop$alive==1,]
      #select 50-60 year olds
      #They were born bewtween 60*12 and 50*12 months ago
      opop.egos <- opop.a[opop.a$dob>(year-(60*12))&opop.a$dob<(year-(50*12)),]
      
      #create a temporary place holder to hold our results within a dataset, across all sims
      out <- data.frame(nm=NA, spouse.a=NA, mom.a=NA, dad.a=NA, n.sib.bio=NA, n.sib.bio.a=NA,n.sib.all=NA, n.sib.all.a=NA, n.child.bio=NA, n.child.bio.a=NA, n.child.all=NA, n.child.all.a=NA, n.neph.bio=NA, n.neph.bio.a=NA, n.neph.all=NA, n.neph.all.a=NA, old.sib=NA, old.sib.a=NA, old.sib.spouse=NA, old.sib.spouse.a = NA, old.sib.child=NA, old.sib.child.a=NA, f.child = NA, f.child.a = NA, s.child = NA, s.child.a = NA, t.child = NA, t.child.a = NA,old.sib.1.child=NA,old.sib.1.child.a=NA,old.sib.2.child=NA,old.sib.2.child.a=NA,old.sib.3.child=NA,old.sib.3.child.a=NA)

      
      #loop over all our selected egos
      for(i in 1:nrow(opop.egos)){
        #grab the data for our currently selected ego
        temp <- opop.egos[i,]
        #lets summarize ego's ties
        #not married?
        out[i,"nm"] <- is.na(temp$mate)
        if(out[i,"nm"]==0){
          out[i,"nm"][temp$dob_mate>year]<- 1
        }
        #spouse alive?
        out[i, "spouse.a"] <- ifelse(out$nm[i],NA,as.numeric(temp$dod_mate>year))

        #mom alive?
        out[i, "mom.a"] <- temp$dod_mom>year
        #dad alive?
        out[i, "dad.a"] <- temp$dod_pop>year
      
        #siblings, bio
        out[i, "n.sib.bio"] <- nrow(opop[(opop$pop==temp$pop |opop$mom==temp$mom)&opop$pid!=temp$pid,])
        out[i, "n.sib.bio.a"] <- sum(opop[(opop$pop==temp$pop |opop$mom==temp$mom)&opop$pid!=temp$pid,"alive"])
        #siblings, bio + in-law
        out[i, "n.sib.all"] <- nrow(
        rbind(opop[(opop$pop==temp$pop |opop$mom==temp$mom)&opop$pid!=temp$pid,],
              opop[opop$pid %in% opop[(opop$pop==temp$pop |opop$mom==temp$mom)&opop$pid!=temp$pid,"mate"],]))
        out[i, "n.sib.all.a"] <- sum(rbind(opop[(opop$pop==temp$pop |opop$mom==temp$mom)&opop$pid!=temp$pid,],
                                         opop[opop$pid %in% opop[(opop$pop==temp$pop |opop$mom==temp$mom)&opop$pid!=temp$pid,"mate"],])$alive)
        #children, bio
        out[i, "n.child.bio"] <- nrow(opop[(opop$pop==temp$pid|opop$mom==temp$pid),])
        out[i, "n.child.bio.a"] <- sum(opop[(opop$pop==temp$pid|opop$mom==temp$pid)&!is.na(opop$pid),"alive"])
        #children, bio+ in-law
        out[i, "n.child.all"] <- nrow(rbind(
          opop[(opop$pop==temp$pid|opop$mom==temp$pid),],
          opop[opop$pid %in%opop[(opop$pop==temp$pid|opop$mom==temp$pid),"mate"],]
          ))
        out[i, "n.child.all.a"] <- sum(rbind(
        opop[(opop$pop==temp$pid|opop$mom==temp$pid),],
        opop[opop$pid %in%opop[(opop$pop==temp$pid|opop$mom==temp$pid),"mate"],]
          )$alive)
        
        #select the data for all the children of ego
        opop.kid <- NULL
        opop.kid <- opop[opop$pop==temp$pid |opop$mom==temp$pid,]
        
        
        #First child
        out[i,"f.child"] <- ifelse(nrow(opop.kid)>0, 1,0)
        out[i,"f.child.a"] <- ifelse(nrow(opop.kid)>0, opop.kid$alive[opop.kid$age==sort(opop.kid$age, decreasing=T)[1]], NA)
        #Second child
        out[i,"s.child"] <- ifelse(nrow(opop.kid)>1, 1,0)
        out[i,"s.child.a"] <- ifelse(nrow(opop.kid)>1, opop.kid$alive[opop.kid$age==sort(opop.kid$age, decreasing=T)[2]], NA)
        #Third child
        out[i,"t.child"] <- ifelse(nrow(opop.kid)>2, 1,0)
        out[i,"t.child.a"] <- ifelse(nrow(opop.kid)>2, opop.kid$alive[opop.kid$age==sort(opop.kid$age, decreasing=T)[3]], NA)
        
        
        #nephews and nieces, bio
        sibs <- rbind(opop[(opop$pop==temp$pop |opop$mom==temp$mom)&opop$pid!=temp$pid,],
                    opop[opop$pid %in% opop[(opop$pop==temp$pop |opop$mom==temp$mom)&opop$pid!=temp$pid,"mate"],])$pid
      
        out[i, "n.neph.bio"] <- nrow(opop[(opop$pop%in%sibs|opop$mom%in%sibs|opop$mom%in%sibs|opop$pop%in%sibs),])
        out[i, "n.neph.bio.a"] <- sum(opop[(opop$pop%in%sibs|opop$mom%in%sibs|opop$mom%in%sibs|opop$pop%in%sibs),"alive"])
        #cousins, bio+ in-law
        out[i, "n.neph.all"] <- nrow(rbind(
          opop[(opop$pop%in%sibs|opop$mom%in%sibs|opop$mom%in%sibs|opop$pop%in%sibs),],
          opop[opop$pid %in%opop[(opop$pop%in%sibs|opop$mom%in%sibs|opop$mom%in%sibs|opop$pop%in%sibs),"mate"],]
          ))
        out[i, "n.neph.all.a"] <- sum(rbind(
          opop[(opop$pop%in%sibs|opop$mom%in%sibs|opop$mom%in%sibs|opop$pop%in%sibs),],
          opop[opop$pid %in%opop[(opop$pop%in%sibs|opop$mom%in%sibs|opop$mom%in%sibs|opop$pop%in%sibs),"mate"],]
          )$alive)
      
        #oldest siblings, bio
        opop.old.sib <- NULL
        opop.old.sib <- opop[(opop$pop==temp$pop |opop$mom==temp$mom)&opop$pid!=temp$pid,]
        opop.old.sib <- opop.old.sib[opop.old.sib$age == max(opop.old.sib$age),]
        opop.old.sib <- opop.old.sib[!is.na(opop.old.sib$pid),]
        out[i,"old.sib"] <- nrow(opop.old.sib)
        out[i,"old.sib.a"] <- sum(opop.old.sib$alive)
        #oldest siblings spouse
        out[i,"old.sib.spouse"] <- ifelse(nrow(opop.old.sib)==1,!is.na(opop.old.sib$mate),NA)
        out[i,"old.sib.spouse.a"] <- ifelse(nrow(opop.old.sib)==1,opop[opop$pid==opop.old.sib$mate,"alive"], NA)
        #oldest siblings children
        #only calculate if ego had an oldest sibling
        if(nrow(opop.old.sib)==1){
          #only calculate if the oldest sibling is still alive
          if(opop.old.sib$alive==0){
            out[i,"old.sib.child"]<-NA
            out[i,"old.sib.child.a"]<-NA
            
            out[i,"old.sib.1.child"]<-NA
            out[i,"old.sib.1.child.a"]<-NA
            out[i,"old.sib.2.child"]<-NA
            out[i,"old.sib.2.child.a"]<-NA
            out[i,"old.sib.3.child"]<-NA
            out[i,"old.sib.3.child.a"]<-NA
          }else{
            out[i,"old.sib.child"]<-nrow(opop[opop$pop==opop.old.sib$pid|opop$mom==opop.old.sib$pid,])
            out[i,"old.sib.child.a"]<-sum(opop[opop$pop==opop.old.sib$pid|opop$mom==opop.old.sib$pid,"alive"])
            
            #Oldest sibling's First child
            out[i,"old.sib.1.child"] <- ifelse(nrow(opop[opop$pop==opop.old.sib$pid|opop$mom==opop.old.sib$pid,])>0, 1,0)
            out[i,"old.sib.1.child.a"] <- ifelse(nrow(opop[opop$pop==opop.old.sib$pid|opop$mom==opop.old.sib$pid,])>0, opop$alive[opop$pop==opop.old.sib$pid|opop$mom==opop.old.sib$pid][opop[opop$pop==opop.old.sib$pid|opop$mom==opop.old.sib$pid,"age"]==sort(opop[opop$pop==opop.old.sib$pid|opop$mom==opop.old.sib$pid,"age"], decreasing=T)[1]], NA)
            #Oldest sibling's Second child
            out[i,"old.sib.2.child"] <- ifelse(nrow(opop[opop$pop==opop.old.sib$pid|opop$mom==opop.old.sib$pid,])>1, 1,0)
            out[i,"old.sib.2.child.a"] <- ifelse(nrow(opop[opop$pop==opop.old.sib$pid|opop$mom==opop.old.sib$pid,])>1, opop$alive[opop$pop==opop.old.sib$pid|opop$mom==opop.old.sib$pid][opop[opop$pop==opop.old.sib$pid|opop$mom==opop.old.sib$pid,"age"]==sort(opop[opop$pop==opop.old.sib$pid|opop$mom==opop.old.sib$pid,"age"], decreasing=T)[2]], NA)
            #Oldest sibling's Third child
            out[i,"old.sib.3.child"] <- ifelse(nrow(opop[opop$pop==opop.old.sib$pid|opop$mom==opop.old.sib$pid,])>2, 1,0)
            out[i,"old.sib.3.child.a"] <- ifelse(nrow(opop[opop$pop==opop.old.sib$pid|opop$mom==opop.old.sib$pid,])>2, opop$alive[opop$pop==opop.old.sib$pid|opop$mom==opop.old.sib$pid][opop[opop$pop==opop.old.sib$pid|opop$mom==opop.old.sib$pid,"age"]==sort(opop[opop$pop==opop.old.sib$pid|opop$mom==opop.old.sib$pid,"age"], decreasing=T)[3]], NA)
            
          }
        }else{
          out[i,"old.sib.child"]<-NA
          out[i,"old.sib.child.a"]<-NA
          
          out[i,"old.sib.1.child"]<-NA
          out[i,"old.sib.1.child.a"]<-NA
          out[i,"old.sib.2.child"]<-NA
          out[i,"old.sib.2.child.a"]<-NA
          out[i,"old.sib.3.child"]<-NA
          out[i,"old.sib.3.child.a"]<-NA
          
        }
      }
    #Keep track of our data
    out$data <- d
    #merge our temporary results container with our permanent results container
    results <- rbind(results, out)
  }    
}

#tabulate average rates of not marrrying
prop.table(table(results$nm[results$data=="2000_white"]))
prop.table(table(results$nm[results$data=="2000_black"]))
#tabulate average rates spouse alive
prop.table(table(results$spouse.a[results$data=="2000_white"]))
prop.table(table(results$spouse.a[results$data=="2000_black"]))
#tabulate average rates of mom alive
prop.table(table(results$mom.a[results$data=="2000_white"]))
prop.table(table(results$mom.a[results$data=="2000_black"]))
#tabulate average rates of dad alive
prop.table(table(results$dad.a[results$data=="2000_white"]))
prop.table(table(results$dad.a[results$data=="2000_black"]))
#tabulate the average number of siblings
summary(results$n.sib.bio[results$data=="2000_white"])
summary(results$n.sib.bio[results$data=="2000_black"])
#tabulate the average number of siblings currently alive
summary(results$n.sib.bio.a[results$data=="2000_white"])
summary(results$n.sib.bio.a[results$data=="2000_black"])
#tabulate the average number of siblings plus in-laws
summary(results$n.sib.all[results$data=="2000_white"])
summary(results$n.sib.all[results$data=="2000_black"])
#tabulate the average number of siblings in-laws currently alive
summary(results$n.sib.all.a[results$data=="2000_white"])
summary(results$n.sib.all.a[results$data=="2000_black"])
#tabulate the average number of biological children
summary(results$n.child.bio[results$data=="2000_white"])
summary(results$n.child.bio[results$data=="2000_black"])
#tabulate the average number of biological children currently alive
summary(results$n.child.bio.a[results$data=="2000_white"])
summary(results$n.child.bio.a[results$data=="2000_black"])
#tabulate the average number of biological children plus in-laws
summary(results$n.child.all[results$data=="2000_white"])
summary(results$n.child.all[results$data=="2000_black"])
#tabulate the average number of biological children plus in-laws currently alive
summary(results$n.child.all.a[results$data=="2000_white"])
summary(results$n.child.all.a[results$data=="2000_black"])
#tabulate the average number of nieces and nephews
summary(results$n.neph.bio[results$data=="2000_white"])
summary(results$n.neph.bio[results$data=="2000_black"])
#tabulate the average number of nieces and nephews currently alive
summary(results$n.neph.bio.a[results$data=="2000_white"])
summary(results$n.neph.bio.a[results$data=="2000_black"])
#tabulate the average number of nieces and nephews plus in-laws
summary(results$n.neph.all[results$data=="2000_white"])
summary(results$n.neph.all[results$data=="2000_black"])
#tabulate the average number of nieces and nephews plus in-laws currently alive
summary(results$n.neph.all.a[results$data=="2000_white"])
summary(results$n.neph.all.a[results$data=="2000_black"])

#tabulate the number of siblings
table(results$n.sib.bio[results$data=="2000_white"])
#tabulate the number of siblings alive
table(results$n.sib.bio.a[results$data=="2000_white"])
#tabulate the number of siblings plus in-laws
table(results$n.sib.all[results$data=="2000_white"])
#tabulate the number of siblings plus in-laws currently alive
table(results$n.sib.all.a[results$data=="2000_white"])
#tabulate the number of children
table(results$n.child.bio[results$data=="2000_white"])
#tabulate the number of children currently alive
table(results$n.child.bio.a[results$data=="2000_white"])
#tabulate the number of children plus in-laws
table(results$n.child.all[results$data=="2000_white"])
#tabulate the number of children plus in-laws currently alive
table(results$n.child.all.a[results$data=="2000_white"])
#tabulate the number of nieces and nephews
table(results$n.neph.bio[results$data=="2000_white"])
#tabulate the number of nieces and nephews currently alive
table(results$n.neph.bio.a[results$data=="2000_white"])
#tabulate the number of nieces and nephews plus in-laws
table(results$n.neph.all[results$data=="2000_white"])
#tabulate the number of nieces and nephews plus in-laws currently alive
table(results$n.neph.all.a[results$data=="2000_white"])

#repeat for the black population
table(results$n.sib.bio[results$data=="2000_black"])
table(results$n.sib.bio.a[results$data=="2000_black"])
table(results$n.sib.all[results$data=="2000_black"])
table(results$n.sib.all.a[results$data=="2000_black"])
table(results$n.child.bio[results$data=="2000_black"])
table(results$n.child.bio.a[results$data=="2000_black"])
table(results$n.child.all[results$data=="2000_black"])
table(results$n.child.all.a[results$data=="2000_black"])
table(results$n.neph.bio[results$data=="2000_black"])
table(results$n.neph.bio.a[results$data=="2000_black"])
table(results$n.neph.all[results$data=="2000_black"])
table(results$n.neph.all.a[results$data=="2000_black"])

    
#Does ego have an oldest sibling?
summary(results$old.sib[results$data=="2000_white"])
summary(results$old.sib[results$data=="2000_black"])
#Is ego's oldest sibling alive
summary(results$old.sib.a[results$data=="2000_white"])
summary(results$old.sib.a[results$data=="2000_black"])
#Does ego have a spouse
mean(results$old.sib.spouse[results$data=="2000_white"],na.rm=T)
mean(results$old.sib.spouse[results$data=="2000_black"],na.rm=T)
#Is ego's oldest sibling's spouse alive
summary(results$old.sib.spouse.a[results$data=="2000_white"],na.rm=T)
summary(results$old.sib.spouse.a[results$data=="2000_black"],na.rm=T)
    
#Tabulate ego's oldest sibling's children
table(results$old.sib.child[results$data=="2000_white"])
table(results$old.sib.child[results$data=="2000_black"])
#Tabulate ego's oldest sibling's children currently alive
table(results$old.sib.child.a[results$data=="2000_white"])
table(results$old.sib.child.a[results$data=="2000_black"])

#Does ego have a first child
summary(results$f.child[results$data=="2000_white"])
summary(results$f.child[results$data=="2000_black"])
#Is ego's oldes child alive
summary(results$f.child.a[results$data=="2000_white"])
summary(results$f.child.a[results$data=="2000_black"])
#Does ego have a second child
summary(results$s.child[results$data=="2000_white"])
summary(results$s.child[results$data=="2000_black"])
#is ego's second child alive
summary(results$s.child.a[results$data=="2000_white"])
summary(results$s.child.a[results$data=="2000_black"])
#does ego have a third child
summary(results$t.child[results$data=="2000_white"])
summary(results$t.child[results$data=="2000_black"])
#is ego's third child alive
summary(results$t.child.a[results$data=="2000_white"])
summary(results$t.child.a[results$data=="2000_black"])

#Does ego's oldest sibling have a first child
summary(results$old.sib.1.child[results$data=="2000_white"])
summary(results$old.sib.1.child[results$data=="2000_black"])
#is ego's oldest sibling's first child alive
summary(results$old.sib.1.child.a[results$data=="2000_white"])
summary(results$old.sib.1.child.a[results$data=="2000_black"])
#Does ego's oldest sibling have a second child
summary(results$old.sib.2.child[results$data=="2000_white"])
summary(results$old.sib.2.child[results$data=="2000_black"])
#is ego's oldest sibling's second child alive
summary(results$old.sib.2.child.a[results$data=="2000_white"])
summary(results$old.sib.2.child.a[results$data=="2000_black"])
#Does ego's oldest sibling have a third child
summary(results$old.sib.3.child[results$data=="2000_white"])
summary(results$old.sib.3.child[results$data=="2000_black"])
#is ego's oldest sibling's third child alive
summary(results$old.sib.3.child.a[results$data=="2000_white"])
summary(results$old.sib.3.child.a[results$data=="2000_black"])



#Look at network characteristics
#load white data
load("2000_white.RData")
#grab first simulation
opop.w <- as.data.frame(sims[1])

#load black data
load("2000_black.RData")
#grab first simulation
opop.b <- as.data.frame(sims[1])

#pick a date - stable pop so the month doesn't actually matter
year <- round(max(c(opop.w$dob,opop.b$dob))-80*12)
#select alive people
opop.w$alive <- as.numeric(opop.w$dod>year&opop.w$dob<year)
#calculate age
opop.w$age <- as.numeric((year - opop.w$dob) / 12)
#subset on being alive
opop.w.a <- opop.w[opop.w$alive==1,]

#select alive people
opop.b$alive <- as.numeric(opop.b$dod>year&opop.b$dob<year)
#calculate age
opop.b$age <- as.numeric((year - opop.b$dob) / 12)
#subset on being alive
opop.b.a <- opop.b[opop.b$alive==1,]

#set up containers for adjaceny matrices
opop.w.adj <- matrix(NA,nrow(opop.w.a),nrow(opop.w.a))
opop.b.adj <- matrix(NA,nrow(opop.b.a),nrow(opop.b.a))

#label rows and columns with pids
row.names(opop.w.adj) <- colnames(opop.w.adj) <- opop.w.a$pid
row.names(opop.b.adj) <- colnames(opop.b.adj) <- opop.b.a$pid

#generate adjacency matrices
for(k in 1:nrow(opop.w.a)){
  #print(k)
  opop.w.adj[k,] <- as.numeric(apply(apply(opop.w.a[,c(1,3,6,8)],1,match, table=opop.w.a[k,c(1,3,6,8)], nomatch=0,incomparables=NA)>0,2,sum)>0)
}
for(k in 1:nrow(opop.b.a)){
  #print(k)
  opop.b.adj[k,] <- as.numeric(apply(apply(opop.b.a[,c(1,3,6,8)],1,match, table=opop.b.a[k,c(1,3,6,8)], nomatch=0,incomparables=NA)>0,2,sum)>0)
}

#set diagonals to zero (you don't get to be linked to yourself)
diag(opop.w.adj)<-0
diag(opop.b.adj)<-0

#generate graph objects from adjaceny matrix
opop.w.graph <- graph_from_adjacency_matrix(opop.w.adj)
opop.b.graph <- graph_from_adjacency_matrix(opop.b.adj)

#calculate the distance table for white and black    
opop.w.dist_table <- distance_table(opop.w.graph)
opop.b.dist_table <- distance_table(opop.b.graph)

#plot distance distributions (white pop in black points)
plot(opop.w.dist_table$res/sum(opop.w.dist_table$res), ylim=c(0,max(c(opop.w.dist_table$res/sum(opop.w.dist_table$res),opop.b.dist_table$res/sum(opop.b.dist_table$res)))), xlim=c(0,max(c(length(opop.w.dist_table$res),length(opop.b.dist_table$res)))))
#add black pop in blue points
points(opop.b.dist_table$res/sum(opop.b.dist_table$res), col="blue")

#number of isolates per person (white then black)    
sum(components(opop.w.graph)$csize==1) / nrow(opop.w.adj)
sum(components(opop.b.graph)$csize==1) / nrow(opop.b.adj)

#number of components without isolates per person
(components(opop.w.graph)$no - sum(components(opop.w.graph)$csize==1)) / (nrow(opop.w.adj))
(components(opop.b.graph)$no - sum(components(opop.b.graph)$csize==1)) / (nrow(opop.b.adj))

#numer of components per person
(components(opop.w.graph)$no) / (nrow(opop.w.adj))
(components(opop.b.graph)$no) / (nrow(opop.b.adj))


#plot distance distributions (white pop in black points)
svg("bw_dist.svg",bg="transparent",width = 6,height = 4.5)

plot(opop.w.dist_table$res/sum(opop.w.dist_table$res), ylim=c(0,max(c(opop.w.dist_table$res/sum(opop.w.dist_table$res),opop.b.dist_table$res/sum(opop.b.dist_table$res)))), xlim=c(0,max(c(length(opop.w.dist_table$res),length(opop.b.dist_table$res)))),type='l',lty=3, lwd=2,ylab="",xlab="",bty='l')
lines(opop.b.dist_table$res/sum(opop.b.dist_table$res), lty=1, lwd=2)
legend(x="topright",bty='n',legend = c("Black","White"),horiz=T,lty=c(1,3),lwd=2)

dev.off()

#number of folks per component (excluding isolates)
(nrow(opop.w.adj)) / (components(opop.w.graph)$no - sum(components(opop.w.graph)$csize==1)) 
(nrow(opop.b.adj)) / (components(opop.b.graph)$no - sum(components(opop.b.graph)$csize==1))

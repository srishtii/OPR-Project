
################### Using 5 destinations for simplicity ##############################33

remove(destinations);destinations<-"Times Square,New York,New York"
destinations<-append(destinations,"Central Park,New York,New York")
destinations<-append(destinations,"Union Station,Washington, D.C.")
destinations<-append(destinations,"3600 S Las Vegas Blvd, Las Vegas, NV 89109")
destinations<-append(destinations,"Grand Central Terminal,New York,New York")
destinations<-append(destinations,"Magic Kingdom,Orlando,Florida")
destinations<-append(destinations,"Disneyland Resort,Anaheim,California")
destinations<-append(destinations,"Golden Gate Bridge,San Francisco Bay Area,California")

#https://en.wikipedia.org/wiki/Tourist_attractions_in_the_United_States

###################### Building the distance matrix ###########################

library(ggmap)
bignumber<-1000000


destinationsDist<-matrix(nrow=length(destinations),ncol=length(destinations))
destinationsDistv<-vector()
for(i in 1:length(destinations)){
  for(j in 1:length(destinations)){
    if(i==j){
      destinationsDist[i,j]<-bignumber;
    }
    else if (i<j){
      destinationsDist[i,j]<-mapdist(destinations[i],destinations[j],mode="driving")$miles
    }
    else{
      destinationsDist[i,j]<-destinationsDist[j,i]
    }
  } 
  Sys.sleep(3)
}

destinationsDist[is.na(destinationsDist)]<-bignumber

#### turning the distance matrix into a vector (to be used as the obj function) #############3

destinationsDistv<-as.vector(t(destinationsDist))

#################### Constraint 1 ########################

constraints1<-matrix(nrow = ncol(destinationsDist),ncol = ncol(destinationsDist)^2)
for(i in 1:ncol(destinationsDist)){
  for(j in 1:ncol(destinationsDist)^2){
    if((j>=(i-1)*ncol(destinationsDist)+1)&&
       (j<=i*ncol(destinationsDist))){
      constraints1[i,j]<-1
    }
    else{
      constraints1[i,j]<-0
    }
  }
}
head(constraints1,5)

################ Constraint 2 ###############################

constraints2<-diag(x = 1, nrow=ncol(destinationsDist), ncol = ncol(destinationsDist))
for(i in 2:ncol(destinationsDist)){
  constraints2<-cbind(constraints2,diag(x = 1, nrow=ncol(destinationsDist), ncol = ncol(destinationsDist)))
}
head(constraints2,5)
constraints<-matrix()
constraints<-rbind(constraints1,constraints2)

################# Combining the two contraints together ##########################

rhs<-rep(1,nrow(constraints));
signs<-rep('=',nrow(constraints))

######################## Solving the LP ########################

library(lpSolve)
solving<-function(){
  res<<-lp("min",
           destinationsDistv,
           constraints,
           signs,
           rhs,
           all.bin = TRUE
           presolve=TRUE
)
  ressolution<<-matrix(res$solution,ncol = ncol(destinationsDist),byrow = TRUE)
  
############# converting matrix into order ####################
  
  routeorder<<-which(ressolution==1, arr.ind=TRUE)
  ord.destinations<<-vector()
  for(i in 1:(nrow(routeorder))){
    if(i==1){
      ord.destinations[i]<<-routeorder[i,1]
    }else{
      ord.destinations[i]<<-routeorder[ord.destinations[i-1],1]
    }
  }
  ord.destinations_address<<-destinations[ord.destinations]
}
solving()

############### Identifying and eliminating sub routes ################

getsubroute<-function(routeorder){
  routeorder1<-routeorder
  routeorder1<-as.data.frame(routeorder1)
  routeorder1$subroute<-0
  routeorder1<-as.data.frame(routeorder1)
  ord.destinations1<-rep(0,nrow(routeorder1))
  i<-1
  subroutenum<-1
  while(sum(routeorder1$subroute==0)!=0){#-1
    if(i==1){
      ord.destinations1[i]<-routeorder1[i,1]
      routeorder1[i,3]<-subroutenum
    }else{
      ord.destinations1[i]<-routeorder1[ord.destinations1[i-1],1]
      
      if(routeorder1[ord.destinations1[i-1],3]==0){
        routeorder1[ord.destinations1[i-1],3]<-subroutenum
        
      }else{
        routeorder1[ord.destinations1[i-1],3]<-subroutenum
        subroutenum<-subroutenum+1
        ord.destinations1[i]<-routeorder1[match(0,routeorder1$subroute),1]  
      }
    }
    i<-i+1
  }
  routeorder1<-routeorder1[order(routeorder1[,3],routeorder1[,2]),]
  numsubroute<-unique(routeorder1[,3])
  for(i in 1:length(numsubroute)){#eliminate single city subroute
    if(sum(routeorder1[,3]==i) == 1){
      routeorder1<-routeorder1[!(routeorder1[,3])==i,]
    }
  }
  printrouteorder<<-routeorder1
  names(printrouteorder)<-c("City I","City J","SubRoute")
  for(i in 1:length(unique(routeorder1[,3]))){
    newconstraint<-rep(0,length(destinations)^2)
    for(j in 1:sum(routeorder1[,3]==i)){
      newconstraint[((routeorder1[routeorder1[,3]==i,1][j]-1)*length(destinations))+routeorder1[routeorder1[,3]==i,2][j]]<-1
      newconstraint[((routeorder1[routeorder1[,3]==i,2][j]-1)*length(destinations))+(routeorder1[routeorder1[,3]==i,1][j])]<-1
    }
    constraints<<-rbind(constraints,newconstraint)
    signs<<-append(signs,"<=")
    rhs<<-append(rhs,sum(routeorder1[,3]==i)-1)
  }
}



###################### Subtour Elimination ######################

iteration<-0 
getsubroute(routeorder)
a<- length(unique(printrouteorder[,3]))>1 && length(unique(ord.destinations))<length(destinations)
while (a!=0){
  getsubroute(routeorder)
  solving()
  iteration<-1+iteration
  print(paste("ITERATION: ",iteration))
  print("Subtours Eliminated:")
  print(printrouteorder)
  print(paste("Constraints:",nrow(constraints)))
  print(res)
  cat("/n")
  
a<- sum(length(unique(printrouteorder[,3])))>1, 
length(unique(ord.destinations))<length(destinations))
}

############### Plotting the Route in a Map ########################

route_df<-vector()
route_df1<- vector()
for(i in 1:(length(ord.destinations_address)-1)){
  route_df1 <- route(ord.destinations_address[i], ord.destinations_address[i+1], structure = "route")
  route_df<-rbind(route_df,route_df1)
}

route_df<-route_df[,8:9]
gcode<-as.data.frame(geocode(ord.destinations_address))

map<- qmap(location= "USA", source= "stamen", maptype= "toner", zoom=4)
map <- map + geom_path(aes(x = lon, y = lat),colour = "#00ACC1", size = .75,data = route_df, lineend = "round")
map <- map + geom_point(data=gcode, aes(x=gcode$lon, y=gcode$lat),size=2)


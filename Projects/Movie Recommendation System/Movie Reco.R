#Note
#The format of data is very important 
#When you are recommending Items to a person then first column has name of persons
#When you are matching product then first column has names of movies


#similarity function

simDistance<-function(x,p1,p2){
  #x:Dataset
  #p1:Person1
  #p2:Person2
  in1<-0
  in2<-0
  rn_p1<-0
  rn_p2<-0
  for(item in x[,2][x[,1]==p1]){
    in2<-c(in2,which(item==x[,2][x[,1]==p2]))
  }
  for(item in x[,2][x[,1]==p2]){
    in1<-c(in1,which(item==x[,2][x[,1]==p1]))
  }
  rn_p1<-x[,3][x[,1]==p1]
  rn_p2<-x[,3][x[,1]==p2]
  rn_p1<-rn_p1[sort(in1[-1])]
  rn_p2<-rn_p2[in2[-1]]
  #sum<-sum(sapply(rn_p2-rn_p1,"^",2,simplify=T))
  sum<-sum((rn_p2-rn_p1)^2)
  sim<-1/(1+sum)
  #Pearson Correlation Coefficient
  cor(rn_p1,rn_p2)
  
} 

#Ranking the Critics
topMatches<-function(x,person,top,similarity=simDistance){
  sim<-NA
  name<-NA
  for(i in unique(x[,1])){
    if(i!=person){
      sim<-c(sim,similarity(x,person,i))
      name<-c(name,i)
    }
  }
  df<-data.frame(critic=name[-1],similarity=sim[-1],stringsAsFactors = F)
  df<-df[order(-df[,2]),]
  df[1:top,]
}

#simdata
topMatches2<-function(x,person,similarity=simDistance){
  sim<-NA
  name<-NA
  for(i in unique(x[,1])){
    if(i!=person){
      sim<-c(sim,similarity(x,person,i))
      name<-c(name,i)
    }
  }
  df<-data.frame(critic=name[-1],similarity=sim[-1],stringsAsFactors = F)
  df
}

#Recommending Items
movies<-unique(criticsdf$Movies)#Note criticsdf was loaded into the
#Environment
persons<-unique(criticsdf$Name)
recoItem<-function(x,item=movies,similarity=topMatches2,person){
  rank<-NA
  mov<-NA
  i<-1
  recodf<-topMatches2(x,person)
  item_vec<-setdiff(item,x[,2][x[,1]==person])
  for(name in recodf$critic){
    for(movie in item_vec){
      i<-i+1
      rank<-c(rank,x[,3][x[,1]==name & x[,2]==movie])
      if(length(rank)<i){
        rank[i]<-NA
      }
    }
  }
  df<-as.data.frame(matrix(rank[-1],nrow=nrow(recodf),
                           ncol=length(item_vec),byrow=TRUE))
  colnames(df)<-item_vec
  for(i in 1:ncol(df)){
    df[,i]<-df[,i]*recodf[,2]
  }
  dff<-cbind(recodf,df)
  dff<-dff[order(-dff[,2]),]
  #Finding Similarity Score
  v<-0
  dff<-dff[dff[,2]>0,]
  for(i in 3:ncol(dff)){
    v<-c(v,(sum(dff[,i],na.rm = T))/sum(dff[,2][!is.na(dff[,i])]))
  }
  sm_df<-data.frame(v[-1],row.names = item_vec)
  colnames(sm_df)<-"Sim_Rating"
  dff
}
#R code faster
#1) Initialize each vector 
#2)Initialize and assign length to the vector in advance

#Matching Products
changeform<-function(x){
  ranking<-0 #initializing ranking vector
  persons<-0
  name<-colnames(x)
  j<-1
  nom<-unique(x[,2]) #movies
  non<-unique(x[,1]) #names
  n<-1
  df<-as.data.frame(matrix(c(1,1,1),ncol=3))
  colnames(df)<-c("Movies","Name","Rankings")
  for(item in nom){
    for(person in non){
      ranking<-c(ranking,x[,3][x[,1]==person & x[,2]==item])
      if(length(persons)==length(ranking)-1){
        persons<-c(persons,person)
      }
    }
    df<-rbind(df,data.frame("Movies"=rep(item,length(ranking[-1])),"Name"=persons[-1],"Rankings"=ranking[-1]))
    ranking<-0
    persons<-0
  }
  df[-1,]
}


#Let's consider the below platform matrix isthe structure of your cube
#each value in a 2-dimensional position represents the number of cubes present in the third dimension
#zero represents that there is a hole and water won't get accumulated in that space.

platform=matrix(data = c(3,3,3,3,5,3,
                         3,0,2,3,1,3,
                         3,1,2,3,1,3,
                         3,3,3,1,3,3),byrow = TRUE,nrow =4,ncol=6)



WaterStoredinPlatfrom<-function(platform){

##Creating a matrix that would store the outer boundary (column-wise) of cube to store the water in it
  
Column_outside_cavity<-matrix(nrow = 2,ncol = ncol(platform))

for(i in 1:(ncol(platform))){
  j=1
  while(platform[j,i]<platform[j+1,i] & (j+1)<nrow(platform)){
    j=1+j
  }
  k=nrow(platform)
  while(platform[k-1,i]>platform[k,i] & (k-1)>1 ){
    k=(k-1)
  }
  
  Column_outside_cavity[1,i]<-j
  Column_outside_cavity[2,i]<-k
} 

#Since, there will be no water storing space available outside the stored positions (column wise) 
#Hence, converting it to zero as zero represents there is no cube present at that position.

for(i in 1:ncol(platform)){
  if(Column_outside_cavity[1,i]!=1){
  platform[1:(Column_outside_cavity[1,i]-1),i]<-0
}
  }

for(i in 1:ncol(platform)){
  if(Column_outside_cavity[2,i]!=ncol(platform) & (Column_outside_cavity[2,i]+1)<=nrow(platform)){
  platform[(Column_outside_cavity[2,i]+1):nrow(platform),i]<-0
}
 }

##Similarly, Creating a matrix that would store the outer boundary (row-wise) of cube to store the water in it


row_cavity<-(matrix(nrow = nrow(platform),ncol = 2))
for(i in 1:nrow(platform)){
 j=1
while(platform[i,j]<platform[i,j+1] & (j+1)<ncol(platform)){
  j=1+j
}
k=ncol(platform)
while(platform[i,k-1]>platform[i,k] & (k-1)>1 ){
  k=(k-1)
}

row_cavity[i,1]<-j
row_cavity[i,2]<-k
} 

#Since, there will be no water storing space available outside the stored positions (row wise) 
#Hence, converting it to zero as zero represents there is no cube present at that position.

for(i in 1:nrow(platform)){
  if(row_cavity[i,2]!=nrow(platform) & (row_cavity[i,2]+1)<=ncol(platform)){
    platform[i,(row_cavity[i,2]+1):ncol(platform)]<-0
  }
}

for(i in 1:nrow(platform)){
  if(row_cavity[i,1]!=1){
    platform[i,1:(row_cavity[i,1]-1)]<-0
  }
}


####Found out the boundary that can store water inside


#Creating a function to find out the possible volume of water stored at each position (at i-th row and j-th column)

vol_water_contained<-function(platform,i,j){
  x_i=i
  y_j=j
if(i!=1 & j!=1 & i!=nrow(platform) & j != ncol(platform)){ #Not considering the outer boundaries of platform
    if(platform[i,j]!=0 & platform[i+1,j]!=0 & platform[i-1,j]!=0 & platform[i,j+1]!=0 & platform[i,j-1]!=0){ #Checking it's neighbour positions that it should not have any hole considering the outer boundaries of platform
#from i-th and j-th position going in all four direction to find the maximum height of the cube 
      a=i
      while(platform[a,j]<=platform[a+1,j]){
        a=1+a
        if(a==nrow(platform)){break
        }
      }
      b=i
      while(platform[b,j]<=platform[b-1,j]){
        b=b-1
        if(b==1){break
        }
      }
      c=j
      while(platform[i,c]<=platform[i,c+1]){
        c=1+c
        if(c==ncol(platform)){break
        }
      }
      d=j
      #if(platform[i,d]<=platform[i,d-1]){
      while(platform[i,d]<=platform[i,d-1]){
        d=d-1
        if(d==1){break
        }    
      }
      
##Once we found out the space where water can be accummulated, confirming in that space is there any hole
      z=matrix()
      m=expand.grid(seq((b+1),(a-1)),seq((d+1),(c-1)))
      for(w in 1:nrow(m)){
        z[w]=platform[m$Var1[w],m$Var2[w]]!=0
      }
      
      if(all(platform[(a-1),seq((d+1),(c-1))]!=0 &
         platform[(b+1),seq((d+1),(c-1))]!=0 &
         platform[seq((b+1),(a-1)),(c-1)]!=0 &
         platform[seq((b+1),(a-1)),(d+1)]!=0 &
         z)==TRUE){
        
##taking the minimum height of the obtained space and subtracting it with the current position to get the volume of water occupied by that particular i-th and j-th position        
      return(abs(min(platform[a,j],platform[b,j],platform[i,c],platform[i,d])-platform[x_i,y_j]))
    }else{
  return(0)
}
}else{return(0)
}
}else{
  return(0)
}
}

  




#######################
##Running the algoithm for an each position
vol_water<-matrix(nrow =nrow(platform),ncol=ncol(platform))
for(i in 1:nrow(platform)){
  for(j in 1:ncol(platform)){
vol_water[i,j]<-vol_water_contained(platform = platform,i,j)    
  }
}
total_volume_of_water_contained=sum(vol_water)
#print(total_volume_of_water_contained)
return(print(paste0("Total volume of water collected is ",total_volume_of_water_contained," cubic unit")))
}

#finally using the function to get the volume occupied by water by the cube structure.
WaterStoredinPlatfrom(platform)

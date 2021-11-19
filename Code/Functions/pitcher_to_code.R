#######################

#Provide a function for matching pitchers in the sports book archive with 
#First and last names based on team

#More work to do here
#Code for sorting out duplicates
both <- ind_L & ind
codenames[both]
pitchers[both,]
#For pitchers in both, we distinguish them based on team

ind_L <- ind_L & !ind
sum(ind_L)


ind_tot <- ind | ind_L
sum(ind_tot)

###############
pitcher_search<- function(string){
  char_min <- stringdist::stringdist(string, team_list)
  indx <- which.min(char_min)
  tm <- team_list[indx][1]
  tm
}
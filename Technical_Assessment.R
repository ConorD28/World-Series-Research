# Technical Assessment
# A program that is designed to determine the effective field goal percentage and percentage of team shots attempted within specific shot zones.
# by: Conor Desmond
# Date: 08/22/2022

shot_data = read.csv("shots_data (1).csv")
length(shot_data$fgmade)

NC_three_makes = 0 #Keeps track of non-corner threes made
NC_three_attempts = 0 #Keeps track of non-corner threes attempted
C_three_makes = 0 #Keeps track of corner threes made
C_three_attempts = 0 #Keeps track of corner threes attempted
Two_Pt_makes = 0 #Keeps track of two-pointers made
Two_Pt_attempts = 0 #Keeps track of two-pointers attempted
total_attempts = 0 #Keeps track of total shot attempts

shot_zones = function(i, team_name){
  for(i in 1:504){
  if(shot_data$team[i] == team_name){
    if(((abs(shot_data$y[i])^2 + abs(shot_data$x[i])^2) > 23.75^2) && shot_data$y[i] > 7.8){  #Non Corner 3's
      NC_three_attempts = NC_three_attempts + 1
      if(shot_data$fgmade[i] == 1)
        NC_three_makes = NC_three_makes + 1
      }
  
  else if(abs(shot_data$y[i]) <= 7.8 && abs(shot_data$x[i]) > 22){  #Corner 3's
    C_three_attempts = C_three_attempts + 1
    if(shot_data$fgmade[i] == 1)
      C_three_makes = C_three_makes + 1
  }
  
  else { #If this else is entered then a two-pointer was attempted
    Two_Pt_attempts = Two_Pt_attempts + 1
    if(shot_data$fgmade[i] == 1)
      Two_Pt_makes = Two_Pt_makes + 1
      }
    }
  }
  eFg_NC_three = ((NC_three_makes + (.5 * NC_three_makes))/NC_three_attempts) * 100 #effective field goal percentage within the non-corner three zone
  eFg_C_three = ((C_three_makes + (.5 * C_three_makes))/C_three_attempts) * 100 #effective field goal percentage within the corner three zone
  eFg_Two_Pt = (Two_Pt_makes/Two_Pt_attempts) * 100 #effective field goal percentage within the two-point zone
  
  print(paste((paste(team_name, " efG% in the non-corner three zone: ", (round(eFg_NC_three, 3)))), "%"))
  print(paste((paste(team_name, " efG% in the corner three zone: ", (round(eFg_C_three, 3)))), "%"))
  print(paste((paste(team_name, " efG% in the Two-point zone: ", (round(eFg_Two_Pt, 3)))), "%"))
  
  total_attempts = NC_three_attempts + C_three_attempts + Two_Pt_attempts #total shot attempts
  
  NC_three_shot_distribution = (NC_three_attempts/total_attempts) * 100 #shot distribution within the non-corner three zone
  C_three_shot_distribution = (C_three_attempts/total_attempts) * 100 #shot distribution within the corner three zone
  Two_Point_shot_distribution = (Two_Pt_attempts/total_attempts) * 100 #shot distribution within the Two-point zone
  
  print(paste((paste(team_name, " shot distribution in the non-corner three zone: ", (round(NC_three_shot_distribution, 3)))), "%"))
  print(paste((paste(team_name, " shot distribution in the corner three zone: ", (round(C_three_shot_distribution, 3)))), "%"))
  print(paste((paste(team_name, " shot distribution in the Two-point zone: ", (round(Two_Point_shot_distribution, 3)))), "%"))
}

shot_zones(i, "Team A")
shot_zones(i, "Team B")

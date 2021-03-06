testData <-as.data.frame( structure(c(0.747578202060495, -0.394878203920588, 0.0971471451253907, 
    0.798746200512111, 0.241444364734077, 0.361811022353942, -1.03480316140194, 0.0589386133691457, 
    -0.55419677392639, -0.562661660742482, 1.42423741634248, -2.040944224145, 0.117924257703877, 
    -0.481174112157237, -0.205061945286086, 1.20877246564713, 0.842186698796901, 
    -1.27648432948994, -0.319532170740518, 1.53328707600887, 0.512890221082854, -2.79816396813142, 
    0.714527896578262, 0.0776947411602069, 2.72888925126265, -1.07293876678246, 0.333538314622528, 
    1.19634716067723, -1.21543411186972, 0.00599397109241984, 0.205459738988958, 
    -0.914178994222314, -1.13237968372859, 2.81465057432105, -0.273045006345869, 
    0.00111711981794718), .Dim = c(6L, 6L), .Dimnames = list(NULL, c("A1", "A2", 
    "A3", "B1", "B2", "B3"))))

testData2 <-as.data.frame( structure(list(Z1 = c(2.44, 2.66, 2.32, 2, 1.94, 2.21), Z2 = c(2.31, 
    2.5, 2.56, 1.78, 1.97, 2.15), Z3 = c(2.25, 2.43, 2.28, 1.91, 1.78, 2.37), Α1 = c(2.22, 
    2.41, 2.63, 2.06, 1.79, 2.04), Α2 = c(2.12, 2.53, 2.35, 2.13, 1.75, 2.16), Α3 = c(2.22, 
    2.68, 2.78, 2.19, 1.87, 2.15), Β1 = c(2.25, 2.69, 3.25, 2.16, 2.09, 2.09), Β2 = c(2.25, 
    2.69, 3.25, 2.15, 2.04, 2.09), Β3 = c(2.44, 2.47, 2.84, 2.43, 1.97, 2.04), C1 = c(2.31, 
    3.25, 3.16, 2.19, 1.81, 2), C2 = c(2.25, 2.94, 2.75, 2.5, 1.84, 2.16), C3 = c(2.12, 
    2.72, 2.69, 2.59, 1.87, 2.16), D1 = c(2.25, 3.22, 3.25, 2.66, 1.94, NA), D2 = c(2.22, 
    2.93, 2.03, 2.91, 1.85, NA), D3 = c(2.22, 3.06, 3.09, 3.03, 1.81, NA), E1 = c(2.38, 
    2.75, 2.87, 2.75, 1.85, NA), E2 = c(2.06, 2.81, 2.75, 2.53, 1.68, NA), E3 = c(2.12, 
    2.91, 2.78, 2.59, 1.69, NA), F1 = c(2.22, 2.56, 2.41, 2.09, 1.72, NA), F2 = c(2.03, 
    2.68, 2.53, 2.37, 1.72, NA), F3 = c(2.12, 2.78, 2.44, 2.16, 1.78, NA), G1 = c(2.28, 
    2.78, 3.65, 2.09, 1.87, NA), G2 = c(1.97, 2.78, 3.38, 2.22, 1.94, NA), G3 = c(2.16, 
    2.62, 3.43, 2.28, 1.84, NA), H1 = c(2.25, 2.6, 2.66, 2.15, 1.91, NA), H2 = c(2.06, 
    2.75, 2.56, 2.15, 1.82, NA), H3 = c(2, 2.78, 2.62, 2.06, 1.82, NA), I1 = c(2.07, 
    2.5, 3.19, 2.05, 1.87, NA), I2 = c(2.03, 2.69, 2.75, 2.35, 1.97, NA), I3 = c(1.9, 
    2.56, 2.72, 2.12, 1.75, NA), J1 = c(2.06, 2.53, 2.56, 2.15, 1.94, NA), J2 = c(1.9, 
    2.62, 2.75, 2.13, 1.72, NA), J3 = c(2, 2.66, 2.44, 2.22, 1.91, NA)), .Names = c("Z1", 
    "Z2", "Z3", "Α1", "Α2", "Α3", "Β1", "Β2", "Β3", "C1", "C2", "C3", "D1", 
    "D2", "D3", "E1", "E2", "E3", "F1", "F2", "F3", "G1", "G2", "G3", "H1", "H2", 
    "H3", "I1", "I2", "I3", "J1", "J2", "J3"), class = "data.frame", row.names = c(NA, 
    -6L)))

save(file='data/testData.rda',compress='xz',compression_level=9,list=c('testData','testData2'))

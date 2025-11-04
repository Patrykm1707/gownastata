#### LISTA 4

#Zad 1.
wad <- c(11,21,15,8,7,17,18)
porem <- c(7,4,9,3,12,18,5)

N <- choose(14,7) # liczba permutacji z ciekawosci
N
wez <- c(wad,porem) # wez to zlacz
wez

h <- c(1,0,1,0,1,0,1,0,1,0,1,0,0,1)

suma <- sum(wez[h==1])
suma

#Zad 2.
klas <- c(65,79,90,75,61,85,98,80,97,75)
nowe <- c(90,98,73,79,84,81,98,90,83,88)

alfons <- 0.05

diff <- klas - nowe
N <- 500
sumo <- sum(diff)
n <- length(diff)
stat <- numeric(n)
csgo <- 0
for (i in 1:N){
  for (j in 1:n){
    stat[j] <- ifelse(runif(1) < 0.5,diff[j],-diff[j])
  }
  if (sum(stat)!=sumo) csgo = csgo+1
}
(p.value <- csgo/N)
# p.value > 0.05 - brak podstaw do odrzucenia H0

#Zad 3. liczba permutacji
(permy <- 2**(length(klas)))

#Zad 4.
k15 <- c(141.85, 134.36, 131.87, 137.28, 122.72, 136.44, 128.96, 136.86)
k17 <- c(125.10, 122.00, 123.10, 119.92, 124.11, 121.91, 122.00, 135.95, 127.10, 125.10)

#H0 k15>k17
# MC
N <- 500
n <- length(k17)
sumo <- sum(k17)
csgo <- 0
kombi <- c(k15,k17)
for (i in 1:N){
  D <- sample(kombi,n)
  if(sum(D) <= sumo){ csgo = csgo+1 }
}
(p.value <- csgo/N)
# p-value < 0.05 - odrzucamy H0

#Zad 5.
k15 <- c(141.85, 134.36, 131.87, 137.28, 122.72, 136.44, 128.96, 136.86)
k17 <- c(125.10, 122.00, 123.10, 119.92, 124.11, 121.91, 122.00, 135.95, 127.10, 125.10)
k17_scaled <- k17*0.9

#H0 k15=k17*0.9
# MC
N <- 500
n <- length(k17_scaled)
sumo <- sum(k17_scaled)
csgo <- 0
kombi <- c(k15,k17_scaled)
for (i in 1:N){
  D <- sample(kombi,n)
  if(sum(D) != sumo){ csgo = csgo+1 }
}
(p.value <- csgo/N)
# p-value > 0.05 - brak podstaw do odrzucenia H0.

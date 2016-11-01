##--- set variables

set.seed(200)

eksd = as.Date(Sys.Date())+sample(c(-1, 1), 100, replace = TRUE)*sample(2000, 100, replace = TRUE)

dos = c(75, 100, 50, 125)



recept_data = data.frame(atc = as.character(rep(c("a","b1"), each = 50)), 
                         alph = rep(letters[1:10], length = 100), 
                         eksd = eksd, 
                         pnr = sample(c(2000,1000,3000,4000,5000,6000,7000,8000,9000), 100, replace = TRUE), 
                         atc = rnorm(100), 
                         strnum = sample(dos, 100, replace = TRUE), 
                         apk = sample(3, 100, replace = TRUE), 
                         packsize = sample(c(5, 9, 10, 18, 20), 100, replace = TRUE),
                         stringsAsFactors=FALSE)
str(recept_data$atc)
str(recept_data$eksd)
recept_data = recept_data[order(recept_data$pnr), ]



inddto = sort(sample(recept_data$eksd, 8*4, replace = TRUE))

inddto1 = inddto[1:8]
inddto2 = inddto[9:16]
inddto3 = inddto[17:24]
inddto4 = inddto[25:32]
as.Date(apply(cbind(inddto1 + sample(15, 8, replace = TRUE), inddto2-1), 1, min), origin = "1970-01-01")
inddto
datoer = data.frame(pnr = unique(sample(c(2000,1000,3000,4000,5000,6000,7000,8000,9000), length(inddto1))), 
                    max_indl = sample(4, 8, replace = TRUE), 
                    inddto1 = inddto1, 
                    uddto1  = as.Date(apply(cbind(inddto1 + sample(15, 8, replace = TRUE), inddto2-1), 1, min),
                                      origin = "1970-01-01"), 
                    inddto2 = inddto2,  
                    uddto2  = as.Date(apply(cbind(inddto2 + sample(15, 8, replace = TRUE), inddto3-1), 1, min), 
                                      origin = "1970-01-01"),   
                    inddto3 = inddto3,  
                    uddto3  = as.Date(apply(cbind(inddto3 + sample(15, 8, replace = TRUE), inddto4-1), 1, min), 
                                      origin = "1970-01-01"), 
                    inddto4 = inddto4, 
                    uddto4  = as.Date(inddto4 + sample(15, 8, replace = TRUE), origin = "1970-01-01"))

datoer = datoer[order(datoer$pnr), ]
datoer0 = datoer


library(reshape2)

datoer1 = melt(datoer[, 1:ncol(datoer) %% 2 == 1], id.vars = "pnr")
datoer2 = melt(datoer[, c(0, 1, 3:ncol(datoer) %% 2) == 0], id.vars = "pnr")
datoer1 = datoer1[order(datoer1$pnr), c(1, 3)]
datoer2 = datoer2[order(datoer2$pnr), c(1, 3)]

names(datoer1) = c("pnr", "inddto")
names(datoer2) = c("pnr", "uddto")


datoer = cbind(datoer1, uddto = datoer2[, 2])
head(datoer)

behandling     = "1a"
antal_recepter = 5

mindos = c(50, 25, 10, 50)
mindos = mindos[order(dos)]
maxdos = c(200, 150, 75, 150)
maxdos = maxdos[order(dos)]
(typdos = c(100, 75, 50, 100))
typdos = typdos[order(dos)]
dos = sort(dos)

max_depot = 10

(dates = c(Sys.Date()-1500, Sys.Date()+1300))

head(recept_data)

pnrunique = unique(recept_data$pnr)
receptnr  = rep(1, length(recept_data$pnr))

for (i in 1:length(pnrunique)) {
  for (j in 1:length(recept_data$pnr)) {
    if (recept_data$pnr[j] == pnrunique[i]) {
      if (j > 1 && recept_data$pnr[j] != recept_data$pnr[j-1])
        receptnr[j] = 1
      else if (j > 1)
        receptnr[j] = receptnr[j-1]+1
    }
  }
}

recept_data$receptnr = receptnr


#write.table(recept_data, file = "/home/helene/research/Software/medicin-macro/test-sas/recept_data1.csv", 
#            quote = FALSE, sep = ",", row.names = FALSE)
#write.table(datoer0, file = "/home/helene/research/Software/medicin-macro/test-sas/datoer1.csv", 
#            quote = FALSE, sep = ",", row.names = FALSE)


library(reshape2)

datoer1 = melt(datoer[, 1:ncol(datoer) %% 2 == 1], id.vars = "pnr")
datoer2 = melt(datoer[, c(0, 1, 3:ncol(datoer) %% 2) == 0], id.vars = "pnr")
datoer1 = datoer1[order(datoer1$pnr), c(1, 3)]
datoer2 = datoer2[order(datoer2$pnr), c(1, 3)]

names(datoer1) = c("pnr", "inddto")
names(datoer2) = c("pnr", "uddto")


datoer = cbind(datoer1, uddto = datoer2[, 2])



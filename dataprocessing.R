#-----------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Ubiqum - Market Basket Analysis
# Version: 1
# Purpose: Preprocessing data for market basket analysis, binning products
#          into product type lists
#-----------------------------------------------------------------------------#

#Load libraries
source("scripts/libraries.R")

##### DATA ACQUISITION ########################################################

#load csv data
transData <- read.transactions("data/ElectronidexTransactions2017.csv", 
                               format = "basket", 
                               sep = ",")

prodData <- read.csv("data/ElectronidexTransactions2017.csv", 
                     header = FALSE)


### Loading PDF product type data
text <- pdf_text("data/ElectronidexItems2017.pdf")

text2 <- strsplit(text, "\n")
text2 <- text2 %>% unlist(recursive = TRUE)

text3 <- gsub("\r", "", text2)

prod.list <- text3 %>% as.data.frame()


##### PRODUCT-TYPE BINNING LISTS ##############################################

##create product type lists
#x = row numbers of items, y - object name
listName <- function(x){
        list.name <- prod.list[x,1] %>% as.character() %>% trimws("l")
}

laptop.list <- listName(2:11)
desktop.list <- listName(13:21)
monitor.list <- listName(23:32)
mice.list <- listName(34:43)
keyboard.list <- listName(45:53)
mousekeyboard.list <- listName(55:63)
compheadphones.list <- listName(64:74)
activeheadphones.list <- listName(76:81)
compcords.list <- listName(83:91)
accessories.list <- listName(93:96)
speakers.list <- listName(98:106)
printers.list <- listName(108:112)
printink.list <- listName(114:118)
compstands.list <- listName(120:124)
comptablets.list <- listName(126:130)
extdrives.list <- listName(132:136)
smarthome.list <- listName(138:142)

#add non-listed items from basket
#not all products in the data were included on product type pdf list
laptop.list <- c(laptop.list, "Alienware Laptop", "Apple MacBook Pro", 
                 "Eluktronics Pro Gaming Laptop", "ASUS Chromebook") 

activeheadphones.list <- c(activeheadphones.list, 
                           "Panasonic On-Ear Stereo Headphones", 
                           "APIE Bluetooth Headphone", 
                           "Otium Wireless Sports Bluetooth Headphone", 
                           "Philips Flexible Earhook Headphone")

printink.list <- c(printink.list, "printer Toner")
comptablets.list <- c(comptablets.list, "tabletlet", "tablet Pro")
extdrives.list <- c(extdrives.list, "Slim extdrive")


##### REPLACE LIST CONTENT WITH PRODUCT TYPE ##################################

#string replacement lists
prodReplace <- function(x,y){
        list1 <- x %>% as.data.frame()
        list1$name <- y
        list2 <- list1$name
        names(list2) <- list1$.
        list2
}

laptop.replace <- prodReplace(laptop.list, 
                              "laptop")

desktop.replace <- prodReplace(desktop.list, 
                               "desktop")

monitor.replace <- prodReplace(monitor.list, 
                               "monitor")

mice.replace <- prodReplace(mice.list, 
                            "mouse")

keyboard.replace <- prodReplace(keyboard.list, 
                                "keyboard")

mousekeyboard.replace <- prodReplace(mousekeyboard.list, 
                                     "mousekeyboard")

compheadphones.replace <- prodReplace(compheadphones.list, 
                                      "compheadphones")

activeheadphones.replace <- prodReplace(activeheadphones.list, 
                                        "activeheadphones")

compcords.replace <- prodReplace(compcords.list, 
                                 "compcord")

accessories.replace <- prodReplace(accessories.list, 
                                   "accessories")

speakers.replace <- prodReplace(speakers.list, 
                                "speaker")

printers.replace <- prodReplace(printers.list, 
                                "printer")

printink.replace <- prodReplace(printink.list, 
                                "printink")

compstands.replace <- prodReplace(compstands.list, 
                                  "compstand")

comptablets.replace <- prodReplace(comptablets.list, 
                                   "tablet")

extdrives.replace <- prodReplace(extdrives.list, 
                                 "extdrive")

smarthome.replace <- prodReplace(smarthome.list, 
                                 "smarthome")


##### CREATE PRODUCT TYPE SPARSE MATRIX #######################################

#String replacement function
stringReplace <- function(z){
        inList <- z
        repLace <- prodData
        repLace <- sapply(repLace, function(x) str_replace_all(x, inList))
        data.frame(repLace)
}

productList <- c(laptop.replace, desktop.replace, monitor.replace, 
                 mice.replace, keyboard.replace, mousekeyboard.replace,
                 compheadphones.replace, activeheadphones.replace, 
                 compcords.replace, accessories.replace, speakers.replace,
                 printers.replace, printink.replace, compstands.replace, 
                 comptablets.replace, extdrives.replace, smarthome.replace)

typePrep <- stringReplace(productList)

#CSV creation/re-import
write.table(typePrep, 
            file = "output/typePrep.csv", 
            sep=",", 
            row.names = FALSE, 
            col.names = FALSE)

typeData <- read.transactions("output/typePrep.csv", 
                              format = "basket", 
                              sep=",")

##### OUTPUT FILES ############################################################

save(transData, file = "output/transData.RDS")
save(prodData, file = "output/prodData.RDS")
save(typeData, file = "output/typeData.RDS")

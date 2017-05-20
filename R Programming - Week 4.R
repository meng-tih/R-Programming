home <- "C:/Users/PoissonQuiCoule/Documents/R_assignment/ProgAssignment3-data"
setwd(home)

best <- function(state, outcome) {
        data_ori <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        datas <- data_ori[, c(2, 7, 11, 17, 23)]
        colnames(datas) <- c("Hospital.Name", "State", "heart attack", "heart failure", "pneumonia")
        
        if (max(names(datas) == outcome) == 0) {
                stop("outcome not found")
        }
        if (max(datas$State == state) == 0) {
                stop("state not found")
        }
        min_outcome <- min(as.numeric(datas[outcome][datas["State"] == state]), na.rm = T)
        hospital_name <- datas$Hospital.Name[datas$State == state & datas[outcome] == min_outcome]
        # if we have many hospital we get the first hospital on alphabetical order
        min(hospital_name)
}

rankhospital <- function(state, outcome, num = "best") {
        data_ori <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        datas <- data_ori[, c(2, 7, 11, 17, 23)]
        colnames(datas) <- c("Hospital.Name", "State", "heart attack", "heart failure", "pneumonia")
        
        if (max(names(datas) == outcome) == 0) {
                stop("outcome not found")
        }

        if (max(datas$State == state) == 0) {
                stop("state not found")
        }
        
        if (num == "worst") {
                top <- 1
        } else if (num == "best") {
                top <- 1
        } else {
                top <- num
        }
        
        outcome_numerical <- c(1:5)[colnames(datas) == outcome]
        datas_clean <- datas[datas[outcome] != "Not Available" & !is.na(datas[outcome]) & datas["State"] == state,]
        if (num == "worst") {
                datas_order <- datas_clean[order(as.numeric(datas_clean[,outcome_numerical]), rev(datas_clean$Hospital.Name), na.last = TRUE, decreasing = TRUE),]
        } else {
                datas_order <- datas_clean[order(as.numeric(datas_clean[,outcome_numerical]), datas_clean$Hospital.Name, na.last = TRUE, decreasing = FALSE),]
        }

        if (top > nrow(datas_order)) {
                return(NA)
        }
        tail(head(datas_order$Hospital.Name, top), 1)
}


rankall <- function(outcome, num = "best") {
        data_ori <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        datas <- data_ori[, c(2, 7, 11, 17, 23)]
        colnames(datas) <- c("Hospital.Name", "State", "heart attack", "heart failure", "pneumonia")

        
        
        for (i in unique(datas$State[!is.na(datas["State"])])) {
                print(paste(rankhospital(state = i, outcome = outcome, num = num), i, sep = "   "))
                
        }
        
        
}

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)


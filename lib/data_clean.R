# load data
loan <- read.csv("loan.csv", header = TRUE, as.is = TRUE)

# delete joint loan
table(loan$application_type)
loan <- loan[loan$application_type=="INDIVIDUAL",]

# select target variables and response based on business understanding
loan <- loan[,c("annual_inc", "collections_12_mths_ex_med",
                "delinq_2yrs", "desc", "dti", "earliest_cr_line", "emp_length", 
                "home_ownership", "inq_last_6mths", "installment", "int_rate", 
                "verification_status", "issue_d", "loan_amnt","mths_since_last_delinq", 
                "mths_since_last_major_derog", "mths_since_last_record", "open_acc", 
                "pub_rec", "purpose", "revol_bal", "revol_util", "term", "title", 
                "total_acc", "open_acc_6m", "open_il_6m", "open_il_12m", "open_il_24m", 
                "mths_since_rcnt_il", "total_bal_il", "il_util", "open_rv_12m", "open_rv_24m", 
                "max_bal_bc", "all_util", "total_rev_hi_lim", "inq_fi","total_cu_tl", 
                "inq_last_12m", "acc_now_delinq", "tot_coll_amt", "tot_cur_bal", "loan_status")]

# response: Loan status (only remain Default, Charge off, Fully paid)
# 1: Default, charge off
# 0: Fully Paid
table(loan$loan_status)
loan <- loan[loan$loan_status %in% c("Default", "Charged Off", "Fully Paid"), ]
loan$loan_status <- ifelse(loan$loan_status=="Fully Paid", "0", "1")

####################################################
# NA problem

# count NAs in each column
cNA <- apply(loan, 2, is.na)
cNA <- colSums(cNA)
cNA

# collections_12_mths_ex_med
# missing value, majority=0 ==> set NA=0
table(loan$collections_12_mths_ex_med)
loan$collections_12_mths_ex_med[is.na(loan$collections_12_mths_ex_med)] <- 0

# mths_since_last_delinq
# NA since no delinquency ==> NA=infinity
# 0 change to 1, change mths_since_last_delinq to turnover, NA change to 0
table(loan$mths_since_last_delinq)
loan$mths_since_last_delinq[loan$mths_since_last_delinq==0] <- 1
loan$mths_since_last_delinq_T <- 1/loan$mths_since_last_delinq
loan$mths_since_last_delinq_T[is.na(loan$mths_since_last_delinq_T)] <- 0

# mths_since_last_major_derog
# NA since no most recent 90-day or worse rating ==> NA=infinity
# 0 change to 1, change mths_since_last_delinq to turnover, NA change to 0
table(loan$mths_since_last_major_derog)
loan$mths_since_last_major_derog[loan$mths_since_last_major_derog==0] <- 1
loan$mths_since_last_major_derog_T <- 1/loan$mths_since_last_major_derog
loan$mths_since_last_major_derog_T[is.na(loan$mths_since_last_major_derog_T)] <- 0

# mths_since_last_record
# NA since no the last public record ==> NA=infinity
# 0 change to 1, change mths_since_last_delinq to turnover, NA change to 0
table(loan$mths_since_last_record)
loan$mths_since_last_record[loan$mths_since_last_record==0] <- 1
loan$mths_since_last_record_T <- 1/loan$mths_since_last_record
loan$mths_since_last_record_T[is.na(loan$mths_since_last_record_T)] <- 0

# remove original columns
loan <- loan[ , -which(names(loan) %in% c("mths_since_last_delinq", 
                                          "mths_since_last_major_derog", 
                                          "mths_since_last_record"))]

# revol_util
# revol_util=NA ==> revol_bal=0 ==> high risk ==> set NA = 100
loan$revol_util[is.na(loan$revol_util)] <- 100

# total_rev_hi_lim
# missing value ==> set NA=mean
loan$total_rev_hi_lim[is.na(loan$total_rev_hi_lim)==TRUE] <- mean(loan$total_rev_hi_lim,na.rm = T)

# tot_coll_amt
# missing value ==> set NA=mean
loan$tot_coll_amt[is.na(loan$tot_coll_amt)==TRUE] <- mean(loan$tot_coll_amt,na.rm = T)

# tot_cur_bal
# missing value ==> set NA=mean
loan$tot_cur_bal[is.na(loan$tot_cur_bal)==TRUE] <- mean(loan$tot_cur_bal,na.rm = T)

# delete columns(14): almost NA
# open_acc_6m, open_il_6m, open_il_12m, open_il_24m, mths_since_rcnt_il, total_bal_il
# il_util, open_rv_12m, open_rv_24m, max_bal_bc, all_util, inq_fi, total_cu_tl, inq_last_12m
loan <- loan[ , -which(names(loan) %in% c("open_acc_6m", "open_il_6m", "open_il_12m", 
                                          "open_il_24m", "mths_since_rcnt_il", "total_bal_il", 
                                          "il_util", "open_rv_12m", "open_rv_24m", "max_bal_bc", 
                                          "all_util", "inq_fi", "total_cu_tl", "inq_last_12m"))]

####################################################
# character problem

# earliest_cr_line (delete later)
ymd <- function(x){
  year <- substr(x,5,8)
  month <- match(substr(x,1,3),month.abb)
  day <- "01"
  return(as.Date(paste(year, month, day, sep = "-")))
}
loan$earliest_cr_line <- ymd(loan$earliest_cr_line)

# issue_d (delete later)
loan$issue_d <- ymd(loan$issue_d)

# credit_month (new: # month b/w issue_d and earliest_cr_line)
loan$credit_month <- round(as.numeric(difftime(loan$issue_d, 
                                               loan$earliest_cr_line), units = "days")/30, 0)
loan <- subset(loan, select=-c(earliest_cr_line, issue_d))

# term
loan$term <- substr(loan$term, 2, 3)

# emp_length
loan$emp_length <- gsub("([0-9]+).*$", "\\1", loan$emp_length)
loan$emp_length <- as.numeric(ifelse(loan$emp_length %in% c("< 1","n/a"), 
                                     0, loan$emp_length))

# title (delete)
loan <- subset(loan, select=-c(title))

# desc
loan$desc <- as.character(loan$desc)
loan$desc <- as.character(loan$desc)
r <- gregexpr("Borrower added on [0-9]{2}/[0-9]{2}/[0-9]{2} > ", loan$desc)
desc <- regmatches(loan$desc, r, invert = TRUE)
desc <- unlist(gsub("<br>", "", desc))
desc <- gsub("c\\(\\\" ","", desc)
desc <- gsub(" \\\", \\\"", "", desc)
desc <- gsub("\\\")","", desc)
loan$desc <- desc


# desc_len (new: word length of desc)
loan$desc_len <- sapply(strsplit(loan$desc, split = " "), 
                        function(x) length(unique(x)))
loan <- subset(loan, select=-c(desc))

####################################################
# dependent variable & factor features
loan$loan_status <- as.factor(loan$loan_status)
loan$home_ownership <- as.factor(loan$home_ownership)
loan$verification_status <- as.factor(loan$verification_status)
loan$purpose <- as.factor(loan$purpose)

####################################################
# output
write.csv(loan,"loan_cleaned.csv", row.names = FALSE)

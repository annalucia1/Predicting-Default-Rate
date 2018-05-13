loan <- read.csv('LoanStats_securev1_2016Q1.csv', header = TRUE, stringsAsFactors = FALSE, skip = 1)
loanT <- loan
library(zoo)
# How do we understand data? Let's use dates related features as example.
# There are "" in the last_pymnt_d, why?
head(loan[, c('issue_d', 'last_pymnt_d', 'next_pymnt_d')])
dim(subset(loan, next_pymnt_d == ""))
with(subset(loan, next_pymnt_d == ""), table(loan_status)) # either charged off or fully paid
with(subset(loan, next_pymnt_d == "" & last_pymnt_d == ""), table(loan_status)) # all charged off

with(subset(loan, last_pymnt_d == ""), table(loan_status))
summary(subset(loan, last_pymnt_d == "")$last_pymnt_amnt) # means user didn't pay last month

table(subset(loan, next_pymnt_d == "" & last_pymnt_d == "")$loan_status) # all charged off
table(subset(loan, next_pymnt_d == "" & last_pymnt_d != "")$loan_status) # charged off or fully paid
table(subset(loan, next_pymnt_d != "" & last_pymnt_d == "")$loan_status) # no loan satisfy this.
table(subset(loan, next_pymnt_d != "" & last_pymnt_d != "")$loan_status)

loan <- subset(loan, loan_status != "")

# Based on the observation.
# One possible interesting question to explore is at which phase, payment is likely to be missed.
# what other options do we have here?
# 1) Upon loan initial application, predict whether it will be charge off or default.
# 2) Throughout loan payment period, predict whether next payment will be missing,
#                                    or in next quarter, whether loan status will be changed.
# what else?

loan$issue_d_1 <- as.Date(as.yearmon(loan$issue_d, "%b-%Y"))
loan$issue_year <- as.character(format(loan$issue_d_1, "%Y"))
loan$issue_mon <- as.character(format(loan$issue_d_1, "%m"))

loan$last_pymnt_d_1 <- as.Date(as.yearmon(loan$last_pymnt_d, "%b-%Y"))
loan$last_pymnt_year <- as.character(format(loan$last_pymnt_d_1, "%Y"))
loan$last_pymnt_mon <- as.character(format(loan$last_pymnt_d_1, "%m"))

loan$last_pymnt_from_issue <- with(loan, last_pymnt_d_1 - issue_d_1, NA)
table(loan$last_pymnt_from_issue)
loan$last_pymnt_from_issue_cat <- with(loan, as.character(cut(as.numeric(last_pymnt_from_issue), 
                                                              c(-1, 0, 92, 184, 275, 366, 457, 549, 639, 730))))
table(loan$last_pymnt_from_issue_cat)
# note about table, it won't show NA.
sum(table(loan$last_pymnt_from_issue_cat))

# Why there are so many NA
table(subset(loan, is.na(last_pymnt_from_issue_cat))$last_pymnt_d)
# We see ~148 have no payment at all, the rest have same last payment date as issue date.
# So we need some change in code.
loan$last_pymnt_from_issue[which(is.na(loan$last_pymnt_from_issue))] <- 0
loan$last_pymnt_from_issue_cat[which(is.na(loan$last_pymnt_from_issue_cat))] <- 'no pymnt'

# Then if we want to check if last_pymnt_from_issue_cat could be a useful feature:
by.pymnt.gap <- with(loan, table(loan_status, last_pymnt_from_issue_cat))
by.pymnt.gap <- by.pymnt.gap[c("Charged Off", "Default", "Late (31-120 days)",
                               "Late (16-30 days)", "In Grace Period", "Current",
                               "Fully Paid"), ]
round(100 * by.pymnt.gap / apply(by.pymnt.gap, 1, sum), 3)
# result of mod1 is a bit surprising at first.
loan$last_pymnt_from_issue_cat <- relevel(as.factor(loan$last_pymnt_from_issue_cat), ref = 'no pymnt')
loan$loan_status_binary <- as.factor(ifelse(loan$loan_status %in% c('Fully Paid', 'Current'), 'okay', 'past_due'))
mod1 <- glm(loan_status_binary ~ last_pymnt_from_issue_cat, loan, family = 'binomial')
summary(mod1)
mod2 <- glm(loan_status_binary ~ last_pymnt_from_issue, loan, family = 'binomial')
# consider use piecewise linear regression
summary(mod2)

# why the result is weird from mod1?
# this is complete or more accurately quasi complete separatable data.
# MLE for logistic regression doesn't exist in the case of complete or more accurately quasi.
# https://stats.stackexchange.com/questions/224863/understanding-complete-separation-for-logistic-regression/224864#224864
with(loan, table(loan_status_binary, last_pymnt_from_issue_cat))

# Let's start with feature processing.
# Similarly, we should process all dates related columns same.
date.cols <- colnames(loan)[c(which(grepl('_d$', colnames(loan))),
                              which(grepl('_date$', colnames(loan))))]
# "issue_d"                   "last_pymnt_d"              "next_pymnt_d"             
# "last_credit_pull_d"        "hardship_start_date"       "hardship_end_date"        
# "payment_plan_start_date"   "debt_settlement_flag_date" "settlement_date"          

for (col_i in date.cols) {
  loan[, col_i] <-  as.Date(as.yearmon(loan[, col_i], "%b-%Y"))
}
loan$mths_since_issue <- as.integer((as.Date('2017-11-01') - loan$issue_d) /30)
loan$mths_since_last_credit_pull <- as.integer((as.Date('2017-11-01') - loan$last_credit_pull_d) /30)
TransformToLengthFromIssueDate <- function(loan, col.name, new.col.name, other.level) {
  # get difference in months.
  loan[, new.col.name] <-
    ifelse(is.na(loan[, col.name]), other.level,
           as.character(cut(as.integer((loan[, col.name] - loan$issue_d) /30), 
                            c(min(as.integer((loan[, col.name] - loan$issue_d) /30), na.rm = T) - 1,
                              quantile(as.integer((loan[, col.name] - loan$issue_d) /30), c(0.1, 0.9), na.rm = T),
                              max(as.integer((loan[, col.name] - loan$issue_d) /30), na.rm = T)))))
  return(loan)
}
loan <- TransformToLengthFromIssueDate(loan, 'hardship_start_date' ,'hardship_since_issue', 'no_hs')
loan <- TransformToLengthFromIssueDate(loan, 'settlement_date' ,'settlement_since_issue', 'no_settle')
loan <- loan[, -which(colnames(loan) %in% date.cols)]

# Remove features with same value
num.value <- sapply(loan, function(x){return(length(unique(x)))})
colnames(loan)[intersect(which(sapply(loan, function(x){return(is.character(x))})), 
                         which(num.value >= 50))]
# Clear that some features are supposed to be numeric, but it is not
head(loan$int_rate)
which(sapply(loan[1, ], function(x){return(grepl('%', x))}))
loan$int_rate <- as.numeric(sapply(strsplit(loan$int_rate, '%'), '[', 1))
loan$revol_util <- as.numeric(sapply(strsplit(loan$revol_util, '%'), '[', 1))

loan$earliest_cr_line <-  as.Date(as.yearmon(loan$earliest_cr_line, "%b-%Y"))
loan$mths_since_crline <- as.integer((as.Date('2017-11-01') - loan$earliest_cr_line) /30)
# For categorical features with too many levels, could collapse levels as we did before.
feat.w.many.levels <- colnames(loan)[intersect(which(sapply(loan, function(x) {
  return(is.character(x))})),
  which(num.value >= 50))]
# "id"         "emp_title"  "url"        "zip_code"   "addr_state"
loan <- loan[, -which(colnames(loan) %in% c(names(which(num.value == 1)), feat.w.many.levels))]

# Update features to reflect loan is jointly applied
colnames(loan)[which(grepl('joint', colnames(loan)))]
loan$dti <- ifelse(!is.na(loan$dti_joint), loan$dti_joint, loan$dti)
loan$annual_inc <- ifelse(!is.na(loan$annual_inc_joint), loan$annual_inc_joint, loan$annual_inc)
loan$verification_status <- ifelse(!is.na(loan$verification_status_joint), loan$verification_status_joint, loan$verification_status)

loan <- loan[, -which(grepl('joint', colnames(loan)))]

# dealing with missing value
num.NA <- sort(sapply(loan, function(x) { sum(is.na(x))} ), decreasing = TRUE)
sort(num.NA, decreasing = TRUE)[1:10]
# check columns with a lot of NA, for example, hardship related features
colnames(loan)[which(grepl('hardship', colnames(loan)))]
summary(loan$orig_projected_additional_accrued_interest)
loan$orig_projected_additional_accrued_interest[which(is.na(loan$orig_projected_additional_accrued_interest))] <- 0

# check some columns and find out not only NA but also empty value.
loan$hardship_reason <- ifelse(loan$hardship_reason == '', 'no_hs', loan$hardship_reason)
loan$hardship_status <- ifelse(loan$hardship_status == '', 'no_hs', loan$hardship_status)
loan$hardship_loan_status <- ifelse(loan$hardship_loan_status == '', 'no_hs', loan$hardship_loan_status)
loan$hardship_amount[which(is.na(loan$hardship_amount))] <- 0
loan$hardship_dpd[which(is.na(loan$hardship_dpd))] <- 0
loan$hardship_payoff_balance_amount[which(is.na(loan$hardship_payoff_balance_amount))] <- 0
loan$hardship_last_payment_amount[which(is.na(loan$hardship_last_payment_amount))] <- 0

loan <- loan[, -which(colnames(loan) %in% c('deferral_term', 'hardship_start_date', 
                                            'hardship_end_date', 'payment_plan_start_date',
                                            'hardship_length', 'hardship_type'))]

num.empty <- sapply(loan[, colnames(loan)[which(sapply(loan, function(x){return(is.character(x))}))]],
                    function(x){return(length(which(x == "")))})
num.empty[which(num.empty > 0)]
loan <- loan[, -which(colnames(loan) %in% c('verification_status', 'desc', 'title'))]

# Similarly for settlement
loan$settlement_amount[which(is.na(loan$settlement_amount))] <- 0
loan$settlement_percentage[which(is.na(loan$settlement_percentage))] <- 0
loan$settlement_term[which(is.na(loan$settlement_term))] <- 0
loan$settlement_status <- ifelse(is.na(loan$settlement_status), 'no_settlement', loan$settlement_status)

num.NA <- sort(sapply(loan, function(x) { sum(is.na(x))} ), decreasing = TRUE)
sort(num.NA, decreasing = TRUE)[1:10]

for(col_i in setdiff(names(num.NA)[which(grepl('mths_since', names(num.NA))& num.NA > 0)],
                     c('mths_since_issue', 'mths_since_crline', 'mths_since_last_credit_pull'))) {
  breaks <- quantile(loan[, col_i], c(0.1, 0.5, 0.9), na.rm = T)
  breaks <- c(min(loan[, col_i], na.rm = T) - 1, breaks, max(loan[, col_i], na.rm = T))
  loan[, col_i] <- ifelse(is.na(loan[, col_i]),
                          'not_avail', as.character(cut(loan[, col_i], breaks = breaks)))
}

# check NA again
num.NA <- sort(sapply(loan, function(x) { sum(is.na(x))} ), decreasing = TRUE)
sort(num.NA, decreasing = TRUE)[which(num.NA > 0)]
# is it bcuz no open account? Not for all the cases.
summary(subset(loan, is.na(il_util))$open_act_il)
# is it becuz of the limit is 0? since open_act_il = total_bal_il / total_il_high_credit_limit
# not for all the cases.
with(subset(loan, is.na(il_util)), table(total_il_high_credit_limit))
dim(loan[which(is.na(loan$il_util) & loan$total_il_high_credit_limit != 0), ])

loan$il_util <- ifelse(is.na(loan$il_util) & loan$total_il_high_credit_limit != 0, 
                       loan$total_bal_il/ loan$total_il_high_credit_limit, loan$il_util)
summary(subset(loan, is.na(il_util) & total_il_high_credit_limit == 0)$open_act_il)
loan$il_util <-  ifelse(is.na(loan$il_util), 'no_il',
                        as.character(cut(loan$il_util, 
                                         c(min(loan$il_util, na.rm = T) - 0.01,
                                           quantile(loan$il_util, na.rm = T, c(0.1, 0.9)),
                                           max(loan$il_util, na.rm = T)))))
table(loan$il_util)

loan$mo_sin_old_il_acct <-  ifelse(is.na(loan$mo_sin_old_il_acct), 'no_il',
                                   as.character(cut(loan$mo_sin_old_il_acct, 
                                                    c(min(loan$mo_sin_old_il_acct, na.rm = T) - 0.01,
                                                      quantile(loan$mo_sin_old_il_acct, na.rm = T, c(0.1, 0.9)),
                                                      max(loan$mo_sin_old_il_acct, na.rm = T)))))
# is it bcuz there is no open account? No.
summary(subset(loan, is.na(num_tl_120dpd_2m))$open_acc)
with(subset(loan, is.na(num_tl_120dpd_2m)), summary(num_tl_30dpd))
loan$num_tl_120dpd_2m <- ifelse(is.na(loan$num_tl_120dpd_2m), 0, loan$num_tl_120dpd_2m)

num.NA <- sort(sapply(loan, function(x) { sum(is.na(x))} ), decreasing = TRUE)
for(col_i in names(num.NA)[num.NA > 0]) {
  loan[, col_i] <- ifelse(is.na(loan[, col_i]), median(loan[, col_i], na.rm = T), loan[, col_i])
}

loan <- loan[, -which(colnames(loan) %in% c('grade', 'int_rate', 'sub_grade', 'policy_code'))]
loan$loan_status_binary <- as.factor(ifelse(loan$loan_status %in% c('Fully Paid', 'Current'), 'okay', 'past_due'))
loan$loan_status_binary <- relevel(loan$loan_status_binary, ref = "okay")

numeric.feats <- colnames(loan)[which(sapply(loan, function(x){return(is.numeric(x))}))]
for(col_i in numeric.feats) {
  formula = paste(col_i, " ~ loan_status_binary")
  p.val <- t.test(as.formula(formula), data = loan)$p.value
  if(p.val >= 0.05) {
    loan[, col_i] <- NULL
  }
}

cat.feats <- colnames(loan)[which(sapply(loan, function(x){return(is.character(x))}))]
cat.feats <- setdiff(cat.feats, 'loan_status_binary')
for(col_i in cat.feats) {
  p.val <- chisq.test(x = loan[, col_i], y = loan$loan_status_binary)$p.value
  if(p.val >= 0.05) {
    loan[, col_i] <- NULL
  }
}

loan$fico_range_high <- NULL
loan$fico_range_low <- NULL
loan$last_fico <- with(loan, (last_fico_range_high + last_fico_range_low)/2)
loan$last_fico_range_high <- loan$last_fico_range_low <- NULL
loan$emp_length <- ifelse(loan$emp_length == 'n/a', loan$emp_length,
                          ifelse(loan$emp_length %in% c('< 1 year', '1 year', '2 years', '3 years'),
                                 '< 3 years', ifelse(loan$emp_length %in% c('4 years', '5 years', '6 years', '7 years'), 
                                                     '4-7 years', '> 8 years')))

# I am not giving out much code about modeling since I hope our students could spend more time with more freedom on it.
set.seed(1)
train.ind <- sample(1:dim(loan)[1], 0.7* dim(loan)[1])
train <- loan[train.ind, ]
test <- loan[-train.ind, ]

library(glmnet)
# binary model
ind <- sparse.model.matrix( ~. , loan[, -which(colnames(loan) %in% c('loan_status_binary', 'loan_status'))])
dep <- loan$loan_status_binary
Sys.time()
cv.mod <- cv.glmnet(ind[1:10000, ], dep[1:10000], family = 'binomial')
Sys.time()

Sys.time()
cv.mod <- cv.glmnet(ind, dep, family = 'binomial') # this might take a long time.
Sys.time()

plot(cv.mod)
cv.mod$lambda.1se
coef <- coef(cv.mod, s = 'lambda.1se')

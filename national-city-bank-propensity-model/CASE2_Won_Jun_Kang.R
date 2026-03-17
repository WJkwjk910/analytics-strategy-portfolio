#load required libraries
library(dplyr)
library(readr)
library(vtreat)
library(caret)
library(pROC)
library(randomForest)

#set working directory to where files are
setwd("/Users/juny910/Desktop/CS_96/CS-96/Cases/Spring/II National City Bank")

#load datasets
current <- read_csv("training/CurrentCustomerMktgResults.csv")
vehicle <- read_csv("training/householdVehicleData.csv")
credit  <- read_csv("training/householdCreditData.csv")
axiom   <- read_csv("training/householdAxiomData.csv")

#merge vehicle credit and axiom into current
current <- current %>%
  left_join(vehicle, by = "HHuniqueID") %>%
  left_join(credit,  by = "HHuniqueID") %>%
  left_join(axiom,   by = "HHuniqueID")

#drop columns not usable with vtreat
current <- current %>%
  select(-any_of(c("CallStart", "CallEnd")))

#clean $ and ',' from annualdonations
current$annualDonations <- as.numeric(gsub("[$,]", "", current$annualDonations))

#convert outcome to numeric for vtreat (1 = Accepted, 0 = DidNotAccept)
current$Y_AcceptedOffer <- ifelse(current$Y_AcceptedOffer == "Accepted", 1, 0)

#prepare the training data
outcome_name <- "Y_AcceptedOffer"

#create treatment plan
treatment_plan <- designTreatmentsC(
  dframe = current,
  varlist = setdiff(names(current), outcome_name),
  outcomename = outcome_name,
  verbose = FALSE
)

#get usable variable names
usable_vars <- treatment_plan$scoreFrame %>%
  filter(code %in% c("clean", "lev", "catB", "catP")) %>%
  pull(varName)

#apply treatment plan
train <- prepare(treatment_plan, current, varRestriction = usable_vars)

#add back target variable as factor
train$Y_AcceptedOffer <- as.factor(current$Y_AcceptedOffer)


#split into train/test
set.seed(123)
train_index <- createDataPartition(train$Y_AcceptedOffer, p = 0.8, list = FALSE)
train_set <- train[train_index, ]
test_set  <- train[-train_index, ]

#train random forest model
set.seed(123)
rf_model <- randomForest(
  Y_AcceptedOffer ~ ., 
  data = train_set, 
  importance = TRUE,
  ntree = 200
)
varImpPlot(rf_model)

#logistic regression model 
#train logistic regression model
log_model <- glm(Y_AcceptedOffer ~ ., data = train_set, family = "binomial")

#predict on test set
log_pred <- predict(log_model, newdata = test_set, type = "response")

#convert target to numeric
test_target <- as.numeric(as.character(test_set$Y_AcceptedOffer))

#evaluate AUC for logistic regression
auc_log <- roc(test_target, log_pred)$auc
print(paste("Logistic Regression AUC:", round(auc_log, 3)))


#check model performance on the test data and compare with random forest
#predict probabilities
rf_probs <- predict(rf_model, newdata = test_set, type = "prob")[, 2]

#predict class labels
rf_preds <- ifelse(rf_probs > 0.5, "Accepted", "DidNotAccept")

#convert test set actuals to factor labels for comparison
actual <- ifelse(test_set$Y_AcceptedOffer == 1, "Accepted", "DidNotAccept")

#confusion matrix
confusionMatrix(factor(rf_preds), factor(actual))

# AUC score
roc_obj <- roc(actual, rf_probs, levels = c("DidNotAccept", "Accepted"), direction = "<")
auc(roc_obj)

#load prospective customers
prospects <- read_csv("ProspectiveCustomers.csv")

#merge with the same external data
prospects_full <- prospects %>%
  left_join(vehicle, by = "HHuniqueID") %>%
  left_join(credit,  by = "HHuniqueID") %>%
  left_join(axiom,   by = "HHuniqueID")

#drop unusable columns
prospects_full <- prospects_full %>%
  select(-any_of(c("CallStart", "CallEnd")))

#clean donation field if it exists
if ("annualDonations" %in% colnames(prospects_full)) {
  prospects_full$annualDonations <- as.numeric(gsub("[$,]", "", prospects_full$annualDonations))
}

#vtreat treatment
prospects_treated <- prepare(treatment_plan, prospects_full, varRestriction = usable_vars)

#score with trained model
prospects_full$score <- predict(rf_model, newdata = prospects_treated, type = "prob")[, 2]

#extract top 100 prospects
top_100 <- prospects_full %>%
  arrange(desc(score)) %>%
  slice_head(n = 100) %>%
  select(HHuniqueID, score)

#save to CSV
write_csv(top_100, "Top100Prospects.csv")


#seperate
#compare with kaggle data for second opinion
#Kaggle Validation Mode

#load and combine kaggle datasets
kaggle_train <- read_csv("training/carInsurance_train.csv")
kaggle_test  <- read_csv("training/carInsurance_test.csv")

#add source label and combine
kaggle_train$source <- "train"
kaggle_test$source <- "test"

#combine for cleaning
kaggle_all <- bind_rows(kaggle_train, kaggle_test)

#remove unneeded columns
kaggle_all <- kaggle_all %>%
  select(-any_of(c("CallStart", "CallEnd", "Id"))) %>%
  na.omit()

#drop rows with missing target
kaggle_all <- kaggle_all %>% filter(!is.na(CarInsurance))

#convert target to factor
kaggle_all$CarInsurance <- as.factor(kaggle_all$CarInsurance)

#train/test split from the cleaned combined dataset
set.seed(123)
kaggle_split <- createDataPartition(kaggle_all$CarInsurance, p = 0.8, list = FALSE)
kaggle_train_clean <- kaggle_all[kaggle_split, ]
kaggle_test_clean  <- kaggle_all[-kaggle_split, ]

#train Random Forest
set.seed(456)
rf_kaggle <- randomForest(CarInsurance ~ ., data = kaggle_train_clean, ntree = 100)

#predict and evaluate
rf_probs_kaggle <- predict(rf_kaggle, newdata = kaggle_test_clean, type = "prob")[,2]
kaggle_test_clean$CarInsurance <- as.numeric(as.character(kaggle_test_clean$CarInsurance))
roc_kaggle <- roc(kaggle_test_clean$CarInsurance, rf_probs_kaggle)

#print AUC
print(paste("Kaggle Model AUC:", round(auc(roc_kaggle), 3)))




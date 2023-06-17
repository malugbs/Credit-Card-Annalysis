##pacotes usados
library(ggplot2) # Data visualization
library(ggpubr) # Data visualization
library(plotly) # Interactive data visualizations
library(psych) # Will be used for correlation visualizations
library(rattle) # Graphing decision trees
library(caret) # Machine learning
library(RColorBrewer) # Color palette
library(stargazer) # Tables 
library(tidyverse) # Organizing data
library(GGally) # Correlation plots
library(caTools) #separar dados
library(glm2) # Log regression

"#E41A1C" "#377EB8" "#4DAF4A" "#984EA3" "#FF7F00" "#FFFF33" "#A65628"

df <- read.csv("C:/Users/guima/Documents/r/Credit Card customers/BankChurners.csv")

summary(df)

df$Income_Category <- factor(df$Income_Category, levels = c("Unknown", "Less than $40K", "$40K - $60K", "$60K - $80K", "$80K - $120K", "$120K +"))
df$Education_Level <- factor(df$Education_Level, levels = c("Unknown", "Uneducated", "High School", "College", "Graduate", "Post-Graduate", "Doctorate"))

df <- df %>%
  select(!Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1) %>%
  select(!Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2) %>%
  select(!CLIENTNUM) %>%
  mutate(att = ifelse(Attrition_Flag == "Existing Customer", 0, 1))

#distribuição da idade
df %>%
  ggplot(aes(x=Customer_Age)) +
  geom_histogram( binwidth=5, colour="white", fill="#E41A1C", alpha=0.8) +
  geom_density(eval(bquote(aes(y=..count..*5))),colour="#FF7F00", fill="#FF7F00", alpha=0.3) + 
  scale_x_continuous(breaks=seq(20,75,5)) +
  geom_vline(xintercept = 46, linetype="dashed") +
  annotate("text", x=35, y=2250, label="Age below median", size=4, color="black") + 
  annotate("text", x=65, y=2250, label="Age above median", size=4, color="black") +
  theme_light() +
  labs(title="Age Distribution",
       fill="",
       x="",
       y="") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#overview
df %>%
  mutate(Gendert = ifelse(Gender == "M", "Male", "Female")) %>%
  group_by(Attrition_Flag, Gendert, Income_Category) %>%
  summarise(amount = n()) %>%
  mutate(percentage = amount*100/sum(amount))

df %>%
  mutate(Gendert = ifelse(Gender == "M", "Male", "Female")) %>%
  group_by(Gendert, Income_Category, Education_Level) %>%
  summarise(amount = n()) %>%
  mutate(percentage = amount*100/sum(amount))

#cancelados por salário
df %>%
  group_by(`Income_Category`,`Attrition_Flag`) %>%
  summarise(amount = n()) %>%
  mutate(percentage = amount*100/sum(amount)) %>%
  ggplot(aes(fill=`Attrition_Flag`, y=percentage, x=`Attrition_Flag`)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  facet_wrap(~`Income_Category`) +
  theme_light() +
  labs(title="(%) of attrition by income group",
       fill="Attrition Flag",
       x="",
       y="") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(legend.position="top")

#cancelamento e cartão
df %>%
  group_by(Card_Category,`Attrition_Flag`) %>%
  summarise(amount = n()) %>%
  mutate(percentage = amount*100/sum(amount)) %>%
  ggplot(aes(fill=`Attrition_Flag`, y=percentage, x=`Attrition_Flag`)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  facet_wrap(~`Card_Category`) +
  theme_light() +
  labs(title="(%) of attrition by Card type",
       fill="Attrition Flag",
       x="",
       y="") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(legend.position="top")

#educação e cancelamento
df %>%
  group_by(`Education_Level`,`Attrition_Flag`) %>%
  summarise(amount = n()) %>%
  mutate(percentage = amount*100/sum(amount)) %>%
  ggplot(aes(fill=`Attrition_Flag`, y=percentage, x=`Attrition_Flag`)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  facet_wrap(~`Education_Level`) +
  theme_light() +
  labs(title="(%) of attrition by education",
       fill="Attrition Flag",
       x="",
       y="") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(legend.position="top")

#status e cancelamento
df %>%
  group_by(Marital_Status,`Attrition_Flag`) %>%
  summarise(amount = n()) %>%
  mutate(percentage = amount*100/sum(amount)) %>%
  ggplot(aes(fill=`Attrition_Flag`, y=percentage, x=`Attrition_Flag`)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  facet_wrap(~`Marital_Status`) +
  theme_light() +
  labs(title="(%) of attrition by marital status",
       fill="Attrition Flag",
       x="",
       y="") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(legend.position="top")

#balance
df %>%
  group_by(Income_Category, `Attrition_Flag`) %>%
  summarise(amount = mean(Total_Revolving_Bal)) %>%
  ggplot(aes(fill=`Attrition_Flag`, y=amount, x=`Attrition_Flag`)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::label_dollar(scale = 1)) +
  facet_wrap(~`Income_Category`) +
  theme_light() +
  labs(title="Average revolving balance by income category",
       fill="Attrition Flag",
       x="",
       y="") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(legend.position="top")

#transactions
df %>%
  ggplot(aes(fill=Education_Level, y=Total_Trans_Ct, x=Attrition_Flag)) + 
  geom_boxplot() +
  theme(legend.position="none") +
  scale_fill_brewer(palette = "Set1") +
  theme_light() +
  facet_wrap(~`Income_Category`) +
  labs(title="Transaction Count by groups",
       fill="Education",
       x="",
       y="") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(legend.position="top")

df %>%
  ggplot(aes(fill=Education_Level, y=Total_Trans_Amt, x=Attrition_Flag)) + 
  geom_boxplot() +
  theme(legend.position="none") +
  scale_fill_brewer(palette = "Set1") +
  theme_light() +
  facet_wrap(~`Income_Category`) +
  labs(title="Transaction amount by groups",
       fill="Education",
       x="",
       y="") +
  scale_y_continuous(labels = scales::label_dollar(scale = 1)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(legend.position="top")

df %>%
  group_by(Card_Category,Attrition_Flag) %>%
  summarise(`Average Transactions Count` = mean(Total_Trans_Ct),
            `Average Transactions Amount` = mean(Total_Trans_Amt))

#correlograma
df %>%
  ggcorr(method = c("everything", "pearson"), low = "#FFFF33", mid = "#FF7F00"  , high = "#E41A1C", 
         label = TRUE, label_size = 4, label_color = "white",size =4, layout.exp =1) +
  labs(title="Basic Correlogram",
       x="",
       y="") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(legend.position="bottom")

#tirando a variável categórica 

#separando em training e testing
set.seed(237)

train_index = createDataPartition(y = df$att,  # y = our dependent variable.
                                  p = .6,  # Specifies split into 70% & 30%.
                                  list = FALSE,  # Sets results to matrix form. 
                                  times = 1)  # Sets number of partitions to create to 1. 


#Now we can split our data into train and test data using the randomly sampled train_index that we just formed.

train_data = df[train_index,]  # Use train_index of iris data to create train_data.
test_data = df[-train_index,]  # Use whatever that is not in train_index to create test_data.

#glm modelo
model = glm(att~.,
            data = train_data,
            family = "binomial", maxit = 100)

res <- predict(model, test_data, type = 'response')

res <- predict(model, train_data, type = 'response')


#acurácia
confmatrix <- table(Actual_Value = train_data$att, predicted_value = res > 0.5)
confmatrix

(confmatrix[[1,1]] + confmatrix[[2,2]])*100 / sum(confmatrix)

##Decision tree
df <- df %>%
  select(!att)

set.seed(237)

train_index = createDataPartition(y = df$Attrition_Flag,  # y = our dependent variable.
                                  p = .7,  # Specifies split into 70% & 30%.
                                  list = FALSE,  # Sets results to matrix form. 
                                  times = 1)  # Sets number of partitions to create to 1. 


#Now we can split our data into train and test data using the randomly sampled train_index that we just formed.

train_data = df[train_index,]  # Use train_index of iris data to create train_data.
test_data = df[-train_index,]  # Use whatever that is not in train_index to create test_data.

fitControl = trainControl(method = "cv", number = 10, savePredictions = TRUE)

### Create model
#### Check the predicted accuracy of our decision tree model by running it on resamples of our train data. Later we will test the accuracy of the model by running a prediction on our test data.

dt_model = train( `Attrition_Flag`~ ., # Set Y variable followed by '~'. The period indicates to include all variables for prediction. 
                 data = train_data, # Data
                 method = 'rpart', # Specify SVM model
                 trControl = fitControl) # Use cross validation

confusionMatrix(dt_model)

### Create object of importance of our variables 

dt_importance <- varImp(dt_model)

#### Create plot of importance of variables

ggplot(data = dt_importance, mapping = aes(x = dt_importance[,1])) + # Data & mapping
  geom_boxplot() + # Create box plot
  theme_light() +
  labs(title="Variable importance: Decision tree model",
       fill="",
       x="",
       y="") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

### Now lets plot the decision tree using fancyRpartPlot() from the RATTLE package. This will give us clear insight into how the model makes its predictions.

fancyRpartPlot(dt_model$finalModel, sub = '')

###PREDICTION: Decision tree model - Use the created dt_model to run a prediction on the test data.

prediction_dt = predict(dt_model, test_data)

####Check the proportion of the predictions which were accurate.

t1 = table(prediction_dt, test_data$Attrition_Flag) %>% # Create prediction table. 
  prop.table() %>% # Convert table values into proportions instead of counts. 
  round(2) # Round numbers to 2 significant values. 

stargazer(t, type = "html")

(t[[1,1]] + t[[2,2]])*100 / sum(t)
##Random Forest
###Create an object for a 10 fold cross validation. We will use this in our train() function to set trControl next.

fitControl = trainControl(method = "cv", number = 10, savePredictions = TRUE)

###Create a training model using train() from the CARET package.

rf_model = train(
  `Attrition_Flag` ~ .,  # Set Y variable followed by "~." to include all variables in formula.
  method = 'rf',  # Set method as random forest.
  trControl = fitControl,  # Set cross validation settings
  data = train_data)  # Set data as train_data. 

###Use the varImp() function to grab the importance of each variable in our random forest model and then plot them.

rf_importance = varImp(rf_model) 

#### Create box plot of importance of variables
ggplot(data = rf_importance, mapping = aes(x = rf_importance[,1])) + # Data & mapping
  geom_boxplot() + # Create box plot
  labs(title = "Variable importance: Random forest model") + # Title
  theme_light() # Theme

###Now, lets check the predicted accuracy of the random forest model by running it on resamples of our train data. Later we will test the accuracy of the model by running a prediction on our test data, which is data our model has not seen before.

t2=confusionMatrix(rf_model)

print(t)
###Prediction: Random forest model - We will now use our created random forest model in order to predict species on our test data (i.e., the data set our ‘machine’ has not seen before).

####Use the created rf_model to run a prediction on the test data.

prediction_rf = predict(rf_model, test_data)

####Check the accuracy of our random forest model on our test data.

t2 = table(prediction_rf, test_data$`Attrition_Flag`) %>% # Create prediction table. 
  prop.table() %>% # Convert table values into proportions instead of counts. 
  round(2) # Round numbers to 2 significant values. 

##Support Vector Machine (SVM)
###Model the SVM model with a 10 fold cross validation.

fitControl = trainControl(method = "cv", number = 10, savePredictions = TRUE)

###Create a predictor model with the train() function from the CARET package. Specify method = 'svmLinear' to run a SVM model.

svm_model = train(`Attrition_Flag` ~ ., # Set Y variable followed by '~'. The period indicates to include all variables for prediction. 
                  data = train_data, # Data
                  method = 'svmLinear', # Specify SVM model
                  trControl = fitControl) # Use cross validation

###Check the predicted accuracy of our naive Bayes model by running it on resamples of our train data. Later we will test the accuracy of the model by running a prediction on our test data.

confusionMatrix(svm_model)

###Use the varImp() function to grab the importance of each variable in our random forest model and then plot them. *****

svm_importance = varImp(svm_model)

###Create box plot
ggplot(data = svm_importance, mapping = aes(x = svm_importance[,1])) + # Data & mapping
  geom_boxplot() + # Create box plot
  labs(title = "Variable importance: Support vector machine model") + # Title
  theme_light() # Theme

###PREDICTION: Support Vector Machine - Use the created svm_model to run a prediction on the test data.

prediction_svm = predict(svm_model, test_data)

####Check the proportion of the predictions which were accurate.

t3 = table(prediction_svm, test_data$`Attrition_Flag`) %>% # Create prediction table. 
  prop.table() %>% # Convert table values into proportions instead of counts. 
  round(2) # Round numbers to 2 significant values. 


cbind(t1,t2,t3)


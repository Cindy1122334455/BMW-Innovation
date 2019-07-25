# BMW INNOVATION CHALLENGE
**Adobe Clickstream Analytics for BMW**
## OBJECTIVE  
* Using analytics to understand the prospective car buyer’s browsing and site usage behavior 
* Identifying opportunities to convert visitors to leads that will convert to purchase.
* Developing an algorithm that will predict which visitors are likely to convert to a lead and with what degree of confidence

## DATA MANIPULATION
We combined the "Serverall" Dataset and "Lookup" Datasets to a new Dataset and then then investigated into the variable named "post event list" which records user's behavoirs on the website. We found that event 201 indicated lead form submit success so we converted the variable in the dataset named “Lead Completed_new forms" to "Lead" dataset and "Lead + Non-lead" dataset.

**Data Dictionary**

     * user.ID: a unique string for each user
     * Lead.Complete: indicator of lead / non-lead
     * Visit.Num: number of visits
     * BYO: build your own model
     * Reserve: reserve a vehicle
     * Order.Now: order a vehicle
     * Test.Drive: test Drive
     * Get.Quote: get a quote
     * Special.Offers: exclusive / special Offers
     * Gallery: browse gallery 
     * Specifications: spec & features 
     * Lease: lease vehicle
     * Compare: compare with different brands
     * BMW.Value: what can drivers get after purchase
     * BMW.Exp: e.g. driving schools, delivery programs
     * Estimate.Pay: estimate of payment
     * Add.Vehicle: indicates if the user account is created
     * Geo.City: city
     * Geo.Region: region 
     * Geo.Country: country
     * First.Hit.Referrer: Google, Facebook, BMW etc.
     * Visit.Ref.Type: search engines, email, bookmarked etc.
     * Model.Selected: model the user selected

### DATA MODELING
**Logistic Regression**

Changing category variables into dummy variables to put in the logistics regression and test variables validity.
Using stepwise regression deleted insignificant variables and recalculated model reliability index(AUC) to increase 
the credibility of model.Variable significant increased after stepwise regression.AUC increased after deleting 
variables using stepwise regression.We used a train dataset and a test dataset to calculate the accuracy rate of 
our model and gained an accuracy rate of 69.52%.

Using 10-fold cross-validation to double check the model:

     * Separate the dataset into 10 sample randomly.
     * Using same regression method and accuracy calculation methods to check the accuracy rate of 10-fold.
     * Calculate the mean accuracy and maximum accuracy rate in the 10-fold cross validation.
     * Get an average accuracy rate of 76.74% for the 10-fold cross-validation.

**Random Forest**

We have applied the Random Forest Classification algorithm to make some predictions about the possibility of 
being leads.Then we looked at relative importance by plotting the mean decrease in Gini calculated across all trees.
ROC curve is used to identify the accuracy of the test and accuracy is measured by the area under the ROC curve. 

## WEBSITE FEATURES & RECOMMENDATIONS 

     * UI & UX
     * Highlight these features in the homepage
     * Guide users to experience these features through UX design 
     * Create more interactive features that engage users emotionally and visually

**ONLINE MARKETING**

     * Place/Utilize these features in the online marketing campaigns (e.g. Email marketing)
     * Utilize more visual techniques
     * VR & AR























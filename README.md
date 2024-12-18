The R script analyzes loan performance data, focusing on borrower defaults and expected financial losses. It filters the servicing data to include loans aged 36 months or less. Flags are created to identify defaulted loans and loans with valid settlements within the first 36 months. These flags are merged with the original loan data, and missing or invalid values for key variables (e.g., FICO scores, CLTV, DTI) are handled. 

The script then builds logistic regression (logit) and probit models to predict default likelihood based on borrower characteristics. It evaluates model performance using metrics such as deviance, AIC, and confusion matrices. Expected and observed losses are calculated to estimate financial impact, scaled for business context. Finally, an extended model incorporating additional predictors (e.g., new owner, property insured) and interaction terms is developed to improve predictive accuracy. The analysis aims to assess loan risk and guide decision-making.

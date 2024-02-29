# HMO Health Insurance Cost Prediction and Analysis

## Project Overview
This project delivers a sophisticated prediction system for HMO health insurance costs utilizing R. We applied machine learning techniques to accurately classify customers by risk, thereby enabling better resource distribution and enhancing targeted marketing strategies.

## Objectives
- **Risk Classification**: Achieving a high accuracy rate in identifying high-risk policyholders through machine learning.
- **Customer Segmentation**: Improving marketing efforts and risk assessment by identifying key customer segments.

## Methodology
We developed machine learning models - SVM and Decision Trees, using `caret` and `e1071`. For customer segmentation, we utilized the `arules` package to employ the Apriori algorithm, revealing column associations and high-confidence rules that increased segmentation efficacy by 59%.

## Technologies Used
- **R**: For statistical computing and graphics.
- **caret**, **e1071**, **rpart**: For implementing machine learning algorithms.
- **arules**: For association rule learning.
- **Shiny**: To deploy the model as an interactive web application.
- **ggplot2**: For creating advanced visualizations.

## Conclusion
The project underscores the potential of machine learning in the health insurance domain, showcasing a considerable uplift in risk classification and customer segmentation. The deployment of this model in a Shiny app provides an interactive tool for HMOs, fostering an advanced approach to marketing and resource management.

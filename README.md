# Trojan Traffic Classification

### Tool(s) used:
![R](https://img.shields.io/badge/c-%2300599C.svg?style=for-the-badge&logo=c&logoColor=white)

### Findings: 
From the three models I investigated, along with a previously completed k-NN model, we can say that a decision tree with cost matrix would be the best model to classify Trojan web traffic from benign traffic. With only a few truly significant variables, the model does not need to be complicated, and as shown above complexity will actually reduce the effectiveness of a model using this data. Additionally, if we are developing this model for a hypothetical business use-case, determining Trojan attacks is much more important than reducing false positives. As such, the 100% effectiveness rate in determining true Trojan traffic is a tremendous positive with the decision tree, especially since it does not come at too high a cost with regards to false positives. As such, the final decision of using a decision tree with a cost matrix not only makes sense with regards to consideration of accuracy, but also of any potential realworld use-cases. 


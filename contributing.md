### Overview 
This file provides guidelines for contributions to the SITS package, including issues and pull requests.

### Creating an issue
The authors welcome your remarks, points and questions. When creating an issue, please provide a minimal working example that would allow reproducing the problem. You can also suggest improvements and pose questions about how do your work using SITS. Please write in English if possible,  since that would benefit the community. If you are insecure in writing in English, you can write in French, Spanish, Portuguese, Italian or German. 

### Developing a new machine learning function 
We welcome contributors that would like to include their machine learning algorithms to be used together with SITS. To write a new algorithm, please look carefully at how function such as `sits_rfor` and `sits_mlp` work. All ML functions take a trained data set organised as a sits tibble, and their should be provide a `predict` function as an R closure. An R closure is a function written by another function.  A closure can access its own arguments, and variables defined in its parent. See the explanation by Hadley Wickham at http://adv-r.had.co.nz/Functional-programming.html. 

#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
    DATA_VECTOR(age);
    DATA_VECTOR(length);
    DATA_INTEGER(modnum);
    int n = length.size();  //n = length(y)

    PARAMETER(log_linf);
    PARAMETER(log_k);
    PARAMETER(log_Delta);
    PARAMETER(log_a50);
    PARAMETER(a0);
    // should make separate a0 bc it can be negative so shouldn't be in log space
    PARAMETER(logSigma);
    vector<Type> yfit(n);
    
    Type linf = exp(log_linf);
    Type k = exp(log_k);
    Type Delta = exp(log_Delta);
    Type a50 = exp(log_a50);
    
    Type neglogL = 0.0;
    
    if(modnum == 1){
        yfit = linf*(1 - exp(-k*(age-a0))); //Von Bert
        
    }
    
    if(modnum == 2){
        yfit = linf/(1 + exp(-log(19)*(age-a50)/Delta)); //Logistic
    }
    
    neglogL = -sum(dnorm(length, yfit, exp(logSigma), true));
   
    
    ADREPORT(log_linf);
    ADREPORT(log_k);
    ADREPORT(a0);
    ADREPORT(log_a50);
    ADREPORT(log_Delta);
    REPORT(yfit);
    
    return neglogL;

}

#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
    DATA_VECTOR(age);
    DATA_VECTOR(length);
    DATA_INTEGER(modnum);
    int n = length.size();  //n = length(y)

    PARAMETER(log_linf);
    PARAMETER(log_kd);
    PARAMETER(log_aref); // should make separate a0 bc it can be negative so shouldn't be in log space
    PARAMETER(logSigma);
    vector<Type> yfit(n);
    
    Type linf = exp(log_linf);
    Type kd = exp(log_kd);
    Type aref = exp(log_aref);
    
    Type neglogL = 0.0;
    
    if(modnum == 1){
        yfit = linf*(1 - exp(-kd*(age-aref))); //Von Bert
        
    }
    
    if(modnum == 2){
        yfit = linf/(1 + exp(-log(19)*(age-aref)/kd)); //Logistic
    }
    
    neglogL = -sum(dnorm(length, yfit, exp(logSigma), true));
   
    
    ADREPORT(log_linf);
    ADREPORT(log_kd);
    ADREPORT(log_aref);
    //ADREPORT(log_a50);
    //ADREPORT(log_Delta);
    REPORT(yfit);
    
    return neglogL;

}

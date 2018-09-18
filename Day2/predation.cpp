#include <TMB.hpp>

//template <class Type> Type square(Type x){return x*x;}

template<class Type>
Type objective_function<Type>::operator() ()
{
    DATA_VECTOR(pred);
    DATA_VECTOR(prey_1);
    DATA_VECTOR(prey_2);
    DATA_VECTOR(prey_3);
    DATA_VECTOR(consump_1);
    DATA_VECTOR(consump_2);
    DATA_VECTOR(consump_3);
    DATA_INTEGER(Model_Num);
    DATA_INTEGER(n_row);
    
    PARAMETER(log_alpha1);
    PARAMETER(log_alpha2);
    PARAMETER(log_alpha3);
    PARAMETER(beta1);
    PARAMETER(beta2);
    PARAMETER(beta3);
    PARAMETER(gamma1);
    PARAMETER(gamma2);
    PARAMETER(gamma3);
    PARAMETER(lambda1);
    PARAMETER(lambda2);
    PARAMETER(lambda3);
    vector<Type> pfit_1(n_row);
    vector<Type> pfit_2(n_row);
    vector<Type> pfit_3(n_row);
    Type alpha1 = exp(log_alpha1);
    Type alpha2 = exp(log_alpha2);
    Type alpha3 = exp(log_alpha3);
    vector<Type> like1;
    vector<Type> like2;
    vector<Type> like3;
    
    if (Model_Num == 1) {
        //Prey 1
        pfit_1 = alpha1*pred;
        like1 = (log(consump_1)-log(pfit_1))*(log(consump_1)-log(pfit_1));
        
        Type f = sum(like1);
        
        //Prey 2
        pfit_2 = alpha2*pred;
        like2 = (log(consump_2)-log(pfit_2))*(log(consump_2)-log(pfit_2));
        
        Type f2 = sum(like2);
        
        
        //Prey 3
        pfit_3 = alpha3*pred;
        like3 = (log(consump_3)-log(pfit_3))*(log(consump_3)-log(pfit_3));
        
        Type f3 = sum(like3);
        
        Type obj_fun = f + f2 + f3;
        return obj_fun;
    }
    
    if (Model_Num == 2) {
        //Prey 1
        pfit_1 = (alpha1*pred)/(1+beta1*prey_1);
        like1 = (log(consump_1)-log(pfit_1))*(log(consump_1)-log(pfit_1));
        
        Type f = sum(like1);
        
        //Prey 2
        pfit_2 = (alpha2*pred)/(1+beta2*prey_2);
        like2 = (log(consump_2)-log(pfit_2))*(log(consump_2)-log(pfit_2));
    
        Type f2 = sum(like2);
        
        //Prey 3
        pfit_3 = (alpha3*pred)/(1+beta3*prey_3);
        like3 = (log(consump_3)-log(pfit_3))*(log(consump_3)-log(pfit_3));
        
        Type f3 = sum(like3);
    
        Type obj_fun = f + f2 + f3;
        return obj_fun;
    }
    
    
    if (Model_Num == 3) {
        //Prey 1
        pfit_1 = (alpha1*pred*pow(prey_1,(gamma1 - 1)))/(1+beta1*pow(prey_1, gamma1));
        like1 = (log(consump_1)-log(pfit_1))*(log(consump_1)-log(pfit_1));
        
        Type f = sum(like1);
        
        //Prey 2
        pfit_2 = (alpha2*pred*pow(prey_2,(gamma2 - 1)))/(1+beta2*pow(prey_2, gamma2));
        like2 = (log(consump_2)-log(pfit_2))*(log(consump_2)-log(pfit_2));
        
        Type f2 = sum(like2);
        
        //Prey 3
        pfit_3 = (alpha3*pred*pow(prey_3,(gamma3 - 1)))/(1+beta3*pow(prey_3, gamma3));
        like3 = (log(consump_3)-log(pfit_3))*(log(consump_3)-log(pfit_3));
        
        Type f3 = sum(like3);
        
        Type obj_fun = f + f2 + f3;
        return obj_fun;
    }

    if (Model_Num == 4) {
        //Prey 1
        pfit_1 = (alpha1*pred)/(1+beta1*prey_1+lambda1*pred);
        like1 = (log(consump_1)-log(pfit_1))*(log(consump_1)-log(pfit_1));
        
        Type f = sum(like1);
        
        //Prey 2
        pfit_2 = (alpha2*pred)/(1+beta2*prey_2+lambda2*pred);
        like2 = (log(consump_2)-log(pfit_2))*(log(consump_2)-log(pfit_2));
        
        Type f2 = sum(like2);
        
        //Prey 3
        pfit_3 = (alpha3*pred)/(1+beta3*prey_3+lambda3*pred);
        like3 = (log(consump_3)-log(pfit_3))*(log(consump_3)-log(pfit_3));
        
        Type f3 = sum(like3);
        
        Type obj_fun = f + f2 + f3;
        return obj_fun;
    }
    
}

data{
    int N;
    int D;
    int Age[N];
    int Birth[N,D];
}

parameters{         
  real<lower=0, upper=1>  Cor; //# Higher-level priors
  real<lower=0> Decay;         //#
  real<lower=0>  Scale;        //#

  real Mu;

  vector [D] Gamma;   
}

transformed parameters{
    vector<lower=0,upper=1>[D] prob_birth;

    prob_birth = inv_logit(Mu + Gamma);
}

model{
  matrix[D,D] X;                    //#

  Cor ~ beta(10,2);                 //# Priors on Gaussian process
  Decay ~ exponential(1);           //# 
  Scale ~ exponential(1);           //#       

  Mu ~ normal(0, 4);                //# Average rate

  for (i in 1:(D-1)){
  for (j in (i+1):D){
                X[i,j] = Cor * exp(- Decay * (  (i-j)^2 / D^2    ));    //# Estimate Correlations
                X[j,i] = X[i,j];                         //# Fill Other Triangle
                       }}
 for (i in 1:D){
                X[i,i] = 1;                              //# Fill Diag
                       }


  Gamma ~ multi_normal_cholesky( rep_vector(0,D) , Scale*cholesky_decompose(X));

    for (i in 1:N) {
     for(a in 1:Age[i]){
            Birth[i,a] ~ bernoulli(prob_birth[a]); 
        }}
}

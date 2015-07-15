// This admb code does the constrained optimisation using linear programming 
// The linear programming code is based off the "numerical recipe" book
 
DATA_SECTION
// Read in from main.dat and coeff.dat 
// main_code.dat contains info that DO NOT change between year and iteration
// coeff.dat contains info that changes between years and iterations i.e Weight_strategy, b1, and b2 (needs to combine output from the linear programming run + fortran code (for TAC))
// These contain the following variables:
//	- Nb_strategy			: the number of fishing strategies in the model
//	- Nb_species			: the number of species (in order: cod, pollock, yellowfin sole, and halibut (bycatch species))
//	- price					: the price of species (right now exogenous but could be changed later on)
//	- Weight_strategy		: the initial weight "dk" given to each fishing strategy (this is the variable that will be optimized)
//	- Catch_strategy		: the catch proportion by species (cod, halibut, pollock and yellowfin)
//	- nb_b1					: total number of b1 constaints i.e Ax <= b
//  - nb_b2					: total number of b2 constaints i.e Ax >= b
//  - n_b1constr_types		: number of types of b1 constraints (e.g. TAC, OY, upper bound for "dk")
//  - n_b1by_constr			: number of each type of b1 constraints 
//  - n_b2constr_types		: number of types of b2 constraints (e.g. lower bound for "dk") 
//  - n_b2by_constr			: number of each type of b2 constraints 
//  - a0 					: the a0 vector = the objective function
//  - a1 					: the a1 matrix that contains the info on the constraints of the form Ax <= b 
//  - a2 					: the a2 matrix that contains the info on the constraints of the form Ax >= b 
//  - b1 					: the b1 bounds (the upper bounds value of each b1 constraints)
//  - b2 					: the b2 bounds (the upper bounds value of each b2 constraints)
//  - relative_catch		: the results of linear programming results
//  - Actual_Catch			: the actual catch for each species derived from the linear optimization step
//  - Actual_Catch_sub		: temporary matrix that stores the actual catch for each species and strategy
	
	init_int Nb_strategy
	init_int Nb_species
	init_vector price(1,Nb_species)
	init_matrix Catch_strategy(1,Nb_species,1,Nb_strategy)
	init_int nb_b1
	init_int nb_b2
	init_int n_b1constr_types
	init_ivector n_b1by_constr(1,n_b1constr_types)
	init_int n_b2constr_types
	init_ivector n_b2by_constr(1,n_b2constr_types)
	init_vector a0(1,Nb_strategy);
	init_matrix a1(1,nb_b1,1,Nb_strategy)
	init_matrix a2(1,nb_b2,1,Nb_strategy)
	//!! cout << n_b1by_constr << endl;
	init_vector Weight_strategy(1,Nb_strategy)
	init_vector b1(1,nb_b1)
	init_vector b2(1,nb_b2)
	//!! cout << Weight_strategy << endl; exit(1);
	//!! cout << TAC << endl; exit(1);

    !! ad_comm::change_datafile_name("TAC.dat");
	init_vector TAC(1,Nb_species)
 
	int dummy;
	!! dummy=0;
	
 LOCAL_CALCS
  // to update the TAC value within the b1 constraint vector
   for (int it=1;it<=Nb_species;it++){
   b1(it) = TAC(it);
	 if(TAC(it)>0) dummy+=1;
   }
  // If it happens that the TAC for one species becomes NULL i.e below MSST then lower the bounds for the constraints
	 if(dummy<Nb_species){
		for (int it=1;it<=(nb_b2);it++){
		b2(it) = 0;
		}	
	 }
 END_CALCS
	//!!cout << "b1_bounds: " << b1 << endl; exit(1);
	
	vector relative_catch(1,Nb_strategy);
 LOCAL_CALCS		// Need to calculate the average catch between the boundaries for the initial value
  for (int it=1;it<=Nb_strategy;it++){
  relative_catch(it) = Weight_strategy(it);
  }
 END_CALCS
	//!! cout << relative_catch << endl;exit(1);

    vector Actual_Catch(1,Nb_species);
    matrix Actual_Catch_sub(1,Nb_species,1,Nb_strategy)

    !! ad_comm::change_datafile_name("simple.dat");
	init_int nobs
	init_vector Y(1,nobs)
	init_vector x(1,nobs)	

	int rnseed           // Random number seed
	!! rnseed = 123;
	
PARAMETER_SECTION
  init_number a   
  init_number b   
  vector pred_Y(1,nobs)
  objective_function_value obj_fun   

PRELIMINARY_CALCULATIONS_SECTION
  opt_sim();
  for (int it=1;it<=(nb_b1-Nb_species-1);it++){
  b1(it+Nb_species+1) = 5*relative_catch(it);
  }
  for (int it=1;it<=(nb_b2);it++){
  b2(it) = 0.2*relative_catch(it);
  }
  store_results();
  exit(1);
//  cout << b1 << endl;
//  cout << b2 << endl;exit(1);
  
PROCEDURE_SECTION
  pred_Y=a*x+b;
  obj_fun=(norm2(pred_Y-Y)); 
  obj_fun=nobs/2.*log(obj_fun);    // make it a likelihood function so that

  
FUNCTION opt_sim
// This code run the linear programming based on the constraints defined in:
// - the coeff.dat file
// - the main_code.dat file

  // Get the actual catch (with constraints) 
    Get_Actual_Catch();       

FUNCTION  Get_Actual_Catch
// Apply the linear programming as specified in the Numerical recipe book
// Returns:  Actual_Catch
// Input variables:
// - a0 (the objective function based on the variables of interest)
// - the constraints matrix a1, a2, and a3 matrix as described in the lpcode.cpp
// - the vector of boundary conditions b1 and b2 (i.e the lower and upper constraints associated with each type of constraints (i.e TAC, OY, bycatch, change in fishing location)

	// Set the variables to run the linear programming code
    dmatrix a3;
    dvector b3;
	a3.initialize();
	b3.initialize();
    ivector ierr(1,1);
    ierr(1)=0;
		
	cout << ierr(1) << endl;
		
    do 
    {
       lpsimplex(a0,a1,a2,a3,b1,b2,b3,relative_catch,ierr);
       if (ierr(1))
       {
        cout<< "reDoing simplex with a different initial relative catch value dk,t=1"<<ierr<<endl;
		relative_catch *= 0.9;
		for (int it=1;it<=(nb_b1-Nb_species-1);it++){
		b1(it+Nb_species+1) = 5*relative_catch(it);
		}
		for (int it=1;it<=(nb_b2);it++){
		b2(it) = 0.2*relative_catch(it);
		}
		lpsimplex(a0,a1,a2,a3,b1,b2,b3,relative_catch,ierr);
  		cout << "the current value of relative_catch is:" << relative_catch << endl;
       }
    } while (ierr(1));
 
  // Now get the actual catch 
  Actual_Catch.initialize();
  Actual_Catch_sub.initialize();
  for (int ispp=1;ispp<=Nb_species;ispp++) 
  {
    for (int isub=1;isub<=Nb_strategy;isub++) 
    {
      Actual_Catch_sub(ispp,isub) += Catch_strategy(ispp,isub)*relative_catch(isub) ; 
    }
    Actual_Catch(ispp) = sum(Actual_Catch_sub(ispp));
    cout << "The actual catch is: "<< Actual_Catch(ispp)<< endl;
  }
	
FUNCTION  store_results
  // Now store the results on:
  // - Actual catch for each species (including the bycatch species)
  // - The value of the "dk" matrix for the specific year 

  //ofstream results("coeff.dat");   // this is to create an output file names "result.sso" with the results obtained from the step above
  ofstream catches("catches.out");   // this is to create an output file names "result.sso" with the results obtained from the step above
  catches << "# Catch by species" << endl; 
  catches << Actual_Catch << endl;
  //results << "# dk,t" << endl;
  //results << relative_catch << endl;
  //results << "# b1" << endl;
  //results <<  b1 << endl;
  //results << "# b2" << endl;
  //results <<  b2 << endl;
  
FUNCTION void xxerror(const char * s)
    cerr << s << endl;
    exit(1);
  
// This is a piece of code that Jim Ianelli added to set-up the data to use for the simplex code defined below i.e the a matrix
// a0 is a vector of variables to optimize -> used to calculate N
// a1 is a matrix that contains the info on the constraints of the form Ax <= b (if existing) --> this leads to m1
// a2 is a matrix that contains the info on the constraints of the form Ax >= b (if existing) --> this leads to m2
// a3 is a matrix that contains the info on the constraints of the form Ax == b (if existing) --> this leads to m3 
// b1 is a vector with the value of the constant value in the tableau for the Ax <= b type constraint
// b2 is a vector with the value of the constant value in the tableau for the Ax >= b type constraint
// b3 is a vector with the value of the constant value in the tableau for the Ax == b type constraint 
// ierr is a flag. If 0, a finite solution is found, +1 function is unbounded, -1 no solution

FUNCTION  void lpsimplex(dvector& a0,dmatrix& a1, dmatrix& a2,dmatrix & a3, dvector& b1, dvector& b2, dvector& b3, dvector& x,ivector& ierr)
    int N;
      N=a0.indexmax()-a0.indexmin()+1; 
    int m1;
    if (allocated(a1))
      m1=a1.indexmax()-a1.indexmin()+1; 
    else
      m1=0;
    int m2;
    if (allocated(a2))
      m2=a2.indexmax()-a2.indexmin()+1; 
    else
      m2=0;
    int m3;
    if (allocated(a3))
      m3=a3.indexmax()-a3.indexmin()+1; 
    else
      m3=0;
    int M=m1+m2+m3;
    // check that all vectors and matrices have the right shape
    //check_all_matrix_shapes();
    dmatrix a(1,M+2,1,N+1);
    a.initialize();
  
    a(1)(2,N+1).shift(a0.indexmin())=a0;		// to fill in the objective function
    a(1,1)=0;									// this is the left hand variable of the objective function 
    int i;										
    int ii=1;
    for (i=2;i<=m1+1;i++)
    {
      a(i)(2,N+1).shift(a1(ii).indexmin())=-a1(ii);			// this is to fill in the a matrix with constraints of type m1 i.e the "-A" in the "b-Ax"
      a(i)(1)=b1(ii++);										// this is the b1 in the "b-Ax" of the constraints of type m1
    }
    ii=1;
    for (i=m1+2;i<=m1+m2+1;i++)
    {
      a(i)(2,N+1).shift(a2(ii).indexmin())=-a2(ii);         // this is to fill in the a matrix with constraints of type m2 i.e the "-A" in the "b-Ax" (this follows the input of m1)
      a(i)(1)=b2(ii++);                                     // this is the b2 in the "b-Ax" of the constraints of type m2
    }
    ii=1;
    for (i=m1+m2+2;i<=m1+m2+m3+1;i++)
    {
      a(i)(2,N+1).shift(a3(ii).indexmin())=-a3(ii);			// this is to fill in the a matrix with constraints of type m3 i.e the "-A" in the "b-Ax" (this follows the input of m2)
      a(i)(1)=b3(ii++);                                     // this is the b3 in the "b-Ax" of the constraints of type m3
    }
  // int icase;
  ivector izrov(1,N);
  ivector iposv(1,M);
  simplx(a,M,N,m1,m2,m3,ierr,izrov,iposv);
  if (ierr(1)) cerr << "Constraint error "<<ierr<<endl;
  // cout << "a" << endl; cout << a << endl; cout << "izrov" << endl; cout << izrov << endl; cout << "iposv" << endl; cout << iposv << endl; 
  for (i=1;i<=N;i++) 
    if (izrov(i)<=N) x(izrov(i))=0.0;
  for (int j=1;j<=M;j++) 
    if (iposv(j)<=N) x(iposv(j))=a(j+1,1);

// (C) Copr. 1986-92 Numerical Recipes Software 888888888888. 
// This is the simplex algorithm that is explained in the numerical recipe book in C++
// m (M in the book) is the total number of contraints in the model (basically the sum of m1,m2,m3)
// m1 (m1 in the book too) is the number of inequality of the form; Ax <= b
// m2 (m2 in the book too) is the number of inequality of the form; Ax >= b
// m3 (m3 in the book too) is the number of equality constraints of the form; Ax == b
// n (N in the book) is the total number of variables (in this case x) to evaluate to maximize the objective function
// a is a 2-dimensional array containing the portion of the tableau without the slack variables function. The size of a is (m+1 rows and n+1 columns). The first row is the objective function and the first colums contains the final answer. There is a m+2 row which contains the auxiliary objective function. It is VERY important to respect the order of the rows of a i.e objective function, then <= constraint, then >= contraints, then = contraints
// iposv contains the number i of the original variable x[i] which is now represented by row j+1 (i.e the constraints). P.S: A value i>n is for slack variable
// izrov contains the number i of the original variable x[i] which is now represented by column j+1 of a (i.e the variables of the objective function). P.S: A value i>n is for slack variable
// icase is a flag. If 0, a finite solution is found, +1 function is unbounded, -1 no solution
// THIS code treats the case of degenerate feasible vectors
// The inequality is tested based on small parameter EPS and this should be adjusted if convergence issue (based on the scale of the input data)
	
FUNCTION  void simplx(dmatrix& a, int m, int n, int m1, int m2, int m3, ivector& icase, ivector& izrov, ivector& iposv)
   const double EPS=1.e-2;
   int i,ip,ir,is,k,kh,kp,m12,nl1,nl2;
   double q1,bmax;
   q1 =0.;
   bmax =0.;

   if (m != (m1+m2+m3)) xxerror("Bad input constraint counts in simplx");
   ivector l1=ivector(1,n+1);
   ivector l2=ivector(1,m);
   ivector l3=ivector(1,m);
   nl1=n;
   for (k=1;k<=n;k++) l1[k]=izrov[k]=k;
   nl2=m;
   for (i=1;i<=m;i++) {
    if (a[i+1][1] < 0.0) xxerror("Bad input tableau in simplx");
    l2[i]=i;
    iposv[i]=n+i;
   }
   for (i=1;i<=m2;i++) l3[i]=1;
   ir=0;
   if (m2+m3) {
    ir=1;
    for (k=1;k<=(n+1);k++) {
     q1=0.0;
     for (i=m1+1;i<=m;i++) q1 += a[i+1][k];
     a[m+2][k] = -q1;
    }
    do {
     simp1(a,m+1,l1,nl1,0,&kp,&bmax);
     if (bmax <= EPS && a[m+2][1] < -EPS) {
      icase(1) = -1;
      cerr << " bmax part "<<m+2<<" "<<bmax<<" "<<a[m+2][1]<<" "<<-EPS<<endl;
      return;
     } else if (bmax <= EPS && a[m+2][1] <= EPS) {
      m12=m1+m2+1;
      for (ip=m12;ip<=m;ip++) {
       if (iposv[ip] == (ip+n)) {
        simp1(a,ip,l1,nl1,1,&kp,&bmax);
        if (bmax > 0.0)
         goto one;
       }
      }
      ir=0;
      --m12;
      for (i=m1+1;i<=m12;i++)
     if (l3[i-m1] == 1)
      for (k=1;k<=n+1;k++)
       a[i+1][k] = -a[i+1][k];
    break;
   }
   simp2(a,n,l2,nl2,&ip,kp,&q1);
   if (ip == 0) {
    icase(1) = -1;
    cerr << " ip == 0 "<<endl;
    return;
   }
   one: simp3(a,m+1,n,ip,kp);
   if (iposv[ip] >= (n+m1+m2+1)) {
    for (k=1;k<=nl1;k++)
     if (l1[k] == kp) break;
    --nl1;
    for (is=k;is<=nl1;is++) l1[is]=l1[is+1];
    ++a[m+2][kp+1];
    for (i=1;i<=m+2;i++) a[i][kp+1] = -a[i][kp+1];
   } else {
    if (iposv[ip] >= (n+m1+1)) {
     kh=iposv[ip]-m1-n;
     if (l3[kh]) {
      l3[kh]=0;
      ++a[m+2][kp+1];
      for (i=1;i<=m+2;i++)
       a[i][kp+1] = -a[i][kp+1];
     }
    }
   }
   is=izrov[kp];
   izrov[kp]=iposv[ip];
   iposv[ip]=is;
  } while (ir);
   }
   for (;;) {
    simp1(a,0,l1,nl1,0,&kp,&bmax);
    if (bmax <= 0.0) {
     icase(1) =0;
     return;
  }
  simp2(a,n,l2,nl2,&ip,kp,&q1);
  if (ip == 0) {
   icase(1)=1;
   return;
  }
  simp3(a,m,n,ip,kp);
  is=izrov[kp];
  izrov[kp]=iposv[ip];
  iposv[ip]=is;
 }
  
  // (C) Copr. 1986-92 Numerical Recipes Software 888888888888. 
  #define EPS 1.0e-2

FUNCTION  void simp1(dmatrix& a,int mm, ivector& ll, int nll, int iabf, int *kp, double *bmax)
 
    int k;
    double test;

    *kp=ll[1];
    *bmax=a[mm+1][*kp+1];
    for (k=2;k<=nll;k++) {
      if (iabf == 0)
        test=a[mm+1][ll[k]+1]-(*bmax);
      else
        test=fabs(a[mm+1][ll[k]+1])-fabs(*bmax);
      if (test > 0.0) {
        *bmax=a[mm+1][ll[k]+1];
        *kp=ll[k];
      }
    }
 
  // (C) Copr. 1986-92 Numerical Recipes Software 888888888888. 
  
FUNCTION  void simp2(dmatrix& a, int n, ivector& l2, int nl2, int *ip, int kp, double *q1)

  int k,ii,i;
  double qp,q0,q;
  qp=0.;
  q0=0.;
  q=0.;

  *ip=0;
  for (i=1;i<=nl2;i++)
    if (a[l2[i]+1][kp+1] < -EPS) break;
  if (i>nl2) return;
  *q1 = -a[l2[i]+1][1]/a[l2[i]+1][kp+1];
  *ip=l2[i];
  for (i=i+1;i<=nl2;i++) {
    ii=l2[i];
    if (a[ii+1][kp+1] < -EPS) {
      q = -a[ii+1][1]/a[ii+1][kp+1];
      if (q < *q1) {
        *ip=ii;
        *q1=q;
      } else if (q == *q1) {
        for (k=1;k<=n;k++) {
          qp = -a[*ip+1][k+1]/a[*ip+1][kp+1];
          q0 = -a[ii+1][k+1]/a[ii+1][kp+1];
          if (q0 != qp) break;
        }
        if (q0 < qp) *ip=ii;
      }
    }
  }

  #undef EPS
  // (C) Copr. 1986-92 Numerical Recipes Software 888888888888. 
  
FUNCTION   void simp3(dmatrix& a,int i1,int k1,int ip,int kp)

  int kk,ii;
  double piv;

  piv=1.0/a[ip+1][kp+1];
  for (ii=1;ii<=i1+1;ii++)
    if (ii-1 != ip) {
      a[ii][kp+1] *= piv;
      for (kk=1;kk<=k1+1;kk++)
        if (kk-1 != kp)
          a[ii][kk] -= a[ip+1][kk]*a[ii][kp+1];
    }
  for (kk=1;kk<=k1+1;kk++)
    if (kk-1 != kp) a[ip+1][kk] *= -piv;
  a[ip+1][kp+1]=piv;

  // (C) Copr. 1986-92 Numerical Recipes Software 888888888888. 
    
  
GLOBALS_SECTION
//  #include <lpcode.cpp>
  #include <admodel.h>

RUNTIME_SECTION
   maximum_function_evaluations 100,200,5000
   convergence_criteria .1,.01,1e-5

TOP_OF_MAIN_SECTION
//  gradient_structure::set_MAX_NVAR_OFFSET(1000);
//  gradient_structure::set_GRADSTACK_BUFFER_SIZE(100000);
//  gradient_structure::set_CMPDIF_BUFFER_SIZE(1000000);
  arrmblsize  = 1000000 ;


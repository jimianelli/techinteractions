// =====================================================================================
//
//       Filename:  lpcode.cpp
//
//       Description:  
//
//       Version:  1.0
//       Created:  1/29/2013 8:36:41 PM
//       Modified:  15/04/2015
//       Compiler:  gcc
//
//       Authors
// 		 James N Ianelli, jim.ianelli@gmail.com - original contributor
//	     Kotaro Ono, kotarono@uw.edu - updated the code			
//
//       Organization:  Resource Ecology and Fisheries Management
//
// =====================================================================================
//

  void lpsimplex(dvector& a0,dmatrix& a1, dmatrix& a2,dmatrix & a3, dvector& b1, dvector& b2, dvector& b3, dvector& x, ivector& ierr);
  void simplx(dmatrix& a, int m, int n, int m1, int m2, int m3, ivector& icase, ivector& izrov, ivector& iposv);
  void simp1(dmatrix& a,int mm, ivector& ll, int nll, int iabf, int *kp, double *bmax);
  void simp2(dmatrix& a, int n, ivector& l2, int nl2, int *ip, int kp, double *q1);
  void simp3(dmatrix& a,int i1,int k1,int ip,int kp);

	
// An error reporting function
  void xxerror(const char * s)
  {
    cerr << s << endl;
    exit(1);
  }
  
// This is a piece of code that Jim Ianelli added to set-up the data to use for the simplex code defined below i.e the a matrix
// a0 is a vector of variables to optimize -> used to calculate N
// a1 is a matrix that contains the info on the constraints of the form Ax <= b (if existing) --> this leads to m1
// a2 is a matrix that contains the info on the constraints of the form Ax >= b (if existing) --> this leads to m2
// a3 is a matrix that contains the info on the constraints of the form Ax == b (if existing) --> this leads to m3 
// b1 is a vector with the value of the constant value in the tableau for the Ax <= b type constraint
// b2 is a vector with the value of the constant value in the tableau for the Ax >= b type constraint
// b3 is a vector with the value of the constant value in the tableau for the Ax == b type constraint 
// ierr is 

  void lpsimplex(dvector& a0,dmatrix& a1, dmatrix& a2,dmatrix & a3, dvector& b1, dvector& b2, dvector& b3, dvector& x,ivector& ierr)
  {
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
  }

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
	
  void simplx(dmatrix& a, int m, int n, int m1, int m2, int m3, ivector& icase,
   ivector& izrov, ivector& iposv)
  {
    const double EPS=1.e-14;
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
  }
  // (C) Copr. 1986-92 Numerical Recipes Software 888888888888. 
  #define EPS 1.0e-7

  void simp1(dmatrix& a,int mm, ivector& ll, int nll, int iabf, int *kp, double *bmax)
  {
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
  }
  // (C) Copr. 1986-92 Numerical Recipes Software 888888888888. 
  void simp2(dmatrix& a, int n, ivector& l2, int nl2, int *ip, int kp, double *q1)
  {
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
  }
  #undef EPS
  // (C) Copr. 1986-92 Numerical Recipes Software 888888888888. 
  void simp3(dmatrix& a,int i1,int k1,int ip,int kp)
  {
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
  }
  // (C) Copr. 1986-92 Numerical Recipes Software 888888888888. 
    


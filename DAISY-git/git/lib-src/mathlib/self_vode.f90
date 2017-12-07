!$
!===================================================================================================
!
!   self version of vode: ordinary differential equations 
!---------------------------------------------------------------------------------------------------
!   public subroutine lists:    No
!
!   public type lists:          No
!
!===================================================================================================
module self_vode

    implicit none
    public

contains
    !$
    !===============================================================================================
    ! dvode: variable-coefficient ordinary differential equation solver,
    ! with fixed-leading-coefficient implementation.
    ! this version is in double precision.
    !
    ! dvode solves the initial value problem for stiff or nonstiff
    ! systems of first order odes,
    !     dy/dt = f(t,y) ,  or, in component form,
    !     dy(i)/dt = f(i) = f(i,t,y(1),y(2),...,y(neq)) (i = 1,...,neq).
    ! dvode is a package based on the episode and episodeb packages, and
    ! on the odepack user interface standard, with minor modifications.
    !---------------------------------------------------------------------------
    ! authors:
    !               peter n. brown and alan c. hindmarsh
    !               center for applied scientific computing, l-561
    !               lawrence livermore national laboratory
    !               livermore, ca 94551
    ! and
    !               george d. byrne
    !               illinois institute of technology
    !               chicago, il 60616
    !---------------------------------------------------------------------------
    ! references:
    !
    ! 1. p. n. brown, g. d. byrne, and a. c. hindmarsh, "vode: a variable
    !    coefficient ode solver," siam j. sci. stat. comput., 10 (1989),
    !    pp. 1038-1051.  also, llnl report ucrl-98412, june 1988.
    ! 2. g. d. byrne and a. c. hindmarsh, "a polyalgorithm for the
    !    numerical solution of ordinary differential equations,"
    !    acm trans. math. software, 1 (1975), pp. 71-96.
    ! 3. a. c. hindmarsh and g. d. byrne, "episode: an effective package
    !    for the integration of systems of ordinary differential
    !    equations," llnl report ucid-30112, rev. 1, april 1977.
    ! 4. g. d. byrne and a. c. hindmarsh, "episodeb: an experimental
    !    package for the integration of systems of ordinary differential
    !    equations with banded jacobians," llnl report ucid-30132, april
    !    1976.
    ! 5. a. c. hindmarsh, "odepack, a systematized collection of ode
    !    solvers," in scientific computing, r. s. stepleman et al., eds.,
    !    north-holland, amsterdam, 1983, pp. 55-64.
    ! 6. k. r. jackson and r. sacks-davis, "an alternative implementation
    !    of variable step-size multistep formulas for stiff odes," acm
    !    trans. math. software, 6 (1980), pp. 295-318.
    !---------------------------------------------------------------------------
    ! summary of usage.
    !
    ! communication between the user and the dvode package, for normal
    ! situations, is summarized here.  this summary describes only a subset
    ! of the full set of options available.  see the full description for
    ! details, including optional communication, nonstandard options,
    ! and instructions for special situations.  see also the example
    ! problem (with program and output) following this summary.
    !
    ! a. first provide a subroutine of the form:
    !           subroutine f (neq, t, y, ydot, rpar, ipar)
    !           double precision t, y(neq), ydot(neq), rpar
    ! which supplies the vector function f by loading ydot(i) with f(i).
    !
    ! b. next determine (or guess) whether or not the problem is stiff.
    ! stiffness occurs when the jacobian matrix df/dy has an eigenvalue
    ! whose real part is negative and large in magnitude, compared to the
    ! reciprocal of the t span of interest.  if the problem is nonstiff,
    ! use a method flag mf = 10.  if it is stiff, there are four standard
    ! choices for mf (21, 22, 24, 25), and dvode requires the jacobian
    ! matrix in some form.  in these cases (mf > 0), dvode will use a
    ! saved copy of the jacobian matrix.  if this is undesirable because of
    ! storage limitations, set mf to the corresponding negative value
    ! (-21, -22, -24, -25).  (see full description of mf below.)
    ! the jacobian matrix is regarded either as full (mf = 21 or 22),
    ! or banded (mf = 24 or 25).  in the banded case, dvode requires two
    ! half-bandwidth parameters ml and mu.  these are, respectively, the
    ! widths of the lower and upper parts of the band, excluding the main
    ! diagonal.  thus the band consists of the locations (i,j) with
    ! i-ml <= j <= i+mu, and the full bandwidth is ml+mu+1.
    !
    ! c. if the problem is stiff, you are encouraged to supply the jacobian
    ! directly (mf = 21 or 24), but if this is not feasible, dvode will
    ! compute it internally by difference quotients (mf = 22 or 25).
    ! if you are supplying the jacobian, provide a subroutine of the form:
    !           subroutine jac (neq, t, y, ml, mu, pd, nrowpd, rpar, ipar)
    !           double precision t, y(neq), pd(nrowpd,neq), rpar
    ! which supplies df/dy by loading pd as follows:
    !     for a full jacobian (mf = 21), load pd(i,j) with df(i)/dy(j),
    ! the partial derivative of f(i) with respect to y(j).  (ignore the
    ! ml and mu arguments in this case.)
    !     for a banded jacobian (mf = 24), load pd(i-j+mu+1,j) with
    ! df(i)/dy(j), i.e. load the diagonal lines of df/dy into the rows of
    ! pd from the top down.
    !     in either case, only nonzero elements need be loaded.
    !
    ! d. write a main program which calls subroutine dvode once for
    ! each point at which answers are desired.  this should also provide
    ! for possible use of logical unit 6 for output of error messages
    ! by dvode.  on the first call to dvode, supply arguments as follows:
    ! f      = name of subroutine for right-hand side vector f.
    !          this name must be declared external in calling program.
    ! neq    = number of first order odes.
    ! y      = array of initial values, of length neq.
    ! t      = the initial value of the independent variable.
    ! tout   = first point where output is desired (.ne. t).
    ! itol   = 1 or 2 according as atol (below) is a scalar or array.
    ! rtol   = relative tolerance parameter (scalar).
    ! atol   = absolute tolerance parameter (scalar or array).
    !          the estimated local error in y(i) will be controlled so as
    !          to be roughly less (in magnitude) than
    !             ewt(i) = rtol*ABS(y(i)) + atol     if itol = 1, or
    !             ewt(i) = rtol*ABS(y(i)) + atol(i)  if itol = 2.
    !          thus the local error test passes if, in each component,
    !          either the absolute error is less than atol (or atol(i)),
    !          or the relative error is less than rtol.
    !          use rtol = 0.0 for pure absolute error control, and
    !          use atol = 0.0 (or atol(i) = 0.0) for pure relative error
    !          control.  caution: actual (global) errors may exceed these
    !          local tolerances, so choose them conservatively.
    ! itask  = 1 for normal computation of output values of y at t = tout.
    ! istate = integer flag (input and output).  set istate = 1.
    ! iopt   = 0 to indicate no optional input used.
    ! rwork  = real work array of length at least:
    !             20 + 16*neq                      for mf = 10,
    !             22 +  9*neq + 2*neq**2           for mf = 21 or 22,
    !             22 + 11*neq + (3*ml + 2*mu)*neq  for mf = 24 or 25.
    ! lrw    = declared length of rwork (in user's dimension statement).
    ! iwork  = integer work array of length at least:
    !             30        for mf = 10,
    !             30 + neq  for mf = 21, 22, 24, or 25.
    !          if mf = 24 or 25, input in iwork(1),iwork(2) the lower
    !          and upper half-bandwidths ml,mu.
    ! liw    = declared length of iwork (in user's dimension statement).
    ! jac    = name of subroutine for jacobian matrix (mf = 21 or 24).
    !          if used, this name must be declared external in calling
    !          program.  if not used, pass a dummy name.
    ! mf     = method flag.  standard values are:
    !          10 for nonstiff (adams) method, no jacobian used.
    !          21 for stiff (bdf) method, user-supplied full jacobian.
    !          22 for stiff method, internally generated full jacobian.
    !          24 for stiff method, user-supplied banded jacobian.
    !          25 for stiff method, internally generated banded jacobian.
    ! rpar,ipar = user-defined real and integer arrays passed to f and jac.
    ! note that the main program must declare arrays y, rwork, iwork,
    ! and possibly atol, rpar, and ipar.
    !
    ! e. the output from the first call (or any call) is:
    !      y = array of computed values of y(t) vector.
    !      t = corresponding value of independent variable (normally tout).
    ! istate = 2  if dvode was successful, negative otherwise.
    !          -1 means excess work done on this call. (perhaps wrong mf.)
    !          -2 means excess accuracy requested. (tolerances too small.)
    !          -3 means illegal input detected. (see printed message.)
    !          -4 means repeated error test failures. (check all input.)
    !          -5 means repeated convergence failures. (perhaps bad
    !             jacobian supplied or wrong choice of mf or tolerances.)
    !          -6 means error weight became zero during problem. (solution
    !             component i vanished, and atol or atol(i) = 0.)
    !
    ! f. to continue the integration after a successful return, simply
    ! reset tout and call dvode again.  no other parameters need be reset.
    !
    !---------------------------------------------------------------------------
    ! example problem
    !
    ! the following is a simple example problem, with the coding
    ! needed for its solution by dvode.  the problem is from chemical
    ! kinetics, and consists of the following three rate equations:
    !     dy1/dt = -.04*y1 + 1.e4*y2*y3
    !     dy2/dt = .04*y1 - 1.e4*y2*y3 - 3.e7*y2**2
    !     dy3/dt = 3.e7*y2**2
    ! on the interval from t = 0.0 to t = 4.e10, with initial conditions
    ! y1 = 1.0, y2 = y3 = 0.  the problem is stiff.
    !
    ! the following coding solves this problem with dvode, using mf = 21
    ! and printing results at t = .4, 4., ..., 4.e10.  it uses
    ! itol = 2 and atol much smaller for y2 than y1 or y3 because
    ! y2 has much smaller values.
    ! at the end of the run, statistical quantities of interest are
    ! printed. (see optional output in the full description below.)
    ! to generate fortran source code, replace c in column 1 with a blank
    ! in the coding below.
    !
    !     external fex, jex
    !     double precision atol, rpar, rtol, rwork, t, tout, y
    !     dimension y(3), atol(3), rwork(67), iwork(33)
    !     neq = 3
    !     y(1) = 1.0d0
    !     y(2) = 0.0d0
    !     y(3) = 0.0d0
    !     t = 0.0d0
    !     tout = 0.4d0
    !     itol = 2
    !     rtol = 1.d-4
    !     atol(1) = 1.d-8
    !     atol(2) = 1.d-14
    !     atol(3) = 1.d-6
    !     itask = 1
    !     istate = 1
    !     iopt = 0
    !     lrw = 67
    !     liw = 33
    !     mf = 21
    !     do 40 iout = 1,12
    !       call dvode(fex,neq,y,t,tout,itol,rtol,atol,itask,istate,
    !    1            iopt,rwork,lrw,iwork,liw,jex,mf,rpar,ipar)
    !       write(6,20)t,y(1),y(2),y(3)
    ! 20    format(' at t =',d12.4,'   y =',3d14.6)
    !       if (istate < 0) go to 80
    ! 40    tout = tout*10.
    !     write(6,60) iwork(11),iwork(12),iwork(13),iwork(19),
    !    1            iwork(20),iwork(21),iwork(22)
    ! 60  format(/' no. steps =',i4,'   no. f-s =',i4,
    !    1       '   no. j-s =',i4,'   no. lu-s =',i4/
    !    2       '  no. nonlinear iterations =',i4/
    !    3       '  no. nonlinear convergence failures =',i4/
    !    4       '  no. error test failures =',i4/)
    !     stop
    ! 80  write(6,90)istate
    ! 90  format(///' error halt: istate =',i3)
    !     stop
    !     end
    !
    !     subroutine fex (neq, t, y, ydot, rpar, ipar)
    !     double precision rpar, t, y, ydot
    !     dimension y(neq), ydot(neq)
    !     ydot(1) = -.04d0*y(1) + 1.d4*y(2)*y(3)
    !     ydot(3) = 3.d7*y(2)*y(2)
    !     ydot(2) = -ydot(1) - ydot(3)
    !     return
    !     end
    !
    !     subroutine jex (neq, t, y, ml, mu, pd, nrpd, rpar, ipar)
    !     double precision pd, rpar, t, y
    !     dimension y(neq), pd(nrpd,neq)
    !     pd(1,1) = -.04d0
    !     pd(1,2) = 1.d4*y(3)
    !     pd(1,3) = 1.d4*y(2)
    !     pd(2,1) = .04d0
    !     pd(2,3) = -pd(1,3)
    !     pd(3,2) = 6.d7*y(2)
    !     pd(2,2) = -pd(1,2) - pd(3,2)
    !     return
    !     end
    !
    ! the following output was obtained from the above program on a
    ! cray-1 computer with the cft compiler.
    !
    ! at t =  4.0000e-01   y =  9.851680e-01  3.386314e-05  1.479817e-02
    ! at t =  4.0000e+00   y =  9.055255e-01  2.240539e-05  9.445214e-02
    ! at t =  4.0000e+01   y =  7.158108e-01  9.184883e-06  2.841800e-01
    ! at t =  4.0000e+02   y =  4.505032e-01  3.222940e-06  5.494936e-01
    ! at t =  4.0000e+03   y =  1.832053e-01  8.942690e-07  8.167938e-01
    ! at t =  4.0000e+04   y =  3.898560e-02  1.621875e-07  9.610142e-01
    ! at t =  4.0000e+05   y =  4.935882e-03  1.984013e-08  9.950641e-01
    ! at t =  4.0000e+06   y =  5.166183e-04  2.067528e-09  9.994834e-01
    ! at t =  4.0000e+07   y =  5.201214e-05  2.080593e-10  9.999480e-01
    ! at t =  4.0000e+08   y =  5.213149e-06  2.085271e-11  9.999948e-01
    ! at t =  4.0000e+09   y =  5.183495e-07  2.073399e-12  9.999995e-01
    ! at t =  4.0000e+10   y =  5.450996e-08  2.180399e-13  9.999999e-01
    !
    ! no. steps = 595   no. f-s = 832   no. j-s =  13   no. lu-s = 112
    !  no. nonlinear iterations = 831
    !  no. nonlinear convergence failures =   0
    !  no. error test failures =  22
    !---------------------------------------------------------------------------
    ! full description of user interface to dvode.
    !
    ! the user interface to dvode consists of the following parts.
    !
    ! i.   the call sequence to subroutine dvode, which is a driver
    !      routine for the solver.  this includes descriptions of both
    !      the call sequence arguments and of user-supplied routines.
    !      following these descriptions is
    !        * a description of optional input available through the
    !          call sequence,
    !        * a description of optional output (in the work arrays), and
    !        * instructions for interrupting and restarting a solution.
    !
    ! ii.  descriptions of other routines in the dvode package that may be
    !      (optionally) called by the user.  these provide the ability to
    !      alter error message handling, save and restore the internal
    !      common, and obtain specified derivatives of the solution y(t).
    !
    ! iii. descriptions of common blocks to be declared in overlay
    !      or similar environments.
    !
    ! iv.  description of two routines in the dvode package, either of
    !      which the user may replace with his own version, if desired.
    !      these relate to the measurement of errors.
    !
    !---------------------------------------------------------------------------
    ! part i.  call sequence.
    !
    ! the call sequence parameters used for input only are
    !     f, neq, tout, itol, rtol, atol, itask, iopt, lrw, liw, jac, mf,
    ! and those used for both input and output are
    !     y, t, istate.
    ! the work arrays rwork and iwork are also used for conditional and
    ! optional input and optional output.  (the term output here refers
    ! to the return from subroutine dvode to the user's calling program.)
    !
    ! the legality of input parameters will be thoroughly checked on the
    ! initial call for the problem, but not checked thereafter unless a
    ! change in input parameters is flagged by istate = 3 in the input.
    !
    ! the descriptions of the call arguments are as follows.
    !
    ! f      = the name of the user-supplied subroutine defining the
    !          ode system.  the system must be put in the first-order
    !          form dy/dt = f(t,y), where f is a vector-valued function
    !          of the scalar t and the vector y.  subroutine f is to
    !          compute the function f.  it is to have the form
    !               subroutine f (neq, t, y, ydot, rpar, ipar)
    !               double precision t, y(neq), ydot(neq), rpar
    !          where neq, t, and y are input, and the array ydot = f(t,y)
    !          is output.  y and ydot are arrays of length neq.
    !          subroutine f should not alter y(1),...,y(neq).
    !          f must be declared external in the calling program.
    !
    !          subroutine f may access user-defined real and integer
    !          work arrays rpar and ipar, which are to be dimensioned
    !          in the main program.
    !
    !          if quantities computed in the f routine are needed
    !          externally to dvode, an extra call to f should be made
    !          for this purpose, for consistent and accurate results.
    !          if only the derivative dy/dt is needed, use dvindy instead.
    !
    ! neq    = the size of the ode system (number of first order
    !          ordinary differential equations).  used only for input.
    !          neq may not be increased during the problem, but
    !          can be decreased (with istate = 3 in the input).
    !
    ! y      = a real array for the vector of dependent variables, of
    !          length neq or more.  used for both input and output on the
    !          first call (istate = 1), and only for output on other calls.
    !          on the first call, y must contain the vector of initial
    !          values.  in the output, y contains the computed solution
    !          evaluated at t.  if desired, the y array may be used
    !          for other purposes between calls to the solver.
    !
    !          this array is passed as the y argument in all calls to
    !          f and jac.
    !
    ! t      = the independent variable.  in the input, t is used only on
    !          the first call, as the initial point of the integration.
    !          in the output, after each call, t is the value at which a
    !          computed solution y is evaluated (usually the same as tout).
    !          on an error return, t is the farthest point reached.
    !
    ! tout   = the next value of t at which a computed solution is desired.
    !          used only for input.
    !
    !          when starting the problem (istate = 1), tout may be equal
    !          to t for one call, then should /= t for the next call.
    !          for the initial t, an input value of tout /= t is used
    !          in order to determine the direction of the integration
    !          (i.e. the algebraic SIGN of the step sizes) and the rough
    !          scale of the problem.  integration in either direction
    !          (forward or backward in t) is permitted.
    !
    !          if itask = 2 or 5 (one-step modes), tout is ignored after
    !          the first call (i.e. the first call with tout /= t).
    !          otherwise, tout is required on every call.
    !
    !          if itask = 1, 3, or 4, the values of tout need not be
    !          monotone, but a value of tout which backs up is limited
    !          to the current internal t interval, whose endpoints are
    !          tcur - hu and tcur.  (see optional output, below, for
    !          tcur and hu.)
    !
    ! itol   = an indicator for the type of error control.  see
    !          description below under atol.  used only for input.
    !
    ! rtol   = a relative error tolerance parameter, either a scalar or
    !          an array of length neq.  see description below under atol.
    !          input only.
    !
    ! atol   = an absolute error tolerance parameter, either a scalar or
    !          an array of length neq.  input only.
    !
    !          the input parameters itol, rtol, and atol determine
    !          the error control performed by the solver.  the solver will
    !          control the vector e = (e(i)) of estimated local errors
    !          in y, according to an inequality of the form
    !                      rms-norm of ( e(i)/ewt(i) )   <=   1,
    !          where       ewt(i) = rtol(i)*ABS(y(i)) + atol(i),
    !          and the rms-norm (root-mean-square norm) here is
    !          rms-norm(v) = SQRT(sum v(i)**2 / neq).  here ewt = (ewt(i))
    !          is a vector of weights which must always be positive, and
    !          the values of rtol and atol should all be non-negative.
    !          the following table gives the types (scalar/array) of
    !          rtol and atol, and the corresponding form of ewt(i).
    !
    !             itol    rtol       atol          ewt(i)
    !              1     scalar     scalar     rtol*ABS(y(i)) + atol
    !              2     scalar     array      rtol*ABS(y(i)) + atol(i)
    !              3     array      scalar     rtol(i)*ABS(y(i)) + atol
    !              4     array      array      rtol(i)*ABS(y(i)) + atol(i)
    !
    !          when either of these parameters is a scalar, it need not
    !          be dimensioned in the user's calling program.
    !
    !          if none of the above choices (with itol, rtol, and atol
    !          fixed throughout the problem) is suitable, more general
    !          error controls can be obtained by substituting
    !          user-supplied routines for the setting of ewt and/or for
    !          the norm calculation.  see part iv below.
    !
    !          if global errors are to be estimated by making a repeated
    !          run on the same problem with smaller tolerances, then all
    !          components of rtol and atol (i.e. of ewt) should be scaled
    !          down uniformly.
    !
    ! itask  = an index specifying the task to be performed.
    !          input only.  itask has the following values and meanings.
    !          1  means normal computation of output values of y(t) at
    !             t = tout (by overshooting and interpolating).
    !          2  means take one step only and return.
    !          3  means stop at the first internal mesh point at or
    !             beyond t = tout and return.
    !          4  means normal computation of output values of y(t) at
    !             t = tout but without overshooting t = tcrit.
    !             tcrit must be input as rwork(1).  tcrit may be equal to
    !             or beyond tout, but not behind it in the direction of
    !             integration.  this option is useful if the problem
    !             has a singularity at or beyond t = tcrit.
    !          5  means take one step, without passing tcrit, and return.
    !             tcrit must be input as rwork(1).
    !
    !          note:  if itask = 4 or 5 and the solver reaches tcrit
    !          (within roundoff), it will return t = tcrit (exactly) to
    !          indicate this (unless itask = 4 and tout comes before tcrit,
    !          in which case answers at t = tout are returned first).
    !
    ! istate = an index used for input and output to specify the
    !          the state of the calculation.
    !
    !          in the input, the values of istate are as follows.
    !          1  means this is the first call for the problem
    !             (initializations will be done).  see note below.
    !          2  means this is not the first call, and the calculation
    !             is to continue normally, with no change in any input
    !             parameters except possibly tout and itask.
    !             (if itol, rtol, and/or atol are changed between calls
    !             with istate = 2, the new values will be used but not
    !             tested for legality.)
    !          3  means this is not the first call, and the
    !             calculation is to continue normally, but with
    !             a change in input parameters other than
    !             tout and itask.  changes are allowed in
    !             neq, itol, rtol, atol, iopt, lrw, liw, mf, ml, mu,
    !             and any of the optional input except h0.
    !             (see iwork description for ml and mu.)
    !          note:  a preliminary call with tout = t is not counted
    !          as a first call here, as no initialization or checking of
    !          input is done.  (such a call is sometimes useful to include
    !          the initial conditions in the output.)
    !          thus the first call for which tout /= t requires
    !          istate = 1 in the input.
    !
    !          in the output, istate has the following values and meanings.
    !           1  means nothing was done, as tout was equal to t with
    !              istate = 1 in the input.
    !           2  means the integration was performed successfully.
    !          -1  means an excessive amount of work (more than mxstep
    !              steps) was done on this call, before completing the
    !              requested task, but the integration was otherwise
    !              successful as far as t.  (mxstep is an optional input
    !              and is normally 500.)  to continue, the user may
    !              simply reset istate to a value > 1 and call again.
    !              (the excess work step counter will be reset to 0.)
    !              in addition, the user may increase mxstep to avoid
    !              this error return.  (see optional input below.)
    !          -2  means too much accuracy was requested for the precision
    !              of the machine being used.  this was detected before
    !              completing the requested task, but the integration
    !              was successful as far as t.  to continue, the tolerance
    !              parameters must be reset, and istate must be set
    !              to 3.  the optional output tolsf may be used for this
    !              purpose.  (note: if this condition is detected before
    !              taking any steps, then an illegal input return
    !              (istate = -3) occurs instead.)
    !          -3  means illegal input was detected, before taking any
    !              integration steps.  see written message for details.
    !              note:  if the solver detects an infinite loop of calls
    !              to the solver with illegal input, it will cause
    !              the run to stop.
    !          -4  means there were repeated error test failures on
    !              one attempted step, before completing the requested
    !              task, but the integration was successful as far as t.
    !              the problem may have a singularity, or the input
    !              may be inappropriate.
    !          -5  means there were repeated convergence test failures on
    !              one attempted step, before completing the requested
    !              task, but the integration was successful as far as t.
    !              this may be caused by an inaccurate jacobian matrix,
    !              if one is being used.
    !          -6  means ewt(i) became zero for some i during the
    !              integration.  pure relative error control (atol(i)=0.0)
    !              was requested on a variable which has now vanished.
    !              the integration was successful as far as t.
    !
    !          note:  since the normal output value of istate is 2,
    !          it does not need to be reset for normal continuation.
    !          also, since a negative input value of istate will be
    !          regarded as illegal, a negative output value requires the
    !          user to change it, and possibly other input, before
    !          calling the solver again.
    !
    ! iopt   = an integer flag to specify whether or not any optional
    !          input is being used on this call.  input only.
    !          the optional input is listed separately below.
    !          iopt = 0 means no optional input is being used.
    !                   default values will be used in all cases.
    !          iopt = 1 means optional input is being used.
    !
    ! rwork  = a real working array (double precision).
    !          the length of rwork must be at least
    !             20 + nyh*(maxord + 1) + 3*neq + lwm    where
    !          nyh    = the initial value of neq,
    !          maxord = 12 (if meth = 1) or 5 (if meth = 2) (unless a
    !                   smaller value is given as an optional input),
    !          lwm = length of work space for matrix-related data:
    !          lwm = 0             if miter = 0,
    !          lwm = 2*neq**2 + 2  if miter = 1 or 2, and mf>0,
    !          lwm = neq**2 + 2    if miter = 1 or 2, and mf<0,
    !          lwm = neq + 2       if miter = 3,
    !          lwm = (3*ml+2*mu+2)*neq + 2 if miter = 4 or 5, and mf>0,
    !          lwm = (2*ml+mu+1)*neq + 2   if miter = 4 or 5, and mf<0.
    !          (see the mf description for meth and miter.)
    !          thus if maxord has its default value and neq is constant,
    !          this length is:
    !             20 + 16*neq                    for mf = 10,
    !             22 + 16*neq + 2*neq**2         for mf = 11 or 12,
    !             22 + 16*neq + neq**2           for mf = -11 or -12,
    !             22 + 17*neq                    for mf = 13,
    !             22 + 18*neq + (3*ml+2*mu)*neq  for mf = 14 or 15,
    !             22 + 17*neq + (2*ml+mu)*neq    for mf = -14 or -15,
    !             20 +  9*neq                    for mf = 20,
    !             22 +  9*neq + 2*neq**2         for mf = 21 or 22,
    !             22 +  9*neq + neq**2           for mf = -21 or -22,
    !             22 + 10*neq                    for mf = 23,
    !             22 + 11*neq + (3*ml+2*mu)*neq  for mf = 24 or 25.
    !             22 + 10*neq + (2*ml+mu)*neq    for mf = -24 or -25.
    !          the first 20 words of rwork are reserved for conditional
    !          and optional input and optional output.
    !
    !          the following word in rwork is a conditional input:
    !            rwork(1) = tcrit = critical value of t which the solver
    !                       is not to overshoot.  required if itask is
    !                       4 or 5, and ignored otherwise.  (see itask.)
    !
    ! lrw    = the length of the array rwork, as declared by the user.
    !          (this will be checked by the solver.)
    !
    ! iwork  = an integer work array.  the length of iwork must be at least
    !             30        if miter = 0 or 3 (mf = 10, 13, 20, 23), or
    !             30 + neq  otherwise (ABS(mf) = 11,12,14,15,21,22,24,25).
    !          the first 30 words of iwork are reserved for conditional and
    !          optional input and optional output.
    !
    !          the following 2 words in iwork are conditional input:
    !            iwork(1) = ml     these are the lower and upper
    !            iwork(2) = mu     half-bandwidths, respectively, of the
    !                       banded jacobian, excluding the main diagonal.
    !                       the band is defined by the matrix locations
    !                       (i,j) with i-ml <= j <= i+mu.  ml and mu
    !                       must satisfy  0 <=  ml,mu  <= neq-1.
    !                       these are required if miter is 4 or 5, and
    !                       ignored otherwise.  ml and mu may in fact be
    !                       the band parameters for a matrix to which
    !                       df/dy is only approximately equal.
    !
    ! liw    = the length of the array iwork, as declared by the user.
    !          (this will be checked by the solver.)
    !
    ! note:  the work arrays must not be altered between calls to dvode
    ! for the same problem, except possibly for the conditional and
    ! optional input, and except for the last 3*neq words of rwork.
    ! the latter space is used for internal scratch space, and so is
    ! available for use by the user outside dvode between calls, if
    ! desired (but not for use by f or jac).
    !
    ! jac    = the name of the user-supplied routine (miter = 1 or 4) to
    !          compute the jacobian matrix, df/dy, as a function of
    !          the scalar t and the vector y.  it is to have the form
    !               subroutine jac (neq, t, y, ml, mu, pd, nrowpd,
    !                               rpar, ipar)
    !               double precision t, y(neq), pd(nrowpd,neq), rpar
    !          where neq, t, y, ml, mu, and nrowpd are input and the array
    !          pd is to be loaded with partial derivatives (elements of the
    !          jacobian matrix) in the output.  pd must be given a first
    !          dimension of nrowpd.  t and y have the same meaning as in
    !          subroutine f.
    !               in the full matrix case (miter = 1), ml and mu are
    !          ignored, and the jacobian is to be loaded into pd in
    !          columnwise manner, with df(i)/dy(j) loaded into pd(i,j).
    !               in the band matrix case (miter = 4), the elements
    !          within the band are to be loaded into pd in columnwise
    !          manner, with diagonal lines of df/dy loaded into the rows
    !          of pd. thus df(i)/dy(j) is to be loaded into pd(i-j+mu+1,j).
    !          ml and mu are the half-bandwidth parameters. (see iwork).
    !          the locations in pd in the two triangular areas which
    !          correspond to nonexistent matrix elements can be ignored
    !          or loaded arbitrarily, as they are overwritten by dvode.
    !               jac need not provide df/dy exactly.  a crude
    !          approximation (possibly with a smaller bandwidth) will do.
    !               in either case, pd is preset to zero by the solver,
    !          so that only the nonzero elements need be loaded by jac.
    !          each call to jac is preceded by a call to f with the same
    !          arguments neq, t, and y.  thus to gain some efficiency,
    !          intermediate quantities shared by both calculations may be
    !          saved in a user common block by f and not recomputed by jac,
    !          if desired.  also, jac may alter the y array, if desired.
    !          jac must be declared external in the calling program.
    !               subroutine jac may access user-defined real and integer
    !          work arrays, rpar and ipar, whose dimensions are set by the
    !          user in the main program.
    !
    ! mf     = the method flag.  used only for input.  the legal values of
    !          mf are 10, 11, 12, 13, 14, 15, 20, 21, 22, 23, 24, 25,
    !          -11, -12, -14, -15, -21, -22, -24, -25.
    !          mf is a signed two-digit integer, mf = jsv*(10*meth + miter).
    !          jsv = SIGN(mf) indicates the jacobian-saving strategy:
    !            jsv =  1 means a copy of the jacobian is saved for reuse
    !                     in the corrector iteration algorithm.
    !            jsv = -1 means a copy of the jacobian is not saved
    !                     (valid only for miter = 1, 2, 4, or 5).
    !          meth indicates the basic linear multistep method:
    !            meth = 1 means the implicit adams method.
    !            meth = 2 means the method based on backward
    !                     differentiation formulas (bdf-s).
    !          miter indicates the corrector iteration method:
    !            miter = 0 means functional iteration (no jacobian matrix
    !                      is involved).
    !            miter = 1 means chord iteration with a user-supplied
    !                      full (neq by neq) jacobian.
    !            miter = 2 means chord iteration with an internally
    !                      generated (difference quotient) full jacobian
    !                      (using neq extra calls to f per df/dy value).
    !            miter = 3 means chord iteration with an internally
    !                      generated diagonal jacobian approximation
    !                      (using 1 extra call to f per df/dy evaluation).
    !            miter = 4 means chord iteration with a user-supplied
    !                      banded jacobian.
    !            miter = 5 means chord iteration with an internally
    !                      generated banded jacobian (using ml+mu+1 extra
    !                      calls to f per df/dy evaluation).
    !          if miter = 1 or 4, the user must supply a subroutine jac
    !          (the name is arbitrary) as described above under jac.
    !          for other values of miter, a dummy argument can be used.
    !
    ! rpar     user-specified array used to communicate real parameters
    !          to user-supplied subroutines.  if rpar is a vector, then
    !          it must be dimensioned in the user's main program.  if it
    !          is unused or it is a scalar, then it need not be
    !          dimensioned.
    !
    ! ipar     user-specified array used to communicate integer parameter
    !          to user-supplied subroutines.  the comments on dimensioning
    !          rpar apply to ipar.
    !---------------------------------------------------------------------------
    ! optional input.
    !
    ! the following is a list of the optional input provided for in the
    ! call sequence.  (see also part ii.)  for each such input variable,
    ! this table lists its name as used in this documentation, its
    ! location in the call sequence, its meaning, and the default value.
    ! the use of any of this input requires iopt = 1, and in that
    ! case all of this input is examined.  a value of zero for any
    ! of these optional input variables will cause the default value to be
    ! used.  thus to use a subset of the optional input, simply preload
    ! locations 5 to 10 in rwork and iwork to 0.0 and 0 respectively, and
    ! then set those of interest to nonzero values.
    !
    ! name    location      meaning and default value
    !
    ! h0      rwork(5)  the step size to be attempted on the first step.
    !                   the default value is determined by the solver.
    !
    ! hmax    rwork(6)  the maximum absolute step size allowed.
    !                   the default value is infinite.
    !
    ! hmin    rwork(7)  the minimum absolute step size allowed.
    !                   the default value is 0.  (this lower bound is not
    !                   enforced on the final step before reaching tcrit
    !                   when itask = 4 or 5.)
    !
    ! maxord  iwork(5)  the maximum order to be allowed.  the default
    !                   value is 12 if meth = 1, and 5 if meth = 2.
    !                   if maxord exceeds the default value, it will
    !                   be reduced to the default value.
    !                   if maxord is changed during the problem, it may
    !                   cause the current order to be reduced.
    !
    ! mxstep  iwork(6)  maximum number of (internally defined) steps
    !                   allowed during one call to the solver.
    !                   the default value is 500.
    !
    ! mxhnil  iwork(7)  maximum number of messages printed (per problem)
    !                   warning that t + h = t on a step (h = step size).
    !                   this must be positive to result in a non-default
    !                   value.  the default value is 10.
    !
    !---------------------------------------------------------------------------
    ! optional output.
    !
    ! as optional additional output from dvode, the variables listed
    ! below are quantities related to the performance of dvode
    ! which are available to the user.  these are communicated by way of
    ! the work arrays, but also have internal mnemonic names as shown.
    ! except where stated otherwise, all of this output is defined
    ! on any successful return from dvode, and on any return with
    ! istate = -1, -2, -4, -5, or -6.  on an illegal input return
    ! (istate = -3), they will be unchanged from their existing values
    ! (if any), except possibly for tolsf, lenrw, and leniw.
    ! on any error return, output relevant to the error will be defined,
    ! as noted below.
    !
    ! name    location      meaning
    !
    ! hu      rwork(11) the step size in t last used (successfully).
    !
    ! hcur    rwork(12) the step size to be attempted on the next step.
    !
    ! tcur    rwork(13) the current value of the independent variable
    !                   which the solver has actually reached, i.e. the
    !                   current internal mesh point in t.  in the output,
    !                   tcur will always be at least as far from the
    !                   initial value of t as the current argument t,
    !                   but may be farther (if interpolation was done).
    !
    ! tolsf   rwork(14) a tolerance scale factor, greater than 1.0,
    !                   computed when a request for too much accuracy was
    !                   detected (istate = -3 if detected at the start of
    !                   the problem, istate = -2 otherwise).  if itol is
    !                   left unaltered but rtol and atol are uniformly
    !                   scaled up by a factor of tolsf for the next call,
    !                   then the solver is deemed likely to succeed.
    !                   (the user may also ignore tolsf and alter the
    !                   tolerance parameters in any other way appropriate.)
    !
    ! nst     iwork(11) the number of steps taken for the problem so far.
    !
    ! nfe     iwork(12) the number of f evaluations for the problem so far.
    !
    ! nje     iwork(13) the number of jacobian evaluations so far.
    !
    ! nqu     iwork(14) the method order last used (successfully).
    !
    ! nqcur   iwork(15) the order to be attempted on the next step.
    !
    ! imxer   iwork(16) the index of the component of largest magnitude in
    !                   the weighted local error vector ( e(i)/ewt(i) ),
    !                   on an error return with istate = -4 or -5.
    !
    ! lenrw   iwork(17) the length of rwork actually required.
    !                   this is defined on normal returns and on an illegal
    !                   input return for insufficient storage.
    !
    ! leniw   iwork(18) the length of iwork actually required.
    !                   this is defined on normal returns and on an illegal
    !                   input return for insufficient storage.
    !
    ! nlu     iwork(19) the number of matrix lu decompositions so far.
    !
    ! nni     iwork(20) the number of nonlinear (newton) iterations so far.
    !
    ! ncfn    iwork(21) the number of convergence failures of the nonlinear
    !                   solver so far.
    !
    ! netf    iwork(22) the number of error test failures of the integrator
    !                   so far.
    !
    ! the following two arrays are segments of the rwork array which
    ! may also be of interest to the user as optional output.
    ! for each array, the table below gives its internal name,
    ! its base address in rwork, and its description.
    !
    ! name    base address      description
    !
    ! yh      21             the nordsieck history array, of size nyh by
    !                        (nqcur + 1), where nyh is the initial value
    !                        of neq.  for j = 0,1,...,nqcur, column j+1
    !                        of yh contains hcur**j/factorial(j) times
    !                        the j-th derivative of the interpolating
    !                        polynomial currently representing the
    !                        solution, evaluated at t = tcur.
    !
    ! acor     lenrw-neq+1   array of size neq used for the accumulated
    !                        corrections on each step, scaled in the output
    !                        to represent the estimated local error in y
    !                        on the last step.  this is the vector e in
    !                        the description of the error control.  it is
    !                        defined only on a successful return from dvode.
    !
    !---------------------------------------------------------------------------
    ! interrupting and restarting
    !
    ! if the integration of a given problem by dvode is to be
    ! interrrupted and then later continued, such as when restarting
    ! an interrupted run or alternating between two or more ode problems,
    ! the user should save, following the return from the last dvode call
    ! prior to the interruption, the contents of the call sequence
    ! variables and internal common blocks, and later restore these
    ! values before the next dvode call for that problem.  to save
    ! and restore the common blocks, use subroutine dvsrco, as
    ! described below in part ii.
    !
    ! in addition, if non-default values for either lun or mflag are
    ! desired, an extra call to xsetun and/or xsetf should be made just
    ! before continuing the integration.  see part ii below for details.
    !
    !---------------------------------------------------------------------------
    ! part ii.  other routines callable.
    !
    ! the following are optional calls which the user may make to
    ! gain additional capabilities in conjunction with dvode.
    ! (the routines xsetun and xsetf are designed to conform to the
    ! slatec error handling package.)
    !
    !     form of call                  function
    !  call xsetun(lun)           set the logical unit number, lun, for
    !                             output of messages from dvode, if
    !                             the default is not desired.
    !                             the default value of lun is 6.
    !
    !  call xsetf(mflag)          set a flag to control the printing of
    !                             messages by dvode.
    !                             mflag = 0 means do not print. (danger:
    !                             this risks losing valuable information.)
    !                             mflag = 1 means print (the default).
    !
    !                             either of the above calls may be made at
    !                             any time and will take effect immediately.
    !
    !  call dvsrco(rsav,isav,job) saves and restores the contents of
    !                             the internal common blocks used by
    !                             dvode. (see part iii below.)
    !                             rsav must be a real array of length 49
    !                             or more, and isav must be an integer
    !                             array of length 40 or more.
    !                             job=1 means save common into rsav/isav.
    !                             job=2 means restore common from rsav/isav.
    !                                dvsrco is useful if one is
    !                             interrupting a run and restarting
    !                             later, or alternating between two or
    !                             more problems solved with dvode.
    !
    !  call dvindy(,,,,,)         provide derivatives of y, of various
    !        (see below.)         orders, at a specified point t, if
    !                             desired.  it may be called only after
    !                             a successful return from dvode.
    !
    ! the detailed instructions for using dvindy are as follows.
    ! the form of the call is:
    !
    !  call dvindy (t, k, rwork(21), nyh, dky, iflag)
    !
    ! the input parameters are:
    !
    ! t         = value of independent variable where answers are desired
    !             (normally the same as the t last returned by dvode).
    !             for valid results, t must lie between tcur - hu and tcur.
    !             (see optional output for tcur and hu.)
    ! k         = integer order of the derivative desired.  k must satisfy
    !             0 <= k <= nqcur, where nqcur is the current order
    !             (see optional output).  the capability corresponding
    !             to k = 0, i.e. computing y(t), is already provided
    !             by dvode directly.  since nqcur >= 1, the first
    !             derivative dy/dt is always available with dvindy.
    ! rwork(21) = the base address of the history array yh.
    ! nyh       = column length of yh, equal to the initial value of neq.
    !
    ! the output parameters are:
    !
    ! dky       = a real array of length neq containing the computed value
    !             of the k-th derivative of y(t).
    ! iflag     = integer flag, returned as 0 if k and t were legal,
    !             -1 if k was illegal, and -2 if t was illegal.
    !             on an error return, a message is also written.
    !---------------------------------------------------------------------------
    ! part iii.  common blocks.
    ! if dvode is to be used in an overlay situation, the user
    ! must declare, in the primary overlay, the variables in:
    !   (1) the call sequence to dvode,
    !   (2) the two internal common blocks
    !         /dvod01/  of length  81  (48 double precision words
    !                         followed by 33 integer words),
    !         /dvod02/  of length  9  (1 double precision word
    !                         followed by 8 integer words),
    !
    ! if dvode is used on a system in which the contents of internal
    ! common blocks are not preserved between calls, the user should
    ! declare the above two common blocks in his main program to insure
    ! that their contents are preserved.
    !
    !---------------------------------------------------------------------------
    ! part iv.  optionally replaceable solver routines.
    !
    ! below are descriptions of two routines in the dvode package which
    ! relate to the measurement of errors.  either routine can be
    ! replaced by a user-supplied version, if desired.  however, since such
    ! a replacement may have a major impact on performance, it should be
    ! done only when absolutely necessary, and only with great caution.
    ! (note: the means by which the package version of a routine is
    ! superseded by the user's version may be system-dependent.)
    !
    ! (a) dewset.
    ! the following subroutine is called just before each internal
    ! integration step, and sets the array of error weights, ewt, as
    ! described under itol/rtol/atol above:
    !     subroutine dewset (neq, itol, rtol, atol, ycur, ewt)
    ! where neq, itol, rtol, and atol are as in the dvode call sequence,
    ! ycur contains the current dependent variable vector, and
    ! ewt is the array of weights set by dewset.
    !
    ! if the user supplies this subroutine, it must return in ewt(i)
    ! (i = 1,...,neq) a positive quantity suitable for comparison with
    ! errors in y(i).  the ewt array returned by dewset is passed to the
    ! dvnorm routine (see below.), and also used by dvode in the computation
    ! of the optional output imxer, the diagonal jacobian approximation,
    ! and the increments for difference quotient jacobians.
    !
    ! in the user-supplied version of dewset, it may be desirable to use
    ! the current values of derivatives of y.  derivatives up to order nq
    ! are available from the history array yh, described above under
    ! optional output.  in dewset, yh is identical to the ycur array,
    ! extended to nq + 1 columns with a column length of nyh and scale
    ! factors of h**j/factorial(j).  on the first call for the problem,
    ! given by nst = 0, nq is 1 and h is temporarily set to 1.0.
    ! nyh is the initial value of neq.  the quantities nq, h, and nst
    ! can be obtained by including in dewset the statements:
    !     double precision rvod, h, hu
    !     common /dvod01/ rvod(48), ivod(33)
    !     common /dvod02/ hu, ncfn, netf, nfe, nje, nlu, nni, nqu, nst
    !     nq = ivod(28)
    !     h = rvod(21)
    ! thus, for example, the current value of dy/dt can be obtained as
    ! ycur(nyh+i)/h  (i=1,...,neq)  (and the division by h is
    ! unnecessary when nst = 0).
    !
    ! (b) dvnorm.
    ! the following is a real function routine which computes the weighted
    ! root-mean-square norm of a vector v:
    !     d = dvnorm (n, v, w)
    ! where:
    !   n = the length of the vector,
    !   v = real array of length n containing the vector,
    !   w = real array of length n containing weights,
    !   d = SQRT( (1/n) * sum(v(i)*w(i))**2 ).
    ! dvnorm is called with n = neq and with w(i) = 1.0/ewt(i), where
    ! ewt is as set by subroutine dewset.
    !
    ! if the user supplies this function, it should return a non-negative
    ! value of dvnorm suitable for use in the error control in dvode.
    ! none of the arguments should be altered by dvnorm.
    ! for example, a user-supplied dvnorm routine might:
    !   -substitute a MAX-norm of (v(i)*w(i)) for the rms-norm, or
    !   -ignore some components of v in the norm, with the effect of
    !    suppressing the error control on those components of y.
    !---------------------------------------------------------------------------
    ! revision history (yyyymmdd)
    !  19890615  date written.  initial release.
    !  19890922  added interrupt/restart ability, minor changes throughout.
    !  19910228  minor revisions in line format,  prologue, etc.
    !  19920227  modifications by d. pang:
    !            (1) applied subgennam to get generic intrinsic names.
    !            (2) changed intrinsic names to generic in comments.
    !            (3) added *deck lines before each routine.
    !  19920721  names of routines and labeled common blocks changed, so as
    !            to be unique in combined single/double precision code (ach).
    !  19920722  minor revisions to prologue (ach).
    !  19920831  conversion to double precision done (ach).
    !  19921106  fixed minor bug: etaq,etaqm1 in dvstep save statement (ach).
    !  19921118  changed lunsav/mflgsv to ixsav (ach).
    !  19941222  removed mf overwrite; attached SIGN to h in estimated second 
    !            deriv. in dvhin; misc. comment changes throughout (ach).
    !  19970515  minor corrections to comments in prologue, dvjac (ach).
    !  19981111  corrected block b by adding final line, go to 200 (ach).
    !  20020430  various upgrades (ach): use odepack error handler package.
    !            replaced d1mach by dumach.  various changes to main
    !            prologue and other routine prologues.
    !---------------------------------------------------------------------------
    ! other routines in the dvode package.
    !
    ! in addition to subroutine dvode, the dvode package includes the
    ! following subroutines and function routines:
    !  dvhin     computes an approximate step size for the initial step.
    !  dvindy    computes an interpolated value of the y vector at t = tout.
    !  dvstep    is the core integrator, which does one step of the
    !            integration and the associated error control.
    !  dvset     sets all method coefficients and test constants.
    !  dvnlsd    solves the underlying nonlinear system -- the corrector.
    !  dvjac     computes and preprocesses the jacobian matrix j = df/dy
    !            and the newton iteration matrix p = i - (h/l1)*j.
    !  dvsol     manages solution of linear system in chord iteration.
    !  dvjust    adjusts the history array on a change of order.
    !  dewset    sets the error weight vector ewt before each step.
    !  dvnorm    computes the weighted r.m.s. norm of a vector.
    !  dvsrco    is a user-callable routine to save and restore
    !            the contents of the internal common blocks.
    !  dacopy    is a routine to copy one two-dimensional array to another.
    !  dgefa and dgesl   are routines from linpack for solving full
    !            systems of linear algebraic equations.
    !  dgbfa and dgbsl   are routines from linpack for solving banded
    !            linear systems.
    !  daxpy, dscal, and dcopy are basic linear algebra modules (blas).
    !  dumach    sets the unit roundoff of the machine.
    !  xerrwd, xsetun, xsetf, ixsav, and iumach handle the printing of all
    !            error messages and warnings.  xerrwd is machine-dependent.
    ! note:  dvnorm, dumach, ixsav, and iumach are function routines.
    ! all the others are subroutines.
    !===============================================================================================
    subroutine dvode (f, neq, y, t, tout, itol, rtol, atol, itask, istate, iopt, rwork, lrw, iwork, liw, jac, mf, rpar, ipar)
    
        external f, jac
        double precision y, t, tout, rtol, atol, rwork, rpar
        integer neq, itol, itask, istate, iopt, lrw, iwork, liw, mf, ipar
        dimension y(*), rtol(*), atol(*), rwork(lrw), iwork(liw), rpar(*), ipar(*)
        
        ! type declarations for labeled common block dvod01 
        double precision acnrm, ccmxj, conp, crate, drc, el, eta, etamax, h, hmin, hmxi, hnew, hscal, prl1, rc, rl1, tau, tq, tn, uround
        integer icf, init, ipup, jcur, jstart, jsv, kflag, kuth, l, lmax, lyh, lewt, lacor, lsavf, lwm, liwm, locjs, maxord, meth, miter, msbj, mxhnil, mxstep,     &
            &   n, newh, newq, nhnil, nq, nqnyh, nqwait, nslj, nslp, nyh
        
        ! type declarations for labeled common block dvod02 
        double precision hu
        integer ncfn, netf, nfe, nje, nlu, nni, nqu, nst
        
        ! type declarations for local variables 
!        external dvnlsd
        logical ihit
        double precision atoli, big, ewti, four, h0, hmax, hmx, hun, one, pt2, rh, rtoli, size, tcrit, tnext, tolsf, tp, two, zero
        integer i, ier, iflag, imxer, jco, kgo, leniw, lenj, lenp, lenrw, lenwm, lf0, mband, mfa, ml, mord, mu, mxhnl0, mxstp0, niter, nslast
        character(len = 80) msg
        
        dimension mord(2)
        
        ! the following fortran-77 declaration is to cause the values of the listed (local) variables to be saved between calls to dvode.
        save mord, mxhnl0, mxstp0
        save zero, one, two, four, pt2, hun
        
        !-----------------------------------------------------------------------
        ! the following internal common blocks contain variables which are
        ! communicated between subroutines in the dvode package, or which are
        ! to be saved between calls to dvode.
        ! in each block, real variables precede integers.
        ! the block /dvod01/ appears in subroutines dvode, dvindy, dvstep,
        ! dvset, dvnlsd, dvjac, dvsol, dvjust and dvsrco.
        ! the block /dvod02/ appears in subroutines dvode, dvindy, dvstep,
        ! dvnlsd, dvjac, and dvsrco.
        !
        ! the variables stored in the internal common blocks are as follows:
        !
        ! acnrm  = weighted r.m.s. norm of accumulated correction vectors.
        ! ccmxj  = threshhold on drc for updating the jacobian. (see drc.)
        ! conp   = the saved value of tq(5).
        ! crate  = estimated corrector convergence rate constant.
        ! drc    = relative change in h*rl1 since last dvjac call.
        ! el     = real array of integration coefficients.  see dvset.
        ! eta    = saved tentative ratio of new to old h.
        ! etamax = saved maximum value of eta to be allowed.
        ! h      = the step size.
        ! hmin   = the minimum absolute value of the step size h to be used.
        ! hmxi   = inverse of the maximum absolute value of h to be used.
        !          hmxi = 0.0 is allowed and corresponds to an infinite hmax.
        ! hnew   = the step size to be attempted on the next step.
        ! hscal  = stepsize in scaling of yh array.
        ! prl1   = the saved value of rl1.
        ! rc     = ratio of current h*rl1 to value on last dvjac call.
        ! rl1    = the reciprocal of the coefficient el(1).
        ! tau    = real vector of past nq step sizes, length 13.
        ! tq     = a real vector of length 5 in which dvset stores constants
        !          used for the convergence test, the error test, and the
        !          selection of h at a new order.
        ! tn     = the independent variable, updated on each step taken.
        ! uround = the machine unit roundoff.  the smallest positive real number
        !          such that  1.0 + uround /= 1.0
        ! icf    = integer flag for convergence failure in dvnlsd:
        !            0 means no failures.
        !            1 means convergence failure with out of date jacobian
        !                   (recoverable error).
        !            2 means convergence failure with current jacobian or
        !                   singular matrix (unrecoverable error).
        ! init   = saved integer flag indicating whether initialization of the
        !          problem has been done (init = 1) or not.
        ! ipup   = saved flag to signal updating of newton matrix.
        ! jcur   = output flag from dvjac showing jacobian status:
        !            jcur = 0 means j is not current.
        !            jcur = 1 means j is current.
        ! jstart = integer flag used as input to dvstep:
        !            0  means perform the first step.
        !            1  means take a new step continuing from the last.
        !            -1 means take the next step with a new value of maxord,
        !                  hmin, hmxi, n, meth, miter, and/or matrix parameters.
        !          on return, dvstep sets jstart = 1.
        ! jsv    = integer flag for jacobian saving, = SIGN(mf).
        ! kflag  = a completion code from dvstep with the following meanings:
        !               0      the step was succesful.
        !              -1      the requested error could not be achieved.
        !              -2      corrector convergence could not be achieved.
        !              -3, -4  fatal error in vnls (can not occur here).
        ! kuth   = input flag to dvstep showing whether h was reduced by the
        !          driver.  kuth = 1 if h was reduced, = 0 otherwise.
        ! l      = integer variable, nq + 1, current order plus one.
        ! lmax   = maxord + 1 (used for dimensioning).
        ! locjs  = a pointer to the saved jacobian, whose storage starts at
        !          wm(locjs), if jsv = 1.
        ! lyh, lewt, lacor, lsavf, lwm, liwm = saved integer pointers
        !          to segments of rwork and iwork.
        ! maxord = the maximum order of integration method to be allowed.
        ! meth/miter = the method flags.  see mf.
        ! msbj   = the maximum number of steps between j evaluations, = 50.
        ! mxhnil = saved value of optional input mxhnil.
        ! mxstep = saved value of optional input mxstep.
        ! n      = the number of first-order odes, = neq.
        ! newh   = saved integer to flag change of h.
        ! newq   = the method order to be used on the next step.
        ! nhnil  = saved counter for occurrences of t + h = t.
        ! nq     = integer variable, the current integration method order.
        ! nqnyh  = saved value of nq*nyh.
        ! nqwait = a counter controlling the frequency of order changes.
        !          an order change is about to be considered if nqwait = 1.
        ! nslj   = the number of steps taken as of the last jacobian update.
        ! nslp   = saved value of nst as of last newton matrix update.
        ! nyh    = saved value of the initial value of neq.
        ! hu     = the step size in t last used.
        ! ncfn   = number of nonlinear convergence failures so far.
        ! netf   = the number of error test failures of the integrator so far.
        ! nfe    = the number of f evaluations for the problem so far.
        ! nje    = the number of jacobian evaluations so far.
        ! nlu    = the number of matrix lu decompositions so far.
        ! nni    = number of nonlinear iterations so far.
        ! nqu    = the method order last used.
        ! nst    = the number of steps taken for the problem so far.
        !-----------------------------------------------------------------------
        
        common /dvod01/ acnrm, ccmxj, conp, crate, drc, el(13), eta, etamax, h, hmin, hmxi, hnew, hscal, prl1, rc, rl1, tau(13), tq(5), tn, uround,             &
            &   icf, init, ipup, jcur, jstart, jsv, kflag, kuth, l, lmax, lyh, lewt, lacor, lsavf, lwm, liwm, locjs, maxord, meth, miter, msbj, mxhnil, mxstep, &
            &   n, newh, newq, nhnil, nq, nqnyh, nqwait, nslj, nslp, nyh
        common /dvod02/ hu, ncfn, netf, nfe, nje, nlu, nni, nqu, nst
        
        data  mord(1) /12/, mord(2) /5/, mxstp0 /500/, mxhnl0 /10/
        data zero /0.0d0/, one /1.0d0/, two /2.0d0/, four /4.0d0/, pt2 /0.2d0/, hun /100.0d0/
        
        !-----------------------------------------------------------------------
        ! block a.
        ! this code block is executed on every call.
        ! it tests istate and itask for legality and branches appropriately.
        ! if istate > 1 but the flag init shows that initialization has
        ! not yet been done, an error return occurs.
        ! if istate = 1 and tout = t, return immediately.
        if (istate < 1 .or. istate > 3) go to 601
        if (itask < 1 .or. itask > 5) go to 602
        if (istate == 1) go to 10
        if (init /= 1) go to 603
        if (istate == 2) go to 200
        go to 20
10      init = 0
        if (tout == t) return
        
        !-----------------------------------------------------------------------
        ! block b.
        ! the next code block is executed for the initial call (istate = 1),
        ! or for a continuation call with parameter changes (istate = 3).
        ! it contains checking of all input and various initializations.
        !
        ! first check legality of the non-optional input neq, itol, iopt,
        ! mf, ml, and mu.
20      if (neq <= 0) go to 604
        if (istate == 1) go to 25
        if (neq > n) go to 605
25      n = neq
        if (itol < 1 .or. itol > 4) go to 606
        if (iopt < 0 .or. iopt > 1) go to 607
        
        jsv = SIGN(1,mf)
        mfa = ABS(mf)
        meth = mfa/10
        miter = mfa - 10*meth
        if (meth < 1 .or. meth > 2) go to 608
        if (miter < 0 .or. miter > 5) go to 608
        if (miter <= 3) go to 30
        
        ml = iwork(1)
        mu = iwork(2)
        if (ml < 0 .or. ml >= n) go to 609
        if (mu < 0 .or. mu >= n) go to 610
30      continue

        ! next process and check the optional input.
        if (iopt == 1) go to 40
        maxord = mord(meth)
        mxstep = mxstp0
        mxhnil = mxhnl0
        if (istate == 1) h0 = zero
        hmxi = zero
        hmin = zero
        go to 60
40      maxord = iwork(5)
        if (maxord < 0) go to 611
        if (maxord == 0) maxord = 100
        maxord = MIN(maxord,mord(meth))
        mxstep = iwork(6)
        if (mxstep < 0) go to 612
        if (mxstep == 0) mxstep = mxstp0
        mxhnil = iwork(7)
        if (mxhnil < 0) go to 613
        if (mxhnil == 0) mxhnil = mxhnl0
        if (istate /= 1) go to 50
        
        h0 = rwork(5)
        if ((tout - t)*h0 < zero) go to 614
50      hmax = rwork(6)
        if (hmax < zero) go to 615
        hmxi = zero
        if (hmax > zero) hmxi = one/hmax
        hmin = rwork(7)
        if (hmin < zero) go to 616
        
        !-----------------------------------------------------------------------
        ! set work array pointers and check lengths lrw and liw.
        ! pointers to segments of rwork and iwork are named by prefixing l to
        ! the name of the segment.  e.g., the segment yh starts at rwork(lyh).
        ! segments of rwork (in order) are denoted  yh, wm, ewt, savf, acor.
        ! within wm, locjs is the location of the saved jacobian (jsv > 0).
60      lyh = 21
        if (istate == 1) nyh = n
        lwm = lyh + (maxord + 1)*nyh
        jco = MAX(0,jsv)
        if (miter == 0) lenwm = 0
        if (miter == 1 .or. miter == 2) then
            lenwm = 2 + (1 + jco)*n*n
            locjs = n*n + 3
        end if
        if (miter == 3) lenwm = 2 + n
        if (miter == 4 .or. miter == 5) then
            mband = ml + mu + 1
            lenp = (mband + ml)*n
            lenj = mband*n
            lenwm = 2 + lenp + jco*lenj
            locjs = lenp + 3
        end if
        
        lewt = lwm + lenwm
        lsavf = lewt + n
        lacor = lsavf + n
        lenrw = lacor + n - 1
        iwork(17) = lenrw
        liwm = 1
        leniw = 30 + n
        if (miter == 0 .or. miter == 3) leniw = 30
        iwork(18) = leniw
        if (lenrw > lrw) go to 617
        if (leniw > liw) go to 618
        
        ! check rtol and atol for legality. 
        rtoli = rtol(1)
        atoli = atol(1)
        do 70 i = 1,n
            if (itol >= 3) rtoli = rtol(i)
            if (itol == 2 .or. itol == 4) atoli = atol(i)
            if (rtoli < zero) go to 619
            if (atoli < zero) go to 620
70      continue
        if (istate == 1) go to 100
        
        ! if istate = 3, set flag to signal parameter changes to dvstep. 
        jstart = -1
        if (nq <= maxord) go to 90
        ! maxord was reduced below nq.  copy yh(*,maxord+2) into savf. 
        call dcopy (n, rwork(lwm), 1, rwork(lsavf), 1)
        ! reload wm(1) = rwork(lwm), since lwm may have changed. 
90      if (miter > 0) rwork(lwm) = SQRT(uround)
        go to 200
        
        !-----------------------------------------------------------------------
        ! block c.
        ! the next block is for the initial call only (istate = 1).
        ! it contains all remaining initializations, the initial call to f,
        ! and the calculation of the initial step size.
        ! the error weights in ewt are inverted after being loaded.
100     uround = dumach()
        tn = t
        if (itask /= 4 .and. itask /= 5) go to 110
        tcrit = rwork(1)
        if ((tcrit - tout)*(tout - t) < zero) go to 625
        if (h0 /= zero .and. (t + h0 - tcrit)*h0 > zero) h0 = tcrit - t
110     jstart = 0
        if (miter > 0) rwork(lwm) = SQRT(uround)
        
        ccmxj = pt2
        msbj = 50
        nhnil = 0
        nst = 0
        nje = 0
        nni = 0
        ncfn = 0
        netf = 0
        nlu = 0
        nslj = 0
        nslast = 0
        hu = zero
        nqu = 0
        
        ! initial call to f.  (lf0 points to yh(*,2).) 
        lf0 = lyh + nyh
        call f (n, t, y, rwork(lf0), rpar, ipar)
        nfe = 1
        
        ! load the initial value vector in yh. 
        call dcopy (n, y, 1, rwork(lyh), 1)
        
        ! load and invert the ewt array.  (h is temporarily set to 1.0.) 
        nq = 1
        h = one
        call dewset (n, itol, rtol, atol, rwork(lyh), rwork(lewt))
        do 120 i = 1,n
            if (rwork(i+lewt-1) <= zero) go to 621
120     rwork(i+lewt-1) = one/rwork(i+lewt-1)
        if (h0 /= zero) go to 180
        
        ! call dvhin to set initial step size h0 to be attempted. 
        call dvhin (n, t, rwork(lyh), rwork(lf0), f, rpar, ipar, tout, uround, rwork(lewt), itol, atol, y, rwork(lacor), h0, niter, ier)
        nfe = nfe + niter
        if (ier /= 0) go to 622
        
        ! adjust h0 if necessary to meet hmax bound. 
180     rh = ABS(h0)*hmxi
        if (rh > one) h0 = h0/rh
        ! load h with h0 and scale yh(*,2) by h0. 
        h = h0
        call dscal (n, h0, rwork(lf0), 1)
        go to 270
        
        !-----------------------------------------------------------------------
        ! block d.
        ! the next code block is for continuation calls only (istate = 2 or 3)
        ! and is to check stop conditions before taking a step.
200     nslast = nst
        kuth = 0
        
        go to (210, 250, 220, 230, 240), itask
210     if ((tn - tout)*h < zero) go to 250
        call dvindy (tout, 0, rwork(lyh), nyh, y, iflag)
        if (iflag /= 0) go to 627
        t = tout
        go to 420
220     tp = tn - hu*(one + hun*uround)
        if ((tp - tout)*h > zero) go to 623
        if ((tn - tout)*h < zero) go to 250
        go to 400
230     tcrit = rwork(1)
        if ((tn - tcrit)*h > zero) go to 624
        if ((tcrit - tout)*h < zero) go to 625
        if ((tn - tout)*h < zero) go to 245
        call dvindy (tout, 0, rwork(lyh), nyh, y, iflag)
        if (iflag /= 0) go to 627
        t = tout
        go to 420
240     tcrit = rwork(1)
        if ((tn - tcrit)*h > zero) go to 624
245     hmx = ABS(tn) + ABS(h)
        ihit = ABS(tn - tcrit) <= hun*uround*hmx
        if (ihit) go to 400
        tnext = tn + hnew*(one + four*uround)
        if ((tnext - tcrit)*h <= zero) go to 250
        h = (tcrit - tn)*(one - four*uround)
        kuth = 1
        
        !-----------------------------------------------------------------------
        ! block e.
        ! the next block is normally executed for all calls and contains
        ! the call to the one-step core integrator dvstep.
        !
        ! this is a looping point for the integration steps.
        !
        ! first check for too many steps being taken, update ewt (if not at
        ! start of problem), check for too much accuracy being requested, and
        ! check for h below the roundoff level in t.
250     continue
        if ((nst-nslast) >= mxstep) go to 500
        call dewset (n, itol, rtol, atol, rwork(lyh), rwork(lewt))
        do 260 i = 1,n
            if (rwork(i+lewt-1) <= zero) go to 510
260     rwork(i+lewt-1) = one/rwork(i+lewt-1)
270     tolsf = uround*dvnorm (n, rwork(lyh), rwork(lewt))

        if (tolsf <= one) go to 280
        tolsf = tolsf*two
        if (nst == 0) go to 626
        go to 520
280     if ((tn + h) /= tn) go to 290
        nhnil = nhnil + 1
        if (nhnil > mxhnil) go to 290
        msg = 'dvode--  warning: internal t (=r1) and h (=r2) are'
        call xerrwd (msg, 50, 101, 1, 0, 0, 0, 0, zero, zero)
        msg='      such that in the machine, t + h = t on the next step  '
        call xerrwd (msg, 60, 101, 1, 0, 0, 0, 0, zero, zero)
        msg = '      (h = step size). solver will continue anyway'
        call xerrwd (msg, 50, 101, 1, 0, 0, 0, 2, tn, h)
        if (nhnil < mxhnil) go to 290
        msg = 'dvode--  above warning has been issued i1 times.  '
        call xerrwd (msg, 50, 102, 1, 0, 0, 0, 0, zero, zero)
        msg = '      it will not be issued again for this problem'
        call xerrwd (msg, 50, 102, 1, 1, mxhnil, 0, 0, zero, zero)
290     continue

        !-----------------------------------------------------------------------
        ! call dvstep (y, yh, nyh, yh, ewt, savf, vsav, acor, wm, iwm, f, jac, f, dvnlsd, rpar, ipar)
        call dvstep (y, rwork(lyh), nyh, rwork(lyh), rwork(lewt), rwork(lsavf), y, rwork(lacor), rwork(lwm), iwork(liwm), f, jac, f, dvnlsd, rpar, ipar)
        kgo = 1 - kflag
        
        ! branch on kflag. note: in this version, kflag can not be set to -3. kflag == 0,   -1,  -2
        go to (300, 530, 540), kgo
        
        !-----------------------------------------------------------------------
        ! block f.
        ! the following block handles the case of a successful return from the
        ! core integrator (kflag = 0).  test for stop conditions.
300     init = 1
        kuth = 0
        
        go to (310, 400, 330, 340, 350), itask
        ! itask = 1.  if tout has been reached, interpolate. 
310     if ((tn - tout)*h < zero) go to 250
        call dvindy (tout, 0, rwork(lyh), nyh, y, iflag)
        t = tout
        go to 420
        ! itask = 3.  jump to exit if tout was reached. 
330     if ((tn - tout)*h >= zero) go to 400
        go to 250
        ! itask = 4.  see if tout or tcrit was reached.  adjust h if necessary.
340     if ((tn - tout)*h < zero) go to 345
        call dvindy (tout, 0, rwork(lyh), nyh, y, iflag)
        t = tout
        go to 420
345     hmx = ABS(tn) + ABS(h)
        ihit = ABS(tn - tcrit) <= hun*uround*hmx
        if (ihit) go to 400
        tnext = tn + hnew*(one + four*uround)
        if ((tnext - tcrit)*h <= zero) go to 250
        h = (tcrit - tn)*(one - four*uround)
        kuth = 1
        go to 250
        ! itask = 5.  see if tcrit was reached and jump to exit. 
350     hmx = ABS(tn) + ABS(h)
        ihit = ABS(tn - tcrit) <= hun*uround*hmx
        
        !-----------------------------------------------------------------------
        ! block g.
        ! the following block handles all successful returns from dvode.
        ! if itask /= 1, y is loaded from yh and t is set accordingly.
        ! istate is set to 2, and the optional output is loaded into the work
        ! arrays before returning.
400     continue
        call dcopy (n, rwork(lyh), 1, y, 1)
        t = tn
        if (itask /= 4 .and. itask /= 5) go to 420
        if (ihit) t = tcrit
420     istate = 2
        rwork(11) = hu
        rwork(12) = hnew
        rwork(13) = tn
        iwork(11) = nst
        iwork(12) = nfe
        iwork(13) = nje
        iwork(14) = nqu
        iwork(15) = newq
        iwork(19) = nlu
        iwork(20) = nni
        iwork(21) = ncfn
        iwork(22) = netf
        return
        
        !-----------------------------------------------------------------------
        ! block h.
        ! the following block handles all unsuccessful returns other than
        ! those for illegal input.  first the error message routine is called.
        ! if there was an error test or convergence test failure, imxer is set.
        ! then y is loaded from yh, and t is set to tn.
        ! the optional output is loaded into the work arrays before returning.
        
        ! the maximum number of steps was taken before reaching tout. 
500     msg = 'dvode--  at current t (=r1), mxstep (=i1) steps   '
        call xerrwd (msg, 50, 201, 1, 0, 0, 0, 0, zero, zero)
        msg = '      taken on this call before reaching tout     '
        call xerrwd (msg, 50, 201, 1, 1, mxstep, 0, 1, tn, zero)
        istate = -1
        go to 580
        
        ! ewt(i) <= 0.0 for some i (not at start of problem). 
510     ewti = rwork(lewt+i-1)
        msg = 'dvode--  at t (=r1), ewt(i1) has become r2 <= 0.'
        call xerrwd (msg, 50, 202, 1, 1, i, 0, 2, tn, ewti)
        istate = -6
        go to 580
        
        ! too much accuracy requested for machine precision. 
520     msg = 'dvode--  at t (=r1), too much accuracy requested  '
        call xerrwd (msg, 50, 203, 1, 0, 0, 0, 0, zero, zero)
        msg = '      for precision of machine:   see tolsf (=r2) '
        call xerrwd (msg, 50, 203, 1, 0, 0, 0, 2, tn, tolsf)
        rwork(14) = tolsf
        istate = -2
        go to 580
        
        ! kflag = -1.  error test failed repeatedly or with ABS(h) = hmin. 
530     msg = 'dvode--  at t(=r1) and step size h(=r2), the error'
        call xerrwd (msg, 50, 204, 1, 0, 0, 0, 0, zero, zero)
        msg = '      test failed repeatedly or with ABS(h) = hmin'
        call xerrwd (msg, 50, 204, 1, 0, 0, 0, 2, tn, h)
        istate = -4
        go to 560
        
        ! kflag = -2.  convergence failed repeatedly or with ABS(h) = hmin. 
540     msg = 'dvode--  at t (=r1) and step size h (=r2), the    '
        call xerrwd (msg, 50, 205, 1, 0, 0, 0, 0, zero, zero)
        msg = '      corrector convergence failed repeatedly     '
        call xerrwd (msg, 50, 205, 1, 0, 0, 0, 0, zero, zero)
        msg = '      or with ABS(h) = hmin   '
        call xerrwd (msg, 30, 205, 1, 0, 0, 0, 2, tn, h)
        istate = -5
        
        ! compute imxer if relevant. 
560     big = zero
        imxer = 1
        do 570 i = 1,n
            size = ABS(rwork(i+lacor-1)*rwork(i+lewt-1))
            if (big >= size) go to 570
            big = size
            imxer = i
570     continue
        iwork(16) = imxer
        
        ! set y vector, t, and optional output. 
580     continue
        call dcopy (n, rwork(lyh), 1, y, 1)
        t = tn
        rwork(11) = hu
        rwork(12) = h
        rwork(13) = tn
        iwork(11) = nst
        iwork(12) = nfe
        iwork(13) = nje
        iwork(14) = nqu
        iwork(15) = nq
        iwork(19) = nlu
        iwork(20) = nni
        iwork(21) = ncfn
        iwork(22) = netf
        return
        
        !-----------------------------------------------------------------------
        ! block i.
        ! the following block handles all error returns due to illegal input
        ! (istate = -3), as detected before calling the core integrator.
        ! first the error message routine is called.   if the illegal input
        ! is a negative istate, the run is aborted (apparent infinite loop).
601     msg = 'dvode--  istate (=i1) illegal '
        call xerrwd (msg, 30, 1, 1, 1, istate, 0, 0, zero, zero)
        if (istate < 0) go to 800
        go to 700
602     msg = 'dvode--  itask (=i1) illegal  '
        call xerrwd (msg, 30, 2, 1, 1, itask, 0, 0, zero, zero)
        go to 700
603     msg='dvode--  istate (=i1) > 1 but dvode not initialized      '
        call xerrwd (msg, 60, 3, 1, 1, istate, 0, 0, zero, zero)
        go to 700
604     msg = 'dvode--  neq (=i1) < 1     '
        call xerrwd (msg, 30, 4, 1, 1, neq, 0, 0, zero, zero)
        go to 700
605     msg = 'dvode--  istate = 3 and neq increased (i1 to i2)  '
        call xerrwd (msg, 50, 5, 1, 2, n, neq, 0, zero, zero)
        go to 700
606     msg = 'dvode--  itol (=i1) illegal   '
        call xerrwd (msg, 30, 6, 1, 1, itol, 0, 0, zero, zero)
        go to 700
607     msg = 'dvode--  iopt (=i1) illegal   '
        call xerrwd (msg, 30, 7, 1, 1, iopt, 0, 0, zero, zero)
        go to 700
608     msg = 'dvode--  mf (=i1) illegal     '
        call xerrwd (msg, 30, 8, 1, 1, mf, 0, 0, zero, zero)
        go to 700
609     msg = 'dvode--  ml (=i1) illegal:  <0 or >=neq (=i2)'
        call xerrwd (msg, 50, 9, 1, 2, ml, neq, 0, zero, zero)
        go to 700
610     msg = 'dvode--  mu (=i1) illegal:  <0 or >=neq (=i2)'
        call xerrwd (msg, 50, 10, 1, 2, mu, neq, 0, zero, zero)
        go to 700
611     msg = 'dvode--  maxord (=i1) < 0  '
        call xerrwd (msg, 30, 11, 1, 1, maxord, 0, 0, zero, zero)
        go to 700
612     msg = 'dvode--  mxstep (=i1) < 0  '
        call xerrwd (msg, 30, 12, 1, 1, mxstep, 0, 0, zero, zero)
        go to 700
613     msg = 'dvode--  mxhnil (=i1) < 0  '
        call xerrwd (msg, 30, 13, 1, 1, mxhnil, 0, 0, zero, zero)
        go to 700
614     msg = 'dvode--  tout (=r1) behind t (=r2)      '
        call xerrwd (msg, 40, 14, 1, 0, 0, 0, 2, tout, t)
        msg = '      integration direction is given by h0 (=r1)  '
        call xerrwd (msg, 50, 14, 1, 0, 0, 0, 1, h0, zero)
        go to 700
615     msg = 'dvode--  hmax (=r1) < 0.0  '
        call xerrwd (msg, 30, 15, 1, 0, 0, 0, 1, hmax, zero)
        go to 700
616     msg = 'dvode--  hmin (=r1) < 0.0  '
        call xerrwd (msg, 30, 16, 1, 0, 0, 0, 1, hmin, zero)
        go to 700
617     continue
        msg='dvode--  rwork length needed, lenrw (=i1), exceeds lrw (=i2)'
        call xerrwd (msg, 60, 17, 1, 2, lenrw, lrw, 0, zero, zero)
        go to 700
618     continue
        msg='dvode--  iwork length needed, leniw (=i1), exceeds liw (=i2)'
        call xerrwd (msg, 60, 18, 1, 2, leniw, liw, 0, zero, zero)
        go to 700
619     msg = 'dvode--  rtol(i1) is r1 < 0.0        '
        call xerrwd (msg, 40, 19, 1, 1, i, 0, 1, rtoli, zero)
        go to 700
620     msg = 'dvode--  atol(i1) is r1 < 0.0        '
        call xerrwd (msg, 40, 20, 1, 1, i, 0, 1, atoli, zero)
        go to 700
621     ewti = rwork(lewt+i-1)
        msg = 'dvode--  ewt(i1) is r1 <= 0.0         '
        call xerrwd (msg, 40, 21, 1, 1, i, 0, 1, ewti, zero)
        go to 700
622     continue
        msg='dvode--  tout (=r1) too close to t(=r2) to start integration'
        call xerrwd (msg, 60, 22, 1, 0, 0, 0, 2, tout, t)
        go to 700
623     continue
        msg='dvode--  itask = i1 and tout (=r1) behind tcur - hu (= r2)  '
        call xerrwd (msg, 60, 23, 1, 1, itask, 0, 2, tout, tp)
        go to 700
624     continue
        msg='dvode--  itask = 4 or 5 and tcrit (=r1) behind tcur (=r2)   '
        call xerrwd (msg, 60, 24, 1, 0, 0, 0, 2, tcrit, tn)
        go to 700
625     continue
        msg='dvode--  itask = 4 or 5 and tcrit (=r1) behind tout (=r2)   '
        call xerrwd (msg, 60, 25, 1, 0, 0, 0, 2, tcrit, tout)
        go to 700
626     msg = 'dvode--  at start of problem, too much accuracy   '
        call xerrwd (msg, 50, 26, 1, 0, 0, 0, 0, zero, zero)
        msg='      requested for precision of machine:   see tolsf (=r1) '
        call xerrwd (msg, 60, 26, 1, 0, 0, 0, 1, tolsf, zero)
        rwork(14) = tolsf
        go to 700
627     msg='dvode--  trouble from dvindy.  itask = i1, tout = r1.       '
        call xerrwd (msg, 60, 27, 1, 1, itask, 0, 1, tout, zero)
        
700     continue
        istate = -3
        return
        
800     msg = 'dvode--  run aborted:  apparent infinite loop     '
        call xerrwd (msg, 50, 303, 2, 0, 0, 0, 0, zero, zero)
        
        return
    end subroutine dvode

    !$
    !===============================================================================================
    ! call sequence input -- n, t0, y0, ydot, f, rpar, ipar, tout, uround,
    !                        ewt, itol, atol, y, temp
    ! call sequence output -- h0, niter, ier
    ! common block variables accessed -- none
    !
    ! subroutines called by dvhin:  f
    ! function routines called by dvhi: dvnorm
    !---------------------------------------------------------------------------
    ! this routine computes the step size, h0, to be attempted on the
    ! first step, when the user has not supplied a value for this.
    !
    ! first we check that tout - t0 differs significantly from zero.  then
    ! an iteration is done to approximate the initial second derivative
    ! and this is used to define h from w.r.m.s.norm(h**2 * yddot / 2) = 1.
    ! a bias factor of 1/2 is applied to the resulting h.
    ! the SIGN of h0 is inferred from the initial values of tout and t0.
    !
    ! communication with dvhin is done with the following variables:
    !
    ! n      = size of ode system, input.
    ! t0     = initial value of independent variable, input.
    ! y0     = vector of initial conditions, input.
    ! ydot   = vector of initial first derivatives, input.
    ! f      = name of subroutine for right-hand side f(t,y), input.
    ! rpar, ipar = dummy names for user's real and integer work arrays.
    ! tout   = first output value of independent variable
    ! uround = machine unit roundoff
    ! ewt, itol, atol = error weights and tolerance parameters
    !                   as described in the driver routine, input.
    ! y, temp = work arrays of length n.
    ! h0     = step size to be attempted, output.
    ! niter  = number of iterations (and of f evaluations) to compute h0,
    !          output.
    ! ier    = the error flag, returned with the value
    !          ier = 0  if no trouble occurred, or
    !          ier = -1 if tout and t0 are considered too close to proceed.
    !===============================================================================================
    subroutine dvhin (n, t0, y0, ydot, f, rpar, ipar, tout, uround, ewt, itol, atol, y, temp, h0, niter, ier)
    
        external f
        double precision t0, y0, ydot, rpar, tout, uround, ewt, atol, y, temp, h0
        integer n, ipar, itol, niter, ier
        dimension y0(*), ydot(*), ewt(*), atol(*), y(*), temp(*), rpar(*), ipar(*)
        
        ! type declarations for local variables 
        double precision afi, atoli, delyi, h, half, hg, hlb, hnew, hrat, hub, hun, pt1, t1, tdist, tround, two, yddnrm
        integer i, iter
        
        !-----------------------------------------------------------------------
        ! the following fortran-77 declaration is to cause the values of the listed (local) variables to be saved between calls to this integrator.
        save half, hun, pt1, two
        data half /0.5d0/, hun /100.0d0/, pt1 /0.1d0/, two /2.0d0/
        
        niter = 0
        tdist = ABS(tout - t0)
        tround = uround*MAX(ABS(t0),ABS(tout))
        
        if (tdist < two*tround) go to 100
        
        ! set a lower bound on h based on the roundoff level in t0 and tout. 
        hlb = hun*tround
        ! set an upper bound on h based on tout-t0 and the initial y and ydot. 
        hub = pt1*tdist
        
        atoli = atol(1)
        do 10 i = 1, n
            if (itol == 2 .or. itol == 4) atoli = atol(i)
            delyi = pt1*ABS(y0(i)) + atoli
            afi = ABS(ydot(i))
            if (afi*hub > delyi) hub = delyi/afi
10      continue
        
        ! set initial guess for h as geometric mean of upper and lower bounds. 
        iter = 0
        hg = SQRT(hlb*hub)
        ! if the bounds have crossed, exit with the mean value. 
        if (hub < hlb) then
            h0 = hg
            go to 90
        end if
        
        ! looping point for iteration. 
50      continue

        ! estimate the second derivative as a difference quotient in f. 
        h = SIGN (hg, tout - t0)
        t1 = t0 + h
        do 60 i = 1, n
60      y(i) = y0(i) + h*ydot(i)
        call f (n, t1, y, temp, rpar, ipar)
        do 70 i = 1, n
70      temp(i) = (temp(i) - ydot(i))/h
        yddnrm = dvnorm (n, temp, ewt)
        
        ! get the corresponding new value of h. 
        if (yddnrm*hub*hub > two) then
            hnew = SQRT(two/yddnrm)
        else
            hnew = SQRT(hg*hub)
        end if
        iter = iter + 1
        
        !-----------------------------------------------------------------------
        ! test the stopping conditions.
        ! stop if the new and previous h values differ by a factor of < 2.
        ! stop if four iterations have been done.  also, stop with previous h
        ! if hnew/hg > 2 after first iteration, as this probably means that
        ! the second derivative value is bad because of cancellation error.
        if (iter >= 4) go to 80
        hrat = hnew/hg
        if ( (hrat > half) .and. (hrat < two) ) go to 80
        if ( (iter >= 2) .and. (hnew > two*hg) ) then
            hnew = hg
            go to 80
        end if
        hg = hnew
        go to 50
        
        ! iteration done.  apply bounds, bias factor, and SIGN.  then exit. 
80      h0 = hnew*half
        if (h0 < hlb) h0 = hlb
        if (h0 > hub) h0 = hub
90      h0 = SIGN(h0, tout - t0)
        niter = iter
        ier = 0
        return
        
        ! error return for tout - t0 too small. 
100     ier = -1

        return
    end subroutine dvhin

    !$
    !===============================================================================================
    ! call sequence input -- t, k, yh, ldyh
    ! call sequence output -- dky, iflag
    ! common block variables accessed:
    !     /dvod01/ --  h, tn, uround, l, n, nq
    !     /dvod02/ --  hu
    !
    ! subroutines called by dvindy: dscal, xerrwd
    ! function routines called by dvindy: none
    !---------------------------------------------------------------------------
    ! dvindy computes interpolated values of the k-th derivative of the
    ! dependent variable vector y, and stores it in dky.  this routine
    ! is called within the package with k = 0 and t = tout, but may
    ! also be called by the user for any k up to the current order.
    ! (see detailed instructions in the usage documentation.)
    !---------------------------------------------------------------------------
    ! the computed values in dky are gotten by interpolation using the
    ! nordsieck history array yh.  this array corresponds uniquely to a
    ! vector-valued polynomial of degree nqcur or less, and dky is set
    ! to the k-th derivative of this polynomial at t.
    ! the formula for dky is:
    !              q
    !  dky(i)  =  sum  c(j,k) * (t - tn)**(j-k) * h**(-j) * yh(i,j+1)
    !             j=k
    ! where  c(j,k) = j*(j-1)*...*(j-k+1), q = nqcur, tn = tcur, h = hcur.
    ! the quantities  nq = nqcur, l = nq+1, n, tn, and h are
    ! communicated by common.  the above sum is done in reverse order.
    ! iflag is returned negative if either k or t is out of bounds.
    !
    ! discussion above and comments in driver explain all variables.
    !===============================================================================================
    subroutine dvindy (t, k, yh, ldyh, dky, iflag)
    
        double precision t, yh, dky
        integer k, ldyh, iflag
        dimension yh(ldyh,*), dky(*)

        ! type declarations for labeled common block dvod01 
        double precision acnrm, ccmxj, conp, crate, drc, el, eta, etamax, h, hmin, hmxi, hnew, hscal, prl1, rc, rl1, tau, tq, tn, uround
        integer icf, init, ipup, jcur, jstart, jsv, kflag, kuth, l, lmax, lyh, lewt, lacor, lsavf, lwm, liwm, locjs, maxord, meth, miter, msbj, mxhnil, mxstep,     &
            &   n, newh, newq, nhnil, nq, nqnyh, nqwait, nslj, nslp, nyh
        
        ! type declarations for labeled common block dvod02 
        double precision hu
        integer ncfn, netf, nfe, nje, nlu, nni, nqu, nst
        
        ! type declarations for local variables 
        double precision c, hun, r, s, tfuzz, tn1, tp, zero
        integer i, ic, j, jb, jb2, jj, jj1, jp1
        character(len = 80) msg
        
        !-----------------------------------------------------------------------
        ! the following fortran-77 declaration is to cause the values of the
        ! listed (local) variables to be saved between calls to this integrator.
        save hun, zero
        
        common /dvod01/ acnrm, ccmxj, conp, crate, drc, el(13), eta, etamax, h, hmin, hmxi, hnew, hscal, prl1, rc, rl1, tau(13), tq(5), tn, uround,             &
            &   icf, init, ipup, jcur, jstart, jsv, kflag, kuth, l, lmax, lyh, lewt, lacor, lsavf, lwm, liwm, locjs, maxord, meth, miter, msbj, mxhnil, mxstep, &
            &   n, newh, newq, nhnil, nq, nqnyh, nqwait, nslj, nslp, nyh
        common /dvod02/ hu, ncfn, netf, nfe, nje, nlu, nni, nqu, nst
        
        data hun /100.0d0/, zero /0.0d0/
        
        iflag = 0
        if (k < 0 .or. k > nq) go to 80
        tfuzz = hun*uround*SIGN(ABS(tn) + ABS(hu), hu)
        tp = tn - hu - tfuzz
        tn1 = tn + tfuzz
        if ((t-tp)*(t-tn1) > zero) go to 90
        
        s = (t - tn)/h
        ic = 1
        if (k == 0) go to 15
        jj1 = l - k
        do 10 jj = jj1, nq
10      ic = ic*jj
15      c = real(ic)
        do 20 i = 1, n
20      dky(i) = c*yh(i,l)

        if (k == nq) go to 55
        jb2 = nq - k
        do 50 jb = 1, jb2
            j = nq - jb
            jp1 = j + 1
            ic = 1
            if (k == 0) go to 35
            jj1 = jp1 - k
            do 30 jj = jj1, j
30          ic = ic*jj
35          c = real(ic)
            do 40 i = 1, n
40          dky(i) = c*yh(i,jp1) + s*dky(i)
50      continue
        if (k == 0) return
55      r = h**(-k)
        call dscal (n, r, dky, 1)
        return
        
80      msg = 'dvindy-- k (=i1) illegal      '
        call xerrwd (msg, 30, 51, 1, 1, k, 0, 0, zero, zero)
        iflag = -1
        return
90      msg = 'dvindy-- t (=r1) illegal      '
        call xerrwd (msg, 30, 52, 1, 0, 0, 0, 1, t, zero)
        msg='      t not in interval tcur - hu (= r1) to tcur (=r2)      '
        call xerrwd (msg, 60, 52, 1, 0, 0, 0, 2, tp, tn)
        iflag = -2
        
        return
    end subroutine dvindy

    !$
    !===============================================================================================
    ! call sequence input -- y, yh, ldyh, yh1, ewt, savf, vsav,
    !                        acor, wm, iwm, f, jac, psol, vnls, rpar, ipar
    ! call sequence output -- yh, acor, wm, iwm
    ! common block variables accessed:
    !     /dvod01/  acnrm, el(13), h, hmin, hmxi, hnew, hscal, rc, tau(13),
    !               tq(5), tn, jcur, jstart, kflag, kuth,
    !               l, lmax, maxord, n, newq, nq, nqwait
    !     /dvod02/  hu, ncfn, netf, nfe, nqu, nst
    !
    ! subroutines called by dvstep: f, daxpy, dcopy, dscal,
    !                               dvjust, vnls, dvset
    ! function routines called by dvstep: dvnorm
    !---------------------------------------------------------------------------
    ! dvstep performs one step of the integration of an initial value
    ! problem for a system of ordinary differential equations.
    ! dvstep calls subroutine vnls for the solution of the nonlinear system
    ! arising in the time step.  thus it is independent of the problem
    ! jacobian structure and the type of nonlinear system solution method.
    ! dvstep returns a completion flag kflag (in common).
    ! a return with kflag = -1 or -2 means either ABS(h) = hmin or 10
    ! consecutive failures occurred.  on a return with kflag negative,
    ! the values of tn and the yh array are as of the beginning of the last
    ! step, and h is the last step size attempted.
    !
    ! communication with dvstep is done with the following variables:
    !
    ! y      = an array of length n used for the dependent variable vector.
    ! yh     = an ldyh by lmax array containing the dependent variables
    !          and their approximate scaled derivatives, where
    !          lmax = maxord + 1.  yh(i,j+1) contains the approximate
    !          j-th derivative of y(i), scaled by h**j/factorial(j)
    !          (j = 0,1,...,nq).  on entry for the first step, the first
    !          two columns of yh must be set from the initial values.
    ! ldyh   = a constant integer >= n, the first dimension of yh.
    !          n is the number of odes in the system.
    ! yh1    = a one-dimensional array occupying the same space as yh.
    ! ewt    = an array of length n containing multiplicative weights
    !          for local error measurements.  local errors in y(i) are
    !          compared to 1.0/ewt(i) in various error tests.
    ! savf   = an array of working storage, of length n.
    !          also used for input of yh(*,maxord+2) when jstart = -1
    !          and maxord < the current order nq.
    ! vsav   = a work array of length n passed to subroutine vnls.
    ! acor   = a work array of length n, used for the accumulated
    !          corrections.  on a successful return, acor(i) contains
    !          the estimated one-step local error in y(i).
    ! wm,iwm = real and integer work arrays associated with matrix
    !          operations in vnls.
    ! f      = dummy name for the user supplied subroutine for f.
    ! jac    = dummy name for the user supplied jacobian subroutine.
    ! psol   = dummy name for the subroutine passed to vnls, for
    !          possible use there.
    ! vnls   = dummy name for the nonlinear system solving subroutine,
    !          whose real name is dependent on the method used.
    ! rpar, ipar = dummy names for user's real and integer work arrays.
    !===============================================================================================
    subroutine dvstep (y, yh, ldyh, yh1, ewt, savf, vsav, acor, wm, iwm, f, jac, psol, vnls, rpar, ipar)
    
        external f, jac, psol, vnls
        double precision y, yh, yh1, ewt, savf, vsav, acor, wm, rpar
        integer ldyh, iwm, ipar
        dimension y(*), yh(ldyh,*), yh1(*), ewt(*), savf(*), vsav(*), acor(*), wm(*), iwm(*), rpar(*), ipar(*)

        ! type declarations for labeled common block dvod01 
        double precision acnrm, ccmxj, conp, crate, drc, el, eta, etamax, h, hmin, hmxi, hnew, hscal, prl1, rc, rl1, tau, tq, tn, uround
        integer icf, init, ipup, jcur, jstart, jsv, kflag, kuth, l, lmax, lyh, lewt, lacor, lsavf, lwm, liwm, locjs, maxord, meth, miter, msbj, mxhnil, mxstep,     &
            &   n, newh, newq, nhnil, nq, nqnyh, nqwait, nslj, nslp, nyh
        
        ! type declarations for labeled common block dvod02 
        double precision hu
        integer ncfn, netf, nfe, nje, nlu, nni, nqu, nst
        
        ! type declarations for local variables 
        double precision addon, bias1,bias2,bias3, cnquot, ddn, dsm, dup, etacf, etamin, etamx1, etamx2, etamx3, etamxf, etaq, etaqm1, etaqp1, flotl, one, onepsm, r, thresh, told, zero
        integer i, i1, i2, iback, j, jb, kfc, kfh, mxncf, ncf, nflag
        
        !-----------------------------------------------------------------------
        ! the following fortran-77 declaration is to cause the values of the
        ! listed (local) variables to be saved between calls to this integrator.
        save addon, bias1, bias2, bias3, etacf, etamin, etamx1, etamx2, etamx3, etamxf, etaq, etaqm1, kfc, kfh, mxncf, onepsm, thresh, one, zero
        
        !
        common /dvod01/ acnrm, ccmxj, conp, crate, drc, el(13), eta, etamax, h, hmin, hmxi, hnew, hscal, prl1, rc, rl1, tau(13), tq(5), tn, uround,             &
            &   icf, init, ipup, jcur, jstart, jsv, kflag, kuth, l, lmax, lyh, lewt, lacor, lsavf, lwm, liwm,locjs, maxord, meth, miter, msbj, mxhnil, mxstep,  &
            &   n, newh, newq, nhnil, nq, nqnyh, nqwait, nslj, nslp, nyh
        common /dvod02/ hu, ncfn, netf, nfe, nje, nlu, nni, nqu, nst
        
        data kfc/-3/, kfh/-7/, mxncf/10/
        data addon /1.0d-6/, bias1 /6.0d0/, bias2 /6.0d0/, bias3 /10.0d0/
        data etacf /0.25d0/, etamin /0.1d0/, etamxf /0.2d0/, etamx1 /1.0d4/, etamx2 /10.0d0/,etamx3 /10.0d0/, onepsm /1.00001d0/, thresh /1.5d0/
        data one/1.0d0/, zero/0.0d0/
        
        kflag = 0
        told = tn
        ncf = 0
        jcur = 0
        nflag = 0
        if (jstart > 0) go to 20
        if (jstart == -1) go to 100
        
        !-----------------------------------------------------------------------
        ! on the first call, the order is set to 1, and other variables are
        ! initialized.  etamax is the maximum ratio by which h can be increased
        ! in a single step.  it is normally 10, but is larger during the
        ! first step to compensate for the small initial h.  if a failure
        ! occurs (in corrector convergence or error test), etamax is set to 1
        ! for the next increase.
        lmax = maxord + 1
        nq = 1
        l = 2
        nqnyh = nq*ldyh
        tau(1) = h
        prl1 = one
        rc = zero
        etamax = etamx1
        nqwait = 2
        hscal = h
        go to 200
        
        !-----------------------------------------------------------------------
        ! take preliminary actions on a normal continuation step (jstart>0).
        ! if the driver changed h, then eta must be reset and newh set to 1.
        ! if a change of order was dictated on the previous step, then
        ! it is done here and appropriate adjustments in the history are made.
        ! on an order decrease, the history array is adjusted by dvjust.
        ! on an order increase, the history array is augmented by a column.
        ! on a change of step size h, the history array yh is rescaled.
20      continue
        if (kuth == 1) then
            eta = MIN(eta,h/hscal)
            newh = 1
        end if
        
50      if (newh == 0) go to 200
        if (newq == nq) go to 150
        if (newq < nq) then
            call dvjust (yh, ldyh, -1)
            nq = newq
            l = nq + 1
            nqwait = l
            go to 150
        end if
        
        if (newq > nq) then
            call dvjust (yh, ldyh, 1)
            nq = newq
            l = nq + 1
            nqwait = l
            go to 150
        end if
        
        !-----------------------------------------------------------------------
        ! the following block handles preliminaries needed when jstart = -1.
        ! if n was reduced, zero out part of yh to avoid undefined references.
        ! if maxord was reduced to a value less than the tentative order newq,
        ! then nq is set to maxord, and a new h ratio eta is chosen.
        ! otherwise, we take the same preliminary actions as for jstart > 0.
        ! in any case, nqwait is reset to l = nq + 1 to prevent further
        ! changes in order for that many steps.
        ! the new h ratio eta is limited by the input h if kuth = 1,
        ! by hmin if kuth = 0, and by hmxi in any case.
        ! finally, the history array yh is rescaled.
100     continue
        lmax = maxord + 1
        if (n == ldyh) go to 120
        i1 = 1 + (newq + 1)*ldyh
        i2 = (maxord + 1)*ldyh
        if (i1 > i2) go to 120
        
        do 110 i = i1, i2
110     yh1(i) = zero

120     if (newq <= maxord) go to 140
        flotl = real(lmax)
        if (maxord < nq-1) then
            ddn = dvnorm (n, savf, ewt)/tq(1)
            eta = one/((bias1*ddn)**(one/flotl) + addon)
        end if
        if (maxord == nq .and. newq == nq+1) eta = etaq
        if (maxord == nq-1 .and. newq == nq+1) then
            eta = etaqm1
            call dvjust (yh, ldyh, -1)
        end if
        if (maxord == nq-1 .and. newq == nq) then
            ddn = dvnorm (n, savf, ewt)/tq(1)
            eta = one/((bias1*ddn)**(one/flotl) + addon)
            call dvjust (yh, ldyh, -1)
        end if
        eta = MIN(eta,one)
        nq = maxord
        l = lmax
        
140     if (kuth == 1) eta = MIN(eta,ABS(h/hscal))
        if (kuth == 0) eta = MAX(eta,hmin/ABS(hscal))
        eta = eta/MAX(one,ABS(hscal)*hmxi*eta)
        newh = 1
        nqwait = l
        if (newq <= maxord) go to 50
        
        ! rescale the history array for a change in h by a factor of eta. 
150     r = one
        do 180 j = 2, l
            r = r*eta
            call dscal (n, r, yh(1,j), 1 )
180     continue
        h = hscal*eta
        hscal = h
        rc = rc*eta
        nqnyh = nq*ldyh
        
        !-----------------------------------------------------------------------
        ! this section computes the predicted values by effectively
        ! multiplying the yh array by the pascal triangle matrix.
        ! dvset is called to calculate all integration coefficients.
        ! rc is the ratio of new to old values of the coefficient h/el(2)=h/l1.
200     tn = tn + h
        i1 = nqnyh + 1
        do 220 jb = 1, nq
            i1 = i1 - ldyh
            do 210 i = i1, nqnyh
210         yh1(i) = yh1(i) + yh1(i+ldyh)
220     continue
        call dvset
        rl1 = one/el(2)
        rc = rc*(rl1/prl1)
        prl1 = rl1
        
        ! call the nonlinear system solver. 
        call vnls (y, yh, ldyh, vsav, savf, ewt, acor, iwm, wm, f, jac, psol, nflag, rpar, ipar)
        if (nflag == 0) go to 450
        
        !-----------------------------------------------------------------------
        ! the vnls routine failed to achieve convergence (nflag /= 0).
        ! the yh array is retracted to its values before prediction.
        ! the step size h is reduced and the step is retried, if possible.
        ! otherwise, an error exit is taken.
        ncf = ncf + 1
        ncfn = ncfn + 1
        etamax = one
        tn = told
        i1 = nqnyh + 1
        do 430 jb = 1, nq
            i1 = i1 - ldyh
            do 420 i = i1, nqnyh
420         yh1(i) = yh1(i) - yh1(i+ldyh)
430     continue

        if (nflag < -1) go to 680
        if (ABS(h) <= hmin*onepsm) go to 670
        if (ncf == mxncf) go to 670
        eta = etacf
        eta = MAX(eta,hmin/ABS(h))
        nflag = -1
        go to 150
        
        !-----------------------------------------------------------------------
        ! the corrector has converged (nflag = 0).  the local error test is
        ! made and control passes to statement 500 if it fails.
450     continue
        dsm = acnrm/tq(2)
        if (dsm > one) go to 500
        
        !-----------------------------------------------------------------------
        ! after a successful step, update the yh and tau arrays and decrement
        ! nqwait.  if nqwait is then 1 and nq < maxord, then acor is saved
        ! for use in a possible order increase on the next step.
        ! if etamax = 1 (a failure occurred this step), keep nqwait >= 2.
        kflag = 0
        nst = nst + 1
        hu = h
        nqu = nq
        do 470 iback = 1, nq
            i = l - iback
470     tau(i+1) = tau(i)
        tau(1) = h
        do 480 j = 1, l
            call daxpy (n, el(j), acor, 1, yh(1,j), 1 )
480     continue

        nqwait = nqwait - 1
        if ((l == lmax) .or. (nqwait /= 1)) go to 490
        call dcopy (n, acor, 1, yh(1,lmax), 1 )
        conp = tq(5)
490     if (etamax /= one) go to 560
        if (nqwait < 2) nqwait = 2
        
        newq = nq
        newh = 0
        eta = one
        hnew = h
        go to 690
        
        !-----------------------------------------------------------------------
        ! the error test failed.  kflag keeps track of multiple failures.
        ! restore tn and the yh array to their previous values, and prepare
        ! to try the step again.  compute the optimum step size for the
        ! same order.  after repeated failures, h is forced to decrease
        ! more rapidly.
500     kflag = kflag - 1
        netf = netf + 1
        nflag = -2
        tn = told
        i1 = nqnyh + 1
        do 520 jb = 1, nq
            i1 = i1 - ldyh
            do 510 i = i1, nqnyh
510         yh1(i) = yh1(i) - yh1(i+ldyh)
520     continue
        if (ABS(h) <= hmin*onepsm) go to 660
        etamax = one
        if (kflag <= kfc) go to 530
        
        ! compute ratio of new h to current h at the current order. 
        flotl = real(l)
        eta = one/((bias2*dsm)**(one/flotl) + addon)
        eta = MAX(eta,hmin/ABS(h),etamin)
        if ((kflag <= -2) .and. (eta > etamxf)) eta = etamxf
        go to 150
        
        !-----------------------------------------------------------------------
        ! control reaches this section if 3 or more consecutive failures
        ! have occurred.  it is assumed that the elements of the yh array
        ! have accumulated errors of the wrong order.  the order is reduced
        ! by one, if possible.  then h is reduced by a factor of 0.1 and
        ! the step is retried.  after a total of 7 consecutive failures,
        ! an exit is taken with kflag = -1.
530     if (kflag == kfh) go to 660
        if (nq == 1) go to 540
        eta = MAX(etamin,hmin/ABS(h))
        call dvjust (yh, ldyh, -1)
        l = nq
        nq = nq - 1
        nqwait = l
        go to 150
540     eta = MAX(etamin,hmin/ABS(h))
        h = h*eta
        hscal = h
        tau(1) = h
        call f (n, tn, y, savf, rpar, ipar)
        nfe = nfe + 1
        do 550 i = 1, n
550     yh(i,2) = h*savf(i)
        nqwait = 10
        go to 200
        
        !-----------------------------------------------------------------------
        ! if nqwait = 0, an increase or decrease in order by one is considered.
        ! factors etaq, etaqm1, etaqp1 are computed by which h could
        ! be multiplied at order q, q-1, or q+1, respectively.
        ! the largest of these is determined, and the new order and
        ! step size set accordingly.
        ! a change of h or nq is made only if h increases by at least a
        ! factor of thresh.  if an order change is considered and rejected,
        ! then nqwait is set to 2 (reconsider it after 2 steps).
        
        ! compute ratio of new h to current h at the current order. 
560     flotl = real(l)
        etaq = one/((bias2*dsm)**(one/flotl) + addon)
        if (nqwait /= 0) go to 600
        nqwait = 2
        etaqm1 = zero
        if (nq == 1) go to 570
        
        ! compute ratio of new h to current h at the current order less one. 
        ddn = dvnorm (n, yh(1,l), ewt)/tq(1)
        etaqm1 = one/((bias1*ddn)**(one/(flotl - one)) + addon)
570     etaqp1 = zero
        if (l == lmax) go to 580
        
        ! compute ratio of new h to current h at current order plus one. 
        cnquot = (tq(5)/conp)*(h/tau(2))**l
        do 575 i = 1, n
575     savf(i) = acor(i) - cnquot*yh(i,lmax)
        dup = dvnorm (n, savf, ewt)/tq(3)
        etaqp1 = one/((bias3*dup)**(one/(flotl + one)) + addon)
580     if (etaq >= etaqp1) go to 590
        if (etaqp1 > etaqm1) go to 620
        go to 610
590     if (etaq < etaqm1) go to 610
600     eta = etaq
        newq = nq
        go to 630
610     eta = etaqm1
        newq = nq - 1
        go to 630
620     eta = etaqp1
        newq = nq + 1
        call dcopy (n, acor, 1, yh(1,lmax), 1)
        
        ! test tentative new h against thresh, etamax, and hmxi, then exit. 
630     if (eta < thresh .or. etamax == one) go to 640
        eta = MIN(eta,etamax)
        eta = eta/MAX(one,ABS(h)*hmxi*eta)
        newh = 1
        hnew = h*eta
        go to 690
640     newq = nq
        newh = 0
        eta = one
        hnew = h
        go to 690
        
        !-----------------------------------------------------------------------
        ! all returns are made through this section.
        ! on a successful return, etamax is reset and acor is scaled.
660     kflag = -1
        go to 720
670     kflag = -2
        go to 720
680     if (nflag == -2) kflag = -3
        if (nflag == -3) kflag = -4
        go to 720
690     etamax = etamx3
        if (nst <= 10) etamax = etamx2
700     r = one/tq(2)
        call dscal (n, r, acor, 1)
720     jstart = 1
        
        return
    end subroutine dvstep

    !$
    !===============================================================================================
    ! call sequence communication: none
    ! common block variables accessed:
    !     /dvod01/ -- el(13), h, tau(13), tq(5), l(= nq + 1),
    !                 meth, nq, nqwait
    !
    ! subroutines called by dvset: none
    ! function routines called by dvset: none
    !---------------------------------------------------------------------------
    ! dvset is called by dvstep and sets coefficients for use there.
    !
    ! for each order nq, the coefficients in el are calculated by use of
    !  the generating polynomial lambda(x), with coefficients el(i).
    !      lambda(x) = el(1) + el(2)*x + ... + el(nq+1)*(x**nq).
    ! for the backward differentiation formulas,
    !                                     nq-1
    !      lambda(x) = (1 + x/xi*(nq)) * product (1 + x/xi(i) ) .
    !                                     i = 1
    ! for the adams formulas,
    !                              nq-1
    !      (d/dx) lambda(x) = c * product (1 + x/xi(i) ) ,
    !                              i = 1
    !      lambda(-1) = 0,    lambda(0) = 1,
    ! where c is a normalization constant.
    ! in both cases, xi(i) is defined by
    !      h*xi(i) = t sub n  -  t sub (n-i)
    !              = h + tau(1) + tau(2) + ... tau(i-1).
    !
    !
    ! in addition to variables described previously, communication
    ! with dvset uses the following:
    !   tau    = a vector of length 13 containing the past nq values
    !            of h.
    !   el     = a vector of length 13 in which vset stores the
    !            coefficients for the corrector formula.
    !   tq     = a vector of length 5 in which vset stores constants
    !            used for the convergence test, the error test, and the
    !            selection of h at a new order.
    !   meth   = the basic method indicator.
    !   nq     = the current order.
    !   l      = nq + 1, the length of the vector stored in el, and
    !            the number of columns of the yh array being used.
    !   nqwait = a counter controlling the frequency of order changes.
    !            an order change is about to be considered if nqwait = 1.
    !===============================================================================================
    subroutine dvset ()

        ! type declarations for labeled common block dvod01 
        double precision acnrm, ccmxj, conp, crate, drc, el, eta, etamax, h, hmin, hmxi, hnew, hscal, prl1, rc, rl1, tau, tq, tn, uround
        integer icf, init, ipup, jcur, jstart, jsv, kflag, kuth, l, lmax, lyh, lewt, lacor, lsavf, lwm, liwm, locjs, maxord, meth, miter, msbj, mxhnil, mxstep,     &
            &   n, newh, newq, nhnil, nq, nqnyh, nqwait, nslj, nslp, nyh
        
        ! type declarations for local variables 
        double precision ahatn0, alph0, cnqm1, cortes, csum, elp, em, em0, floti, flotl, flotnq, hsum, one, rxi, rxis, s, six, t1, t2, t3, t4, t5, t6, two, xi, zero
        integer i, iback, j, jp1, nqm1, nqm2
        dimension em(13)
        
        !-----------------------------------------------------------------------
        ! the following fortran-77 declaration is to cause the values of the
        ! listed (local) variables to be saved between calls to this integrator.
        save cortes, one, six, two, zero
        
        common /dvod01/ acnrm, ccmxj, conp, crate, drc, el(13), eta, etamax, h, hmin, hmxi, hnew, hscal, prl1, rc, rl1, tau(13), tq(5), tn, uround,         &
            &   icf, init, ipup, jcur, jstart, jsv, kflag, kuth, l, lmax, lyh, lewt, lacor, lsavf, lwm, liwm, locjs, maxord, meth, miter, msbj, mxhnil, mxstep,     &
            &   n, newh, newq, nhnil, nq, nqnyh, nqwait, nslj, nslp, nyh
        
        data cortes /0.1d0/
        data one  /1.0d0/, six /6.0d0/, two /2.0d0/, zero /0.0d0/
        
        flotl = real(l)
        nqm1 = nq - 1
        nqm2 = nq - 2
        go to (100, 200), meth
        
        ! set coefficients for adams methods. 
100     if (nq /= 1) go to 110
        el(1) = one
        el(2) = one
        tq(1) = one
        tq(2) = two
        tq(3) = six*tq(2)
        tq(5) = one
        go to 300
        
110     hsum = h
        em(1) = one
        flotnq = flotl - one
        do 115 i = 2, l
115     em(i) = zero
        do 150 j = 1, nqm1
            if ((j /= nqm1) .or. (nqwait /= 1)) go to 130
            s = one
            csum = zero
            do 120 i = 1, nqm1
                csum = csum + s*em(i)/real(i+1)
120         s = -s
            tq(1) = em(nqm1)/(flotnq*csum)
130         rxi = h/hsum
            do 140 iback = 1, j
                i = (j + 2) - iback
140         em(i) = em(i) + em(i-1)*rxi
            hsum = hsum + tau(j)
150     continue

        ! compute integral from -1 to 0 of polynomial and of x times it. 
        s = one
        em0 = zero
        csum = zero
        do 160 i = 1, nq
            floti = real(i)
            em0 = em0 + s*em(i)/floti
            csum = csum + s*em(i)/(floti+one)
160     s = -s

        ! in el, form coefficients of normalized integrated polynomial. 
        s = one/em0
        el(1) = one
        do 170 i = 1, nq
170     el(i+1) = s*em(i)/real(i)
        xi = hsum/h
        tq(2) = xi*em0/csum
        tq(5) = xi/el(l)
        if (nqwait /= 1) go to 300
        
        ! for higher order control constant, multiply polynomial by 1+x/xi(q). 
        rxi = one/xi
        do 180 iback = 1, nq
            i = (l + 1) - iback
180     em(i) = em(i) + em(i-1)*rxi

        ! compute integral of polynomial. 
        s = one
        csum = zero
        do 190 i = 1, l
            csum = csum + s*em(i)/real(i+1)
190     s = -s
        tq(3) = flotl*em0/csum
        go to 300
        
        ! set coefficients for bdf methods. 
200     do 210 i = 3, l
210     el(i) = zero
        el(1) = one
        el(2) = one
        alph0 = -one
        ahatn0 = -one
        hsum = h
        rxi = one
        rxis = one
        if (nq == 1) go to 240
        
        do 230 j = 1, nqm2
            ! in el, construct coefficients of (1+x/xi(1))*...*(1+x/xi(j+1)). 
            hsum = hsum + tau(j)
            rxi = h/hsum
            jp1 = j + 1
            alph0 = alph0 - one/real(jp1)
            do 220 iback = 1, jp1
                i = (j + 3) - iback
220         el(i) = el(i) + el(i-1)*rxi
230     continue

        alph0 = alph0 - one/real(nq)
        rxis = -el(2) - alph0
        hsum = hsum + tau(nqm1)
        rxi = h/hsum
        ahatn0 = -el(2) - rxi
        do 235 iback = 1, nq
            i = (nq + 2) - iback
235     el(i) = el(i) + el(i-1)*rxis

240     t1 = one - ahatn0 + alph0
        t2 = one + real(nq)*t1
        tq(2) = ABS(alph0*t2/t1)
        tq(5) = ABS(t2/(el(l)*rxi/rxis))
        if (nqwait /= 1) go to 300
        cnqm1 = rxis/el(l)
        t3 = alph0 + one/real(nq)
        t4 = ahatn0 + rxi
        elp = t3/(one - t4 + t3)
        tq(1) = ABS(elp/cnqm1)
        hsum = hsum + tau(nq)
        rxi = h/hsum
        t5 = alph0 - one/real(nq+1)
        t6 = ahatn0 - rxi
        elp = t2/(one - t6 + t5)
        tq(3) = ABS(elp*rxi*(flotl + one)*t5)
300     tq(4) = cortes*tq(2)
        
        return
    end subroutine dvset

    !$
    !===============================================================================================
    ! call sequence input -- yh, ldyh, iord
    ! call sequence output -- yh
    ! common block input -- nq, meth, lmax, hscal, tau(13), n
    ! common block variables accessed:
    !     /dvod01/ -- hscal, tau(13), lmax, meth, n, nq,
    !
    ! subroutines called by dvjust: daxpy
    ! function routines called by dvjust: none
    !---------------------------------------------------------------------------
    ! this subroutine adjusts the yh array on reduction of order,
    ! and also when the order is increased for the stiff option (meth = 2).
    ! communication with dvjust uses the following:
    ! iord  = an integer flag used when meth = 2 to indicate an order
    !         increase (iord = +1) or an order decrease (iord = -1).
    ! hscal = step size h used in scaling of nordsieck array yh.
    !         (if iord = +1, dvjust assumes that hscal = tau(1).)
    ! see references 1 and 2 for details.
    !===============================================================================================
    subroutine dvjust (yh, ldyh, iord)
    
        double precision yh
        integer ldyh, iord
        dimension yh(ldyh,*)

        ! type declarations for labeled common block dvod01 
        double precision acnrm, ccmxj, conp, crate, drc, el, eta, etamax, h, hmin, hmxi, hnew, hscal, prl1, rc, rl1, tau, tq, tn, uround
        integer icf, init, ipup, jcur, jstart, jsv, kflag, kuth, l, lmax, lyh, lewt, lacor, lsavf, lwm, liwm, locjs, maxord, meth, miter, msbj, mxhnil, mxstep,     &
            &   n, newh, newq, nhnil, nq, nqnyh, nqwait, nslj, nslp, nyh
        
        ! type declarations for local variables 
        double precision alph0, alph1, hsum, one, prod, t1, xi,xiold, zero
        integer i, iback, j, jp1, lp1, nqm1, nqm2, nqp1
        
        !-----------------------------------------------------------------------
        ! the following fortran-77 declaration is to cause the values of the
        ! listed (local) variables to be saved between calls to this integrator.
        save one, zero
        
        common /dvod01/ acnrm, ccmxj, conp, crate, drc, el(13), eta, etamax, h, hmin, hmxi, hnew, hscal, prl1, rc, rl1, tau(13), tq(5), tn, uround,         &
            &   icf, init, ipup, jcur, jstart, jsv, kflag, kuth, l, lmax, lyh, lewt, lacor, lsavf, lwm, liwm, locjs, maxord, meth, miter, msbj, mxhnil, mxstep,     &
            &   n, newh, newq, nhnil, nq, nqnyh, nqwait, nslj, nslp, nyh
        
        data one /1.0d0/, zero /0.0d0/
        
        if ((nq == 2) .and. (iord /= 1)) return
        nqm1 = nq - 1
        nqm2 = nq - 2
        
        ! nonstiff option. check to see if the order is being increased or decreased.
        go to (100, 200), meth
100     continue
        if (iord == 1) go to 180
        ! order decrease. 
        do 110 j = 1, lmax
110     el(j) = zero
        el(2) = one
        hsum = zero
        
        do 130 j = 1, nqm2
            ! construct coefficients of x*(x+xi(1))*...*(x+xi(j)). 
            hsum = hsum + tau(j)
            xi = hsum/hscal
            jp1 = j + 1
            do 120 iback = 1, jp1
                i = (j + 3) - iback
120         el(i) = el(i)*xi + el(i-1)
130     continue

        ! construct coefficients of integrated polynomial. 
        do 140 j = 2, nqm1
140     el(j+1) = real(nq)*el(j)/real(j)
        ! subtract correction terms from yh array. 
        do 170 j = 3, nq
            do 160 i = 1, n
160         yh(i,j) = yh(i,j) - yh(i,l)*el(j)
170     continue
        return
        
        ! order increase. zero out next column in yh array. 
180     continue
        lp1 = l + 1
        do 190 i = 1, n
190     yh(i,lp1) = zero
        return
        
        ! stiff option. check to see if the order is being increased or decreased.
200     continue
        if (iord == 1) go to 300
        
        ! order decrease. 
        do 210 j = 1, lmax
210     el(j) = zero
        el(3) = one
        hsum = zero
        do 230 j = 1,nqm2
            ! construct coefficients of x*x*(x+xi(1))*...*(x+xi(j)). 
            hsum = hsum + tau(j)
            xi = hsum/hscal
            jp1 = j + 1
            do 220 iback = 1, jp1
                i = (j + 4) - iback
220         el(i) = el(i)*xi + el(i-1)
230     continue

        ! subtract correction terms from yh array. 
        do 250 j = 3,nq
            do 240 i = 1, n
240         yh(i,j) = yh(i,j) - yh(i,l)*el(j)
250     continue
        return
        
        ! order increase. 
300     do 310 j = 1, lmax
310     el(j) = zero
        el(3) = one
        alph0 = -one
        alph1 = one
        prod = one
        xiold = one
        hsum = hscal
        if (nq == 1) go to 340
        do 330 j = 1, nqm1
            ! construct coefficients of x*x*(x+xi(1))*...*(x+xi(j)). 
            jp1 = j + 1
            hsum = hsum + tau(jp1)
            xi = hsum/hscal
            prod = prod*xi
            alph0 = alph0 - one/real(jp1)
            alph1 = alph1 + one/xi
            do 320 iback = 1, jp1
                i = (j + 4) - iback
320         el(i) = el(i)*xiold + el(i-1)
            xiold = xi
330     continue
340     continue

        t1 = (-alph0 - alph1)/prod
        ! load column l + 1 in yh array. 
        lp1 = l + 1
        do 350 i = 1, n
350     yh(i,lp1) = t1*yh(i,lmax)
        ! add correction terms to yh array. 
        nqp1 = nq + 1
        
        do 370 j = 3, nqp1
            call daxpy (n, el(j), yh(1,lp1), 1, yh(1,j), 1 )
370     continue
        
        return
    end subroutine dvjust

    !$
    !===============================================================================================
    ! call sequence input -- y, yh, ldyh, savf, ewt, acor, iwm, wm,
    !                        f, jac, nflag, rpar, ipar
    ! call sequence output -- yh, acor, wm, iwm, nflag
    ! common block variables accessed:
    !     /dvod01/ acnrm, crate, drc, h, rc, rl1, tq(5), tn, icf,
    !                jcur, meth, miter, n, nslp
    !     /dvod02/ hu, ncfn, netf, nfe, nje, nlu, nni, nqu, nst
    !
    ! subroutines called by dvnlsd: f, daxpy, dcopy, dscal, dvjac, dvsol
    ! function routines called by dvnlsd: dvnorm
    !---------------------------------------------------------------------------
    ! subroutine dvnlsd is a nonlinear system solver, which uses functional
    ! iteration or a chord (modified newton) method.  for the chord method
    ! direct linear algebraic system solvers are used.  subroutine dvnlsd
    ! then handles the corrector phase of this integration package.
    !
    ! communication with dvnlsd is done with the following variables. (for
    ! more details, please see the comments in the driver subroutine.)
    !
    ! y          = the dependent variable, a vector of length n, input.
    ! yh         = the nordsieck (taylor) array, ldyh by lmax, input
    !              and output.  on input, it contains predicted values.
    ! ldyh       = a constant >= n, the first dimension of yh, input.
    ! vsav       = unused work array.
    ! savf       = a work array of length n.
    ! ewt        = an error weight vector of length n, input.
    ! acor       = a work array of length n, used for the accumulated
    !              corrections to the predicted y vector.
    ! wm,iwm     = real and integer work arrays associated with matrix
    !              operations in chord iteration (miter /= 0).
    ! f          = dummy name for user supplied routine for f.
    ! jac        = dummy name for user supplied jacobian routine.
    ! pdum       = unused dummy subroutine name.  included for uniformity
    !              over collection of integrators.
    ! nflag      = input/output flag, with values and meanings as follows:
    !              input
    !                  0 first call for this time step.
    !                 -1 convergence failure in previous call to dvnlsd.
    !                 -2 error test failure in dvstep.
    !              output
    !                  0 successful completion of nonlinear solver.
    !                 -1 convergence failure or singular matrix.
    !                 -2 unrecoverable error in matrix preprocessing
    !                    (cannot occur here).
    !                 -3 unrecoverable error in solution (cannot occur
    !                    here).
    ! rpar, ipar = dummy names for user's real and integer work arrays.
    !
    ! ipup       = own variable flag with values and meanings as follows:
    !              0,            do not update the newton matrix.
    !              miter /= 0, update newton matrix, because it is the
    !                            initial step, order was changed, the error
    !                            test failed, or an update is indicated by
    !                            the scalar rc or step counter nst.
    !
    ! for more details, see comments in driver subroutine.
    !===============================================================================================
    subroutine dvnlsd (y, yh, ldyh, vsav, savf, ewt, acor, iwm, wm, f, jac, pdum, nflag, rpar, ipar)
    
        external f, jac, pdum
        double precision y, yh, vsav, savf, ewt, acor, wm, rpar
        integer ldyh, iwm, nflag, ipar
        dimension y(*), yh(ldyh,*), vsav(*), savf(*), ewt(*), acor(*), iwm(*), wm(*), rpar(*), ipar(*)

        ! type declarations for labeled common block dvod01 
        double precision acnrm, ccmxj, conp, crate, drc, el, eta, etamax, h, hmin, hmxi, hnew, hscal, prl1, rc, rl1, tau, tq, tn, uround
        integer icf, init, ipup, jcur, jstart, jsv, kflag, kuth, l, lmax, lyh, lewt, lacor, lsavf, lwm, liwm, locjs, maxord, meth, miter, msbj, mxhnil, mxstep,     &
            &   n, newh, newq, nhnil, nq, nqnyh, nqwait, nslj, nslp, nyh
        
        ! type declarations for labeled common block dvod02 
        double precision hu
        integer ncfn, netf, nfe, nje, nlu, nni, nqu, nst
        
        ! type declarations for local variables 
        double precision ccmax, crdown, cscale, dcon, del, delp, one, rdiv, two, zero
        integer i, ierpj, iersl, m, maxcor, msbp
        
        !-----------------------------------------------------------------------
        ! the following fortran-77 declaration is to cause the values of the
        ! listed (local) variables to be saved between calls to this integrator.
        save ccmax, crdown, maxcor, msbp, rdiv, one, two, zero
        
        common /dvod01/ acnrm, ccmxj, conp, crate, drc, el(13), eta, etamax, h, hmin, hmxi, hnew, hscal, prl1, rc, rl1, tau(13), tq(5), tn, uround,         &
            &   icf, init, ipup, jcur, jstart, jsv, kflag, kuth, l, lmax, lyh, lewt, lacor, lsavf, lwm, liwm, locjs, maxord, meth, miter, msbj, mxhnil, mxstep,     &
            &   n, newh, newq, nhnil, nq, nqnyh, nqwait, nslj, nslp, nyh
        common /dvod02/ hu, ncfn, netf, nfe, nje, nlu, nni, nqu, nst
        
        data ccmax /0.3d0/, crdown /0.3d0/, maxcor /3/, msbp /20/, rdiv  /2.0d0/
        data one /1.0d0/, two /2.0d0/, zero /0.0d0/
        
        !-----------------------------------------------------------------------
        ! on the first step, on a change of method order, or after a
        ! nonlinear convergence failure with nflag = -2, set ipup = miter
        ! to force a jacobian update when miter /= 0.
        if (jstart == 0) nslp = 0
        if (nflag == 0) icf = 0
        if (nflag == -2) ipup = miter
        if ( (jstart == 0) .or. (jstart == -1) ) ipup = miter
        
        ! if this is functional iteration, set crate == 1 and drop to 220
        if (miter == 0) then
            crate = one
            go to 220
        end if
        
        !-----------------------------------------------------------------------
        ! rc is the ratio of new to old values of the coefficient h/el(2)=h/l1.
        ! when rc differs from 1 by more than ccmax, ipup is set to miter
        ! to force dvjac to be called, if a jacobian is involved.
        ! in any case, dvjac is called at least every msbp steps.
        drc = ABS(rc-one)
        if (drc > ccmax .or. nst >= nslp+msbp) ipup = miter
        
        !-----------------------------------------------------------------------
        ! up to maxcor corrector iterations are taken.  a convergence test is
        ! made on the r.m.s. norm of each correction, weighted by the error
        ! weight vector ewt.  the sum of the corrections is accumulated in the
        ! vector acor(i).  the yh array is not altered in the corrector loop.
220     m = 0
        delp = zero
        call dcopy (n, yh(1,1), 1, y, 1 )
        call f (n, tn, y, savf, rpar, ipar)
        nfe = nfe + 1
        if (ipup <= 0) go to 250
        
        !-----------------------------------------------------------------------
        ! if indicated, the matrix p = i - h*rl1*j is reevaluated and
        ! preprocessed before starting the corrector iteration.  ipup is set
        ! to 0 as an indicator that this has been done.
        call dvjac (y, yh, ldyh, ewt, acor, savf, wm, iwm, f, jac, ierpj, rpar, ipar)
        ipup = 0
        rc = one
        drc = zero
        crate = one
        nslp = nst
        
        ! if matrix is singular, take error return to force cut in step size. 
        if (ierpj /= 0) go to 430
250     do 260 i = 1,n
260     acor(i) = zero
        ! this is a looping point for the corrector iteration. 
270     if (miter /= 0) go to 350

        ! in the case of functional iteration, update y directly from the result of the last function evaluation.
        do 280 i = 1,n
280     savf(i) = rl1*(h*savf(i) - yh(i,2))
        do 290 i = 1,n
290     y(i) = savf(i) - acor(i)
        del = dvnorm (n, y, ewt)
        do 300 i = 1,n
300     y(i) = yh(i,1) + savf(i)
        call dcopy (n, savf, 1, acor, 1)
        go to 400
        
        !-----------------------------------------------------------------------
        ! in the case of the chord method, compute the corrector error,
        ! and solve the linear system with that as right-hand side and
        ! p as coefficient matrix.  the correction is scaled by the factor
        ! 2/(1+rc) to account for changes in h*rl1 since the last dvjac call.
350     do 360 i = 1,n
360     y(i) = (rl1*h)*savf(i) - (rl1*yh(i,2) + acor(i))
        call dvsol (wm, iwm, y, iersl)
        nni = nni + 1
        if (iersl > 0) go to 410
        if (meth == 2 .and. rc /= one) then
            cscale = two/(one + rc)
            call dscal (n, cscale, y, 1)
        end if
        del = dvnorm (n, y, ewt)
        call daxpy (n, one, y, 1, acor, 1)
        do 380 i = 1,n
380     y(i) = yh(i,1) + acor(i)

        ! test for convergence.  if m > 0, an estimate of the convergence rate constant is stored in crate, and this is used in the test.
400     if (m /= 0) crate = MAX(crdown*crate,del/delp)
        dcon = del*MIN(one,crate)/tq(4)
        if (dcon <= one) go to 450
        m = m + 1
        if (m == maxcor) go to 410
        if (m >= 2 .and. del > rdiv*delp) go to 410
        delp = del
        call f (n, tn, y, savf, rpar, ipar)
        nfe = nfe + 1
        go to 270
        
410     if (miter == 0 .or. jcur == 1) go to 430
        icf = 1
        ipup = miter
        go to 220
        
430     continue
        nflag = -1
        icf = 2
        ipup = miter
        return
        
        ! return for successful step. 
450     nflag = 0
        jcur = 0
        icf = 0
        if (m == 0) acnrm = del
        if (m > 0) acnrm = dvnorm (n, acor, ewt)
        
        return
    end subroutine dvnlsd

    !$
    !===============================================================================================
    ! call sequence input -- y, yh, ldyh, ewt, ftem, savf, wm, iwm,
    !                        f, jac, rpar, ipar
    ! call sequence output -- wm, iwm, ierpj
    ! common block variables accessed:
    !     /dvod01/  ccmxj, drc, h, rl1, tn, uround, icf, jcur, locjs,
    !               miter, msbj, n, nslj
    !     /dvod02/  nfe, nst, nje, nlu
    !
    ! subroutines called by dvjac: f, jac, dacopy, dcopy, dgbfa, dgefa,
    !                              dscal
    ! function routines called by dvjac: dvnorm
    !---------------------------------------------------------------------------
    ! dvjac is called by dvnlsd to compute and process the matrix
    ! p = i - h*rl1*j , where j is an approximation to the jacobian.
    ! here j is computed by the user-supplied routine jac if
    ! miter = 1 or 4, or by finite differencing if miter = 2, 3, or 5.
    ! if miter = 3, a diagonal approximation to j is used.
    ! if jsv = -1, j is computed from scratch in all cases.
    ! if jsv = 1 and miter = 1, 2, 4, or 5, and if the saved value of j is
    ! considered acceptable, then p is constructed from the saved j.
    ! j is stored in wm and replaced by p.  if miter /= 3, p is then
    ! subjected to lu decomposition in preparation for later solution
    ! of linear systems with p as coefficient matrix. this is done
    ! by dgefa if miter = 1 or 2, and by dgbfa if miter = 4 or 5.
    !
    ! communication with dvjac is done with the following variables.  (for
    ! more details, please see the comments in the driver subroutine.)
    ! y          = vector containing predicted values on entry.
    ! yh         = the nordsieck array, an ldyh by lmax array, input.
    ! ldyh       = a constant >= n, the first dimension of yh, input.
    ! ewt        = an error weight vector of length n.
    ! savf       = array containing f evaluated at predicted y, input.
    ! wm         = real work space for matrices.  in the output, it contains
    !              the inverse diagonal matrix if miter = 3 and the lu
    !              decomposition of p if miter is 1, 2 , 4, or 5.
    !              storage of matrix elements starts at wm(3).
    !              storage of the saved jacobian starts at wm(locjs).
    !              wm also contains the following matrix-related data:
    !              wm(1) = SQRT(uround), used in numerical jacobian step.
    !              wm(2) = h*rl1, saved for later use if miter = 3.
    ! iwm        = integer work space containing pivot information,
    !              starting at iwm(31), if miter is 1, 2, 4, or 5.
    !              iwm also contains band parameters ml = iwm(1) and
    !              mu = iwm(2) if miter is 4 or 5.
    ! f          = dummy name for the user supplied subroutine for f.
    ! jac        = dummy name for the user supplied jacobian subroutine.
    ! rpar, ipar = dummy names for user's real and integer work arrays.
    ! rl1        = 1/el(2) (input).
    ! ierpj      = output error flag,  = 0 if no trouble, 1 if the p
    !              matrix is found to be singular.
    ! jcur       = output flag to indicate whether the jacobian matrix
    !              (or approximation) is now current.
    !              jcur = 0 means j is not current.
    !              jcur = 1 means j is current.
    !===============================================================================================
    subroutine dvjac (y, yh, ldyh, ewt, ftem, savf, wm, iwm, f, jac, ierpj, rpar, ipar)
    
        external f, jac
        double precision y, yh, ewt, ftem, savf, wm, rpar
        integer ldyh, iwm, ierpj, ipar
        dimension y(*), yh(ldyh,*), ewt(*), ftem(*), savf(*), wm(*), iwm(*), rpar(*), ipar(*)

        ! type declarations for labeled common block dvod01 
        double precision acnrm, ccmxj, conp, crate, drc, el, eta, etamax, h, hmin, hmxi, hnew, hscal, prl1, rc, rl1, tau, tq, tn, uround
        integer icf, init, ipup, jcur, jstart, jsv, kflag, kuth, l, lmax, lyh, lewt, lacor, lsavf, lwm, liwm, locjs, maxord, meth, miter, msbj, mxhnil, mxstep,     &
            &   n, newh, newq, nhnil, nq, nqnyh, nqwait, nslj, nslp, nyh
        
        ! type declarations for labeled common block dvod02 
        double precision hu
        integer ncfn, netf, nfe, nje, nlu, nni, nqu, nst
        
        ! type declarations for local variables 
        double precision con, di, fac, hrl1, one, pt1, r, r0, srur, thou, yi, yj, yjj, zero
        integer i, i1, i2, ier, ii, j, j1, jj, jok, lenp, mba, mband, meb1, meband, ml, ml3, mu, np1
        
        !-----------------------------------------------------------------------
        ! the following fortran-77 declaration is to cause the values of the
        ! listed (local) variables to be saved between calls to this subroutine.
        save one, pt1, thou, zero
        
        common /dvod01/ acnrm, ccmxj, conp, crate, drc, el(13), eta, etamax, h, hmin, hmxi, hnew, hscal, prl1, rc, rl1, tau(13), tq(5), tn, uround,         &
            &   icf, init, ipup, jcur, jstart, jsv, kflag, kuth, l, lmax, lyh, lewt, lacor, lsavf, lwm, liwm, locjs, maxord, meth, miter, msbj, mxhnil, mxstep,     &
            &   n, newh, newq, nhnil, nq, nqnyh, nqwait, nslj, nslp, nyh
        common /dvod02/ hu, ncfn, netf, nfe, nje, nlu, nni, nqu, nst
        
        data one /1.0d0/, thou /1000.0d0/, zero /0.0d0/, pt1 /0.1d0/
        
        ierpj = 0
        hrl1 = h*rl1
        
        ! see whether j should be evaluated (jok = -1) or not (jok = 1). 
        jok = jsv
        if (jsv == 1) then
            if (nst == 0 .or. nst > nslj+msbj) jok = -1
            if (icf == 1 .and. drc < ccmxj) jok = -1
            if (icf == 2) jok = -1
        end if
        ! end of setting jok. 
        
        if (jok == -1 .and. miter == 1) then
            ! if jok = -1 and miter = 1, call jac to evaluate jacobian. 
            nje = nje + 1
            nslj = nst
            jcur = 1
            lenp = n*n
            do 110 i = 1,lenp
110         wm(i+2) = zero
            call jac (n, tn, y, 0, 0, wm(3), n, rpar, ipar)
            if (jsv == 1) call dcopy (lenp, wm(3), 1, wm(locjs), 1)
        end if
        
        if (jok == -1 .and. miter == 2) then
            ! if miter = 2, make n calls to f to approximate the jacobian. 
            nje = nje + 1
            nslj = nst
            jcur = 1
            fac = dvnorm (n, savf, ewt)
            r0 = thou*ABS(h)*uround*real(n)*fac
            if (r0 == zero) r0 = one
            srur = wm(1)
            j1 = 2
            do 230 j = 1,n
                yj = y(j)
                r = MAX(srur*ABS(yj),r0/ewt(j))
                y(j) = y(j) + r
                fac = one/r
                call f (n, tn, y, ftem, rpar, ipar)
                do 220 i = 1,n
220             wm(i+j1) = (ftem(i) - savf(i))*fac
                y(j) = yj
                j1 = j1 + n
230         continue
            nfe = nfe + n
            lenp = n*n
            if (jsv == 1) call dcopy (lenp, wm(3), 1, wm(locjs), 1)
        end if
        
        if (jok == 1 .and. (miter == 1 .or. miter == 2)) then
            jcur = 0
            lenp = n*n
            call dcopy (lenp, wm(locjs), 1, wm(3), 1)
        end if
        
        if (miter == 1 .or. miter == 2) then
            ! multiply jacobian by scalar, add identity, and do lu decomposition. 
            con = -hrl1
            call dscal (lenp, con, wm(3), 1)
            j = 3
            np1 = n + 1
            do 250 i = 1,n
                wm(j) = wm(j) + one
250         j = j + np1
            nlu = nlu + 1
            call dgefa (wm(3), n, n, iwm(31), ier)
            if (ier /= 0) ierpj = 1
            return
        end if
        ! end of code block for miter = 1 or 2. 
        
        if (miter == 3) then
            ! if miter = 3, construct a diagonal approximation to j and p. 
            nje = nje + 1
            jcur = 1
            wm(2) = hrl1
            r = rl1*pt1
            do 310 i = 1,n
310         y(i) = y(i) + r*(h*savf(i) - yh(i,2))
            call f (n, tn, y, wm(3), rpar, ipar)
            nfe = nfe + 1
            do 320 i = 1,n
                r0 = h*savf(i) - yh(i,2)
                di = pt1*r0 - h*(wm(i+2) - savf(i))
                wm(i+2) = one
                if (ABS(r0) < uround/ewt(i)) go to 320
                if (ABS(di) == zero) go to 330
                wm(i+2) = pt1*r0/di
320         continue
            return
330         ierpj = 1
            return
        end if
        ! end of code block for miter = 3. 
        
        ! set constants for miter = 4 or 5. 
        ml = iwm(1)
        mu = iwm(2)
        ml3 = ml + 3
        mband = ml + mu + 1
        meband = mband + ml
        lenp = meband*n
        
        if (jok == -1 .and. miter == 4) then
            ! if jok = -1 and miter = 4, call jac to evaluate jacobian. 
            nje = nje + 1
            nslj = nst
            jcur = 1
            do 410 i = 1,lenp
410         wm(i+2) = zero
            call jac (n, tn, y, ml, mu, wm(ml3), meband, rpar, ipar)
            if (jsv == 1) call dacopy (mband, n, wm(ml3), meband, wm(locjs), mband)
        end if
        
        if (jok == -1 .and. miter == 5) then
            ! if miter = 5, make ml+mu+1 calls to f to approximate the jacobian. 
            nje = nje + 1
            nslj = nst
            jcur = 1
            mba = MIN(mband,n)
            meb1 = meband - 1
            srur = wm(1)
            fac = dvnorm (n, savf, ewt)
            r0 = thou*ABS(h)*uround*real(n)*fac
            if (r0 == zero) r0 = one
            do 560 j = 1,mba
                do 530 i = j,n,mband
                    yi = y(i)
                    r = MAX(srur*ABS(yi),r0/ewt(i))
530             y(i) = y(i) + r
                call f (n, tn, y, ftem, rpar, ipar)
                do 550 jj = j,n,mband
                    y(jj) = yh(jj,1)
                    yjj = y(jj)
                    r = MAX(srur*ABS(yjj),r0/ewt(jj))
                    fac = one/r
                    i1 = MAX(jj-mu,1)
                    i2 = MIN(jj+ml,n)
                    ii = jj*meb1 - ml + 2
                    do 540 i = i1,i2
540                 wm(ii+i) = (ftem(i) - savf(i))*fac
550             continue
560         continue
            nfe = nfe + mba
            if (jsv == 1) call dacopy (mband, n, wm(ml3), meband, wm(locjs), mband)
        end if
        
        if (jok == 1) then
            jcur = 0
            call dacopy (mband, n, wm(locjs), mband, wm(ml3), meband)
        end if
        
        ! multiply jacobian by scalar, add identity, and do lu decomposition.
        con = -hrl1
        call dscal (lenp, con, wm(3), 1 )
        ii = mband + 2
        do 580 i = 1,n
            wm(ii) = wm(ii) + one
580     ii = ii + meband
        nlu = nlu + 1
        call dgbfa (wm(3), meband, n, ml, mu, iwm(31), ier)
        if (ier /= 0) ierpj = 1
        
        return
    end subroutine dvjac

    !$
    !===============================================================================================
    ! call sequence input -- nrow, ncol, a, nrowa, nrowb
    ! call sequence output -- b
    ! common block variables accessed -- none
    !
    ! subroutines called by dacopy: dcopy
    ! function routines called by dacopy: none
    !---------------------------------------------------------------------------
    ! this routine copies one rectangular array, a, to another, b,
    ! where a and b may have different row dimensions, nrowa and nrowb.
    ! the data copied consists of nrow rows and ncol columns.
    !===============================================================================================
    subroutine dacopy (nrow, ncol, a, nrowa, b, nrowb)
    
        double precision a, b
        integer nrow, ncol, nrowa, nrowb
        dimension a(nrowa,ncol), b(nrowb,ncol)

        integer ic
        
        do 20 ic = 1,ncol
            call dcopy (nrow, a(1,ic), 1, b(1,ic), 1)
20      continue
        
        return
    end subroutine dacopy

    !$
    !===============================================================================================
    ! call sequence input -- wm, iwm, x
    ! call sequence output -- x, iersl
    ! common block variables accessed:
    !     /dvod01/ -- h, rl1, miter, n
    !
    ! subroutines called by dvsol: dgesl, dgbsl
    ! function routines called by dvsol: none
    !---------------------------------------------------------------------------
    ! this routine manages the solution of the linear system arising from
    ! a chord iteration.  it is called if miter /= 0.
    ! if miter is 1 or 2, it calls dgesl to accomplish this.
    ! if miter = 3 it updates the coefficient h*rl1 in the diagonal
    ! matrix, and then computes the solution.
    ! if miter is 4 or 5, it calls dgbsl.
    ! communication with dvsol uses the following variables:
    ! wm    = real work space containing the inverse diagonal matrix if
    !         miter = 3 and the lu decomposition of the matrix otherwise.
    !         storage of matrix elements starts at wm(3).
    !         wm also contains the following matrix-related data:
    !         wm(1) = SQRT(uround) (not used here),
    !         wm(2) = hrl1, the previous value of h*rl1, used if miter = 3.
    ! iwm   = integer work space containing pivot information, starting at
    !         iwm(31), if miter is 1, 2, 4, or 5.  iwm also contains band
    !         parameters ml = iwm(1) and mu = iwm(2) if miter is 4 or 5.
    ! x     = the right-hand side vector on input, and the solution vector
    !         on output, of length n.
    ! iersl = output flag.  iersl = 0 if no trouble occurred.
    !         iersl = 1 if a singular matrix arose with miter = 3.
    !===============================================================================================
    subroutine dvsol (wm, iwm, x, iersl)
    
        double precision wm, x
        integer iwm, iersl
        dimension wm(*), iwm(*), x(*)
        
        ! type declarations for labeled common block dvod01 
        double precision acnrm, ccmxj, conp, crate, drc, el, eta, etamax, h, hmin, hmxi, hnew, hscal, prl1, rc, rl1, tau, tq, tn, uround
        integer icf, init, ipup, jcur, jstart, jsv, kflag, kuth, l, lmax, lyh, lewt, lacor, lsavf, lwm, liwm, locjs, maxord, meth, miter, msbj, mxhnil, mxstep,     &
            &   n, newh, newq, nhnil, nq, nqnyh, nqwait, nslj, nslp, nyh
        
        ! type declarations for local variables 
        integer i, meband, ml, mu
        double precision di, hrl1, one, phrl1, r, zero
        
        !-----------------------------------------------------------------------
        ! the following fortran-77 declaration is to cause the values of the
        ! listed (local) variables to be saved between calls to this integrator.
        save one, zero
        
        common /dvod01/ acnrm, ccmxj, conp, crate, drc, el(13), eta, etamax, h, hmin, hmxi, hnew, hscal, prl1, rc, rl1, tau(13), tq(5), tn, uround,         &
            &   icf, init, ipup, jcur, jstart, jsv, kflag, kuth, l, lmax, lyh, lewt, lacor, lsavf, lwm, liwm, locjs, maxord, meth, miter, msbj, mxhnil, mxstep,     &
            &   n, newh, newq, nhnil, nq, nqnyh, nqwait, nslj, nslp, nyh
        
        data one /1.0d0/, zero /0.0d0/
        
        iersl = 0
        go to (100, 100, 300, 400, 400), miter
100     call dgesl (wm(3), n, n, iwm(31), x, 0)
        return
        
300     phrl1 = wm(2)
        hrl1 = h*rl1
        wm(2) = hrl1
        if (hrl1 == phrl1) go to 330
        r = hrl1/phrl1
        do 320 i = 1,n
            di = one - r*(one - one/wm(i+2))
            if (ABS(di) == zero) go to 390
320     wm(i+2) = one/di
        
330     do 340 i = 1,n
340     x(i) = wm(i+2)*x(i)
        return
390     iersl = 1
        return
        
400     ml = iwm(1)
        mu = iwm(2)
        meband = 2*ml + mu + 1
        call dgbsl (wm(3), meband, n, ml, mu, iwm(31), x, 0)
        
        return
    end subroutine dvsol

    !$
    !===============================================================================================
    ! call sequence input -- rsav, isav, job
    ! call sequence output -- rsav, isav
    ! common block variables accessed -- all of /dvod01/ and /dvod02/
    !
    ! subroutines/functions called by dvsrco: none
    !---------------------------------------------------------------------------
    ! this routine saves or restores (depending on job) the contents of the
    ! common blocks dvod01 and dvod02, which are used internally by dvode.
    !
    ! rsav = real array of length 49 or more.
    ! isav = integer array of length 41 or more.
    ! job  = flag indicating to save or restore the common blocks:
    !        job  = 1 if common is to be saved (written to rsav/isav).
    !        job  = 2 if common is to be restored (read from rsav/isav).
    !        a call with job = 2 presumes a prior call with job = 1.
    !===============================================================================================
    subroutine dvsrco (rsav, isav, job)
    
        double precision rsav
        integer isav, job
        dimension rsav(*), isav(*)

        double precision rvod1, rvod2
        integer ivod1, ivod2
        integer i, leniv1, leniv2, lenrv1, lenrv2
        
        !-----------------------------------------------------------------------
        ! the following fortran-77 declaration is to cause the values of the
        ! listed (local) variables to be saved between calls to this integrator.
        save lenrv1, leniv1, lenrv2, leniv2
        
        common /dvod01/ rvod1(48), ivod1(33)
        common /dvod02/ rvod2(1), ivod2(8)
        data lenrv1/48/, leniv1/33/, lenrv2/1/, leniv2/8/
        
        if (job == 2) go to 100
        do 10 i = 1,lenrv1
10      rsav(i) = rvod1(i)
        do 15 i = 1,lenrv2
15      rsav(lenrv1+i) = rvod2(i)
        
        do 20 i = 1,leniv1
20      isav(i) = ivod1(i)
        do 25 i = 1,leniv2
25      isav(leniv1+i) = ivod2(i)
        
        return
        
100     continue
        do 110 i = 1,lenrv1
110     rvod1(i) = rsav(i)
        do 115 i = 1,lenrv2
115     rvod2(i) = rsav(lenrv1+i)
        
        do 120 i = 1,leniv1
120     ivod1(i) = isav(i)
        do 125 i = 1,leniv2
125     ivod2(i) = isav(leniv1+i)
        
        return
    end subroutine dvsrco

    !$
    !===============================================================================================
    !***begin prologue  dewset
    !***subsidiary
    !***purpose  set error weight vector.
    !***type      double precision (sewset-s, dewset-d)
    !***author  hindmarsh, alan c., (llnl)
    !***description
    !
    !  this subroutine sets the error weight vector ewt according to
    !      ewt(i) = rtol(i)*ABS(ycur(i)) + atol(i),  i = 1,...,n,
    !  with the subscript on rtol and/or atol possibly replaced by 1 above,
    !  depending on the value of itol.
    !
    !***see also  dlsode
    !***routines called  (none)
    !***revision history  (yymmdd)
    !   791129  date written
    !   890501  modified prologue to slatec/ldoc format.  (fnf)
    !   890503  minor cosmetic changes.  (fnf)
    !   930809  renamed to allow single/double precision versions. (ach)
    !***end prologue  dewset
    !**end
    !===============================================================================================
    subroutine dewset (n, itol, rtol, atol, ycur, ewt)
    
        integer n, itol
        integer i
        double precision rtol, atol, ycur, ewt
        dimension rtol(*), atol(*), ycur(n), ewt(n)
        
        ! first executable statement  dewset
        go to (10, 20, 30, 40), itol
10      continue
        do 15 i = 1,n
15      ewt(i) = rtol(1)*ABS(ycur(i)) + atol(1)
        return
20      continue
        do 25 i = 1,n
25      ewt(i) = rtol(1)*ABS(ycur(i)) + atol(i)
        return
30      continue
        do 35 i = 1,n
35      ewt(i) = rtol(i)*ABS(ycur(i)) + atol(1)
        return
40      continue
        do 45 i = 1,n
45      ewt(i) = rtol(i)*ABS(ycur(i)) + atol(i)
        
        return
    end subroutine dewset

    !$
    !===============================================================================================
    !***begin prologue  dvnorm
    !***subsidiary
    !***purpose  weighted root-mean-square vector norm.
    !***type      double precision (svnorm-s, dvnorm-d)
    !***author  hindmarsh, alan c., (llnl)
    !***description
    !
    !  this function routine computes the weighted root-mean-square norm
    !  of the vector of length n contained in the array v, with weights
    !  contained in the array w of length n:
    !    dvnorm = SQRT( (1/n) * sum( v(i)*w(i) )**2 )
    !
    !***see also  dlsode
    !***routines called  (none)
    !***revision history  (yymmdd)
    !   791129  date written
    !   890501  modified prologue to slatec/ldoc format.  (fnf)
    !   890503  minor cosmetic changes.  (fnf)
    !   930809  renamed to allow single/double precision versions. (ach)
    !***end prologue  dvnorm
    !**end
    !===============================================================================================
    double precision function dvnorm (n, v, w)
        integer n,   i
        double precision v, w,   sum
        dimension v(n), w(n)
        
        ! first executable statement  dvnorm
        sum = 0.0d0
        do 10 i = 1,n
10      sum = sum + (v(i)*w(i))**2
        dvnorm = SQRT(sum/n)
        
        return
    end function dvnorm

    !$
    !===============================================================================================
    !***begin prologue  xerrwd
    !***subsidiary
    !***purpose  write error message with values.
    !***category  r3c
    !***type      double precision (xerrwv-s, xerrwd-d)
    !***author  hindmarsh, alan c., (llnl)
    !***description
    !
    !  subroutines xerrwd, xsetf, xsetun, and the function routine ixsav,
    !  as given here, constitute a simplified version of the slatec error
    !  handling package.
    !
    !  all arguments are input arguments.
    !
    !  msg    = the message (character array).
    !  nmes   = the length of msg (number of characters).
    !  nerr   = the error number (not used).
    !  level  = the error level..
    !           0 or 1 means recoverable (control returns to caller).
    !           2 means fatal (run is aborted--see note below).
    !  ni     = number of integers (0, 1, or 2) to be printed with message.
    !  i1,i2  = integers to be printed, depending on ni.
    !  nr     = number of reals (0, 1, or 2) to be printed with message.
    !  r1,r2  = reals to be printed, depending on nr.
    !
    !  note..  this routine is machine-dependent and specialized for use
    !  in limited context, in the following ways..
    !  1. the argument msg is assumed to be of type character, and
    !     the message is printed with a format of (1x,a).
    !  2. the message is assumed to take only one line.
    !     multi-line messages are generated by repeated calls.
    !  3. if level = 2, control passes to the statement   stop
    !     to abort the run.  this statement may be machine-dependent.
    !  4. r1 and r2 are assumed to be in double precision and are printed
    !     in d21.13 format.
    !
    !***routines called  ixsav
    !***revision history  (yymmdd)
    !   920831  date written
    !   921118  replaced mflgsv/lunsav by ixsav. (ach)
    !   930329  modified prologue to slatec format. (fnf)
    !   930407  changed msg from character*1 array to variable. (fnf)
    !   930922  minor cosmetic change. (fnf)
    !***end prologue  xerrwd
    !
    !*internal notes:
    !
    ! for a different default logical unit number, ixsav (or a subsidiary
    ! routine that it calls) will need to be modified.
    ! for a different run-abort command, change the statement following
    ! statement 100 at the end.
    !---------------------------------------------------------------------------
    ! subroutines called by xerrwd.. none
    ! function routine called by xerrwd.. ixsav
    !---------------------------------------------------------------------------
    !**end
    !===============================================================================================
    subroutine xerrwd (msg, nmes, nerr, level, ni, i1, i2, nr, r1, r2)
        
        ! declare arguments.
        double precision r1, r2
        integer nmes, nerr, level, ni, i1, i2, nr
        character(len = *) msg
        
        ! declare local variables.
        integer lunit, mesflg
        
        ! get logical unit number and message print flag.
        ! first executable statement  xerrwd
        lunit = ixsav (1, 0, .false.)
        mesflg = ixsav (2, 0, .false.)
        if (mesflg == 0) go to 100
        
        ! write the message.
        write (lunit,10)  msg
10      format(1x,a)
        if (ni == 1) write (lunit, 20) i1
20      format(6x,'in above message,  i1 =',i10)
        if (ni == 2) write (lunit, 30) i1,i2
30      format(6x,'in above message,  i1 =',i10,3x,'i2 =',i10)
        if (nr == 1) write (lunit, 40) r1
40      format(6x,'in above message,  r1 =',d21.13)
        if (nr == 2) write (lunit, 50) r1,r2
50      format(6x,'in above,  r1 =',d21.13,3x,'r2 =',d21.13)
        
        ! abort the run if level = 2.
100     if (level /= 2) return
        stop
    end subroutine xerrwd

    !$
    !===============================================================================================
    !***begin prologue  xsetf
    !***purpose  reset the error print control flag.
    !***category  r3a
    !***type      all (xsetf-a)
    !***keywords  error control
    !***author  hindmarsh, alan c., (llnl)
    !***description
    !
    !   xsetf sets the error print control flag to mflag:
    !      mflag=1 means print all messages (the default).
    !      mflag=0 means no printing.
    !
    !***see also  xerrwd, xerrwv
    !***references  (none)
    !***routines called  ixsav
    !***revision history  (yymmdd)
    !   921118  date written
    !   930329  added slatec format prologue. (fnf)
    !   930407  corrected see also section. (fnf)
    !   930922  made user-callable, and other cosmetic changes. (fnf)
    !***end prologue  xsetf
    !
    ! subroutines called by xsetf.. none
    ! function routine called by xsetf.. ixsav
    !---------------------------------------------------------------------------
    !**end
    !===============================================================================================
    subroutine xsetf (mflag)
    
        integer mflag, junk
        
        ! first executable statement  xsetf
        if (mflag == 0 .or. mflag == 1) junk = ixsav (2,mflag,.TRUE.)
        
        return
    end subroutine xsetf

    !$
    !===============================================================================================
    !***begin prologue  xsetun
    !***purpose  reset the logical unit number for error messages.
    !***category  r3b
    !***type      all (xsetun-a)
    !***keywords  error control
    !***description
    !
    !   xsetun sets the logical unit number for error messages to lun.
    !
    !***author  hindmarsh, alan c., (llnl)
    !***see also  xerrwd, xerrwv
    !***references  (none)
    !***routines called  ixsav
    !***revision history  (yymmdd)
    !   921118  date written
    !   930329  added slatec format prologue. (fnf)
    !   930407  corrected see also section. (fnf)
    !   930922  made user-callable, and other cosmetic changes. (fnf)
    !***end prologue  xsetun
    !
    ! subroutines called by xsetun.. none
    ! function routine called by xsetun.. ixsav
    !---------------------------------------------------------------------------
    !**end
    !===============================================================================================
    subroutine xsetun (lun)
    
        integer lun, junk
        
        ! first executable statement  xsetun
        if (lun > 0) junk = ixsav (1,lun,.TRUE.)
        
        return
    end subroutine xsetun

    !$
    !===============================================================================================
    !***begin prologue  ixsav
    !***subsidiary
    !***purpose  save and recall error message control parameters.
    !***category  r3c
    !***type      all (ixsav-a)
    !***author  hindmarsh, alan c., (llnl)
    !***description
    !
    !  ixsav saves and recalls one of two error message parameters:
    !    lunit, the logical unit number to which messages are printed, and
    !    mesflg, the message print flag.
    !  this is a modification of the slatec library routine j4save.
    !
    !  saved local variables..
    !   lunit  = logical unit number for messages.  the default is obtained
    !            by a call to iumach (may be machine-dependent).
    !   mesflg = print control flag..
    !            1 means print all messages (the default).
    !            0 means no printing.
    !
    !  on input..
    !    ipar   = parameter indicator (1 for lunit, 2 for mesflg).
    !    ivalue = the value to be set for the parameter, if iset = .TRUE.
    !    iset   = logical flag to indicate whether to read or write.
    !             if iset = .TRUE., the parameter will be given
    !             the value ivalue.  if iset = .false., the parameter
    !             will be unchanged, and ivalue is a dummy argument.
    !
    !  on return..
    !    ixsav = the (old) value of the parameter.
    !
    !***see also  xerrwd, xerrwv
    !***routines called  iumach
    !***revision history  (yymmdd)
    !   921118  date written
    !   930329  modified prologue to slatec format. (fnf)
    !   930915  added iumach call to get default output unit.  (ach)
    !   930922  minor cosmetic changes. (fnf)
    !   010425  type declaration for iumach added. (ach)
    !***end prologue  ixsav
    !
    ! subroutines called by ixsav.. none
    ! function routine called by ixsav.. iumach
    !---------------------------------------------------------------------------
    !**end
    !===============================================================================================
    integer function ixsav (ipar, ivalue, iset)
    
        logical iset
        integer ipar, ivalue
        
        integer lunit, mesflg
        
        !-----------------------------------------------------------------------
        ! the following fortran-77 declaration is to cause the values of the
        ! listed (local) variables to be saved between calls to this routine.
        save lunit, mesflg
        data lunit/-1/, mesflg/1/
        
        ! first executable statement  ixsav
        if (ipar == 1) then
            if (lunit == -1) lunit = iumach()
            ixsav = lunit
            if (iset) lunit = ivalue
        end if
        
        if (ipar == 2) then
            ixsav = mesflg
            if (iset) mesflg = ivalue
        end if
        
        return
    end function ixsav

    !$
    !===============================================================================================
    !***begin prologue  iumach
    !***purpose  provide standard output unit number.
    !***category  r1
    !***type      integer (iumach-i)
    !***keywords  machine constants
    !***author  hindmarsh, alan c., (llnl)
    !***description
    ! *usage:
    !        integer  lout, iumach
    !        lout = iumach()
    !
    ! *function return values:
    !     lout : the standard logical unit for fortran output.
    !
    !***references  (none)
    !***routines called  (none)
    !***revision history  (yymmdd)
    !   930915  date written
    !   930922  made user-callable, and other cosmetic changes. (fnf)
    !***end prologue  iumach
    !
    !*internal notes:
    !  the built-in value of 6 is standard on a wide range of fortran
    !  systems.  this may be machine-dependent.
    !**end
    !***first executable statement  iumach
    !===============================================================================================
    integer function iumach()
    
        iumach = 6
        
        return
    end function iumach

    !$
    !===============================================================================================
    !***begin prologue  dumach
    !***purpose  compute the unit roundoff of the machine.
    !***category  r1
    !***type      double precision (rumach-s, dumach-d)
    !***keywords  machine constants
    !***author  hindmarsh, alan c., (llnl)
    !***description
    ! *usage:
    !        double precision  a, dumach
    !        a = dumach()
    !
    ! *function return values:
    !     a : the unit roundoff of the machine.
    !
    ! *description:
    !     the unit roundoff is defined as the smallest positive machine
    !     number u such that  1.0 + u /= 1.0.  this is computed by dumach
    !     in a machine-independent manner.
    !
    !***references  (none)
    !***routines called  dumsum
    !***revision history  (yyyymmdd)
    !   19930216  date written
    !   19930818  added slatec-format prologue.  (fnf)
    !   20030707  added dumsum to force normal storage of comp.  (ach)
    !***end prologue  dumach
    !===============================================================================================
    double precision function dumach ()
        
        double precision u, comp
        
        ! first executable statement  dumach
        u = 1.0d0
10      u = u*0.5d0

        call dumsum(1.0d0, u, comp)
        if (comp /= 1.0d0) go to 10
        dumach = u*2.0d0
        
        return
    end function dumach
        
    !$
    !===============================================================================================
    !
    !===============================================================================================
    subroutine dumsum (a,b,c)
    
        ! routine to force normal storing of a + b, for dumach.
        double precision a, b, c
        c = a + b
        
        return
    end subroutine dumsum

    !$
    !===============================================================================================
    !***begin prologue  dgefa
    !***purpose  factor a matrix using gaussian elimination.
    !***category  d2a1
    !***type      double precision (sgefa-s, dgefa-d, cgefa-c)
    !***keywords  general matrix, linear algebra, linpack,
    !             matrix factorization
    !***author  moler, c. b., (u. of new mexico)
    !***description
    !
    !     dgefa factors a double precision matrix by gaussian elimination.
    !
    !     dgefa is usually called by dgeco, but it can be called
    !     directly with a saving in time if  rcond  is not needed.
    !     (time for dgeco) = (1 + 9/n)*(time for dgefa) .
    !
    !     on entry
    !
    !        a       double precision(lda, n)
    !                the matrix to be factored.
    !
    !        lda     integer
    !                the leading dimension of the array  a .
    !
    !        n       integer
    !                the order of the matrix  a .
    !
    !     on return
    !
    !        a       an upper triangular matrix and the multipliers
    !                which were used to obtain it.
    !                the factorization can be written  a = l*u  where
    !                l  is a product of permutation and unit lower
    !                triangular matrices and  u  is upper triangular.
    !
    !        ipvt    integer(n)
    !                an integer vector of pivot indices.
    !
    !        info    integer
    !                = 0  normal value.
    !                = k  if  u(k,k) == 0.0 .  this is not an error
    !                     condition for this subroutine, but it does
    !                     indicate that dgesl or dgedi will divide by zero
    !                     if called.  use  rcond  in dgeco for a reliable
    !                     indication of singularity.
    !
    !***references  j. j. dongarra, j. r. bunch, c. b. moler, and g. w.
    !                 stewart, linpack users' guide, siam, 1979.
    !***routines called  daxpy, dscal, idamax
    !***revision history  (yymmdd)
    !   780814  date written
    !   890831  modified array declarations.  (wrb)
    !   890831  revision date from version 3.2
    !   891214  prologue converted to version 4.0 format.  (bab)
    !   900326  removed duplicate information from description section.
    !           (wrb)
    !   920501  reformatted the references section.  (wrb)
    !***end prologue  dgefa
    !===============================================================================================
    subroutine dgefa (a, lda, n, ipvt, info)
    
        integer lda,n,ipvt(*),info
        double precision a(lda,*)
        
        double precision t
        integer j,k,kp1,l,nm1
        
        ! gaussian elimination with partial pivoting
        ! first executable statement  dgefa
        info = 0
        nm1 = n - 1
        if (nm1 < 1) go to 70
        do 60 k = 1, nm1
            kp1 = k + 1
            
            ! find l = pivot index
            l = idamax(n-k+1,a(k,k),1) + k - 1
            ipvt(k) = l
            
            ! zero pivot implies this column already triangularized
            if (a(l,k) == 0.0d0) go to 40
            
            ! interchange if necessary
            if (l == k) go to 10
            t = a(l,k)
            a(l,k) = a(k,k)
            a(k,k) = t
10          continue
            
            ! compute multipliers
            t = -1.0d0/a(k,k)
            call dscal(n-k,t,a(k+1,k),1)
            
            ! row elimination with column indexing
            do 30 j = kp1, n
                t = a(l,j)
                if (l == k) go to 20
                a(l,j) = a(k,j)
                a(k,j) = t
20              continue
                call daxpy(n-k,t,a(k+1,k),1,a(k+1,j),1)
30          continue
            go to 50
40          continue
            info = k
50          continue
60      continue
70      continue

        ipvt(n) = n
        if (a(n,n) == 0.0d0) info = n
        
        return
    end subroutine dgefa

    !$
    !===============================================================================================
    !***begin prologue  dgesl
    !***purpose  solve the real system a*x=b or trans(a)*x=b using the
    !            factors computed by dgeco or dgefa.
    !***category  d2a1
    !***type      double precision (sgesl-s, dgesl-d, cgesl-c)
    !***keywords  linear algebra, linpack, matrix, solve
    !***author  moler, c. b., (u. of new mexico)
    !***description
    !
    !     dgesl solves the double precision system
    !     a * x = b  or  trans(a) * x = b
    !     using the factors computed by dgeco or dgefa.
    !
    !     on entry
    !
    !        a       double precision(lda, n)
    !                the output from dgeco or dgefa.
    !
    !        lda     integer
    !                the leading dimension of the array  a .
    !
    !        n       integer
    !                the order of the matrix  a .
    !
    !        ipvt    integer(n)
    !                the pivot vector from dgeco or dgefa.
    !
    !        b       double precision(n)
    !                the right hand side vector.
    !
    !        job     integer
    !                = 0         to solve  a*x = b ,
    !                = nonzero   to solve  trans(a)*x = b  where
    !                            trans(a)  is the transpose.
    !
    !     on return
    !
    !        b       the solution vector  x .
    !
    !     error condition
    !
    !        a division by zero will occur if the input factor contains a
    !        zero on the diagonal.  technically this indicates singularity
    !        but it is often caused by improper arguments or improper
    !        setting of lda .  it will not occur if the subroutines are
    !        called correctly and if dgeco has set rcond > 0.0
    !        or dgefa has set info == 0 .
    !
    !     to compute  inverse(a) * c  where  c  is a matrix
    !     with  p  columns
    !           call dgeco(a,lda,n,ipvt,rcond,z)
    !           if (rcond is too small) go to ...
    !           do 10 j = 1, p
    !              call dgesl(a,lda,n,ipvt,c(1,j),0)
    !        10 continue
    !
    !***references  j. j. dongarra, j. r. bunch, c. b. moler, and g. w.
    !                 stewart, linpack users' guide, siam, 1979.
    !***routines called  daxpy, ddot
    !***revision history  (yymmdd)
    !   780814  date written
    !   890831  modified array declarations.  (wrb)
    !   890831  revision date from version 3.2
    !   891214  prologue converted to version 4.0 format.  (bab)
    !   900326  removed duplicate information from description section.
    !           (wrb)
    !   920501  reformatted the references section.  (wrb)
    !***end prologue  dgesl
    !===============================================================================================
    subroutine dgesl (a, lda, n, ipvt, b, job)
    
        integer lda,n,ipvt(*),job
        double precision a(lda,*),b(*)
        
        double precision t
        integer k,kb,l,nm1
        ! first executable statement dgesl
        nm1 = n - 1
        if (job /= 0) go to 50
        
        ! job = 0, solve  a * x = b, first solve  l*y = b
        if (nm1 < 1) go to 30
        do 20 k = 1, nm1
            l = ipvt(k)
            t = b(l)
            if (l == k) go to 10
            b(l) = b(k)
            b(k) = t
10          continue
            call daxpy(n-k,t,a(k+1,k),1,b(k+1),1)
20      continue
30      continue
        
        ! now solve  u*x = y
        do 40 kb = 1, n
            k = n + 1 - kb
            b(k) = b(k)/a(k,k)
            t = -b(k)
            call daxpy(k-1,t,a(1,k),1,b(1),1)
40      continue
        go to 100
50      continue
        
        ! job = nonzero, solve  trans(a) * x = b, first solve  trans(u)*y = b
        do 60 k = 1, n
            t = ddot(k-1,a(1,k),1,b(1),1)
            b(k) = (b(k) - t)/a(k,k)
60      continue
        
        ! now solve trans(l)*x = y
        if (nm1 < 1) go to 90
        do 80 kb = 1, nm1
            k = n - kb
            b(k) = b(k) + ddot(n-k,a(k+1,k),1,b(k+1),1)
            l = ipvt(k)
            if (l == k) go to 70
            t = b(l)
            b(l) = b(k)
            b(k) = t
70          continue
80      continue
90      continue
100     continue
        
        return
    end subroutine dgesl

    !$
    !===============================================================================================
    !***begin prologue  dgbfa
    !***purpose  factor a band matrix using gaussian elimination.
    !***category  d2a2
    !***type      double precision (sgbfa-s, dgbfa-d, cgbfa-c)
    !***keywords  banded, linear algebra, linpack, matrix factorization
    !***author  moler, c. b., (u. of new mexico)
    !***description
    !
    !     dgbfa factors a double precision band matrix by elimination.
    !
    !     dgbfa is usually called by dgbco, but it can be called
    !     directly with a saving in time if  rcond  is not needed.
    !
    !     on entry
    !
    !        abd     double precision(lda, n)
    !                contains the matrix in band storage.  the columns
    !                of the matrix are stored in the columns of  abd  and
    !                the diagonals of the matrix are stored in rows
    !                ml+1 through 2*ml+mu+1 of  abd .
    !                see the comments below for details.
    !
    !        lda     integer
    !                the leading dimension of the array  abd .
    !                lda must be >= 2*ml + mu + 1 .
    !
    !        n       integer
    !                the order of the original matrix.
    !
    !        ml      integer
    !                number of diagonals below the main diagonal.
    !                0 <= ml <  n .
    !
    !        mu      integer
    !                number of diagonals above the main diagonal.
    !                0 <= mu <  n .
    !                more efficient if  ml <= mu .
    !     on return
    !
    !        abd     an upper triangular matrix in band storage and
    !                the multipliers which were used to obtain it.
    !                the factorization can be written  a = l*u  where
    !                l  is a product of permutation and unit lower
    !                triangular matrices and  u  is upper triangular.
    !
    !        ipvt    integer(n)
    !                an integer vector of pivot indices.
    !
    !        info    integer
    !                = 0  normal value.
    !                = k  if  u(k,k) == 0.0 .  this is not an error
    !                     condition for this subroutine, but it does
    !                     indicate that dgbsl will divide by zero if
    !                     called.  use  rcond  in dgbco for a reliable
    !                     indication of singularity.
    !
    !     band storage
    !
    !           if  a  is a band matrix, the following program segment
    !           will set up the input.
    !
    !                   ml = (band width below the diagonal)
    !                   mu = (band width above the diagonal)
    !                   m = ml + mu + 1
    !                   do 20 j = 1, n
    !                      i1 = MAX(1, j-mu)
    !                      i2 = MIN(n, j+ml)
    !                      do 10 i = i1, i2
    !                         k = i - j + m
    !                         abd(k,j) = a(i,j)
    !                10    continue
    !                20 continue
    !
    !           this uses rows  ml+1  through  2*ml+mu+1  of  abd .
    !           in addition, the first  ml  rows in  abd  are used for
    !           elements generated during the triangularization.
    !           the total number of rows needed in  abd  is  2*ml+mu+1 .
    !           the  ml+mu by ml+mu  upper left triangle and the
    !           ml by ml  lower right triangle are not referenced.
    !
    !***references  j. j. dongarra, j. r. bunch, c. b. moler, and g. w.
    !                 stewart, linpack users' guide, siam, 1979.
    !***routines called  daxpy, dscal, idamax
    !***revision history  (yymmdd)
    !   780814  date written
    !   890531  changed all specific intrinsics to generic.  (wrb)
    !   890831  modified array declarations.  (wrb)
    !   890831  revision date from version 3.2
    !   891214  prologue converted to version 4.0 format.  (bab)
    !   900326  removed duplicate information from description section.
    !           (wrb)
    !   920501  reformatted the references section.  (wrb)
    !***end prologue  dgbfa
    !===============================================================================================
    subroutine dgbfa (abd, lda, n, ml, mu, ipvt, info)
    
        integer lda,n,ml,mu,ipvt(*),info
        double precision abd(lda,*)
        
        double precision t
        integer i,i0,j,ju,jz,j0,j1,k,kp1,l,lm,m,mm,nm1
        
        ! first executable statement  dgbfa
        m = ml + mu + 1
        info = 0
        
        ! zero initial fill-in columns
        j0 = mu + 2
        j1 = MIN(n,m) - 1
        if (j1 < j0) go to 30
        do 20 jz = j0, j1
            i0 = m + 1 - jz
            do 10 i = i0, ml
                abd(i,jz) = 0.0d0
10          continue
20      continue
30      continue

        jz = j1
        ju = 0
        
        ! gaussian elimination with partial pivoting
        nm1 = n - 1
        if (nm1 < 1) go to 130
        do 120 k = 1, nm1
            kp1 = k + 1
            
            ! zero next fill-in column
            jz = jz + 1
            if (jz > n) go to 50
            if (ml < 1) go to 50
            do 40 i = 1, ml
                abd(i,jz) = 0.0d0
40          continue
50          continue
            
            ! find l = pivot index
            lm = MIN(ml,n-k)
            l = idamax(lm+1,abd(m,k),1) + m - 1
            ipvt(k) = l + k - m
            
            ! zero pivot implies this column already triangularized
            if (abd(l,k) == 0.0d0) go to 100
            
            ! interchange if necessary
            if (l == m) go to 60
            t = abd(l,k)
            abd(l,k) = abd(m,k)
            abd(m,k) = t
60          continue
            
            ! compute multipliers
            t = -1.0d0/abd(m,k)
            call dscal(lm,t,abd(m+1,k),1)
            
            ! row elimination with column indexing
            ju = MIN(MAX(ju,mu+ipvt(k)),n)
            mm = m
            if (ju < kp1) go to 90
            
            do 80 j = kp1, ju
                l = l - 1
                mm = mm - 1
                t = abd(l,j)
                if (l == mm) go to 70
                abd(l,j) = abd(mm,j)
                abd(mm,j) = t
70              continue
                call daxpy(lm,t,abd(m+1,k),1,abd(mm+1,j),1)
80          continue
90          continue
            go to 110
100         continue
            info = k
110         continue
120     continue
130     continue

        ipvt(n) = n
        if (abd(m,n) == 0.0d0) info = n
        
        return
    end subroutine dgbfa

    !$
    !===============================================================================================
    !***begin prologue  dgbsl
    !***purpose  solve the real band system a*x=b or trans(a)*x=b using
    !            the factors computed by dgbco or dgbfa.
    !***category  d2a2
    !***type      double precision (sgbsl-s, dgbsl-d, cgbsl-c)
    !***keywords  banded, linear algebra, linpack, matrix, solve
    !***author  moler, c. b., (u. of new mexico)
    !***description
    !
    !     dgbsl solves the double precision band system
    !     a * x = b  or  trans(a) * x = b
    !     using the factors computed by dgbco or dgbfa.
    !
    !     on entry
    !
    !        abd     double precision(lda, n)
    !                the output from dgbco or dgbfa.
    !
    !        lda     integer
    !                the leading dimension of the array  abd .
    !
    !        n       integer
    !                the order of the original matrix.
    !
    !        ml      integer
    !                number of diagonals below the main diagonal.
    !
    !        mu      integer
    !                number of diagonals above the main diagonal.
    !
    !        ipvt    integer(n)
    !                the pivot vector from dgbco or dgbfa.
    !
    !        b       double precision(n)
    !                the right hand side vector.
    !
    !        job     integer
    !                = 0         to solve  a*x = b ,
    !                = nonzero   to solve  trans(a)*x = b , where
    !                            trans(a)  is the transpose.
    !
    !     on return
    !
    !        b       the solution vector  x .
    !
    !     error condition
    !
    !        a division by zero will occur if the input factor contains a
    !        zero on the diagonal.  technically this indicates singularity
    !        but it is often caused by improper arguments or improper
    !        setting of lda .  it will not occur if the subroutines are
    !        called correctly and if dgbco has set rcond > 0.0
    !        or dgbfa has set info == 0 .
    !
    !     to compute  inverse(a) * c  where  c  is a matrix
    !     with  p  columns
    !           call dgbco(abd,lda,n,ml,mu,ipvt,rcond,z)
    !           if (rcond is too small) go to ...
    !           do 10 j = 1, p
    !              call dgbsl(abd,lda,n,ml,mu,ipvt,c(1,j),0)
    !        10 continue
    !
    !***references  j. j. dongarra, j. r. bunch, c. b. moler, and g. w.
    !                 stewart, linpack users' guide, siam, 1979.
    !***routines called  daxpy, ddot
    !***revision history  (yymmdd)
    !   780814  date written
    !   890531  changed all specific intrinsics to generic.  (wrb)
    !   890831  modified array declarations.  (wrb)
    !   890831  revision date from version 3.2
    !   891214  prologue converted to version 4.0 format.  (bab)
    !   900326  removed duplicate information from description section.
    !           (wrb)
    !   920501  reformatted the references section.  (wrb)
    !***end prologue  dgbsl
    !===============================================================================================
    subroutine dgbsl (abd, lda, n, ml, mu, ipvt, b, job)
    
        integer lda,n,ml,mu,ipvt(*),job
        double precision abd(lda,*),b(*)
        
        double precision t
        integer k,kb,l,la,lb,lm,m,nm1
        
        ! first executable statement  dgbsl
        m = mu + ml + 1
        nm1 = n - 1
        if (job /= 0) go to 50
        
        ! job = 0 , solve  a * x = b, first solve l*y = b
        if (ml == 0) go to 30
        if (nm1 < 1) go to 30
        do 20 k = 1, nm1
            lm = MIN(ml,n-k)
            l = ipvt(k)
            t = b(l)
            if (l == k) go to 10
            b(l) = b(k)
            b(k) = t
10          continue
            call daxpy(lm,t,abd(m+1,k),1,b(k+1),1)
20      continue
30      continue
        
        ! now solve  u*x = y
        do 40 kb = 1, n
            k = n + 1 - kb
            b(k) = b(k)/abd(m,k)
            lm = MIN(k,m) - 1
            la = m - lm
            lb = k - lm
            t = -b(k)
            call daxpy(lm,t,abd(la,k),1,b(lb),1)
40      continue
        go to 100
50      continue
        
        ! job = nonzero, solve  trans(a) * x = b, first solve  trans(u)*y = b
        do 60 k = 1, n
            lm = MIN(k,m) - 1
            la = m - lm
            lb = k - lm
            t = ddot(lm,abd(la,k),1,b(lb),1)
            b(k) = (b(k) - t)/abd(m,k)
60      continue
        
        ! now solve trans(l)*x = y
        if (ml == 0) go to 90
        if (nm1 < 1) go to 90
        do 80 kb = 1, nm1
            k = n - kb
            lm = MIN(ml,n-k)
            b(k) = b(k) + ddot(lm,abd(m+1,k),1,b(k+1),1)
            l = ipvt(k)
            if (l == k) go to 70
            t = b(l)
            b(l) = b(k)
            b(k) = t
70          continue
80      continue
90      continue
100     continue
        
        return
    end subroutine dgbsl

    !$
    !===============================================================================================
    !***begin prologue  daxpy
    !***purpose  compute a constant times a vector plus a vector.
    !***category  d1a7
    !***type      double precision (saxpy-s, daxpy-d, caxpy-c)
    !***keywords  blas, linear algebra, triad, vector
    !***author  lawson, c. l., (jpl)
    !           hanson, r. j., (snla)
    !           kincaid, d. r., (u. of texas)
    !           krogh, f. t., (jpl)
    !***description
    !
    !                b l a s  subprogram
    !    description of parameters
    !
    !     --input--
    !        n  number of elements in input vector(s)
    !       da  double precision scalar multiplier
    !       dx  double precision vector with n elements
    !     incx  storage spacing between elements of dx
    !       dy  double precision vector with n elements
    !     incy  storage spacing between elements of dy
    !
    !     --output--
    !       dy  double precision result (unchanged if n <= 0)
    !
    !     overwrite double precision dy with double precision da*dx + dy.
    !     for i = 0 to n-1, replace  dy(ly+i*incy) with da*dx(lx+i*incx) +
    !       dy(ly+i*incy),
    !     where lx = 1 if incx >= 0, else lx = 1+(1-n)*incx, and ly is
    !     defined in a similar way using incy.
    !
    !***references  c. l. lawson, r. j. hanson, d. r. kincaid and f. t.
    !                 krogh, basic linear algebra subprograms for fortran
    !                 usage, algorithm no. 539, transactions on mathematical
    !                 software 5, 3 (september 1979), pp. 308-323.
    !***routines called  (none)
    !***revision history  (yymmdd)
    !   791001  date written
    !   890831  modified array declarations.  (wrb)
    !   890831  revision date from version 3.2
    !   891214  prologue converted to version 4.0 format.  (bab)
    !   920310  corrected definition of lx in description.  (wrb)
    !   920501  reformatted the references section.  (wrb)
    !***end prologue  daxpy
    !===============================================================================================
    subroutine daxpy (n, da, dx, incx, dy, incy)
    
        double precision dx(*), dy(*), da
        integer  :: n, incx, incy
        
        integer  :: ix, iy, i, m, mp1, ns

        ! first executable statement  daxpy
        if (n<=0 .or. da==0.0d0) return
        
        if (incx == incy) if (incx-1) 5,20,60
        
        ! code for unequal or nonpositive increments.
5       ix = 1
        iy = 1
        if (incx < 0) ix = (-n+1)*incx + 1
        if (incy < 0) iy = (-n+1)*incy + 1
        do 10 i = 1,n
            dy(iy) = dy(iy) + da*dx(ix)
            ix = ix + incx
            iy = iy + incy
10      continue
        return
        
        ! code for both increments equal to 1. clean-up loop so remaining vector length is a multiple of 4.
20      m = MOD(n,4)
        if (m == 0) go to 40
        do 30 i = 1,m
            dy(i) = dy(i) + da*dx(i)
30      continue
        if (n < 4) return
40      mp1 = m + 1
        do 50 i = mp1,n,4
            dy(i) = dy(i) + da*dx(i)
            dy(i+1) = dy(i+1) + da*dx(i+1)
            dy(i+2) = dy(i+2) + da*dx(i+2)
            dy(i+3) = dy(i+3) + da*dx(i+3)
50      continue
        return
        
        ! code for equal, positive, non-unit increments.
60      ns = n*incx
        do 70 i = 1,ns,incx
            dy(i) = da*dx(i) + dy(i)
70      continue

        return
    end subroutine daxpy

    !$
    !===============================================================================================
    !***begin prologue  dcopy
    !***purpose  copy a vector.
    !***category  d1a5
    !***type      double precision (scopy-s, dcopy-d, ccopy-c, icopy-i)
    !***keywords  blas, copy, linear algebra, vector
    !***author  lawson, c. l., (jpl)
    !           hanson, r. j., (snla)
    !           kincaid, d. r., (u. of texas)
    !           krogh, f. t., (jpl)
    !***description
    !
    !                b l a s  subprogram
    !    description of parameters
    !
    !     --input--
    !        n  number of elements in input vector(s)
    !       dx  double precision vector with n elements
    !     incx  storage spacing between elements of dx
    !       dy  double precision vector with n elements
    !     incy  storage spacing between elements of dy
    !
    !     --output--
    !       dy  copy of vector dx (unchanged if n <= 0)
    !
    !     copy double precision dx to double precision dy.
    !     for i = 0 to n-1, copy dx(lx+i*incx) to dy(ly+i*incy),
    !     where lx = 1 if incx >= 0, else lx = 1+(1-n)*incx, and ly is
    !     defined in a similar way using incy.
    !
    !***references  c. l. lawson, r. j. hanson, d. r. kincaid and f. t.
    !                 krogh, basic linear algebra subprograms for fortran
    !                 usage, algorithm no. 539, transactions on mathematical
    !                 software 5, 3 (september 1979), pp. 308-323.
    !***routines called  (none)
    !***revision history  (yymmdd)
    !   791001  date written
    !   890831  modified array declarations.  (wrb)
    !   890831  revision date from version 3.2
    !   891214  prologue converted to version 4.0 format.  (bab)
    !   920310  corrected definition of lx in description.  (wrb)
    !   920501  reformatted the references section.  (wrb)
    !***end prologue  dcopy
    !===============================================================================================
    subroutine dcopy (n, dx, incx, dy, incy)
    
        double precision dx(*), dy(*)
        integer  :: n, incx, incy

        integer  :: ix, iy, i, m, mp1, ns
        
        ! first executable statement  dcopy
        if (n <= 0) return
        
        if (incx == incy) if (incx-1) 5,20,60
        
        ! code for unequal or nonpositive increments.
5       ix = 1
        iy = 1
        if (incx < 0) ix = (-n+1)*incx + 1
        if (incy < 0) iy = (-n+1)*incy + 1
        do 10 i = 1,n
            dy(iy) = dx(ix)
            ix = ix + incx
            iy = iy + incy
10      continue
        return
        
        ! code for both increments equal to 1. clean-up loop so remaining vector length is a multiple of 7.
20      m = MOD(n,7)
        if (m == 0) go to 40
        do 30 i = 1,m
            dy(i) = dx(i)
30      continue
        if (n < 7) return
40      mp1 = m + 1
        do 50 i = mp1,n,7
            dy(i) = dx(i)
            dy(i+1) = dx(i+1)
            dy(i+2) = dx(i+2)
            dy(i+3) = dx(i+3)
            dy(i+4) = dx(i+4)
            dy(i+5) = dx(i+5)
            dy(i+6) = dx(i+6)
50      continue
        return
        
        ! code for equal, positive, non-unit increments.
60      ns = n*incx
        do 70 i = 1,ns,incx
            dy(i) = dx(i)
70      continue
        
        return
    end subroutine dcopy

    !$
    !===============================================================================================
    !***begin prologue  ddot
    !***purpose  compute the inner product of two vectors.
    !***category  d1a4
    !***type      double precision (sdot-s, ddot-d, cdotu-c)
    !***keywords  blas, inner product, linear algebra, vector
    !***author  lawson, c. l., (jpl)
    !           hanson, r. j., (snla)
    !           kincaid, d. r., (u. of texas)
    !           krogh, f. t., (jpl)
    !***description
    !
    !                b l a s  subprogram
    !    description of parameters
    !
    !     --input--
    !        n  number of elements in input vector(s)
    !       dx  double precision vector with n elements
    !     incx  storage spacing between elements of dx
    !       dy  double precision vector with n elements
    !     incy  storage spacing between elements of dy
    !
    !     --output--
    !     ddot  double precision dot product (zero if n <= 0)
    !
    !     returns the dot product of double precision dx and dy.
    !     ddot = sum for i = 0 to n-1 of  dx(lx+i*incx) * dy(ly+i*incy),
    !     where lx = 1 if incx >= 0, else lx = 1+(1-n)*incx, and ly is
    !     defined in a similar way using incy.
    !
    !***references  c. l. lawson, r. j. hanson, d. r. kincaid and f. t.
    !                 krogh, basic linear algebra subprograms for fortran
    !                 usage, algorithm no. 539, transactions on mathematical
    !                 software 5, 3 (september 1979), pp. 308-323.
    !***routines called  (none)
    !***revision history  (yymmdd)
    !   791001  date written
    !   890831  modified array declarations.  (wrb)
    !   890831  revision date from version 3.2
    !   891214  prologue converted to version 4.0 format.  (bab)
    !   920310  corrected definition of lx in description.  (wrb)
    !   920501  reformatted the references section.  (wrb)
    !***end prologue  ddot
    !===============================================================================================
    double precision function ddot (n, dx, incx, dy, incy)
    
        double precision dx(*), dy(*)

        integer  :: n, incx, incy, ix, iy, i, m, mp1, ns
        
        ! first executable statement  ddot
        ddot = 0.0d0
        if (n <= 0) return
        
        if (incx == incy) if (incx-1) 5,20,60
        
        ! code for unequal or nonpositive increments.
5       ix = 1
        iy = 1
        if (incx < 0) ix = (-n+1)*incx + 1
        if (incy < 0) iy = (-n+1)*incy + 1
        do 10 i = 1,n
            ddot = ddot + dx(ix)*dy(iy)
            ix = ix + incx
            iy = iy + incy
10      continue
        return
        
        ! code for both increments equal to 1. clean-up loop so remaining vector length is a multiple of 5.
20      m = MOD(n,5)
        if (m == 0) go to 40
        do 30 i = 1,m
            ddot = ddot + dx(i)*dy(i)
30      continue
        if (n < 5) return
40      mp1 = m + 1
        do 50 i = mp1,n,5
            ddot = ddot + dx(i)*dy(i) + dx(i+1)*dy(i+1) + dx(i+2)*dy(i+2) + dx(i+3)*dy(i+3) + dx(i+4)*dy(i+4)
50      continue
        return
        
        ! code for equal, positive, non-unit increments.
60      ns = n*incx
        do 70 i = 1,ns,incx
            ddot = ddot + dx(i)*dy(i)
70      continue
        
        return
    end function ddot

    !$
    !===============================================================================================
    !***begin prologue  dnrm2
    !***purpose  compute the euclidean length (l2 norm) of a vector.
    !***category  d1a3b
    !***type      double precision (snrm2-s, dnrm2-d, scnrm2-c)
    !***keywords  blas, euclidean length, euclidean norm, l2,
    !             linear algebra, unitary, vector
    !***author  lawson, c. l., (jpl)
    !           hanson, r. j., (snla)
    !           kincaid, d. r., (u. of texas)
    !           krogh, f. t., (jpl)
    !***description
    !
    !                b l a s  subprogram
    !    description of parameters
    !
    !     --input--
    !        n  number of elements in input vector(s)
    !       dx  double precision vector with n elements
    !     incx  storage spacing between elements of dx
    !
    !     --output--
    !    dnrm2  double precision result (zero if n <= 0)
    !
    !     euclidean norm of the n-vector stored in dx with storage
    !     increment incx.
    !     if n <= 0, return with result = 0.
    !     if n >= 1, then incx must be >= 1
    !
    !     four phase method using two built-in constants that are
    !     hopefully applicable to all machines.
    !         cutlo = maximum of  SQRT(u/eps)  over all known machines.
    !         cuthi = minimum of  SQRT(v)      over all known machines.
    !     where
    !         eps = smallest no. such that eps + 1. > 1.
    !         u   = smallest positive no.   (underflow limit)
    !         v   = largest  no.            (overflow  limit)
    !
    !     brief outline of algorithm.
    !
    !     phase 1 scans zero components.
    !     move to phase 2 when a component is nonzero and <= cutlo
    !     move to phase 3 when a component is > cutlo
    !     move to phase 4 when a component is >= cuthi/m
    !     where m = n for x() real and m = 2*n for complex.
    !
    !     values for cutlo and cuthi.
    !     from the environmental parameters listed in the imsl converter
    !     document the limiting values are as follows:
    !     cutlo, s.p.   u/eps = 2**(-102) for  honeywell.  close seconds are
    !                   univac and dec at 2**(-103)
    !                   thus cutlo = 2**(-51) = 4.44089e-16
    !     cuthi, s.p.   v = 2**127 for univac, honeywell, and dec.
    !                   thus cuthi = 2**(63.5) = 1.30438e19
    !     cutlo, d.p.   u/eps = 2**(-67) for honeywell and dec.
    !                   thus cutlo = 2**(-33.5) = 8.23181d-11
    !     cuthi, d.p.   same as s.p.  cuthi = 1.30438d19
    !     data cutlo, cuthi /8.232d-11,  1.304d19/
    !     data cutlo, cuthi /4.441e-16,  1.304e19/
    !
    !***references  c. l. lawson, r. j. hanson, d. r. kincaid and f. t.
    !                 krogh, basic linear algebra subprograms for fortran
    !                 usage, algorithm no. 539, transactions on mathematical
    !                 software 5, 3 (september 1979), pp. 308-323.
    !***routines called  (none)
    !***revision history  (yymmdd)
    !   791001  date written
    !   890531  changed all specific intrinsics to generic.  (wrb)
    !   890831  modified array declarations.  (wrb)
    !   890831  revision date from version 3.2
    !   891214  prologue converted to version 4.0 format.  (bab)
    !   920501  reformatted the references section.  (wrb)
    !***end prologue  dnrm2
    !===============================================================================================
    double precision function dnrm2 (n, dx, incx)
    
        integer next
        integer  :: n, nn, incx, i, j
        double precision dx(*), cutlo, cuthi, hitest, sum, xmax, zero, one
        save cutlo, cuthi, zero, one

        data zero, one /0.0d0, 1.0d0/
        data cutlo, cuthi /8.232d-11,  1.304d19/
        
        ! first executable statement  dnrm2
        if (n > 0) go to 10
        
        dnrm2  = zero
        go to 300
        
10      assign 30 to next
        sum = zero
        nn = n * incx

        ! begin main loop
        i = 1
20      go to next,(30, 50, 70, 110)
30      if (ABS(dx(i)) > cutlo) go to 85
        assign 50 to next
        xmax = zero
        
        ! phase 1.  sum is zero
50      if (dx(i) == zero) go to 200
        if (ABS(dx(i)) > cutlo) go to 85
        
        ! prepare for phase 2.
        assign 70 to next
        go to 105
        
        ! prepare for phase 4.
100     i = j
        assign 110 to next
        sum = (sum / dx(i)) / dx(i)
105     xmax = ABS(dx(i))
        go to 115
        
        ! phase 2.  sum is small. scale to avoid destructive underflow.
70      if (ABS(dx(i)) > cutlo) go to 75
        
        ! common code for phases 2 and 4. in phase 4 sum is large.  scale to avoid overflow.
110     if (ABS(dx(i)) <= xmax) go to 115
        sum = one + sum * (xmax / dx(i))**2
        xmax = ABS(dx(i))
        go to 200
        
115     sum = sum + (dx(i)/xmax)**2
        go to 200
        
        ! prepare for phase 3.
75      sum = (sum * xmax) * xmax
        
        ! for real or d.p. set hitest = cuthi/n, for complex, set hitest = cuthi/(2*n)
85      hitest = cuthi / n
        
        !  phase 3.  sum is mid-range.  no scaling.
        do 95 j = i,nn,incx
            if (ABS(dx(j)) >= hitest) go to 100
95      sum = sum + dx(j)**2
        dnrm2 = SQRT(sum)
        go to 300
        
200     continue
        i = i + incx
        if (i <= nn) go to 20

        ! end of main loop. compute square root and adjust for scaling.
        dnrm2 = xmax * SQRT(sum)
300     continue
        
        return
    end function dnrm2

    !$
    !===============================================================================================
    !***begin prologue  dscal
    !***purpose  multiply a vector by a constant.
    !***category  d1a6
    !***type      double precision (sscal-s, dscal-d, cscal-c)
    !***keywords  blas, linear algebra, scale, vector
    !***author  lawson, c. l., (jpl)
    !           hanson, r. j., (snla)
    !           kincaid, d. r., (u. of texas)
    !           krogh, f. t., (jpl)
    !***description
    !
    !                b l a s  subprogram
    !    description of parameters
    !
    !     --input--
    !        n  number of elements in input vector(s)
    !       da  double precision scale factor
    !       dx  double precision vector with n elements
    !     incx  storage spacing between elements of dx
    !
    !     --output--
    !       dx  double precision result (unchanged if n<=0)
    !
    !     replace double precision dx by double precision da*dx.
    !     for i = 0 to n-1, replace dx(ix+i*incx) with  da * dx(ix+i*incx),
    !     where ix = 1 if incx >= 0, else ix = 1+(1-n)*incx.
    !
    !***references  c. l. lawson, r. j. hanson, d. r. kincaid and f. t.
    !                 krogh, basic linear algebra subprograms for fortran
    !                 usage, algorithm no. 539, transactions on mathematical
    !                 software 5, 3 (september 1979), pp. 308-323.
    !***routines called  (none)
    !***revision history  (yymmdd)
    !   791001  date written
    !   890831  modified array declarations.  (wrb)
    !   890831  revision date from version 3.2
    !   891214  prologue converted to version 4.0 format.  (bab)
    !   900821  modified to correct problem with a negative increment.
    !           (wrb)
    !   920501  reformatted the references section.  (wrb)
    !***end prologue  dscal
    !===============================================================================================
    subroutine dscal (n, da, dx, incx)
    
        double precision da, dx(*)
        integer i, incx, ix, m, mp1, n
        
        ! first executable statement  dscal
        if (n <= 0) return
        
        if (incx == 1) goto 20
        
        ! code for increment not equal to 1.
        ix = 1
        if (incx < 0) ix = (-n+1)*incx + 1
        do 10 i = 1,n
            dx(ix) = da*dx(ix)
            ix = ix + incx
10      continue
        return
        
        ! code for increment equal to 1. clean-up loop so remaining vector length is a multiple of 5.
20      m = MOD(n,5)
        if (m == 0) goto 40
        do 30 i = 1,m
            dx(i) = da*dx(i)
30      continue
        if (n < 5) return
40      mp1 = m + 1
        do 50 i = mp1,n,5
            dx(i) = da*dx(i)
            dx(i+1) = da*dx(i+1)
            dx(i+2) = da*dx(i+2)
            dx(i+3) = da*dx(i+3)
            dx(i+4) = da*dx(i+4)
50      continue
        
        return
    end subroutine dscal

    !$
    !===============================================================================================
    !***begin prologue  idamax
    !***purpose  find the smallest index of that component of a vector
    !            having the maximum magnitude.
    !***category  d1a2
    !***type      double precision (isamax-s, idamax-d, icamax-c)
    !***keywords  blas, linear algebra, maximum component, vector
    !***author  lawson, c. l., (jpl)
    !           hanson, r. j., (snla)
    !           kincaid, d. r., (u. of texas)
    !           krogh, f. t., (jpl)
    !***description
    !
    !                b l a s  subprogram
    !    description of parameters
    !
    !     --input--
    !        n  number of elements in input vector(s)
    !       dx  double precision vector with n elements
    !     incx  storage spacing between elements of dx
    !
    !     --output--
    !   idamax  smallest index (zero if n <= 0)
    !
    !     find smallest index of maximum magnitude of double precision dx.
    !     idamax = first i, i = 1 to n, to maximize ABS(dx(ix+(i-1)*incx)),
    !     where ix = 1 if incx >= 0, else ix = 1+(1-n)*incx.
    !
    !***references  c. l. lawson, r. j. hanson, d. r. kincaid and f. t.
    !                 krogh, basic linear algebra subprograms for fortran
    !                 usage, algorithm no. 539, transactions on mathematical
    !                 software 5, 3 (september 1979), pp. 308-323.
    !***routines called  (none)
    !***revision history  (yymmdd)
    !   791001  date written
    !   890531  changed all specific intrinsics to generic.  (wrb)
    !   890531  revision date from version 3.2
    !   891214  prologue converted to version 4.0 format.  (bab)
    !   900821  modified to correct problem with a negative increment.
    !           (wrb)
    !   920501  reformatted the references section.  (wrb)
    !***end prologue  idamax
    !===============================================================================================
    integer function idamax (n, dx, incx)
    
        double precision dx(*), dmax, xmag
        integer i, incx, ix, n
        
        ! first executable statement  idamax
        idamax = 0
        if (n <= 0) return
        idamax = 1
        if (n == 1) return
        
        if (incx == 1) goto 20
        
        ! code for increments not equal to 1.
        ix = 1
        if (incx < 0) ix = (-n+1)*incx + 1
        dmax = ABS(dx(ix))
        ix = ix + incx
        do 10 i = 2,n
            xmag = ABS(dx(ix))
            if (xmag > dmax) then
                idamax = i
                dmax = xmag
            end if
            ix = ix + incx
10      continue
        return
        
        ! code for increments equal to 1.
20      dmax = ABS(dx(1))
        do 30 i = 2,n
            xmag = ABS(dx(i))
            if (xmag > dmax) then
                idamax = i
                dmax = xmag
            end if
30      continue
        
        return
    end function idamax
    
end module self_vode

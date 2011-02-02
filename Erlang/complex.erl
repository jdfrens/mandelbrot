%% Taken from http://www.trapexit.org/Complex_Numbers and modified.
%%
%% Primary copyright remains with original source.
%% Modifications released under Creative Commons Attribution-Share Alike 3.0 United States License.
%%
%% Most important change: renamed the module.

-module(complex).
-export([make/1, make/2, is_complex/1,
         real/1, imag/1,
         add/2, subtract/2, multiply/2, divide/2,
         sqrt/1, root/2, pow/2, reciprocal/1, absolute/1,
         polar/1, rect/1,
         cos/1, sin/1, tan/1, cot/1,
         cosh/1, sinh/1, tanh/1, coth/1]).

%% What does a complex number look like?
-record(complex, {r=0.0, i=0.0}).

%% Shorten the complex number validity test by wrapping it as a macro
%% This also allows the function to be used a guard.
-define(IsComplex(Z), is_record(Z, complex)).

is_complex(Z) -> ?IsComplex(Z).

%% Go make a complex number
make(Real, Imag) when is_integer(Real); is_float(Real),
                      is_integer(Imag); is_float(Imag) -> #complex{r=Real, i=Imag}.
make(Real) when is_integer(Real); is_float(Real)       -> make(Real, 0);
make(Str)  when is_list(Str)                           ->
    [{Real,_}, {Imag,_}] = lists:map(fun string:to_float/1, string:tokens(Str, "+i")),
    make(Real, Imag);
make(Real)                                             -> Real.

%% Return the real or imaginary parts of a complex number
real(Z) when is_integer(Z); is_float(Z) -> Z;
real(Z) when ?IsComplex(Z) -> Z#complex.r.

imag(Z) when ?IsComplex(Z) -> Z#complex.i.

%% ----------------------------------------------------------------------------
%% Arithmetic functions
%%
%% Add complex to complex, or complex to real
add(Z1, Z2) when ?IsComplex(Z1), ?IsComplex(Z2)            -> #complex{r=Z1#complex.r + Z2#complex.r, i=Z1#complex.i + Z2#complex.i};
add(Z, R)   when ?IsComplex(Z), is_integer(R); is_float(R) -> #complex{r=Z#complex.r + R, i=Z#complex.i}.

%% Subtract complex Z2 from complex Z1, or real from complex
subtract(Z1, Z2) when ?IsComplex(Z1), ?IsComplex(Z2)            -> #complex{r=Z1#complex.r - Z2#complex.r, i=Z1#complex.i - Z2#complex.i};
subtract(Z, R)   when ?IsComplex(Z), is_integer(R); is_float(R) -> #complex{r=Z#complex.r - R, i=Z#complex.i}.

%% Multiply complex by complex, or complex by real
multiply(Z1, Z2) when ?IsComplex(Z1), ?IsComplex(Z2) ->
  #complex{r=Z1#complex.r * Z2#complex.r - Z1#complex.i * Z2#complex.i,
           i=Z1#complex.r * Z2#complex.i + Z1#complex.i * Z2#complex.r};
multiply(Z1, R)  when ?IsComplex(Z1),
                      is_integer(R); is_float(R)     -> #complex{r=Z1#complex.r * R, i=Z1#complex.i * R}.

%% Divide complex Z1 by complex Z2 or complex by real
divide(Z1, Z2) when abs(Z2#complex.r) >= abs(Z2#complex.i) ->
  P = Z2#complex.i / Z2#complex.r,
  Q = Z2#complex.r + P * Z2#complex.i,

  #complex{r=(Z1#complex.r + P * Z1#complex.i) / Q,
           i=(Z1#complex.i - P * Z1#complex.r) / Q};

divide(Z1, Z2) when ?IsComplex(Z1), ?IsComplex(Z2) ->
  P = Z2#complex.r / Z2#complex.i,
  Q = Z2#complex.i + P * Z2#complex.r,

  #complex{r=(Z1#complex.r * P + Z1#complex.i) / Q,
           i=(Z1#complex.i * P - Z1#complex.r) / Q};

divide(Z,R) when ?IsComplex(Z), is_integer(R); is_float(R) ->
  #complex{r=Z#complex.r / R, i=Z#complex.i / R}.

%% Absolute value of complex number Z -> |Z|
absolute(Z) when Z#complex.r == 0          -> Z#complex.i;
absolute(Z) when Z#complex.i == 0          -> Z#complex.r;
absolute(Z) when Z#complex.r > Z#complex.i -> abs(Z#complex.r) * math:sqrt(1 + math:pow(abs(Z#complex.i)/abs(Z#complex.r),2));
absolute(Z)                                -> abs(Z#complex.i) * math:sqrt(1 + math:pow(abs(Z#complex.r)/abs(Z#complex.i),2)).


%% Square root of complex Z
%% sqrt(Z) when ?IsComplex(Z) -> root(Z,2).
sqrt(Z) when ?IsComplex(Z) ->
  Modulus = math:pow((Z#complex.r * Z#complex.r) + (Z#complex.i * Z#complex.i), 0.5),
  #complex{r=math:pow((Modulus + Z#complex.r)/2, 0.5),
           i=sign(Z#complex.i) * math:pow((Modulus - Z#complex.r)/2, 0.5)}.


%% Nth root of complex Z
root(Z,N) when Z#complex.r >= 0, Z#complex.i == 0 -> #complex{r=math:pow(Z#complex.r, (1/N)), i=0};
root(Z,N) when Z#complex.r  < 0, Z#complex.i == 0 -> #complex{r=0, i=math:pow(abs(Z#complex.r), (1/N))};
root(Z,N) when Z#complex.i /= 0 ->
  Zpolar = polar(Z),
  Modulo = math:pow(Zpolar#complex.r, (1/N)),
  Alpha  = (Zpolar#complex.i + 2 * math:pi()) / N,

  #complex{r=Modulo * math:cos(Alpha), i=Modulo * math:sin(Alpha)}.


%% Raise complex Z to the power of N
pow(Z,N) when Z#complex.i == 0, is_integer(N); is_float(N) -> #complex{r=math:pow(Z#complex.r,N), i=0};
pow(Z,N) when ?IsComplex(Z),    is_integer(N); is_float(N) ->
  Zpolar = polar(Z),
  rect(#complex{r=math:pow(Zpolar#complex.r,N), i=Zpolar#complex.r * N}).

%% Reciprocal of complex Z
reciprocal(Z) when ?IsComplex(Z) -> divide(#complex{r=1,i=0},Z).

%% ----------------------------------------------------------------------------
%% Coordinate conversion functions
%%
%% Rectangular to polar conversion
polar(Z) when Z#complex.r == 0                  -> #complex{r=absolute(Z), i=(sign(Z#complex.i) * math:pi() / 2)};
polar(Z) when Z#complex.r > 0                   -> #complex{r=absolute(Z), i=math:atan(Z#complex.i / Z#complex.r)};
polar(Z) when Z#complex.r < 0, Z#complex.i /= 0 -> #complex{r=absolute(Z), i=(sign(Z#complex.i) * math:pi() + math:atan(Z#complex.i / Z#complex.r))};
polar(Z) when Z#complex.r < 0, Z#complex.i == 0 -> #complex{r=absolute(Z), i=math:pi()}.

%% Polar to rectangular conversion
rect(Z) when Z#complex.r == 0 -> #complex{r=0, i=0};
rect(Z) when Z#complex.i == 0 -> #complex{r=Z#complex.r, i=0};
rect(Z) when ?IsComplex(Z)    -> #complex{r=Z#complex.r * math:cos(Z#complex.i), i=Z#complex.r * math:sin(Z#complex.i)}.

%% ----------------------------------------------------------------------------
%% Trigonometrical functions
%%
cos(Z) when ?IsComplex(Z) -> #complex{r=math:cos(Z#complex.r) * math:cosh(Z#complex.i), i=math:sin(Z#complex.r) * math:sinh(Z#complex.i) * -1}.
sin(Z) when ?IsComplex(Z) -> #complex{r=math:sin(Z#complex.r) * math:cosh(Z#complex.i), i=math:cos(Z#complex.r) * math:sinh(Z#complex.i)}.
tan(Z) when ?IsComplex(Z) -> divide(sin(Z), cos(Z)).
cot(Z) when ?IsComplex(Z) -> divide(cos(Z), sin(Z)).

%% ----------------------------------------------------------------------------
%% Hyperbolic functions
%%
cosh(Z) when ?IsComplex(Z) -> #complex{r=math:cosh(Z#complex.r) * math:cos(Z#complex.i), i=math:sinh(Z#complex.r) * math:sin(Z#complex.i)}.
sinh(Z) when ?IsComplex(Z) -> #complex{r=math:sinh(Z#complex.r) * math:cos(Z#complex.i), i=math:cosh(Z#complex.r) * math:sin(Z#complex.i)}.
tanh(Z) when ?IsComplex(Z) -> divide(sinh(Z), cosh(Z)).
coth(Z) when ?IsComplex(Z) -> divide(cosh(Z), sinh(Z)).

%% ----------------------------------------------------------------------------
%% Utility functions
%%
%% Return +1, 0 or -1 depending on comparison of N with 0
sign(N) when N > 0 -> +1;
sign(N) when N < 0 -> -1;
sign(_)            -> 0.

% written by: J.Paul Elhorst 2016
% University of Groningen
% Department of Economics, Econometrics and Finance
% 9700AV Groningen
% the Netherlands
% j.p.elhorst@rug.nl
%
% REFERENCES: 
% Yesilyurt M.E., Elhorst J.P. (2017), 
% Impacts of neighboring countries on military expenditures; a dynamic spatial panel approach. 
% Journal of Peace Research. 
%
% Produces Table II
%
%A=xlsread('X:\Data\Matlab\Files JPR\datasetwb2015.xlsx'); % WB/SIPRI data set with T=15
A=xlsread('X:\Data\Matlab\Files JPR\datasetcow2015.xlsx'); % COW data set with T=15
W1=xlsread('X:\Data\Matlab\Files JPR\BC1_144.xlsx'); %binary contiguity p=3 matrix
% dimensions of the problem
T=15; % number of time periods
N=144; % number of regions
W=zeros(N,N);
for i=1:N
    for j=1:N
        if (W1(i,j)==1) W(i,j)=1; end
    end
end
% France 45
W(6,45)=0;W(16,45)=0;W(22,45)=0;W(43,45)=0;W(79,45)=0;W(84,45)=0;W(89,45)=0;W(101,45)=0;W(140,45)=0;
W(45,6)=0;W(45,16)=0;W(45,22)=0;W(45,43)=0;W(45,79)=0;W(45,84)=0;W(45,89)=0;W(45,101)=0;W(45,140)=0;
% Netherlands 92
W(37,92)=0;W(137,92)=0;W(140,92)=0;
W(92,37)=0;W(92,137)=0;W(92,140)=0;
% UK 136
W(4,136)=0;W(27,136)=0;W(32,136)=0;W(33,136)=0;W(37,136)=0;W(39,136)=0;W(56,136)=0;W(73,136)=0;W(88,136)=0;W(118,136)=0;W(137,136)=0;W(140,136)=0;
W(136,4)=0;W(136,27)=0;W(136,32)=0;W(136,33)=0;W(136,37)=0;W(136,39)=0;W(136,56)=0;W(136,73)=0;W(136,88)=0;W(136,118)=0;W(136,137)=0;W(136,140)=0;
% US 137
W(92,137)=0;W(93,137)=0;W(136,137)=0;
W(137,92)=0;W(137,93)=0;W(137,136)=0;
%
NN=sum(W,2);
W1=W;
%
% Dominant matrix, China=26 France=45 Russia =109 UK=136 US=137
%
WDOM=zeros(N,N);
WDOMALL=zeros(N,N);
xd=[26 45 109 136 137];
for i=1:5
    for j=1:5
        WDOM(xd(i),xd(j))=1;
        WDOMALL(xd(i),xd(j))=1;
    end
    for j=1:N
        WDOMALL(xd(i),j)=1;
    end
end
for i=1:N
    WDOM(i,i)=0;
    WDOMALL(i,i)=0;
end
We=xlsread('X:\Data\Matlab\Files JPR\Wenemy.xlsx'); % enemy matrix

% W1 is first-order binary contiguity matrix
% W2 is second-order binary contiguity matrix
% W3 is third-order binary contiguity matrix
% WE is enemy matrix
% WE1 sum of first-order binary contiguity and enemy matrix
% WD  sum of first-order binary contiguity and DOM matrix
% WDall sum of first-order binary contiguity and DOMALL matrix

% row-normalize W
W2=W1*W1;
for i=1:N
    for j=1:N
        if (i==j) W2(i,j)=0;
        elseif (W2(i,j)>0) W2(i,j)=1;
        end
    end
end
W3=W2*W1;
for i=1:N
    for j=1:N
        if (i==j) W3(i,j)=0;
        elseif (W3(i,j)>0) W3(i,j)=1;
        end
    end
end
W4=ones(N,N);
for i=1:N
    W4(i,i)=0;
end
WE=zeros(N,N);
WE1=zeros(N,N);
WD=zeros(N,N);
WDall=zeros(N,N);
WDEDOM=zeros(N,N);
for i=1:N
    for j=1:N
        if (We(i,j)==1) WE(i,j)=1; end
        if (We(i,j)==1 || W1(i,j)==1) WE1(i,j)=1; end
        if (WDOM(i,j)==1 || W1(i,j)==1) WD(i,j)=1; end
        if (WDOMALL(i,j)==1 || W1(i,j)==1) WDall(i,j)=1; end
        if (We(i,j)==1 || WDOM(i,j)==1 || W1(i,j)==1) WDEDOM(i,j)=1; end
    end
end
meannumberofneighbors=mean(sum(W1,2))
meannumberofneighbors=mean(sum(W2,2))
meannumberofneighbors=mean(sum(W3,2))
meannumberofneighbors=mean(sum(W4,2))
meannumberofneighbors=mean(sum(WE,2))
meannumberofneighbors=mean(sum(WE1,2))
meannumberofneighbors=mean(sum(WD,2))
meannumberofneighbors=mean(sum(WDall,2))
meannumberofneighbors=mean(sum(WDEDOM,2))
%
y=A(:,2); % ratio
yalt=A(:,9)+log(100); % expenditure
nobs=N*T;
for i=1:nobs
if (A(i,13)==1) A(i,14)=log(A(i,14)*1000000/A(i,11)); else A(1,14)=0; end
end
x=A(:,[3,14,5,6,7]); % column numbers in the data matrix that correspond to the independent variables
corrcoef([y x])
mean([y x])
std([y x])
max([y x])
min([y x])

xconstant=ones(N*T,1);
% 
% beta prior for rho
prior.c = 1.01;
prior.d = 1.01;
opt.tol = 1e-3; opt.disp = 0;
incr = 0.001;
in.cnames = strvcat('W-miles log marginals','model probs');
in.rnames = strvcat('Models','sar','sdm','sem','sdem');
%
% Calculation of Bayesian posterior model probabilities for static models
% 
ted = 1; % set ted=0 for model with spatial fixed effects without time dummies, % set ted=1 for model with spatial and time period fixed effects
model=3;

[yf,xf,meanny,meannx,meanty,meantx]=demean(y,x,N,T,model);

W=normw(W1);
[n,junk] = size(W);
lambda = eigs(sparse(W),speye(n),1,'SR',opt); 
rmin = real(1/lambda) + incr;   
rmax = 1.0 - incr;
[rmin rmax]
out=lndetfull(sparse(W),rmin,rmax);
result = log_marginal_panelprob(yf,xf,W,N,T,model,out,prior,rmin,rmax,incr); 
in.cnames = strvcat('W-miles log marginals','model probs');
in.rnames = strvcat('Models','sar','sdm','sem','sdem');
mprint([result.lmarginal result.probs],in);
lmarginal=[result.lmarginal];

W=normw(W2);
[n,junk] = size(W);
lambda = eigs(sparse(W),speye(n),1,'SR',opt); 
rmin = real(1/lambda) + incr;   
rmax = 1.0 - incr;
[rmin rmax]
out=lndetfull(sparse(W),rmin,rmax);
result = log_marginal_panelprob(yf,xf,W,N,T,model,out,prior,rmin,rmax,incr); 
in.cnames = strvcat('W-miles log marginals','model probs');
in.rnames = strvcat('Models','sar','sdm','sem','sdem');
mprint([result.lmarginal result.probs],in);
lmarginal=[lmarginal;result.lmarginal];

W=normw(W3);
[n,junk] = size(W);
lambda = eigs(sparse(W),speye(n),1,'SR',opt); 
rmin = real(1/lambda) + incr;   
rmax = 1.0 - incr;
[rmin rmax]
out=lndetfull(sparse(W),rmin,rmax);
result = log_marginal_panelprob(yf,xf,W,N,T,model,out,prior,rmin,rmax,incr); 
in.cnames = strvcat('W-miles log marginals','model probs');
in.rnames = strvcat('Models','sar','sdm','sem','sdem');
mprint([result.lmarginal result.probs],in);
lmarginal=[lmarginal;result.lmarginal];

W=normw(WE);
[n,junk] = size(W);
lambda = eigs(sparse(W),speye(n),1,'SR',opt); 
rmin = real(1/lambda) + incr;   
rmax = 1.0 - incr;
[rmin rmax]
out=lndetfull(sparse(W),rmin,rmax);
result = log_marginal_panelprob(yf,xf,W,N,T,model,out,prior,rmin,rmax,incr); 
in.cnames = strvcat('W-miles log marginals','model probs');
in.rnames = strvcat('Models','sar','sdm','sem','sdem');
mprint([result.lmarginal result.probs],in);
lmarginal=[lmarginal;result.lmarginal];

W=normw(WE1);
[n,junk] = size(W);
lambda = eigs(sparse(W),speye(n),1,'SR',opt); 
rmin = real(1/lambda) + incr;   
rmax = 1.0 - incr;
[rmin rmax]
out=lndetfull(sparse(W),rmin,rmax);
result = log_marginal_panelprob(yf,xf,W,N,T,model,out,prior,rmin,rmax,incr); 
in.cnames = strvcat('W-miles log marginals','model probs');
in.rnames = strvcat('Models','sar','sdm','sem','sdem');
mprint([result.lmarginal result.probs],in);
lmarginal=[lmarginal;result.lmarginal];

W=normw(WD);
[n,junk] = size(W);
lambda = eigs(sparse(W),speye(n),1,'SR',opt); 
rmin = real(1/lambda) + incr;   
rmax = 1.0 - incr;
[rmin rmax]
out=lndetfull(sparse(W),rmin,rmax);
result = log_marginal_panelprob(yf,xf,W,N,T,model,out,prior,rmin,rmax,incr); 
in.cnames = strvcat('W-miles log marginals','model probs');
in.rnames = strvcat('Models','sar','sdm','sem','sdem');
mprint([result.lmarginal result.probs],in);
lmarginal=[lmarginal;result.lmarginal];

W=normw(WDall);
[n,junk] = size(W);
lambda = eigs(sparse(W),speye(n),1,'SR',opt); 
rmin = real(1/lambda) + incr;   
rmax = 1.0 - incr;
[rmin rmax]
out=lndetfull(sparse(W),rmin,rmax);
result = log_marginal_panelprob(yf,xf,W,N,T,model,out,prior,rmin,rmax,incr); 
in.cnames = strvcat('W-miles log marginals','model probs');
in.rnames = strvcat('Models','sar','sdm','sem','sdem');
mprint([result.lmarginal result.probs],in);
lmarginal=[lmarginal;result.lmarginal];

W=normw(WDEDOM);
[n,junk] = size(W);
lambda = eigs(sparse(W),speye(n),1,'SR',opt); 
rmin = real(1/lambda) + incr;   
rmax = 1.0 - incr;
[rmin rmax]
out=lndetfull(sparse(W),rmin,rmax);
result = log_marginal_panelprob(yf,xf,W,N,T,model,out,prior,rmin,rmax,incr); 
in.cnames = strvcat('W-miles log marginals','model probs');
in.rnames = strvcat('Models','sar','sdm','sem','sdem');
mprint([result.lmarginal result.probs],in);
lmarginal=[lmarginal;result.lmarginal];

nmodels = length(lmarginal);
adj = max(lmarginal(:,1));
madj = matsub(lmarginal,adj);
xx = exp(madj);
% compute posterior probabilities
psum = sum(xx);
probs = [matdiv(xx,psum)];
[lmarginal/1000 probs]

%
% Calculation of Bayesian posterior model probabilities for dynamic models
% 
[yf,xf,meanny,meannx,meanty,meantx]=demean(y(N+1:end),[y(1:end-N) x(N+1:end,:)],N,T-1,model);

W=normw(W1);
[n,junk] = size(W);
lambda = eigs(sparse(W),speye(n),1,'SR',opt); 
rmin = real(1/lambda) + incr;   
rmax = 1.0 - incr;
[rmin rmax]
out=lndetfull(sparse(W),rmin,rmax);
result = log_marginal_panelprob(yf,xf,W,N,T-1,model,out,prior,rmin,rmax,incr); 
in.cnames = strvcat('W-miles log marginals','model probs');
in.rnames = strvcat('Models','sar','sdm','sem','sdem');
mprint([result.lmarginal result.probs],in);
lmarginald=[result.lmarginal];

W=normw(W2);
[n,junk] = size(W);
lambda = eigs(sparse(W),speye(n),1,'SR',opt); 
rmin = real(1/lambda) + incr;   
rmax = 1.0 - incr;
[rmin rmax]
out=lndetfull(sparse(W),rmin,rmax);
result = log_marginal_panelprob(yf,xf,W,N,T-1,model,out,prior,rmin,rmax,incr); 
in.cnames = strvcat('W-miles log marginals','model probs');
in.rnames = strvcat('Models','sar','sdm','sem','sdem');
mprint([result.lmarginal result.probs],in);
lmarginald=[lmarginald;result.lmarginal];

W=normw(W3);
[n,junk] = size(W);
lambda = eigs(sparse(W),speye(n),1,'SR',opt); 
rmin = real(1/lambda) + incr;   
rmax = 1.0 - incr;
[rmin rmax]
out=lndetfull(sparse(W),rmin,rmax);
result = log_marginal_panelprob(yf,xf,W,N,T-1,model,out,prior,rmin,rmax,incr); 
in.cnames = strvcat('W-miles log marginals','model probs');
in.rnames = strvcat('Models','sar','sdm','sem','sdem');
mprint([result.lmarginal result.probs],in);
lmarginald=[lmarginald;result.lmarginal];

W=normw(WE);
[n,junk] = size(W);
lambda = eigs(sparse(W),speye(n),1,'SR',opt); 
rmin = real(1/lambda) + incr;   
rmax = 1.0 - incr;
[rmin rmax]
out=lndetfull(sparse(W),rmin,rmax);
result = log_marginal_panelprob(yf,xf,W,N,T-1,model,out,prior,rmin,rmax,incr); 
in.cnames = strvcat('W-miles log marginals','model probs');
in.rnames = strvcat('Models','sar','sdm','sem','sdem');
mprint([result.lmarginal result.probs],in);
lmarginald=[lmarginald;result.lmarginal];

W=normw(WE1);
[n,junk] = size(W);
lambda = eigs(sparse(W),speye(n),1,'SR',opt); 
rmin = real(1/lambda) + incr;   
rmax = 1.0 - incr;
[rmin rmax]
out=lndetfull(sparse(W),rmin,rmax);
result = log_marginal_panelprob(yf,xf,W,N,T-1,model,out,prior,rmin,rmax,incr); 
in.cnames = strvcat('W-miles log marginals','model probs');
in.rnames = strvcat('Models','sar','sdm','sem','sdem');
mprint([result.lmarginal result.probs],in);
lmarginald=[lmarginald;result.lmarginal];

W=normw(WD);
[n,junk] = size(W);
lambda = eigs(sparse(W),speye(n),1,'SR',opt); 
rmin = real(1/lambda) + incr;   
rmax = 1.0 - incr;
[rmin rmax]
out=lndetfull(sparse(W),rmin,rmax);
result = log_marginal_panelprob(yf,xf,W,N,T-1,model,out,prior,rmin,rmax,incr); 
in.cnames = strvcat('W-miles log marginals','model probs');
in.rnames = strvcat('Models','sar','sdm','sem','sdem');
mprint([result.lmarginal result.probs],in);
lmarginald=[lmarginald;result.lmarginal];

W=normw(WDall);
[n,junk] = size(W);
lambda = eigs(sparse(W),speye(n),1,'SR',opt); 
rmin = real(1/lambda) + incr;   
rmax = 1.0 - incr;
[rmin rmax]
out=lndetfull(sparse(W),rmin,rmax);
result = log_marginal_panelprob(yf,xf,W,N,T-1,model,out,prior,rmin,rmax,incr); 
in.cnames = strvcat('W-miles log marginals','model probs');
in.rnames = strvcat('Models','sar','sdm','sem','sdem');
mprint([result.lmarginal result.probs],in);
lmarginald=[lmarginald;result.lmarginal];

W=normw(WDEDOM);
[n,junk] = size(W);
lambda = eigs(sparse(W),speye(n),1,'SR',opt); 
rmin = real(1/lambda) + incr;   
rmax = 1.0 - incr;
[rmin rmax]
out=lndetfull(sparse(W),rmin,rmax);
result = log_marginal_panelprob(yf,xf,W,N,T-1,model,out,prior,rmin,rmax,incr); 
in.cnames = strvcat('W-miles log marginals','model probs');
in.rnames = strvcat('Models','sar','sdm','sem','sdem');
mprint([result.lmarginal result.probs],in);
lmarginald=[lmarginald;result.lmarginal];

nmodels = length(lmarginald);
adj = max(lmarginald(:,1));
madj = matsub(lmarginald,adj);
xx = exp(madj);
% compute posterior probabilities
psum = sum(xx);
probsd = [matdiv(xx,psum)];
[lmarginald/1000 probsd]

%
% Results Table II of the paper, only for COW data set, change lines 13-14 for the other data set 
%
kans=[lmarginal/1000 probs lmarginald/1000 probsd]
kans1=reshape(probs,4,8)
sum(kans1,1)
sum(kans1,2)
kans2=reshape(probsd,4,8)
sum(kans2,1)
sum(kans2,2)

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
% Produces Table III
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
yalt=log(A(:,9)*100); % expenditure
nobs=N*T;K=5;
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

W=normw(WDall);
%W=normw(W3);
for t=1:T
    t1=(t-1)*N+1;t2=t*N;
    wx(t1:t2,:)=W*x(t1:t2,:);
    Wy(t1:t2,1)=W*y(t1:t2,1);
    Wyalt(t1:t2,1)=W*yalt(t1:t2,1);
end
nobs=N*T;
et=ones(T,1);
en=ones(N,1);
ent=ones(N*T,1);
%
% Towards a dynamic spatial panel data model
% First with country fixed effects but without time-period fixed effects
% SAR
info.lflag=0;
info.tl=1;
info.stl=1;
info.ted=1; %transformed approach
info.dyn=1;
info.model=1;
results=sar_panel_FE(y(N+1:end),[y(1:end-N) Wy(1:end-N) x(N+1:end,:)],W,T-1,info);
vnames=strvcat('lnratio','lnratio(-1)','W*lnratio(-1)','lnrgdp','lnpop','inttot','civtot','polity2');
results1=sar_jihai(y,[x(N+1:end,:)],W,info);
results.beta=results1.theta1(1:end-2);
results.rho=results1.theta1(end-1);
results.tstat=results1.tstat(1:end-1);
results.sige=results1.theta1(end); %%%
prt_spnew(results,vnames,1);
btemp=results1.theta1;
varcov=results1.varcov;
logliknotfe=results.lik;
%
% SAR model without country fixed effects but with time dummies as kind of check
% 
info.lflag=0;
info.dyn=1;
info.model=2;
results=sar_panel_FE(y(N+1:end),[y(1:end-N) Wy(1:end-N) x(N+1:end,:)],W,T-1,info);
vnames=strvcat('lnratio','lnratio(-1)','W*lnratio(-1)','lnrgdp','lnpop','inttot','civtot','polity2');
prt_spnew(results,vnames,1);
logliknosfe=results.lik;
% 
% Towards a dynamic spatial panel data model, 
% SAR specification no WX variables
% With country and time-period fixed effects
%
info.lflag=0;
info.tl=1;
info.stl=1;
info.ted=1; %transformed approach
info.dyn=1;
info.model=3;
results=sar_panel_FE(y(N+1:end),[y(1:end-N) Wy(1:end-N) x(N+1:end,:)],W,T-1,info);
vnames=strvcat('lnratio','lnratio(-1)','W*lnratio(-1)','lnrgdp','lnpop','inttot','civtot','polity2');
results1=sar_jihai_time(y,[x(N+1:end,:)],W,info);
results.beta=results1.theta1(1:end-2);
results.rho=results1.theta1(end-1);
results.tstat=results1.tstat(1:end-1);
results.sige=results1.theta1(end); %%%
prt_spnew(results,vnames,1);
btemp=results1.theta1;
varcov=results1.varcov;
%
% Direct and indirect effects estimates
% st=short term, lt=long term
% c=(long term) convergence effect of dependent variable
%
NSIM=1000;
[npar dummy]=size(btemp);
px=npar-4;
simresults=zeros(npar-1,NSIM);
simdirst=zeros(px,NSIM);
simindst=zeros(px,NSIM);
simtotst=zeros(px,NSIM);
simdirlt=zeros(px,NSIM);
simindlt=zeros(px,NSIM);
simtotlt=zeros(px,NSIM);
simdirc=zeros(1,NSIM);
simindc=zeros(1,NSIM);
simtotc=zeros(1,NSIM);
simdirYst=zeros(1,NSIM);
simindYst=zeros(1,NSIM);
simtotYst=zeros(1,NSIM);
simdirYlt=zeros(1,NSIM);
simindYlt=zeros(1,NSIM);
simtotYlt=zeros(1,NSIM);
%
% Note: since dep.var=ratio, the indirect effect of var 1 (GDP) is calculated 
% as coefficient of wx - coefficient of wy!!! See Eq.(10) in the paper
%
for sim=1:NSIM
    parms = chol(varcov)'*randn(size(btemp)) + btemp;
    deltasim = parms(npar-1,1); % coef WY(t)
    betasim = parms(3:npar-2,1);
    tausim = parms(1,1); % Coef Y(t-1)
    etasim = parms(2,1); % Coef WY(t-1)
    simresults(:,sim)=[tausim;etasim;betasim;deltasim];
    SS=(eye(N)-deltasim*W)\eye(N);
    SC=SS*((tausim-1)*eye(N)+(deltasim+etasim)*W);
    simdirc(1,sim)=sum(diag(SC))/N; % average direct effect
    simindc(1,sim)=sum(sum(SC,2)-diag(SC))/N; % average indirect effect
    simtotc(1,sim)=simdirc(1,sim)+simindc(1,sim);
    for p=1:px
        C=zeros(N,N);
        CY=zeros(N,N);
        CYlag=zeros(N,N);
        for i=1:N
            for j=1:N
                if (i==j) C(i,j)=betasim(p);
%                else C(i,j)=betasim(px+p)*W(i,j); % this line only for SDM
                end
                if (p==1)
                    if (i==j) CY(i,j)=1+betasim(p);CYlag(i,j)=1+betasim(p)-tausim;
 %                   else
 %                   CY(i,j)=(betasim(px+p)-deltasim)*W(i,j);CYlag(i,j)=(betasim(px+p)-deltasim+etasim)*W(i,j); % this line only for SDM
                    end
                end
            end
        end
        SC=SS*C;
        simdirst(p,sim)=sum(diag(SC))/N; % average direct effect
        simindst(p,sim)=sum(sum(SC,2)-diag(SC))/N; % average indirect effect
        simtotst(p,sim)=simdirst(p,sim)+simindst(p,sim);
        SC=((1-tausim)*eye(N)-(deltasim+etasim)*W)\C;
        simdirlt(p,sim)=sum(diag(SC))/N; % average direct effect
        simindlt(p,sim)=sum(sum(SC,2)-diag(SC))/N; % average indirect effect
        simtotlt(p,sim)=simdirlt(p,sim)+simindlt(p,sim);        
        if (p==1)
        SCY=SS*CY;
        simdirYst(1,sim)=sum(diag(SCY))/N; % average direct effect
        simindYst(1,sim)=sum(sum(SCY,2)-diag(SCY))/N; % average indirect effect
        simtotYst(1,sim)=simdirYst(1,sim)+simindYst(1,sim);
        SCY=((1-tausim)*eye(N)-(deltasim+etasim)*W)\CYlag;
        simdirYlt(1,sim)=sum(diag(SCY))/N; % average direct effect
        simindYlt(1,sim)=sum(sum(SCY,2)-diag(SCY))/N; % average indirect effect
        simtotYlt(1,sim)=simdirYlt(1,sim)+simindYlt(1,sim);
        end
    end
end
[mean(simresults,2) mean(simresults,2)./std(simresults,0,2)]
fprintf(1,'Convergence effect \n');
[mean(simdirc,2) mean(simdirc,2)./std(simdirc,0,2) mean(simindc,2) mean(simindc,2)./std(simindc,0,2)...
    mean(simtotc,2) mean(simtotc,2)./std(simtotc,0,2)]
fprintf(1,'Short term effects \n');
[mean(simdirst,2) mean(simdirst,2)./std(simdirst,0,2) mean(simindst,2) mean(simindst,2)./std(simindst,0,2)...
    mean(simtotst,2) mean(simtotst,2)./std(simtotst,0,2)]
fprintf(1,'Long term effects \n');
[mean(simdirlt,2) mean(simdirlt,2)./std(simdirlt,0,2) mean(simindlt,2) mean(simindlt,2)./std(simindlt,0,2)...
    mean(simtotlt,2) mean(simtotlt,2)./std(simtotlt,0,2)]
fprintf(1,'Short term effects GDP on M\n');
[mean(simdirYst,2) mean(simdirYst,2)./std(simdirYst,0,2) mean(simindYst,2) mean(simindYst,2)./std(simindYst,0,2)...
    mean(simtotYst,2) mean(simtotYst,2)./std(simtotYst,0,2)]
fprintf(1,'Long term effects GDP on M\n');
[mean(simdirYlt,2) mean(simdirYlt,2)./std(simdirYlt,0,2) mean(simindYlt,2) mean(simindYlt,2)./std(simindYlt,0,2)...
    mean(simtotYlt,2) mean(simtotYlt,2)./std(simtotYlt,0,2)]
loglik=results.lik;
% Note: probability > 0.05 implies rejection of spatial fixed effects
LR=-2*(logliknosfe-loglik);
dof=N-1;
probability=1-chis_prb(LR,dof);
fprintf(1,'LR-test joint significance spatial fixed effects, degrees of freedom and probability = %9.4f,%6d,%9.4f \n',LR,dof,probability);
LR=-2*(logliknotfe-loglik);
dof=T-2;
probability=1-chis_prb(LR,dof);
% Note: probability > 0.05 implies rejection of spatial fixed effects
fprintf(1,'LR-test joint significance time-periode fixed effects, degrees of freedom and probability = %9.4f,%6d,%9.4f \n',LR,dof,probability);

clear simresults simdirc simindc simtotc simdirst simindst simtotst simdirlt simindlt simtotlt select;

% Towards a dynamic spatial panel data model, 
% SAR specification no WX variables
% Again with time-period fixed effects
% Check what happens if not ratio but military expenditure is taken as
% dependent variable
%
info.lflag=0;
info.tl=1;
info.stl=1;
info.ted=1; %transformed approach
info.dyn=1;
info.model=3;
results=sar_panel_FE(yalt(N+1:end),[yalt(1:end-N) Wyalt(1:end-N) x(N+1:end,:) wx(N+1:end,1)],W,T-1,info);
vnames=strvcat('lnM','lnM(-1)','W*lnM(-1)','lnrgdp','lnpop','inttot','civtot','polity2','W*lnrgdp');
results1=sar_jihai_time(yalt,[x(N+1:end,:) wx(N+1:end,1)],W,info);
results.beta=results1.theta1(1:end-2);
results.rho=results1.theta1(end-1);
results.tstat=results1.tstat(1:end-1);
results.sige=results1.theta1(end); %%%
prt_spnew(results,vnames,1);
btemp=results1.theta1;
varcov=results1.varcov;
%
% Direct and indirect effects estimates
% st=short term, lt=long term
% c=(long term) convergence effect of dependent variable
%
NSIM=1000;
[npar dummy]=size(btemp);
px=5;
simresults=zeros(npar-1,NSIM);
simdirst=zeros(px,NSIM);
simindst=zeros(px,NSIM);
simtotst=zeros(px,NSIM);
simdirlt=zeros(px,NSIM);
simindlt=zeros(px,NSIM);
simtotlt=zeros(px,NSIM);
simdirc=zeros(1,NSIM);
simindc=zeros(1,NSIM);
simtotc=zeros(1,NSIM);
simdirYst=zeros(1,NSIM);
simindYst=zeros(1,NSIM);
simtotYst=zeros(1,NSIM);
simdirYlt=zeros(1,NSIM);
simindYlt=zeros(1,NSIM);
simtotYlt=zeros(1,NSIM);
%
% Correction posited in Eq. (10) is not necessary now
%
for sim=1:NSIM
    parms = chol(varcov)'*randn(size(btemp)) + btemp;
    deltasim = parms(npar-1,1); % coef WY(t)
    betasim = parms(3:npar-2,1);
    tausim = parms(1,1); % Coef Y(t-1)
    etasim = parms(2,1); % Coef WY(t-1)
    simresults(:,sim)=[tausim;etasim;betasim;deltasim];
    SS=(eye(N)-deltasim*W)\eye(N);
    SC=SS*((tausim-1)*eye(N)+(deltasim+etasim)*W);
    simdirc(1,sim)=sum(diag(SC))/N; % average direct effect
    simindc(1,sim)=sum(sum(SC,2)-diag(SC))/N; % average indirect effect
    simtotc(1,sim)=simdirc(1,sim)+simindc(1,sim);
    for p=1:px
        C=zeros(N,N);
        for i=1:N
            for j=1:N
                if (i==j) C(i,j)=betasim(p);
%                else C(i,j)=betasim(px+p)*W(i,j);
                end
                if (p==1)
                    if (i==j) C(i,j)=betasim(p);
                    else C(i,j)=betasim(px+p)*W(i,j);
                    end
                end
            end
        end
        SC=SS*C;
        simdirst(p,sim)=sum(diag(SC))/N; % average direct effect
        simindst(p,sim)=sum(sum(SC,2)-diag(SC))/N; % average indirect effect
        simtotst(p,sim)=simdirst(p,sim)+simindst(p,sim);
        SC=((1-tausim)*eye(N)-(deltasim+etasim)*W)\C;
        simdirlt(p,sim)=sum(diag(SC))/N; % average direct effect
        simindlt(p,sim)=sum(sum(SC,2)-diag(SC))/N; % average indirect effect
        simtotlt(p,sim)=simdirlt(p,sim)+simindlt(p,sim);        
%         if (p==1)
%         SCY=SS*CY;
%         simdirYst(1,sim)=sum(diag(SCY))/N; % average direct effect
%         simindYst(1,sim)=sum(sum(SCY,2)-diag(SCY))/N; % average indirect effect
%         simtotYst(1,sim)=simdirYst(1,sim)+simindYst(1,sim);
%         SCY=((1-tausim)*eye(N)-(deltasim+etasim)*W)\CYlag;
%         simdirYlt(1,sim)=sum(diag(SCY))/N; % average direct effect
%         simindYlt(1,sim)=sum(sum(SCY,2)-diag(SCY))/N; % average indirect effect
%         simtotYlt(1,sim)=simdirYlt(1,sim)+simindYlt(1,sim);
%         end
    end
end
[mean(simresults,2) mean(simresults,2)./std(simresults,0,2)]
fprintf(1,'Convergence effect \n');
[mean(simdirc,2) mean(simdirc,2)./std(simdirc,0,2) mean(simindc,2) mean(simindc,2)./std(simindc,0,2)...
    mean(simtotc,2) mean(simtotc,2)./std(simtotc,0,2)]
fprintf(1,'Short term effects \n');
[mean(simdirst,2) mean(simdirst,2)./std(simdirst,0,2) mean(simindst,2) mean(simindst,2)./std(simindst,0,2)...
    mean(simtotst,2) mean(simtotst,2)./std(simtotst,0,2)]
fprintf(1,'Long term effects \n');
[mean(simdirlt,2) mean(simdirlt,2)./std(simdirlt,0,2) mean(simindlt,2) mean(simindlt,2)./std(simindlt,0,2)...
    mean(simtotlt,2) mean(simtotlt,2)./std(simtotlt,0,2)]
% fprintf(1,'Short term effects GDP on M\n');
% [mean(simdirYst,2) mean(simdirYst,2)./std(simdirYst,0,2) mean(simindYst,2) mean(simindYst,2)./std(simindYst,0,2)...
%     mean(simtotYst,2) mean(simtotYst,2)./std(simtotYst,0,2)]
% fprintf(1,'Long term effects GDP on M\n');
% [mean(simdirYlt,2) mean(simdirYlt,2)./std(simdirYlt,0,2) mean(simindYlt,2) mean(simindYlt,2)./std(simindYlt,0,2)...
%     mean(simtotYlt,2) mean(simtotYlt,2)./std(simtotYlt,0,2)]
clear simresults simdirc simindc simtotc simdirst simindst simtotst simdirlt simindlt simtotlt select;
% 
% Towards a dynamic spatial panel data model, 
% SDM specification 
% With country and time-period fixed effects
%
info.lflag=0;
info.tl=1;
info.stl=1;
info.ted=1; %transformed approach
info.dyn=1;
info.model=3;
results=sar_panel_FE(y(N+1:end),[y(1:end-N) Wy(1:end-N) x(N+1:end,:) wx(N+1:end,:)],W,T-1,info);
results1=sar_jihai_time(y,[x(N+1:end,:) wx(N+1:end,:)],W,info);
results.beta=results1.theta1(1:end-2);
results.rho=results1.theta1(end-1);
results.tstat=results1.tstat(1:end-1);
results.sige=results1.theta1(end); %%%
vnames=strvcat('lnratio','lnratio(-1)','W*lnratio(-1)','lnrgdp','lnpop','inttot','civtot','polity2','W*lnrgdp','W*lnpop','W*inttot','W*civtot','W*polity2');
prt_spnew(results,vnames,1);
btemp=results1.theta1;
varcov=results1.varcov;
%
% Wald test for dynamic spatial lag model
%
p=K-1;
parmtest=btemp(2+p+1:2+p+p);
varcovtest=results1.varcov(2+p+1:2+p+p,2+p+1:2+p+p);
Rafg=zeros(p,p);
for k=1:p
    Rafg(k,k)=1; % R(1,3)=0 and R(2,4)=0;
end
Wald_spatial_lag=(Rafg*parmtest)'*inv(Rafg*varcovtest*Rafg')*Rafg*parmtest
prob_spatial_lag= 1-chis_cdf (Wald_spatial_lag, p) % probability greater than 0.05 points to insignificance
%
% Wald test for dynamic spatial error model
%
R=zeros(K,1);
coeftest=[btemp(1);btemp(3:2+p);btemp(2);btemp(3+p:2+p+p);btemp(2*K+1)];
varcovcoef=results1.varcov([1,3:2+p,2,3+p:2+p+p,2*K+1],[1,3:2+p,2,3+p:2+p+p,2*K+1]);
for k=1:K
    R(k)=coeftest(2*K+1)*coeftest(k)+coeftest(K+k); 
end
Rafg=zeros(K,2*K+1);
for k=1:K
    Rafg(k,k)    =coeftest(2*K+1);
    Rafg(k,K+k)  =1;
    Rafg(k,2*K+1)=coeftest(k);
end    
Wald_spatial_error=R'*inv(Rafg*varcovcoef*Rafg')*R
prob_spatial_error= 1-chis_cdf (Wald_spatial_error,K) % probability greater than 0.05 points to insignificance
%
% Direct and indirect effects estimates
% st=short term, lt=long term
% c=(long term) convergence effect of dependent variable
%
NSIM=1000;
[npar dummy]=size(btemp);
px=(npar-4)/2;
simresults=zeros(npar-1,NSIM);
simdirst=zeros(px,NSIM);
simindst=zeros(px,NSIM);
simtotst=zeros(px,NSIM);
simdirlt=zeros(px,NSIM);
simindlt=zeros(px,NSIM);
simtotlt=zeros(px,NSIM);
simdirc=zeros(1,NSIM);
simindc=zeros(1,NSIM);
simtotc=zeros(1,NSIM);
simdirYst=zeros(1,NSIM);
simindYst=zeros(1,NSIM);
simtotYst=zeros(1,NSIM);
simdirYlt=zeros(1,NSIM);
simindYlt=zeros(1,NSIM);
simtotYlt=zeros(1,NSIM);
%
% Note: since dep.var=ratio, the indirect effect of var 1 (GDP) is calculated 
% as coefficient of wx - coefficient of wy!!! See Eq.(10) in the paper
%
for sim=1:NSIM
    parms = chol(varcov)'*randn(size(btemp)) + btemp;
    deltasim = parms(npar-1,1); % coef WY(t)
    betasim = parms(3:npar-2,1);
    tausim = parms(1,1); % Coef Y(t-1)
    etasim = parms(2,1); % Coef WY(t-1)
    simresults(:,sim)=[tausim;etasim;betasim;deltasim];
    SS=(eye(N)-deltasim*W)\eye(N);
    SC=SS*((tausim-1)*eye(N)+(deltasim+etasim)*W);
    simdirc(1,sim)=sum(diag(SC))/N; % average direct effect
    simindc(1,sim)=sum(sum(SC,2)-diag(SC))/N; % average indirect effect
    simtotc(1,sim)=simdirc(1,sim)+simindc(1,sim);
    for p=1:px
        C=zeros(N,N);
        CY=zeros(N,N);
        CYlag=zeros(N,N);
        for i=1:N
            for j=1:N
                if (i==j) C(i,j)=betasim(p);
                else C(i,j)=betasim(px+p)*W(i,j);
                end
                if (p==1)
                    if (i==j) CY(i,j)=1+betasim(p);CYlag(i,j)=1+betasim(p)-tausim;
                    else CY(i,j)=(betasim(px+p)-deltasim)*W(i,j);CYlag(i,j)=(betasim(px+p)-deltasim-etasim)*W(i,j);
                    end
                end
            end
        end
        SC=SS*C;
        simdirst(p,sim)=sum(diag(SC))/N; % average direct effect
        simindst(p,sim)=sum(sum(SC,2)-diag(SC))/N; % average indirect effect
        simtotst(p,sim)=simdirst(p,sim)+simindst(p,sim);
        SC=((1-tausim)*eye(N)-(deltasim+etasim)*W)\C;
        simdirlt(p,sim)=sum(diag(SC))/N; % average direct effect
        simindlt(p,sim)=sum(sum(SC,2)-diag(SC))/N; % average indirect effect
        simtotlt(p,sim)=simdirlt(p,sim)+simindlt(p,sim);        
        if (p==1)
        SCY=SS*CY;
        simdirYst(1,sim)=sum(diag(SCY))/N; % average direct effect
        simindYst(1,sim)=sum(sum(SCY,2)-diag(SCY))/N; % average indirect effect
        simtotYst(1,sim)=simdirYst(1,sim)+simindYst(1,sim);
        SCY=((1-tausim)*eye(N)-(deltasim+etasim)*W)\CYlag;
        simdirYlt(1,sim)=sum(diag(SCY))/N; % average direct effect
        simindYlt(1,sim)=sum(sum(SCY,2)-diag(SCY))/N; % average indirect effect
        simtotYlt(1,sim)=simdirYlt(1,sim)+simindYlt(1,sim);
        end
    end
end
[mean(simresults,2) mean(simresults,2)./std(simresults,0,2)]
fprintf(1,'Convergence effect \n');
[mean(simdirc,2) mean(simdirc,2)./std(simdirc,0,2) mean(simindc,2) mean(simindc,2)./std(simindc,0,2)...
    mean(simtotc,2) mean(simtotc,2)./std(simtotc,0,2)]
fprintf(1,'Short term effects \n');
[mean(simdirst,2) mean(simdirst,2)./std(simdirst,0,2) mean(simindst,2) mean(simindst,2)./std(simindst,0,2)...
    mean(simtotst,2) mean(simtotst,2)./std(simtotst,0,2)]
fprintf(1,'Long term effects \n');
[mean(simdirlt,2) mean(simdirlt,2)./std(simdirlt,0,2) mean(simindlt,2) mean(simindlt,2)./std(simindlt,0,2)...
    mean(simtotlt,2) mean(simtotlt,2)./std(simtotlt,0,2)]
fprintf(1,'Short term effects GDP on M\n');
[mean(simdirYst,2) mean(simdirYst,2)./std(simdirYst,0,2) mean(simindYst,2) mean(simindYst,2)./std(simindYst,0,2)...
    mean(simtotYst,2) mean(simtotYst,2)./std(simtotYst,0,2)]
fprintf(1,'Long term effects GDP on M\n');
[mean(simdirYlt,2) mean(simdirYlt,2)./std(simdirYlt,0,2) mean(simindYlt,2) mean(simindYlt,2)./std(simindYlt,0,2)...
    mean(simtotYlt,2) mean(simtotYlt,2)./std(simtotYlt,0,2)]

clear simresults simdirc simindc simtotc simdirst simindst simtotst simdirlt simindlt simtotlt select;
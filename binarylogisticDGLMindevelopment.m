%%%%%%%%%%%%%%%%%%%%
% Binary logistic DGLM 
% % MW Sep 2017
startup; cmap('bw')

%generate some synthetic data 
xm=50; ym=94;   % dimensions (feet) of court
T=48/0.25;       % number of time intervals within game  - 48 mins total

% generate shot attempts with constant probability over the game
prshot = 0.4; % per time interval probability of attempt
shots  = rand(T,1)<prshot; % shot attempt indicator vector
tshot  = find(shots); nshots = length(tshot); 

% now generate the location of each shot
x = [ xm*(2*betarnd(6,8,nshots,1)-1) ym*betarnd(2,12,nshots,1)]; % x-y coordinates (in feet) from basket (=origin)
    figure(1); subplot(1,2,1); scatter(x(:,1),x(:,2),'+'); box on
    xlim([-xm xm]); ylim([0 ym]); text(0,0,'O','fontsize',14,'color','r','verticalalignment','middle','horizontalalignment','center')
    line([-xm xm],[1 1]*ym/2,'color','k','linestyle',':'); set(gca,'XTick',''); set(gca,'YTick','')
    
z = [ atan(x(:,2)./x(:,1)) sqrt(sum(x.^2,2)) ]; % angle(radians) & distance(feet) from basket
za= log(ym/5); % shift factor to roughly center log(distance) around zero
Z = [ ones(nshots,1) z(:,1) log(z(:,2))-za ]; % 2 selected covariates plus intercept 
    figure(1); subplot(1,2,2); scatter(Z(:,2),Z(:,3)+za,'+') 
    xlabel('angle (radians)'); ylabel('log distance (log feet)')    

% now generate shot success probabilities 
theta=[-.5 1.5 -5.5]';  % GLM regression parameter vector
p = length(theta);   % dimension of regression state vector
pscore = 1./(1+exp(-Z*theta)); 
q=NaN(T,1); q(shots)=pscore; 
    
% and now generate actual shot outcomes
y = NaN(T,1); y(shots) =  (rand(nshots,1)<=pscore);  iy=find(y(find(~isnan(y))));
    figure(1); clf
    scatter(tshot,q(tshot),'X'); hold on 
    scatter(tshot,y(tshot),'O'); box off; ylim([0 1])
    legend('probability','outcome'); legend boxoff
    xlabel('time interval'); ylabel('probability')  
    figure(2); clf; box on; hold on
    xlim([-xm xm]); ylim([0 ym]); text(0,0,'O','fontsize',14,'color','r','verticalalignment','middle','horizontalalignment','center')
    line([-xm xm],[1 1]*ym/2,'color','k','linestyle',':'); set(gca,'XTick',''); set(gca,'YTick','')
    inoy=(1:nshots)'; inoy(iy)=[]; % identify missed shots
    scatter(x(iy,1),x(iy,2),'r+'); scatter(x(inoy,1),x(inoy,2),'bo');  
    

% set up DGLM and initial prior     
% first, set up covariates per time interval 
F = Z'; p=size(F,1);  % columns of F are F_t vectors  
mt = zeros(p,1);  % time t=0 prior mean of regression vector
Ct = eye(p); % time t=0 prior variance matrix of regression vector
delta = 0.99;% discount factor: model is single discount random walk for state vector 

% now perform forward filtering (FF) and save summary stats online
smt = zeros(p,T);   % to save online posterior means
sCt = zeros(p,p,T); % to save online posterior variance matrices 
spt = NaN(1,T);   % to save online posterior means of success prob
lmlik = zeros(1,T); % marginal likelihood values per time interval
ishot=0;  
for t=1:T
    if (ismember(t,tshot)) % shot attempt at this time t
        ishot=ishot+1;  ti=tshot(ishot); % time of this shot attempt
        ft = F(:,ishot)'*mt; At = Ct*F(:,ishot)/delta; qt = F(:,ishot)'*At;  At = At/qt; 
            % prior mean and variance of linear predictor, and adaptive vector
            % now compute approx prior Beta(r,s) params - will update with numerical iterations for exact values
        eft = exp(ft); rt = (1+eft)/qt; st = rt/eft; % crude initial values  
        rt = max(0.5,rt); st=max(0.5,st); 
        % iterative numerical solution:    
            ep=.05; drt=1; dst=1; xt = [rt st]'; 
            while (max([drt dst])<ep) 
                r0t = psi(0,rt); s0t = psi(0,st); r1t = psi(1,rt); s1t = psi(1,st); 
                fxt = [ r0t-s0t-ft ; r1t+s1t-qt ];
                Axt = [ [ r1t -s1t ]; [ psi(2,rt) psi(2,st) ] ];
                xt = xt-Axt\fxt;  
                drt=xt(1)-rt; dst=xt(2)-st; 
                rt=xt(1); st=xt(2);
            end
        lmlik(t) = gammaln(rt+st)-gammaln(rt)-gammaln(st)...
                + gammaln(rt+y(t))+gammaln(st+1-y(t))-gammaln(rt+st+1)...
                + gammaln(2)-gammaln(1+y(t))-gammaln(2-y(t));
        rts = rt + y(t); sts = st + 1-y(t); % posterior beta params
        % posterior mean and variance for linear predictor
        fts = psi(0,rts)-psi(0,sts);  qts = psi(1,rts)+psi(1,sts); 
        % psi(0,x) is diagmma fn gamma(x);  psi(1,x) its 1st deriv; psi(2,x) its 2nd deriv
        spt(t) = rts/(rts+sts);  
        % now update state params
        mt = mt + At*(fts-ft); Ct = Ct/delta - At*At'*(qt-qts);  Ct = (Ct+Ct')/2; % last term for numerics
        [ t rt st mt']
        if any(isnan(mt)), error('stop'); end
    end
    smt(:,t) = mt;  sCt(:,:,t) = Ct; 
end

figure(1); clf; 
  subplot(2,1,1); plot(smt'); box off; legend(int2str((1:p)')); legend boxoff
    xlabel('time interval'); ylabel('online state mean') 
subplot(2,1,2); scatter(tshot,spt(tshot),'X'); hold on 
                scatter(tshot,y(tshot),'O'); box off; ylim([0 1])
    legend('probability','outcome'); legend boxoff
    xlabel('time interval'); ylabel('probability')  
 

% now perform backward sampling - BS 
nmc = 1000; % Monte Carlo sample size
MCtheta = zeros(p,T,nmc);   % to save sampled posterior means
MCq = zeros(T,nmc);     % to save sampled posterior success probabilities

% start the BS at t=T: 
thetat = mvnrnd(smt(:,T),sCt(:,:,T),nmc);
MCtheta(:,T,:) = thetat'; % samples of state vector  
MCq(T,:) = 1./(1+exp(-thetat*F(:,nshots))); % implied samples of success probability

% then recurse back over time: 
ishot=nshots+1; 
for t=T-1:-1:1
    if (ismember(t,tshot)) % shot attempt at this time t
        ht = (1-delta)*repmat(smt(:,t)',nmc,1)+ delta*thetat;
        thetat =  mvnrnd(ht,sCt(:,:,t)*(1-delta),nmc);
        MCtheta(:,t,:) = thetat';  
        ishot=ishot-1; ti=tshot(ishot);
        MCq(t,:) = 1./(1+exp(-thetat*F(:,ishot))); % implied samples of success probability 
    end
end

% now look at some retrospective posterior summaries 
figure(1); clf 
pr = prctile(MCq(tshot,:)',[2.5 25 50 75 97.5])'; 
ciplot(pr(:,1),pr(:,5),tshot,[0.8 0.8 0.8]); hold on; box off
ciplot(pr(:,2),pr(:,4),tshot,[0.65 0.65 0.65]); xlim([0 T+1])
plot(tshot,pr(:,3)); scatter(1:T,y,'O'); hold off; ylim([0 1])
legend('95%','50%','median','outcome'); legend boxoff
xlabel('time interval'); ylabel('hit rate')  
 
for j=1:p  % iterate over the state vector elements
    figure(1+j); clf 
    pr = prctile(squeeze(MCtheta(j,tshot,:))',[2.5 25 50 75 97.5])'; 
    ciplot(pr(:,1),pr(:,5),tshot,[0.8 0.8 0.8]); hold on; box off
    ciplot(pr(:,2),pr(:,4),tshot,[0.65 0.65 0.65]); xlim([0 T+1])
    scatter(tshot,pr(:,3),'+'); legend('95%','50%','median'); legend boxoff
    xlabel('time interval'); ylabel(['state vector element ',int2str(j)]) 
end

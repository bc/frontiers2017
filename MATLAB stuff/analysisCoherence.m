numMuscles = 7;
fs = 1000;
ts = 1/fs;
[data,header] = readDataThisDirectory();
forceRef = zeros(size(data,1),numMuscles);
forceOut = zeros(size(data,1),numMuscles + 6);
for i = 0 : 6
    [forceMeasThisMuscle,forceRefThisMuscle] = getMuscleForce(data,header,i);
    forceRef(:,i + 1) = forceRefThisMuscle;
    forceOut(:,i + 1) = forceMeasThisMuscle;
end
forceOut(:,8) = getMuscleForce(data,header,'fx');
forceOut(:,9) = getMuscleForce(data,header,'fy');
forceOut(:,10) = getMuscleForce(data,header,'fz');
forceOut(:,11) = getMuscleForce(data,header,'mx');
forceOut(:,12) = getMuscleForce(data,header,'my');
forceOut(:,13) = getMuscleForce(data,header,'mz');
jr3Force = [getMuscleForce(data,header,'fx') getMuscleForce(data,header,'fy') getMuscleForce(data,header,'fz')];
jr3Force = jr3Force - mean(jr3Force);
forceRef = forceRef - mean(forceRef);
forceOut = forceOut - mean(forceOut);
inNum = size(forceRef,2);
outNum = size(jr3Force,2);
%%
linOrder = 2;
data = iddata(jr3Force,forceRef,0.001);
sys = n4sid(data,linOrder,'Ts',0.001,'Focus','simulation','N4Weight','MOESP');
jr3ForcePredict = dlsim(sys.A,sys.B,sys.C,sys.D,forceRef);
axis = {'fX','fY','fZ'};
disp('Linear system identification performance')
for i = 1 : size(jr3Force,2)
    disp(['VAF for ',axis{i},' was: ',num2str(vaf(jr3Force(:,i),jr3ForcePredict(:,i)))])
end
%%
inputNL = poly1d('deg',10);
linModel = idss(sys.A,sys.B,sys.C,sys.D,'Ts',0.001);
outputNL = poly1d('deg',4);
sys = nlhw(data,linModel,inputNL,[]);
jr3ForcePredictBONL = compare(data,sys,Inf);
jr3ForcePredictBONL = jr3ForcePredictBONL.y;
disp('Cascade system identification performance')
for i = 1 : size(jr3Force,2)
    disp(['VAF for ',axis{i},' was: ',num2str(vaf(jr3Force(:,i),jr3ForcePredictBONL(:,i)))])
end

%%
plotJR3Prediction(jr3Force,jr3ForcePredict,fs)
print('results/linearSysID.pdf','-dpdf')
plotJR3Prediction(jr3Force,jr3ForcePredictBONL,fs)
print('results/cascadeSysID.pdf','-dpdf')
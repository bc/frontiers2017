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
%calcualte bandwidth across muscles and see if there is a pattern as a
%function of muscle
%Confidence itnerval for a typical FRF
%Activation time constant vs relaxation time constant
linOrder = 2;
data = iddata(jr3Force,forceRef,0.001);
sysIDLinear = n4sid(data,linOrder,'Ts',0.001,'Focus','simulation','N4Weight','MOESP');
jr3ForcePredictLinear = compare(data,sysIDLinear,Inf);
jr3ForcePredictLinear = jr3ForcePredictLinear.y;
axisLabel = {'f_x','f_y','f_z'};
%%
disp('Linear system identification performance')
for i = 1 : size(jr3Force,2)
    disp(['VAF for ',axisLabel{i},' was: ',num2str(vaf(jr3Force(:,i),jr3ForcePredictLinear(:,i)))])
end
sysID_SISO_Linear = cell(inNum,outNum);
for i = 1 : outNum
    subplot(3,1,i)
    for j = 1 : inNum
        thisSys = sysIDLinear(i,j);
        thisSys.InputName = ['Muscle ',num2str(j)];
        thisSys.OutputName = axisLabel(i);
        if (j == 1)
            h = bodeplot(thisSys);
            setoptions(h,'FreqUnits','Hz','PhaseVisible','off');
            hold on

        else
            bodeplot(thisSys);
        end
    end
    xlim([0.1,500])
end
legend('M1','M2','M3','M4','M5','M6','M7')
%%
A = abs(dcgain(sysIDLinear));
figure
hold on
for i = 1 : inNum
    quiver3(0,0,0,A((i - 1) * outNum + 1),A((i - 1) * outNum + 2),A((i - 1) * outNum + 3))
end
grid on
xlabel('f_x')
ylabel('f_y')
zlabel('f_z')
legend('M1','M2','M3','M4','M5','M6','M7')
%%
subplot(3,1,1)
ylabel('Force x')
box off
subplot(3,1,2)
ylabel('Force y')
box off
subplot(3,1,3)
ylabel('Force z')
xlabel('Frequency (Hz)')
box off
%%
inputNL = poly1d('deg',4);
linModel = idss(sysIDLinear.A,sysIDLinear.B,sysIDLinear.C,sysIDLinear.D,'Ts',0.001);
outputNL = poly1d('deg',4);
opt = nlhwOptions('Display','on','SearchMethod','lm');
opt.SearchOption.MaxIter = 50;

%sysBONL = nlhw(data,linModel,inputNL,[],opt);
sysBONL = nlhw(data,linModel,inputNL,outputNL);
jr3ForcePredictBONL = compare(data,sysBONL,Inf);
jr3ForcePredictBONL = jr3ForcePredictBONL.y;

disp('Cascade system identification performance')
for i = 1 : size(jr3Force,2)
    disp(['VAF for ',axisLabel{i},' was: ',num2str(vaf(jr3Force(:,i),jr3ForcePredictBONL(:,i)))])
end
A = dcgain(sysIDLinear);
%%
plotJR3Prediction(jr3Force,jr3ForcePredictLinear,fs)
print('results/linearSysID.pdf','-dpdf')
plotJR3Prediction(jr3Force,jr3ForcePredictBONL,fs)
print('results/cascadeSysID.pdf','-dpdf')
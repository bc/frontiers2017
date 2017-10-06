function bodePlot(sys)
numMuscle = length(sys.InputName);
inputName = cell(numMuscle,1);
for i = 1 : numMuscle
    inputName{i} = ['Muscle',num2str(i)];
end
outputName = {'F_x','F_y','F_z'};
sys.InputName = inputName;
sys.OutputName = outputName;
figure
h = bodeplot(sys);
setoptions(h,'FreqUnits','Hz','PhaseVisible','off');
end
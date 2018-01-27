function dynamicForceAnalysis(dynamicFileName, plotFigure)
    if nargin<1
        dynamicFileName = 'hand3_extend_clean_timeseries';
    end
    if nargin<2
        plotFigure = false;
    end
    dataRaw = readtable(['../../frontiers2017_data/',dynamicFileName,'.csv'],'Delimiter',',');
    %%
    %Typical results applying regression analysis to time series data
    %
    %
    regressorMeasured = regressorPostureDepence(dataRaw,'measured');
    outputChannel = {'Force_X','Force_Y','Force_Z','Moment_X','Moment_Y','Moment_Z'};
    observation = [dataRaw.JR3_FX dataRaw.JR3_FY dataRaw.JR3_FZ dataRaw.JR3_MX dataRaw.JR3_MY dataRaw.JR3_MZ];
    plotSample = 20000;
    mdl = fitlm(regressorMeasured,observation(:,1));
    coef = regressorMeasured \ observation(:,1);
    yp = regressorMeasured * coef;
    %%
    %Typical results applying dynamic system analysis to time series data
    deciRatio = 10;
    inputReference = regressorPostureDepence(dataRaw,'reference');
    inputMeasured = regressorPostureDepence(dataRaw,'measured');
    output = [dataRaw.JR3_FX dataRaw.JR3_FY dataRaw.JR3_FZ dataRaw.JR3_MX dataRaw.JR3_MY dataRaw.JR3_MZ];
    inputReference = decimateMatrixColumn(inputReference,deciRatio);
    inputMeasured = decimateMatrixColumn(inputMeasured,deciRatio);
    output = decimateMatrixColumn(output,deciRatio);
    nSamp = size(output,1);
    
    %%
    [stateSpaceModelRef,vafsRef,outputPredRef] = endpointForceID(inputReference,output);
    [stateSpaceModelMeas,vafsMeas,~] = endpointForceID(inputMeasured,output);
    dlmwrite(['../dynamicAnalysisResults/',dynamicFileName,'_Ref_A'], stateSpaceModelRef.A, ',');
    dlmwrite(['../dynamicAnalysisResults/',dynamicFileName,'_Ref_B'], stateSpaceModelRef.B, ',');
    dlmwrite(['../dynamicAnalysisResults/',dynamicFileName,'_Ref_C'], stateSpaceModelRef.C, ',');
    dlmwrite(['../dynamicAnalysisResults/',dynamicFileName,'_Ref_D'], stateSpaceModelRef.D, ',');
    dlmwrite(['../dynamicAnalysisResults/',dynamicFileName,'_Ref_vafs'], vafsRef, ',');
    
    dlmwrite(['../dynamicAnalysisResults/',dynamicFileName,'_Meas_A'], stateSpaceModelMeas.A, ',');
    dlmwrite(['../dynamicAnalysisResults/',dynamicFileName,'_Meas_B'], stateSpaceModelMeas.B, ',');
    dlmwrite(['../dynamicAnalysisResults/',dynamicFileName,'_Meas_C'], stateSpaceModelMeas.C, ',');
    dlmwrite(['../dynamicAnalysisResults/',dynamicFileName,'_Meas_D'], stateSpaceModelMeas.D, ',');
    dlmwrite(['../dynamicAnalysisResults/',dynamicFileName,'_MEas_vafs'], vafsMeas, ',');
    %%
    if plotFigure
        plot(nldat(output(:,1),'domainIncr',0.001))
        hold on
        plot(nldat(outputPredRef(:,1),'domainIncr',0.001))
        xlabel('Time (s)')
        ylabel('F_x')
        title('Endpoint Force Prediction')
        box off
        legend('Measured','Predicted')
        figure
        subplot(1,2,1)
        plot(observation(1:plotSample,1),yp(1:plotSample),'.')
        title('Static Analysis')
        box off
        xlabel('Measured force (N)')
        ylabel('Predicted force (N)')
        xlim([-4,4])
        ylim([-4,4])
        pbaspect([1 1 1])
        subplot(1,2,2)
        plot(output(1:plotSample,1),outputPredRef(1:plotSample,1),'.')
        title('Dynamic Analysis')
        xlabel('Measured force (N)')
        box off
        xlim([-4,4])
        ylim([-4,4])
        pbaspect([1 1 1])
    end
end
function [model,vafs,outputPredCompress] = endpointForceID(input,output)
    numMuscle = size(input,2);
    nSamp = size(input,1);
    numEndpointForceChan = size(output,2);
    input = input - mean(input);
    output = output - mean(output);
    [Sn,R] = dordpi(input,output,10);
    modelOrder = orderselect(Sn);
    [A, C]  =  destac(R,modelOrder);
    while ~isempty(find(abs(eig(A))>1, 1))
        modelOrder = modelOrder - 1;
        [A, C]  =  destac(R,modelOrder);
    end
    disp(['Model order: ',num2str(modelOrder),' was selected'])
    [B, D] =  destbd(input, output, A, C);
    model.A = A;
    model.B = B;
    model.C = C;
    model.D = D;

    outputPrediction = zeros(nSamp, numMuscle, numEndpointForceChan);
    %1st dim sample number, 2nd dim muscle #, 3d dim output channel number
    for muscleNumber = 1 : numMuscle
        [num, denum] = ss2tf(A, B, C, D, muscleNumber);
        for outputNumber = 1 : numEndpointForceChan
            thisNum = num(outputNumber, :);
            outputPrediction(:,muscleNumber,outputNumber) = filter(thisNum, denum, input(:,muscleNumber));
        end
    end
    outputPredCompress = zeros(nSamp, numEndpointForceChan);

    for outputNumber = 1 : numEndpointForceChan
        outputPredictionTemp = zeros(nSamp,1);
        for muscleNumber = 1 : numMuscle
            outputPredictionTemp = outputPredictionTemp + outputPrediction(:,muscleNumber,outputNumber);
        end
        outputPredCompress(:,outputNumber) = outputPredictionTemp;
    end

    vafs = zeros(numEndpointForceChan,1);
    for i = 1 : numEndpointForceChan
       vafs(i) = vaf(output(:,i),outputPredCompress(:,i));
    end

end
function regressor = regressorPostureDepence(data,option)
if nargin < 2
    option = 'reference';
end
if ~strcmp(option,'reference') && ~strcmp(option,'measured')
    error('wrong option: reference or measurement')
end
nSamp = size(data.measured_M0,1);
regressor = zeros(nSamp,7);
for i = 0 : 6
    regressor(:,i + 1) = eval(['data.',option,'_M',num2str(i)]);
end

end

function decimatedOutput = decimateMatrixColumn(data,ratio)
    nCol = size(data,2);
    column1Dec = decimate(data(:,1),ratio);
    decimatedOutput = zeros(length(column1Dec),nCol);
    decimatedOutput(:,1) = column1Dec;
    for i = 2 : nCol
        decimatedOutput(:,i) = decimate(data(:,i),ratio);
    end
end
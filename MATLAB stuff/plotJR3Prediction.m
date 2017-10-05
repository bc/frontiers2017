function plotJR3Prediction(jr3Force,jr3ForcePredict,fs,timeStart,timeEnd,forceMin,forceMax)
    numSamples = size(jr3Force,1);
    ts = 1 / fs;
    time = 0:0.001:(numSamples) / fs - ts;
    if nargin<4
        timeStart = time(1);
        timeEnd = time(end);
        forceMin = -2.5;
        forceMax = 2.5;
    end
    figure
    subplot(3,1,1)
    plot(time,jr3Force(:,1))
    hold on
    plot(time,jr3ForcePredict(:,1),'lineWidth',2,'color','r')
    ylabel('Fx')
    box off
    xlim([timeStart,timeEnd])
    ylim([forceMin,forceMax])
    title(['Force X, VAF: ',num2str(vaf(jr3Force(:,1),jr3ForcePredict(:,1)))])
    subplot(3,1,2)
    plot(time,jr3Force(:,2))
    hold on
    plot(time,jr3ForcePredict(:,2),'lineWidth',2,'color','r')
    ylabel('Fy')
    xlim([timeStart,timeEnd])
    title(['Force Y, VAF: ',num2str(vaf(jr3Force(:,2),jr3ForcePredict(:,2)))])
    ylim([forceMin,forceMax])
    subplot(3,1,3)
    box off
    plot(time,jr3Force(:,3))
    hold on
    plot(time,jr3ForcePredict(:,3),'lineWidth',2,'color','r')
    ylabel('Fz')
    box off
    xlabel('Time (s)')
    xlim([timeStart,timeEnd])
    title(['Force Z, VAF: ',num2str(vaf(jr3Force(:,3),jr3ForcePredict(:,3)))])
    ylim([forceMin,forceMax])
end
function [forceMeasured,forceReference] = getMuscleForce(data,header,muscleNumber)
    if isnumeric(muscleNumber)
        label = ['measured_M',num2str(muscleNumber)];
        colIndex = contains(header,label);
        forceMeasured = data(:,colIndex);
        label = ['reference_M',num2str(muscleNumber)];
        colIndex = contains(header,label);
        forceReference = data(:,colIndex);
    elseif startsWith(muscleNumber,'f') || startsWith(muscleNumber,'m')
        label = ['JR3_',upper(muscleNumber)];
        colIndex = contains(header,label);
        forceMeasured = data(:,colIndex);
        forceReference = [];
    else
        error('column not supported, only numeric muscle number, fx, fy, fz, mx, my, mz supported')
    end
end
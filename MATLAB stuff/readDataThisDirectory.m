function [data,headerNew] = readDataThisDirectory(fileType)
    motorCommand = false;
    data = [];
    if (nargin<1)
        fileType = 'csv';
    end
    fileNames = dir(['*.',fileType]);
    numFiles = length(fileNames);
    for i = 1 : numFiles
        dataThisFile = readtable(fileNames(i).name,'Delimiter',',');
        header = dataThisFile.Properties.VariableNames;
        numVar = length(header);
        isTypeNumeric = false(numVar,1);
        headerNew = header;
        dataThisFileNew = dataThisFile;
        for j = 1 : numVar
           if ~isnumeric(eval(['dataThisFile.',header{j},'(1)']))
               dataThisFileNew(:,j) = [];
               headerNew(j) = [];
           end
        end
        dataThisFileNew = table2array(dataThisFileNew);
        data = [data;dataThisFileNew];
    end
    if ~motorCommand
        commandRemoveIndex = startsWith(headerNew,'command');
        data(:,commandRemoveIndex) = [];
        headerNew(:,commandRemoveIndex) = [];
    end
end
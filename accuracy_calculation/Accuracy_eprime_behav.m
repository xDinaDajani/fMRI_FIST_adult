function Accuracy_eprime_behav(txtfile, outdir, accresp_file)
%% Extracting response information from eprime txt file %%
% Brain Connectivity and Cognition Lab %
% Dina R. Dajani June 2016 %
% txtfile = the full path name to the eprime txt file (E.g.,'/Desktop/filename.txt')
% outdir = the full path name to the directory where you want accuracy file
% to be written to (E.g., '/Volumes/Groups/LUddin_Lab/Lab/Here')
% accresp_file = the full path name to the file that has the information about
% accurate and inaccurate responses for each trial (csv)

%% ===========ensure txtfile and outdir are defined======================
%Check that all input parameters are present
exist txtfile var;
if  ans == 0;
    disp('txtfile is not defined');
end 
exist outdir var;
if  ans == 0;
    disp('outdir is not defined');
end    
exist accresp_file var;
if  ans == 0;
    disp('accresp_file is not defined');
end  
%%  ==========Displaying the subject ID=============================
accresp = importdata(accresp_file);
fileID = fopen(txtfile, 'r', 'n', 'UTF16-LE'); %%if using .txt format
content = textscan(fileID, '%s', 'Delimiter', '\n');
subject = regexp(content{1}, 'Subject:', 'match'); 
subjecti = find(~cellfun('isempty', subject));
subjall = content{1}{subjecti(1)};
subj =regexp(subjall, '\d*', 'match');
disp(sprintf('The subject number is %s', subj{1}));
fclose(fileID); 
%%  ==========Getting the trial and accuracy information===================
fileID = fopen(txtfile, 'r', 'n', 'UTF16-LE');
input = textscan(fileID, '%s', 'delimiter', '\n', 'HeaderLines', 25); %%skipping header info
fclose(fileID);
x =regexp(input{1}, 'FlexibilityTrial$', 'match'); %%finding flexibility trials only to calc acc
flextrialnum = find(~cellfun('isempty', x));
images = regexp(input{1}, 'Flex\d*.JPG', 'match');
imagerow = find(~cellfun('isempty', images)); %finding image names for flexibility trials
resps = regexp(input{1}, 'FlexibilityTrials.RESP', 'match');
resprow = find(~cellfun('isempty', resps)); %finding responses for  each flexibility selection
dimresps = regexp(input{1}, 'HowSimilar.RESP', 'match');
dimresprow= find(~cellfun('isempty', dimresps));%finding responses for hwo are they similar?
if length(flextrialnum) ~= 18; %%warning if the number of flexibility trials does not equal 10
    lengthi = length(flextrialnum)/3;
    X =sprintf('Warning: the number of flexibility trials is not 6, it is %d', lengthi);
    disp(X);
end

%initialize an empty cell
matrix_acc = {};
k = 1;
%% Retrieving trial image and response & assessing accuracy of flexibility trials
%initialize error types to 999, and if there are any errors, this will be
%updated in the loop below
% error types:
% errortype = 1 indicates there were less than 2 button presses
% errortype = 2 indicates a repeated card pair
% errortype = 3 indicates cards do not match along any dimension
% errortype = 4 indicates that the cards match but the specified dimension is incorrect
errortype1 = ones(6,1)*999;
errortype2 = ones(6,1)*999;
errortype3 = ones(6,1)*999;
% loops through each of the 6 trials trial
for i = 1: length(flextrialnum)/3 
    p=0;
    %%this is the name of the trial image
    image = regexp(input{1}{imagerow(k)}, 'Flex\d*.JPG', 'match');
    
    for j = 1:3              
        %%this is the name of the trial image
        if j ==1;
            resp1 = regexp(input{1}{resprow(k+p)}, '\d*', 'match'); %%this is the subj's response (should be 2 digits)
        elseif j==2;
            resp2 = regexp(input{1}{resprow(k+p)}, '\d*', 'match');
        elseif j==3;
            resp3 = regexp(input{1}{resprow(k+p)}, '\d*', 'match');
        end
        %h=shape i=size n=number c=color 0=noresp X=uncat
        if j ==1;
            dimresp1 = regexp(input{1}{dimresprow(k+p)}, '.$', 'match'); %accuracy of control selection 
                                                                         %(1 if correct, 0 if incorrect)
        elseif j==2;
            dimresp2 =regexp(input{1}{dimresprow(k+p)}, '.$', 'match');
        elseif j ==3;
            dimresp3 =regexp(input{1}{dimresprow(k+p)}, '.$', 'match');
        end
        p=p+1;
    end
    % find the correct responses from the accresp csv
    %search the csv for the imagefiles and then pull the accuracies from
    %there
    imagefiles = accresp.textdata(:,2);
    finds = strfind(imagefiles, char(image)); %search accresp csv for trial image on this iteration
    trial_row = find(~cellfun('isempty', finds)); %row in accresp for trial image on this iteration
    inaccs = accresp.data(trial_row, 9:17);%inaccurate responses are columns 9-17

    %If the selection (resp1-3) is not 2 characters, automatically count
    % as incorrect.
    tworesp1 = regexp(resp1, '..', 'match');
    tworesp2 = regexp(resp2, '..', 'match');
    tworesp3 = regexp(resp3, '..', 'match');
    resp1 = str2double(resp1);
    resp2 = str2double(resp2);
    resp3 = str2double(resp3);
    %accuracy for trials with selections <2 button presses
    if isempty(tworesp1) == 1 && isempty(tworesp2) == 1 && isempty(tworesp3) == 1;
        inacc1 = 999;
        inacc2 = 999;
        inacc3 = 999;
        errortype1(i,1) = 1; % errortype = 1 indicates there were less than 2 button presses
        errortype2(i,1) = 1;
        errortype3(i,1) = 1;
    elseif isempty(tworesp1) == 1;
        if isempty(tworesp2) == 1; %%first two selections are less than 2 button presses
           inacc1 = 999;
           inacc2 = 999;
           inacc3 = ~(isempty(find(inaccs == resp3)));  
           errortype1(i,1) = 1;
           errortype2(i,1) = 1;
        elseif isempty(tworesp3) == 1; %first and 3rd selection less than 2 button presses
           inacc1 = 999;
           inacc2= ~(isempty(find(inaccs == resp2)));
           inacc3 = 999;
           errortype1(i,1) = 1;
           errortype3(i,1) = 1;
        else %only first selection is less than 2 button presses
           inacc1 = 999;
           errortype1(i,1) = 1;
           if resp2 == resp3;
              inacc2= ~(isempty(find(inaccs == resp2)));
              inacc3 = 1; 
              errortype3(i,1) = 2; % errortype = 2 indicates a repeated card pair
           else
              inacc2= ~(isempty(find(inaccs == resp2)));
              inacc3 = ~(isempty(find(inaccs == resp3)));
           end
        end
    elseif isempty(tworesp2) == 1;  
        if isempty(tworesp3) ==1; %2nd and 3rd selection are less than 2 button presses
            inacc1 = ~(isempty(find(inaccs == resp1)));
            inacc2 = 999; 
            inacc3 = 999;
            errortype2(i,1) = 1;
            errortype3(i,1) = 1;
        else % only second selection is less than 2 button presses
             inacc2= 999;
             errortype2(i,1) = 1;
             if resp1 == resp3;
                inacc1 = ~(isempty(find(inaccs == resp1))); 
                inacc3 = 1;
                errortype3(i,1) = 2;
             else
                inacc1 = ~(isempty(find(inaccs == resp1)));
                inacc3 = ~(isempty(find(inaccs == resp3)));
             end
        end
    elseif isempty(tworesp3) == 1; %only 3rd selection is less than 2 button presses
        inacc3 = 999;
        errortype3(i,1) = 1;
        if resp1 == resp2;
            inacc1 = ~(isempty(find(inaccs == resp1))); 
            inacc2 = 1;
            errortype2(i,1) = 2; %same card pair choice
        else
            inacc1 = ~(isempty(find(inaccs == resp1))); %inacc is 0 if accurate, 1 if inaccurate
            inacc2= ~(isempty(find(inaccs == resp2)));
        end
    end
   %%only completing the following for trials  with all three selections w/ 2 button presses     
   if isempty(tworesp1) == 0 && isempty(tworesp2) == 0 && isempty(tworesp3) == 0;
         %%if a selection is repeated, count the repetition as incorrect
         if resp1 == resp2 == resp3;    
            inacc1 = ~(isempty(find(inaccs == resp1)));
            inacc2 = 1;
            errortype2(i,1) = 2;
            inacc3 = 1;
            errortype3(i,1) = 2;
        elseif resp1 == resp2;
            inacc1 = ~(isempty(find(inaccs == resp1))); 
            inacc2 = 1;
            errortype2(i,1) = 2;
            inacc3 = ~(isempty(find(inaccs == resp3))); 
        elseif resp1 == resp3; 
            inacc1 = ~(isempty(find(inaccs == resp1))); 
            inacc2= ~(isempty(find(inaccs == resp2)));
            inacc3 = 1; 
            errortype3(i,1) = 2;
        elseif resp2 == resp3;
            inacc1 = ~(isempty(find(inaccs == resp1))); 
            inacc2= ~(isempty(find(inaccs == resp2)));
            inacc3 = 1; 
            errortype3(i,1) = 2;
    %check accuracy of non-repeated, 2 button selections
        else
            inacc1 = ~(isempty(find(inaccs == resp1))); %inacc is 0 if accurate, 1 if inaccurate
            inacc2= ~(isempty(find(inaccs == resp2)));
            inacc3 = ~(isempty(find(inaccs == resp3))); 
        end
   end

  %%retrieving dimension information
  % change this code so that dimension is output even if it was a repeated
  % selection
    if inacc1 == 0 && inacc2 == 0 && inacc3 == 0; %all selections correct
        %%determining the dimension  of selection
        %columns 1 and 5 are shape, 2 and 6 are size, 3 and 7 are color, and
        %4 and 8 are number; only output dimension information if trial is
        %accurate
        dim1i = find(accresp.data(trial_row, 1:8) == resp1); 
        if dim1i == 1 || dim1i == 5 ;
            dim1 = 'shape';
        elseif dim1i == 2 || dim1i ==6;
            dim1 = 'size';
        elseif dim1i == 3 || dim1i == 7;
            dim1 = 'color';
        elseif dim1i == 4 || dim1i == 8;
            dim1 = 'number';
        end
        dim2i = find(accresp.data(trial_row, 1:8) == resp2); 
        if dim2i == 1 || dim2i == 5 ;
            dim2 = 'shape';
        elseif dim2i == 2 || dim2i ==6;
            dim2 = 'size';
        elseif dim2i == 3 || dim2i == 7;
            dim2 = 'color';
        elseif dim2i == 4 || dim2i == 8;
            dim2 = 'number';
        end
        dim3i = find(accresp.data(trial_row, 1:8) == resp3); 
        if dim3i == 1 || dim3i == 5 ;
            dim3 = 'shape';
        elseif dim3i == 2 || dim3i ==6;
            dim3 = 'size';
        elseif dim3i == 3 || dim3i == 7;
            dim3 = 'color';
        elseif dim3i == 4 || dim3i == 8;
            dim3 = 'number';
        end
    elseif inacc1 == 999 && inacc2 == 999 && inacc3 == 999; % all selections are less than 2 button presses
        dim1=999; %%dimension information is missing (999) if less than 2 button resp
        dim2=999;
        dim3=999;
    elseif inacc1 == 1 || inacc2==1 || inacc3 == 1; %% any selection is incorrect
        if inacc1~=999;
        dim1i = find(accresp.data(trial_row, 1:8) == resp1); 
            if isempty(dim1i)
                dim1 = 999;
                errortype1(i,1) = 3; % errortype = 3 cards do not match along any dimension
            else    
                if dim1i == 1 || dim1i == 5 ;
                    dim1 = 'shape';
                elseif dim1i == 2 || dim1i ==6;
                    dim1 = 'size';
                elseif dim1i == 3 || dim1i == 7;
                    dim1 = 'color';
                elseif dim1i == 4 || dim1i == 8;
                    dim1 = 'number';
                end
            end           
        end
        if inacc2~= 999;
        dim2i = find(accresp.data(trial_row, 1:8) == resp2); 
            if isempty(dim2i)
                dim2 = 999;
                errortype2(i,1) = 3;
            else
                if dim2i == 1 || dim2i == 5 ;
                    dim2 = 'shape';
                elseif dim2i == 2 || dim2i ==6;
                    dim2 = 'size';
                elseif dim2i == 3 || dim2i == 7;
                    dim2 = 'color';
                elseif dim2i == 4 || dim2i == 8;
                    dim2 = 'number';
                end
            end
        end
        if inacc3~=999;
        dim3i = find(accresp.data(trial_row, 1:8) == resp3);
            if isempty(dim3i)
                dim3 = 999;
                errortype3(i,1) = 3;
            else
                if dim3i == 1 || dim3i == 5 ;
                    dim3 = 'shape';
                elseif dim3i == 2 || dim3i ==6;
                    dim3 = 'size';
                elseif dim3i == 3 || dim3i == 7;
                    dim3 = 'color';
                elseif dim3i == 4 || dim3i == 8;
                    dim3 = 'number';
                end
            end
        end
    end
    %%this determines accuracy for each selection; inacc = 0 means
    %%selection is correct, inacc = 999 and inacc= 1 are incorrect. acc is
    %%determined by having a correct selection & the correct dimension for
    %%all three selections
    for j = 1:3 % loop through each selection; j represents the selection #
        if j == 1;
            inacc = inacc1;
        elseif j ==2;
            inacc = inacc2;
        elseif j ==3;
            inacc= inacc3;
        end
        if inacc == 0; %%accurate selection, determine if dim was accurate
            if j ==1;
                if strcmp('size', dim1); % if participant chose cards that match on size, did they indicate size as the dimension?
                    if strcmp('i', dimresp1);  
                        Acc1 = 1;
                    else
                        Acc1 = 0;
                        errortype1(i,1) = 4; % errortype = 4 indicates that the cards match but the specified dimension is incorrect
                    end
                elseif strcmp('shape', dim1);
                    if strcmp('h', dimresp1);
                        Acc1 = 1;
                    else
                        Acc1 = 0;
                        errortype1(i,1) = 4; % errortype = 4 indicates that the cards match but the specified dimension is incorrect
                    end
                elseif strcmp('number', dim1);
                    if strcmp('n', dimresp1);
                        Acc1 = 1;
                    else
                        Acc1 = 0;
                        errortype1(i,1) = 4; % errortype = 4 indicates that the cards match but the specified dimension is incorrect
                    end
                elseif strcmp('color', dim1);
                    if strcmp('c', dimresp1);
                        Acc1 = 1;
                    else
                        Acc1 = 0;
                        errortype1(i,1) = 4; % errortype = 4 indicates that the cards match but the specified dimension is incorrect
                    end
                end  
            elseif j ==2;
                if strcmp('size', dim2);
                    if strcmp('i', dimresp2);  
                        Acc2 = 1;
                    else
                        Acc2 = 0;
                        errortype2(i,1) = 4;
                    end
                elseif strcmp('shape', dim2);
                    if strcmp('h',dimresp2);
                        Acc2 = 1;
                    else
                        Acc2 = 0;
                        errortype2(i,1) = 4;
                    end
                elseif strcmp('number', dim2);
                    if strcmp('n', dimresp2);
                        Acc2 = 1;
                    else
                        Acc2 = 0;
                        errortype2(i,1) = 4;
                    end
                elseif strcmp('color', dim2);
                    if strcmp('c', dimresp2);
                        Acc2 = 1;
                    else
                        Acc2 = 0;
                        errortype2(i,1) = 4;
                    end
                end  
            elseif j == 3;
                if strcmp('size', dim3);
                    if strcmp('i',dimresp3);  
                        Acc3 = 1;
                    else
                        Acc3 = 0;
                        errortype3(i,1) = 4;
                    end
                elseif strcmp('shape', dim3);
                    if strcmp('h', dimresp3);
                        Acc3 = 1;
                    else
                        Acc3 = 0;
                        errortype3(i,1) = 4;
                    end
                elseif strcmp('number', dim3);
                    if strcmp('n', dimresp3);
                        Acc3 = 1;
                    else
                        Acc3 = 0;
                        errortype3(i,1) = 4;
                    end
                elseif strcmp('color', dim3);
                    if strcmp('c', dimresp3);
                        Acc3 = 1;
                    else
                        Acc3 = 0;
                        errortype3(i,1) = 4;
                    end
                end  
            end 
        elseif inacc ==1;
            if j ==1;    
                Acc1 = 0;
            elseif j ==2;
                Acc2 = 0;
            elseif j ==3;
                Acc3 = 0;
            end
        elseif inacc == 999;
            if j ==1;    
                Acc1 = 999;
            elseif j ==2;
                Acc2 = 999;
            elseif j ==3;
                Acc3 = 999;
            end
        end
    end
    if Acc1 == 1 && Acc2 ==1 && Acc3 == 1;
        Acc_triali = 1;
    else
        Acc_triali = 0;
    end
    %creating matrix where each row represents a separate trial 
    if isempty(matrix_acc);
        matrix_acc = {char(image), Acc_triali, Acc1, Acc2, Acc3, ...
            resp1, resp2, resp3, dimresp1{1}, dimresp2{1}, dimresp3{1}, dim1, dim2, dim3, ...
            errortype1(i,1), errortype2(i,1), errortype3(i,1)};
    else
        rowi = {char(image), Acc_triali, Acc1, Acc2, Acc3, ...
            resp1, resp2, resp3, dimresp1{1}, dimresp2{1}, dimresp3{1}, dim1, dim2, dim3, ...
            errortype1(i,1), errortype2(i,1), errortype3(i,1)};
        matrix_acc = [matrix_acc; rowi];
    end
    if i == 1 ; 
       k = i + 3;
    else
       k = k + 3;
    end
end
%% Display warning if there is an "X" uncategorized
uncat = find(strcmp('x', matrix_acc(:, 9:11)) == 1);
if ~(isempty(uncat));
    disp('Warning: At least one selection is "uncategorized"');
end
%% Display average accuracy across all trials 
acc_mean = mean(cell2mat(matrix_acc(:,2)));
x = sprintf('The mean accuracy for flex trials is %0.2f', acc_mean);
disp(x);
acc1n = cell2mat(matrix_acc(:,3));
acc_mean1 = mean(acc1n(~(acc1n==999))); 
x = sprintf('The mean accuracy for the 1st selection is %0.2f', acc_mean1);
disp(x);
acc2n = cell2mat(matrix_acc(:,4));
acc_mean2 = mean(acc2n(~(acc1n==999))); 
x = sprintf('The mean accuracy for the 2nd selection is %0.2f', acc_mean2);
disp(x);
acc3n = cell2mat(matrix_acc(:,5));
acc_mean3 = mean(acc3n(~(acc1n==999))); 
x = sprintf('The mean accuracy for the 3rd selection is %0.2f', acc_mean3);
disp(x);
%% Write table to csv
T = cell2table(matrix_acc, 'VariableNames', {'FlexImage', 'Acc_Flex', ...
    'AccFlex1', 'AccFlex2', 'AccFlex3', 'FlexResp1', 'FlexResp2', 'FlexResp3',...
    'FlexDimResp1', 'FlexDimResp2', 'FlexDimResp3', 'FlexDim1', 'FlexDim2', 'FlexDim3', ...
    'ErrorType1', 'ErrorType2', 'ErrorType3'});
[pathstr, name, ext] = fileparts(txtfile);
acc_file = strcat(outdir, '/', name, '_AccRT.csv');
writetable(T, acc_file);
end
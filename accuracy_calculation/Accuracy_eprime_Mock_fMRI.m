function Accuracy_eprime_fMRI(txtfile, outdir, accresp_file)
%% Extracting response information from eprime txt file %%
% Brain Connectivity and Cognition Lab %
% Dina R. Dajani June 2016 %
% txtfile = the full path name to the eprime txt file (E.g.,'/Desktop/filename.txt')
% outdir = the full path name to the directory where you want accuracy file
% to be written to (E.g., '/Volumes/Groups/LUddin_Lab/Lab/Here')
% accresp_file = the full path name to the file that has the information about
% accurate and inaccurate responses for each trial (csv)
% 
%% ===========ensure txtfile and outdir are defined========================
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
%%  ==========Getting the subject ID and Run #=============================
accresp = importdata(accresp_file);
fileID = fopen(txtfile, 'r', 'n', 'UTF16-LE'); %%if using .txt format
content = textscan(fileID, '%s', 'Delimiter', '\n');
subject = regexp(content{1}, 'Subject:', 'match'); 
subjecti = find(~cellfun('isempty', subject));
subjall = content{1}{subjecti(1)};
subj =regexp(subjall, '\d*', 'match');
disp(sprintf('The subject number is %s', subj{1}));
%%  ==========Getting the trial and accuracy information===================
fileID = fopen(txtfile, 'r', 'n', 'UTF16-LE');
input = textscan(fileID, '%s', 'delimiter', '\n', 'HeaderLines', 25); %%skipping header info
fclose(fileID);
x =regexp(input{1}, 'FlexibilityProc', 'match'); %%finding flexibility trials only to calc acc
flextrialnum = find(~cellfun('isempty', x));
images = regexp(input{1}, 'Flex\d*.JPG', 'match');
imagerow = find(~cellfun('isempty', images)); %finding image names for flexibility trials
RTs = regexp(input{1}, 'FlexibilityTrials.RT', 'match');
RTrow = find(~cellfun('isempty', RTs)); %finding RTs for flexibility trials
resps = regexp(input{1}, 'FlexibilityTrials.RESP', 'match');
resprow = find(~cellfun('isempty', resps)); %finding responses for flexibility trials
if length(flextrialnum) ~= 10; %%warning if the number of flexibility trials does not equal 10
    lengthi = length(flextrialnum);
    X =sprintf('Warning: the number of flexibility trials is not 10, it is %d', lengthi);
    disp(X);
end
%% Finding control trials
o =regexp(input{1}, 'ControlProc', 'match'); %%finding control trials only to calc acc
controltrialnum = find(~cellfun('isempty', o));
cimages = regexp(input{1}, 'Slide\d*.JPG', 'match');
cimagerow = find(~cellfun('isempty', cimages)); %finding image names for control trials
cRTs = regexp(input{1}, 'Controls.RT:', 'match');
cRTrow = find(~cellfun('isempty', cRTs)); %finding RTs for control trials
cresps = regexp(input{1}, 'Controls.RESP', 'match');
cresprow = find(~cellfun('isempty', cresps));
caccs = regexp(input{1}, 'Controls.ACC', 'match');
caccrow = find(~cellfun('isempty', caccs));
if length(controltrialnum) ~= 30; %%warning if the number of control trials does not equal 10
    lengthi = length(controltrialnum)/3;
    X =sprintf('Warning: the number of control trials is not 10, it is %d', lengthi);
    disp(X);
end
%initialize an empty cell
matrix_acc = {};
%% Retrieving trial image and response & assessing accuracy of flexibility trials
for i = 1: length(flextrialnum)
    %%this is the name of the trial image
    image = regexp(input{1}{imagerow(i)}, 'Flex\d*.JPG', 'match');
    FlexRT = regexp(input{1}{RTrow(i)}, '\d*', 'match'); 
    resp = regexp(input{1}{resprow(i)}, '\d*', 'match'); %%this is the subj's response (should be 6 digits)
    % find the correct responses from the accresp csv
    %search the csv for the imagefiles and then pull the accuracies from
    %there
    imagefiles = accresp.textdata(:,2);
    finds = strfind(imagefiles, char(image)); %search accresp csv for trial image on this iteration
    trial_row = find(~cellfun('isempty', finds)); %row in accresp for trial image on this iteration
    inaccs = accresp.data(trial_row, 9:16);%inaccurate responses are columns 9-16
    %If the response is not 6 characters, automatically count
    % as incorrect.
    sixresp = regexp(resp, '......', 'match');
    if isempty(sixresp{1}) == 1;
         Acc_triali = 0;
         inacc1 = 999; %%should be 1 because they are inaccurate? or 999?
         inacc2= 999;
         inacc3 = 999;
    else %only compute selection-by-selection accuracy if there were 6 button presses
        % use regexp to get first two responses, 2nd responses, and 3rd
        % responses from 6 digit Resp.  Also determine dimension (e.g., shape) of acc responses
        selections = regexp(resp, '..', 'match');
        first_sel = str2num(selections{1}{1});
        second_sel =str2num(selections{1}{2});
        third_sel = str2num(selections{1}{3});
        if first_sel == second_sel == third_sel;
            inacc1 = ~(isempty(find(inaccs == first_sel)));
            inacc2 = 1;
            inacc3 = 1;
        elseif first_sel == second_sel;
            inacc1 = ~(isempty(find(inaccs == first_sel))); 
            inacc2 = 1;
            inacc3 = ~(isempty(find(inaccs == third_sel))); 
        elseif first_sel == third_sel; 
            inacc1 = ~(isempty(find(inaccs == first_sel))); 
            inacc2= ~(isempty(find(inaccs == second_sel)));
            inacc3 = 1; 
        elseif second_sel == third_sel;
            inacc1 = ~(isempty(find(inaccs == first_sel))); 
            inacc2= ~(isempty(find(inaccs == second_sel)));
            inacc3 = 1; 
        else
            inacc1 = ~(isempty(find(inaccs == first_sel))); %inacc is 0 if accurate, 1 if inaccurate
            inacc2= ~(isempty(find(inaccs == second_sel)));
            inacc3 = ~(isempty(find(inaccs == third_sel))); 
        end
    end
 
    %%this determines accuracy for each selection; inacc = 0 means
    %%selection is correct, inacc = 999 and inacc= 1 are incorrect.
    for j = 1:3
        if j == 1;
            inacc = inacc1;
        elseif j ==2;
            inacc = inacc2;
        elseif j ==3;
            inacc= inacc3;
        end
        if inacc == 0; %%if the subj's response matches one of the correct responses,acc=1
            % if the matrix is empty, the response was accurate
            if j ==1;
                Acc1 = 1;
            elseif j ==2;
                Acc2 = 1;
            elseif j == 3;
                Acc3 = 1;
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
  %%all inacc must be equal to 0 (meaning all are correct) for the trial to be accurate
    if inacc1 == 0 && inacc2 == 0 && inacc3 == 0;
        Acc_triali = 1;
        %%determining the dimension  of selection
        %columns 1 and 5 are shape, 2 and 6 are size, 3 and 7 are color, and
        %4 and 8 are number; only output dimension information is trial is
        %accurate
        dim1i = find(accresp.data(trial_row, 1:8) == first_sel); 
        if dim1i == 1 || dim1i == 5 ;
            dim1 = 'shape';
        elseif dim1i == 2 || dim1i ==6;
            dim1 = 'size';
        elseif dim1i == 3 || dim1i == 7;
            dim1 = 'color';
        elseif dim1i == 4 || dim1i == 8;
            dim1 = 'number';
        end
        dim2i = find(accresp.data(trial_row, 1:8) == second_sel); 
        if dim2i == 1 || dim2i == 5 ;
            dim2 = 'shape';
        elseif dim2i == 2 || dim2i ==6;
            dim2 = 'size';
        elseif dim2i == 3 || dim2i == 7;
            dim2 = 'color';
        elseif dim2i == 4 || dim2i == 8;
            dim2 = 'number';
        end
        dim3i = find(accresp.data(trial_row, 1:8) == third_sel); 
        if dim3i == 1 || dim3i == 5 ;
            dim3 = 'shape';
        elseif dim3i == 2 || dim3i ==6;
            dim3 = 'size';
        elseif dim3i == 3 || dim3i == 7;
            dim3 = 'color';
        elseif dim3i == 4 || dim3i == 8;
            dim3 = 'number';
        end
    elseif inacc1 == 999 && inacc2 == 999 && inacc3 == 999;
        Acc_triali = 0;
        dim1=999; %%dimension information is missing (999) if less than 6 button resp
        dim2=999;
        dim3=999;
    elseif inacc1 == 1 || inacc2==1 || inacc3 == 1;
        Acc_triali = 0;
        if inacc1 == 0;
        dim1i = find(accresp.data(trial_row, 1:8) == first_sel); 
            if dim1i == 1 || dim1i == 5 ;
                dim1 = 'shape';
            elseif dim1i == 2 || dim1i ==6;
                dim1 = 'size';
            elseif dim1i == 3 || dim1i == 7;
                dim1 = 'color';
            elseif dim1i == 4 || dim1i == 8;
                dim1 = 'number';
            end
        else
            dim1 = 999;
        end
        if inacc2 == 0;
        dim2i = find(accresp.data(trial_row, 1:8) == second_sel); 
            if dim2i == 1 || dim2i == 5 ;
                dim2 = 'shape';
            elseif dim2i == 2 || dim2i ==6;
                dim2 = 'size';
            elseif dim2i == 3 || dim2i == 7;
                dim2 = 'color';
            elseif dim2i == 4 || dim2i == 8;
                dim2 = 'number';
            end
        else
            dim2 = 999;
        end
        if inacc3 == 0;
        dim3i = find(accresp.data(trial_row, 1:8) == third_sel); 
            if dim3i == 1 || dim3i == 5 ;
                dim3 = 'shape';
            elseif dim3i == 2 || dim3i ==6;
                dim3 = 'size';
            elseif dim3i == 3 || dim3i == 7;
                dim3 = 'color';
            elseif dim3i == 4 || dim3i == 8;
                dim3 = 'number';
            end
        else 
            dim3 = 999;
        end
    end
    %creating matrix where each row represents a separate trial 
    if isempty(matrix_acc);
        matrix_acc = {char(image), str2double(resp{1}), str2double(FlexRT{1}), Acc_triali, Acc1, Acc2, Acc3, dim1, dim2, dim3};
    else
        rowi = {char(image), str2double(resp{1}), str2double(FlexRT{1}), Acc_triali, Acc1, Acc2, Acc3, dim1, dim2, dim3};
        matrix_acc = [matrix_acc; rowi];
    end
end
%% ========Retrieving accuracy and RT information for control trials ======
control_matrix = {};
k = 1;
for i = 1: length(controltrialnum)/3
    %loop to go through each of the three selections per control trial
    p = 0;
    for j = 1:3              
        %%this is the name of the trial image
        if j == 1;
            image1 = regexp(input{1}{cimagerow(k+p)}, 'Slide\d*.JPG', 'match');
        elseif j == 2;
            image2 = regexp(input{1}{cimagerow(k+p)}, 'Slide\d*.JPG', 'match');
        elseif j == 3;
            image3 = regexp(input{1}{cimagerow(k+p)}, 'Slide\d*.JPG', 'match');
        end
        if j ==1;
            resp1 = regexp(input{1}{cresprow(k+p)}, '\d*', 'match'); %%this is the subj's response (should be 6 digits)
            if isempty(resp1);
                resp1 = '999';
            else
                resp1 = str2double(resp1{1});
            end
        elseif j==2;
            resp2 = regexp(input{1}{cresprow(k+p)}, '\d*', 'match');
            if isempty(resp2);
                resp2 = '999';
            else
                resp2 = str2double(resp2{1});
            end
        elseif j==3;
            resp3 = regexp(input{1}{cresprow(k+p)}, '\d*', 'match');
            if isempty(resp3);
                resp3 = '999';
            else
                resp3 = str2double(resp3{1});
            end
        end
        if j ==1;
            controlacc1 = str2double(regexp(input{1}{caccrow(k+p)}, '\d', 'match')); %accuracy of control selection (1 if correct, 0 if incorrect)
        elseif j==2;
            controlacc2 =str2double(regexp(input{1}{caccrow(k+p)}, '\d', 'match'));
        elseif j ==3;
            controlacc3 =str2double(regexp(input{1}{caccrow(k+p)}, '\d', 'match'));
        end
        if j ==1;
            controlRT1 = regexp(input{1}{cRTrow(k+p)}, '\d*', 'match');
            controlRT1 = str2double(controlRT1{1});
            if controlRT1 == 0;
                controlRT1 = '999';
            end
        elseif j ==2;
            controlRT2 = regexp(input{1}{cRTrow(k+p)}, '\d*', 'match');
            controlRT2 = str2double(controlRT2{1});
            if controlRT2 == 0;
                controlRT2 = '999';
            end
        elseif j ==3;
            controlRT3 = regexp(input{1}{cRTrow(k+p)}, '\d*', 'match');
            controlRT3 = str2double(controlRT3{1});
            if controlRT3 == 0;
                controlRT3 = '999';
            end
        end
        p=p+1;
    end

    %%this determines accuracy for the entire control trial (all three
    %%selections)
     if controlacc1 == 1 && controlacc2 == 1 && controlacc3 == 1;
        Control_acci = 1;
     else
        Control_acci = 0;
     end
    %creating matrix where each row represents a separate trial 
    if isempty(control_matrix);
       control_matrix = {char(image1), char(image2), char(image3), resp1,...
        resp2, resp3, controlRT1, ...
        controlRT2, controlRT3,...
        Control_acci, controlacc1, controlacc2, controlacc3};
    else
        rowi = {char(image1), char(image2), char(image3), resp1,...
        resp2, resp3, controlRT1, ...
        controlRT2, controlRT3,...
        Control_acci, controlacc1, controlacc2, controlacc3};
        control_matrix = [control_matrix; rowi];
    end
    if i == 1 ; 
       k = i + 3;
    else
       k = k + 3;
    end
end
%% Display average accuracy across all trials 
acc_mean = mean(cell2mat(matrix_acc(:,4)));
x = sprintf('The mean accuracy for flex trials is %0.2f', acc_mean);
disp(x);
acc1n = cell2mat(matrix_acc(:,5));
acc_mean1 = mean(acc1n(~(acc1n==999))); 
x = sprintf('The mean accuracy for the 1st selection trials is %0.2f', acc_mean1);
disp(x);
acc2n = cell2mat(matrix_acc(:,6));
acc_mean2 = mean(acc2n(~(acc1n==999))); 
x = sprintf('The mean accuracy for the 2nd selection is %0.2f', acc_mean2);
disp(x);
acc3n = cell2mat(matrix_acc(:,7));
acc_mean3 = mean(acc3n(~(acc1n==999))); 
x = sprintf('The mean accuracy for the 3rd selection is %0.2f', acc_mean3);
disp(x);
acc_control = mean(cell2mat(control_matrix(:, 10)));
disp(sprintf('The mean accuracy for control trials is %0.2f', acc_control));
%% Write table to csv
matrix = horzcat(matrix_acc, control_matrix);
T = cell2table(matrix, 'VariableNames', {'FlexImage', 'FlexResp', 'FlexRT', 'Acc_Flex',...
    'AccFlex1', 'AccFlex2', 'AccFlex3', 'FlexDim1', 'FlexDim2', 'FlexDim3', ...
    'ControlImage1', 'ControlImage2', 'ControlImage3', 'ControlResp1', ...
    'ControlResp2', 'ControlResp3', 'ControlRT1', 'ControlRT2', ...
    'ControlRT3', 'Acc_control', 'AccControl1', 'AccControl2', 'AccControl3'});
[pathstr, name, ext] = fileparts(txtfile);
acc_file = strcat(outdir, '/', name, '_AccRT.csv');
writetable(T, acc_file);
end
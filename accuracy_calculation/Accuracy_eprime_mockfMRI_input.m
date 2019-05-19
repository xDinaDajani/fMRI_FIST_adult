%% Input parameteres for the Accuracy_eprime function %%
%%specify the full path to textfile, accresp_file, and outdir. See
%%Accuracy_eprime script for more information located in 
%%/Volumes/Groups/LUddin_Lab/Lab/Experiments/FIST/accuracy_calculation
clear
acc_folder = '/Volumes/Groups/LUddin_Lab/Lab/Experiments/FIST/accuracy_calculation';
accresp_file = strcat(acc_folder, '/4Match_FIST_correct_responses_fMRI_matlab.csv'); %unless file gets moved, will not have to change %unless file gets moved, will not have to change
addpath(acc_folder)
txt_folder = '/Volumes/Groups/LUddin_Lab/Lab/Experiments/FIST/Data_adults/';

%========================================================================================

%ONLY EDIT SCRIPT BETWEEN THESE LINES!
subject_number = '00089'; % <============ SPECIFY SUBJECT NUMBER (without _FST) HERE!!! 


%========================================================================================
run_number = '1'; % <============ SPECIFY RUN NUMBER57ERE!!!
txtfile = strcat(txt_folder, subject_number,...
    '_FST/FIST_Mock_fMRI_Practice-', subject_number,'-', run_number,'.txt');
outdir = strcat(txt_folder, subject_number,'_FST/AccRT');
if exist(outdir, 'dir') == 0;
    mkdir(outdir);
end
Accuracy_eprime_Mock_fMRI(txtfile, outdir, accresp_file)


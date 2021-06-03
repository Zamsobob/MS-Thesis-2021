% Current Source Density (CSD) transformation of the pre-processed resting-state EEG data.
% 
% Sources:
% https://psychophysiology.cpmc.columbia.edu/software/csdtoolbox/
% https://doi.org/10.1016/j.clinph.2005.08.034
% https://doi.org/10.1016/j.clinph.2005.08.033

% EEGLAB PREFERENCES (STORE 1 DATASET AND DOUBLE PRECISSION) USED:
% pop_editoptions('option_storedisk', 1);
% pop_editoptions('option_single', 0);
%
% addpath(YourPathToEEGLAB); % EEGLAB TO PATH
%
%% INITIAL SETUP

% PATH TO THE NECESSARY FOLDERS
eegfolder = [pwd filesep]; % EEG_MPILMBB_LEMON. PATH TO SCRIPTS
rawfolder = [eegfolder 'EEG_Raw_BIDS_ID\']; % RAW FILES

% SET UP FOLDERS AND PATHS
ppfolder = [eegfolder 'EEG_Preprocessed\'];
final = [ppfolder 'EEG_Final\'];
cd (ppfolder);
if~exist('EEG_CSD', 'dir')
    mkdir 'EEG_CSD'
end
csdfolder = [ppfolder 'EEG_CSD'];
csddir = [eegfolder 'CSDtoolbox'];
statdir = [eegfolder 'EEG_Statistics'];

% ADD CSDTOOLBOX (WHICH IS IN EEGFOLDER) AND ITS SUBFOLDERS TO PATH
addpath(genpath(csddir));

% IMPORT PRE-PROCESSED LIST OF SUBJECTS
cd(statdir);
subject_list = readcell('Diagnostics.csv', "NumHeaderLines", 1);
subject_list = subject_list(:,1)';

%% GENERATE EEG MONTAGE AND TRANSFORMATION MATRICES

% LOAD PREPROCESSED DATA WITH REMOVED VEOG FOR ONE SUBJECT
EEG = pop_loadset('filename', 'sub-010002_Preprocessed.set', 'filepath', final);

% CREATE A COLUMN VECTOR OF CHANNEL LABELS BY TRANSPOSITION
electrodes = {EEG.chanlocs.labels}';
    
% SPECIFY AN EEG MONTAGE OF THE SPATIAL ELECTRODE LOCATIONS USING THE
% CSD TOOLBOX. THE HEAD IS REPRESENTED AS A UNIT SPHERE (RADIUS OF 1)
montage = ExtractMontage('10-5-System_Mastoids_EGI129.csd', electrodes);
    
% GENERATE THE ELECTRODES TIMES ELECTRODES TRANSFORMATION MATRICES 'G'
% AND 'H' THAT THE SURFACE LAPLACIAN IN THE CSD TOOLBOX IS BASED ON.
% 'G' USED FOR SPHERICAL SPLINE INTERPOLATION OF SURFACE POTENTIALS
% 'H' USED FOR CURRENT SOURCE DENSITIES
[G, H] = GetGH(montage); % SPLINE FLEXIBILITY(m) = 4 (DEFAULT)
    
% SAVE G AND H TO LATER IMPORT WHEN COMPUTING THE CSD TRANFORM
cd (csdfolder);
save CSDmontage.mat G H montage;

%% SURFACE LAPLACIAN TRANSFORMATION

% LOOP THROUGH ALL SUBJECTS
for s = 1:length(subject_list)
    
    subject = subject_list{s};
    
    % LOAD PRE-PROCESSED DATA
    EEG = pop_loadset('filename',[subject '_Preprocessed.set'],'filepath', final);
    CSDdata = repmat(NaN,size(EEG.data)); % INITIALIZE
    
    % APPLY THE SURFACE LAPLACIAN TRANSFORM TO EACH EPOCH
    % SMOOTHING CONSTANT(LAMBDA) = 0.00001 = 1.0e-5
    % HEAD RADIUS = 10CM -> RETURNS VALUES OF uV/cm^2
    for ep = 1:length(EEG.epoch)
        Data = squeeze(EEG.data(:,:,ep)); % DATA CONTAINS EEG SIGNALS TO BE TRANSFORMED
        X = CSD(Data, G, H); % X IS THE CSD ESTIMATE OF DATA. 
        CSDdata(:,:,ep) = X;   
    end
    EEG.data = CSDdata; % REPLACE EEG DATA WITH CSD ESTIMATES
    
    % SAVE CSD TRANSFORMED DATA. NOTE: DATA CONTAINS CSD ESTIMATES, NOT EEG SIGNALS
    EEG.setname = [subject '_CSD_Estimates']; % NAME FOR DATASET MENU
    EEG = pop_saveset(EEG, ...
         'filename',[subject '_CSD_Estimates.set'], ...
         'filepath', csdfolder);

     CSDdata(:,:,:) = NaN; % RE-INITIALIZE DATA OUTPUT
end

% VERIFY THE INTEGRITY AND CORRECTNESS OF THE IDENTIFIED EEG MONTAGE
MapMontage(montage) % TOPOGRAPHICAL PLOT OF THE EEG MONTAGE

fprintf('\n\n\n**** LEMON CSD TRANSFORM FINISHED ****\n\n\n');

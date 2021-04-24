% Script for preprocessing resting-state EEG data from the LEMON dataset
% The pre-processing pipeline was developed for the extraction of frontal alpha asymmetry (FAA)
% It is based on recommended guidelines from Smith et al. (2017) and
% designed to be fully automatic, for the purpose of reproducibility.
% 
% 
% Before running this script, 7 subjects were removed from rawfolder due to missing data:
% sub-010203. The VMRK-file is empty. 
% sub-010285. The VMRK-file is missing markers, cannot be read by EEGLAB
% sub-010235, sub-010237, sub-010259, sub-010281, & sub-010293. No data in files.
% 
% 
% EEGLAB PREFERENCES (STORE 1 DATASET AND DOUBLE PRECISSION) USED:
% pop_editoptions('option_storedisk', 1);
% pop_editoptions('option_single', 0);
% 
% Sources:
% https://doi.org/10.1038/sdata.2018.308
% https://doi.org/10.1016/j.ijpsycho.2016.11.005)
% 
% 
%% INITIAL SETUP

% SET VARIABLE TO 1 TO SAVE INTERMEDIATE STEPS. SET TO 0 TO SAVE
% ONLY THE NECESSARY FILES
save_everything = 1;

% SET PATHS
addpath('C:\Users\Mar Nil\Desktop\MATLABdirectory\eeglab2021.0'); % EEGLAB TO PATH
eegfolder = [pwd filesep]; % EEG_MPILMBB_LEMON. PATH TO SCRIPTS
rawfolder = [eegfolder 'EEG_Raw_BIDS_ID\']; % RAW FILES
localizer = [eegfolder 'EEG_Localizer_BIDS_ID\Channel_Loc_62_EOG.ced']; % PATH TO CHANNEL LOCATIONS
file_ext = '.vhdr'; % FILE EXTENSION OF RAW FILES

%% --------------------------NO FURTHER SETTINGS NECESSARY-----------------------------------------

% CREATE FOLDERS FOR THE PREPROCESSED DATA - FIX FOLDERS
if~exist('EEG_Preprocessed', 'dir')
    mkdir 'EEG_Preprocessed' 'EEG_Intermediate'
end
ppfolder = [eegfolder 'EEG_Preprocessed\'];
rsdir = [ppfolder 'EEG_Intermediate'];

if~exist('EEG_Statistics', 'dir')
    mkdir 'EEG_Statistics'
statdir = [eegfolder 'EEG_Statistics'];
cd (ppfolder);

if ~exist('EEG_Final', 'dir')
    mkdir EEG_Final
end
final = [ppfolder 'EEG_Final'];

% CREATE LIST OF SUBJECTS TO LOOP THROUGH
cd (rawfolder);
subject_list=dir(['*/*' file_ext]);
subject_list={subject_list.name};

parfor s = 1:length(subject_list) % PARALLEL COMPUTING TOOLBOX (parfor)
    
    % CURRENT SUBJECT
    subject = subject_list{s};
    subject = extractBefore(subject, file_ext);
    
    fprintf('subject %d: %s\n', s, subject);
    
    % PATH TO THE FOLDER CONTAINING THE CURRENT SUBJECT'S RAW DATA
    subjectfolder = [rawfolder subject filesep];
    
    % IMPORT RAW DATA
    EEG = pop_loadbv(subjectfolder, [subject '.vhdr']);
    
    % IMPORT CHANNEL LOCATIONS WITH FCz AS ONLINE REFERENCE
    EEG = pop_chanedit(EEG, 'load',{localizer, ...
        'filetype', 'autodetect'}, ...
        'setref', {'1:63', 'FCz'}, ...
        'changefield', {63, 'datachan', 0});

    % RESAMPLE TO 250 HZ
    EEG = pop_resample(EEG, 250);
    
    % FIND NON-STIMULUS (NOT EO/EC) EVENTS AND REMOVE THEM 
    allCodes = {EEG.event.code}';
    Idx = strcmp(allCodes, 'Stimulus');
    toRemove = find(Idx == 0); % FIND NON-STIMULUS EVENTS
    EEG = pop_editeventvals(EEG,'delete', toRemove); % DELETE THEM (1:2 HERE)
    
    % RENAME EYES-OPEN EVENTS (S210) TO THE SAME AS EYES-CLOSED EVENTS
    % (S200), AS THEY ARE NOT TREATED SEPARATELY
    allTypes = {EEG.event.type}';
    IdxT = strcmp(allTypes, 'S210');
    [EEG.event(IdxT).type] = deal('S200');
    
    % SAVE RS DATA
    if (save_everything)
    EEG = pop_saveset(EEG, 'filename',[subject '_RS.set'], ...
        'filepath', rsdir);
    end
    
    %% FILTERING
    
    % HIGH-PASS FILTER 1 HZ. 827 POINTS. CUTOFF FREQUENCY (~6dB): 0.5 Hz.
    % ZERO-PHASE. NON-CAUSAL (FIRFILT).
    EEG = pop_eegfiltnew(EEG, 'locutoff',1);
    
    % LOW-PASS FILTER 45 HZ TO SUPPRESS POSSIBLE LINE NOISE. 75 points.
    % CUTOFF FREQUENCY (~6dB): 50.625 Hz. ZERO-PHASE, NON-CAUSAL (FIRFILT)
    EEG = pop_eegfiltnew(EEG, 'hicutoff',45);
    EEG.setname = [subject '_Filter']; % NAME FOR DATASET MENU
   
    % SAVE FILTERED DATA
    if (save_everything)
    EEG = pop_saveset(EEG, 'filename',[subject '_Filt'], ...
        'filepath', rsdir);
    end
    
    %% ARTIFACT REMOVAL WITH CLEAN RAW DATA AND ARTIFACT SUBSPACE RECONSTRUCTION (ASR)
    
    % SAVE ORIGINAL DATA BEFORE REMOVING BAD CHANNELS
    originalchanlocs = EEG.chanlocs; % FOR INTERPOLATION LATER
    oldchans = {EEG.chanlocs.labels};
    origEEG = EEG;
    
     % USE CLEAN_RAWDATA TO IDENTIFY CHANNELS FOR REMOVAL
    EEG = pop_clean_rawdata(EEG, 'FlatlineCriterion', 5, ...
        'ChannelCriterion', 0.8, ...
        'LineNoiseCriterion', 4, ...
        'Highpass', 'off', ...
        'BurstCriterion', 'off', ...
        'WindowCriterion', 'off', ...
        'availableRAM_GB', 8, ...
        'BurstRejection', 'off', ...
        'Distance', 'Euclidian');
    
    % I DO NOT WANT TO REMOVE THE VEOG CHANNEL
    newchans = {EEG.chanlocs.labels}; % SAVE NEW CHANS AFTER CLEAN
    chandiff = setdiff(oldchans, newchans); % DIFFERENCE OLD AND NEW CHANNELS
    
    % IDENTIFY IF VEOG CHANNEL WAS REMOVED WITH CLEAN_RAWDATA
    if any(strcmp(chandiff,'VEOG'))
        chandiff(strncmpi(chandiff,'VEOG', 3)) = [];
    end
    
    % GO BACK TO DATA BEFORE CLEAN_RAW AND REMOVE ONLY EEG CHANNELS,
    % LEAVING EOG CHANNELS IN THE DATASET
    EEG = origEEG;
    if ~isempty(chandiff)
        EEG = pop_select(origEEG, 'nochannel', chandiff);
    end
    
    % PERFORM ARTIFACT SUBSPACE RECONSTRUCTION (ASR) WITH CLEAN_RAWDATA
    EEG = pop_clean_rawdata(EEG, 'FlatlineCriterion', 'off', ...
        'ChannelCriterion', 'off',  ...
        'LineNoiseCriterion', 'off', ...
        'Highpass', 'off', ...
        'BurstCriterion', 20, ...
        'WindowCriterion', 0.25, ...
        'availableRAM_GB', 8, ...
        'BurstRejection', 'on', ...
        'Distance', 'Euclidian', ...
        'WindowCriterionTolerances', [-Inf 7] );
    EEG.setname = [subject '_ASR']; % NAME FOR DATASET MENU

    % SAVE DATA
    if (save_everything)
        EEG = pop_saveset(EEG, 'filename',[subject '_ASR.set'], ...
            'filepath', rsdir);
    end
    
    %% EPOCHING (NOT OVERLAPPED) AND REMOVAL OF BAD EPOCHS BEFORE ICA
    
    % EXTRACT EPOCHS
    EEG = pop_epoch(EEG, {'S200'}, [0 2], ...
        'newname', [subject '_Epoch'], ...
        'epochinfo', 'yes');
    
     % REMOVE BASELINE (MEAN OF THE WHOLE EPOCH)
    EEG = pop_rmbase(EEG, [],[]);
    
    % MARK BAD EPOCHS (-500 TO 500 uV THRESHOLD) AND REJECT THEM
    EEG = pop_eegthresh(EEG,1, ...
        [1:length(EEG.chanlocs)], ...
        -500, 500, ...
        0, 2, ...
        0, 0);
    EEG = pop_rejepoch(EEG, EEG.reject.rejthresh,0);
     
    % APPLY IMPROBABILITY TEST WITH 6SD FOR SINGLE CHANNELS AND 2SD FOR
    % ALL CHANNELS. REJECT SELECTED EPOCHS AGAIN. MAKOTO RECOMMENDATION
    EEG = pop_jointprob(EEG, 1, [1:length(EEG.chanlocs)], ...
        6, 2, 0, 1, 0, [], 0);
    
    %% RUN ICA ON ALL CHANNELS
    EEG = pop_runica(EEG, 'extended', 1, ...
        'interupt', 'on', ...
        'pca', length(EEG.chanlocs));
    EEG.setname = [subject '_ICA_Weights']; % NAME FOR DATASET MENU
    EEG = eeg_checkset(EEG, 'ica');
      
    % SAVE DATA WITH ICA WEIGHTS
    EEG = pop_saveset(EEG, 'filename',[subject '_ICA_Weights.set'], ...
        'filepath', rsdir);
end

fprintf('\n\n\n**** LEMON PREPROCESSING 1 FINISHED ****\n\n\n');

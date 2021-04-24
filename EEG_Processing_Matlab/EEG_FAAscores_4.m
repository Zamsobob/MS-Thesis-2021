% FRONTAL ALPHA ASYMMETRY (FAA) SCORES OF CSD TRANSFORMED EEG DATA
% 
% Sources:
% https://doi.org/10.1016/j.ijpsycho.2016.11.005)

% EEGLAB PREFERENCES (STORE 1 DATASET AND DOUBLE PRECISSION) USED:
% pop_editoptions('option_storedisk', 1);
% pop_editoptions('option_single', 0);

%% INITIAL SETUP

% PATH TO THE NECESSARY FOLDERS
addpath('C:\Users\Mar Nil\Desktop\MATLABdirectory\eeglab2021.0'); % EEGLAB TO PATH
eegfolder = [pwd filesep]; % EEG_MPILMBB_LEMON. PATH TO SCRIPTS
rawfolder = [eegfolder 'EEG_Raw_BIDS_ID\']; % RAW FILES

%% --------------------------NO FURTHER SETTINGS NECESSARY-----------------------------------------

% SET UP FOLDERS AND PATHS
ppfolder = [eegfolder 'EEG_Preprocessed\'];
final = [ppfolder 'EEG_Final\'];
csdfolder = [ppfolder 'EEG_CSD'];
csddir = [eegfolder 'CSDtoolbox'];
statdir = [eegfolder 'EEG_Statistics'];

% IMPORT PRE-PROCESSED LIST OF SUBJECTS
cd(statdir);
subject_list = readcell('Diagnostics.csv', "NumHeaderLines", 1);
subject_list = subject_list(:,1)';

% FRONTAL ELECTRODES
nchans_left = [37 4 36 3]; % LEFT = [F1 F3 F5 F7]
nchans_right = [38 6 39 7]; % RIGHT = [F2 F4 F6 F8]

% INITIALIZE VARIABLES
numelectrodes = 61; % NUMBER OF ELECTRODES IN DATASET
numelecpairs = 4; % NUMBER OF ELECTRODE PAIRS TO COMPARE (E.G., F4-F3)
alphapower = zeros(numelectrodes, length(subject_list)); % ALPHA POWER
asymmetry = zeros(numelecpairs, length(subject_list)); % FAA SCORES

%% FREQUENCY DECOMPOSITION

% LOOP THROUGH ALL SUBJECTS
for s = 1:length(subject_list)
    
    subject = subject_list{s};
    
    % LOAD PREPROCESSED EO AND EC DATASETS
    EEG = pop_loadset('filename',[subject '_CSD_Estimates.set'],'filepath', csdfolder);
   
    %% ANALYSIS OF ALL CHANNELS
    
    % COMPUTE POWER SPECTAL DENSITY (PSD) OF THE EPOCHS FOR ALL CHANNELS
    [spect, freqs] = spectopo(EEG.data, ...
        EEG.pnts, EEG.srate, ...
        'plot', 'off');
    
    % OUTPUT IS IN dB / cm^2 -> 10*log10(uV^2/Hz) / cm^2
    
    %% CONVERT TO ALPHA POWER CSD (uV^2/Hz) / cm^2 AND AVERAGE ACROSS FREQUENCIES
    
    alphaindex = find(freqs >= 8 & freqs <= 13); % FREQUENCY RANGE 8-13 Hz
    
    % CREATE CHANNEL X SUBEJCT MATRIX OF MEAN ALPHA POWER (uV^2)
    for electrode = 1:numelectrodes  
        alphapower(electrode, s) = mean(10.^(spect(electrode, alphaindex)/10));
    end
    
    % CREATE CHANNEL X SUBEJCT MATRIX OF POWER in dB 10*log10(uV^2/Hz)
    % ALPHA POWER IN DB WAS CALCULATED FOR A DIFFERENT PROJECT
    for electrode = 1:numelectrodes  
        alphapowerdB(electrode, s) = mean(spect(electrode, alphaindex));
    end
    
    % CREATE MATRIX OF ASYMMETRY SCORES. ROWS ARE ELECTRODE PAIRS, COLUMNS ARE SUBJECTS
    % FROM ROW 1 TO 4: F2-F1, F4-F3, F6-F5, F8-F7. TRANSPOSE LATER
    for i = 1:numelecpairs
        asymmetry(i, s) = log(alphapower(nchans_right(i),s)) - log(alphapower(nchans_left(i),s));
    end
    
    % CREATE MATRIX OF ASYMMETRY SCORES IN DB. ROWS ARE ELECTRODE PAIRS, COLUMNS ARE SUBJECTS
    % FROM ROW 1 TO 4: F2-F1, F4-F3, F6-F5, F8-F7. TRANSPOSE LATER
    for i = 1:numelecpairs
        asymmetrydB(i, s) = alphapowerdB(nchans_right(i),s) - alphapowerdB(nchans_left(i),s);
    end
    
end

% SUBJECTS AS ROWS
alphapower = alphapower';
alphapowerdB = alphapowerdB';
asymmetry = asymmetry';
asymmetrydB = asymmetrydB';

% EXPORT FILES TO EXCEL AND SAVE IN STATISTICS FOLDER
cd (statdir);
xlswrite('FAAscores_CSD', alphapower, 'Alpha Power');
xlswrite('FAAscores_CSD', asymmetry, 'Asymmetry Scores');
xlswrite('FAAscores_CSD', alphapowerdB, 'Alpha Power dB');
xlswrite('FAAscores_CSD', asymmetrydB, 'Asymmetry Scores dB');

fprintf('\n\n\n**** FAA FINISHED ****\n\n\n');
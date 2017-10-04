%% CN Stations reshape
% Reshape station data
%% Qilian_Q
Qilian_520800_T=table2array(Qilian_520800)
yS = Qilian_520800_T(1,1);
yE = Qilian_520800_T(end,1);
Qilian_520800_df = zeros(yE-yS+1,36);
Qilian_520800_df = reshape(Qilian_520800_T(:,3),36,size(Qilian_520800_df,1))';
y = (yS:yE)';
%% Zhamashike_Q
Zhamashike_520400_T=table2array(Zhamashike_520400)
yS = Zhamashike_520400_T(1,1);
yE = Zhamashike_520400_T(end,1);
Zhamashike_520400_df = zeros(yE-yS+1,36);
Zhamashike_520400_df = reshape(Zhamashike_520400_T(:,3),36,size(Zhamashike_520400_df,1))';
y = (yS:yE)';
%% Yingluosha_Q
Yingluoxia_521400_T=table2array(Yingluoxia_521400)
yS = Yingluoxia_521400_T(1,1);
yE = Yingluoxia_521400_T(end,1);
Yingluoxia_521400_df = zeros(yE-yS+1,36);
Yingluoxia_521400_df = reshape(Yingluoxia_521400_T(:,3),36,size(Yingluoxia_521400_df,1))';
y = (yS:yE)';
%% Qilian_T and Qilian_P
Qilian_52657_T = table2array(Qilian_52657)
yS = Qilian_52657_T(1,1);
yE = Qilian_52657_T(end,1);
Qilian_52657_df_T = zeros(yE-yS+1,36);
Qilian_52657_df_T = reshape(Qilian_52657_T(:,3),36,size(Qilian_52657_df_T,1))';
Qilian_52657_df_T(Qilian_52657_df_T==-9999) = NaN;
Qilian_52657_df_P = zeros(yE-yS+1,36);
Qilian_52657_df_P = reshape(Qilian_52657_T(:,4),36,size(Qilian_52657_df_P,1))';
Qilian_52657_df_P(Qilian_52657_df_P==-9999) = NaN;
y = (yS:yE)';
%% Yeniugou_52645
Yeniugou_52645_T = table2array(Yeniugou_52645)
yS = Yeniugou_52645_T(1,1);
yE = Yeniugou_52645_T(end,1);
Yeniugou_52645_df_T = zeros(yE-yS+1,36);
Yeniugou_52645_df_T = reshape(Yeniugou_52645_T(:,3),36,size(Yeniugou_52645_df_T,1))';
Yeniugou_52645_df_T(Yeniugou_52645_df_T==-9999) = NaN;
Yeniugou_52645_df_P = zeros(yE-yS+1,36);
Yeniugou_52645_df_P = reshape(Yeniugou_52645_T(:,4),36,size(Yeniugou_52645_df_P,1))';
Yeniugou_52645_df_P(Yeniugou_52645_df_P==-9999) = NaN;
y = (yS:yE)';
%% Tuole_52633
Tuole_52633_T = table2array(Tuole_52633)
yS = Tuole_52633_T(1,1);
yE = Tuole_52633_T(end,1);
Tuole_52633_df_T = zeros(yE-yS+1,36);
Tuole_52633_df_T = reshape(Tuole_52633_T(:,3),36,size(Tuole_52633_df_T,1))';
Tuole_52633_df_T(Tuole_52633_df_T==-9999) = NaN;
Tuole_52633_df_P = zeros(yE-yS+1,36);
Tuole_52633_df_P = reshape(Tuole_52633_T(:,4),36,size(Tuole_52633_df_P,1))';
Tuole_52633_df_P(Tuole_52633_df_P==-9999) = NaN;
y = (yS:yE)';
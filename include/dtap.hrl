% dtap.hrl

% Adapted from libosmocore/include/gsm/protocol/gsm_04_08.h

% Section 10.2 + GSM 04.07 12.2.3.1.1 
-define(GSM48_PDISC_GROUP_CC,	16#00).
-define(GSM48_PDISC_BCAST_CC,	16#01).
-define(GSM48_PDISC_PDSS1,	16#02).
-define(GSM48_PDISC_CC,		16#03).
-define(GSM48_PDISC_PDSS2,	16#04).
-define(GSM48_PDISC_MM,		16#05).
-define(GSM48_PDISC_RR,		16#06).
-define(GSM48_PDISC_MM_GPRS,	16#08).
-define(GSM48_PDISC_SMS,	16#09).
-define(GSM48_PDISC_SM_GPRS,	16#0a).
-define(GSM48_PDISC_NC_SS,	16#0b).
-define(GSM48_PDISC_LOC,	16#0c).
-define(GSM48_PDISC_MASK,	16#0f).
-define(GSM48_PDISC_USSD,	16#11).

% Section 10.4
-define(GSM48_MT_RR_INIT_REQ,		16#3c).
-define(GSM48_MT_RR_ADD_ASS,		16#3b).
-define(GSM48_MT_RR_IMM_ASS,		16#3f).
-define(GSM48_MT_RR_IMM_ASS_EXT,	16#39).
-define(GSM48_MT_RR_IMM_ASS_REJ,	16#3a).

-define(GSM48_MT_RR_CIPH_M_CMD,		16#35).
-define(GSM48_MT_RR_CIPH_M_COMPL,	16#32).

-define(GSM48_MT_RR_CFG_CHG_CMD,	16#30).
-define(GSM48_MT_RR_CFG_CHG_ACK,	16#31).
-define(GSM48_MT_RR_CFG_CHG_REJ,	16#33).

-define(GSM48_MT_RR_ASS_CMD,		16#2e).
-define(GSM48_MT_RR_ASS_COMPL,		16#29).
-define(GSM48_MT_RR_ASS_FAIL,		16#2f).
-define(GSM48_MT_RR_HANDO_CMD,		16#2b).
-define(GSM48_MT_RR_HANDO_COMPL,	16#2c).
-define(GSM48_MT_RR_HANDO_FAIL,		16#28).
-define(GSM48_MT_RR_HANDO_INFO,		16#2d).

-define(GSM48_MT_RR_CELL_CHG_ORDER,	16#08).
-define(GSM48_MT_RR_PDCH_ASS_CMD,	16#23).

-define(GSM48_MT_RR_CHAN_REL,		16#0d).
-define(GSM48_MT_RR_PART_REL,		16#0a).
-define(GSM48_MT_RR_PART_REL_COMP,	16#0f).

-define(GSM48_MT_RR_PAG_REQ_1,		16#21).
-define(GSM48_MT_RR_PAG_REQ_2,		16#22).
-define(GSM48_MT_RR_PAG_REQ_3,		16#24).
-define(GSM48_MT_RR_PAG_RESP,		16#27).
-define(GSM48_MT_RR_NOTIF_NCH,		16#20).
-define(GSM48_MT_RR_NOTIF_FACCH,	16#25).
-define(GSM48_MT_RR_NOTIF_RESP,		16#26).

-define(GSM48_MT_RR_SYSINFO_8,		16#18).
-define(GSM48_MT_RR_SYSINFO_1,		16#19).
-define(GSM48_MT_RR_SYSINFO_2,		16#1a).
-define(GSM48_MT_RR_SYSINFO_3,		16#1b).
-define(GSM48_MT_RR_SYSINFO_4,		16#1c).
-define(GSM48_MT_RR_SYSINFO_5,		16#1d).
-define(GSM48_MT_RR_SYSINFO_6,		16#1e).
-define(GSM48_MT_RR_SYSINFO_7,		16#1f).

-define(GSM48_MT_RR_SYSINFO_2bis,	16#02).
-define(GSM48_MT_RR_SYSINFO_2ter,	16#03).
-define(GSM48_MT_RR_SYSINFO_5bis,	16#05).
-define(GSM48_MT_RR_SYSINFO_5ter,	16#06).
-define(GSM48_MT_RR_SYSINFO_9,		16#04).
-define(GSM48_MT_RR_SYSINFO_13,		16#00).

-define(GSM48_MT_RR_SYSINFO_16,		16#3d).
-define(GSM48_MT_RR_SYSINFO_17,		16#3e).

-define(GSM48_MT_RR_CHAN_MODE_MODIF,	16#10).
-define(GSM48_MT_RR_STATUS,		16#12).
-define(GSM48_MT_RR_CHAN_MODE_MODIF_ACK,	16#17).
-define(GSM48_MT_RR_FREQ_REDEF,		16#14).
-define(GSM48_MT_RR_MEAS_REP,		16#15).
-define(GSM48_MT_RR_CLSM_CHG,		16#16).
-define(GSM48_MT_RR_CLSM_ENQ,		16#13).
-define(GSM48_MT_RR_EXT_MEAS_REP,	16#36).
-define(GSM48_MT_RR_EXT_MEAS_REP_ORD,	16#37).
-define(GSM48_MT_RR_GPRS_SUSP_REQ,	16#34).

-define(GSM48_MT_RR_VGCS_UPL_GRANT,	16#08).
-define(GSM48_MT_RR_UPLINK_RELEASE,	16#0e).
-define(GSM48_MT_RR_UPLINK_FREE,	16#0c).
-define(GSM48_MT_RR_UPLINK_BUSY,	16#2a).
-define(GSM48_MT_RR_TALKER_IND,		16#11).

-define(GSM48_MT_RR_APP_INFO,		16#38).

% Table 10.2/3GPP TS 04.08
-define(GSM48_MT_MM_IMSI_DETACH_IND,	16#01).
-define(GSM48_MT_MM_LOC_UPD_ACCEPT,	16#02).
-define(GSM48_MT_MM_LOC_UPD_REJECT,	16#04).
-define(GSM48_MT_MM_LOC_UPD_REQUEST,	16#08).

-define(GSM48_MT_MM_AUTH_REJ,		16#11).
-define(GSM48_MT_MM_AUTH_REQ,		16#12).
-define(GSM48_MT_MM_AUTH_RESP,		16#14).
-define(GSM48_MT_MM_ID_REQ,		16#18).
-define(GSM48_MT_MM_ID_RESP,		16#19).
-define(GSM48_MT_MM_TMSI_REALL_CMD,	16#1a).
-define(GSM48_MT_MM_TMSI_REALL_COMPL,	16#1b).

-define(GSM48_MT_MM_CM_SERV_ACC,	16#21).
-define(GSM48_MT_MM_CM_SERV_REJ,	16#22).
-define(GSM48_MT_MM_CM_SERV_ABORT,	16#23).
-define(GSM48_MT_MM_CM_SERV_REQ,	16#24).
-define(GSM48_MT_MM_CM_SERV_PROMPT,	16#25).
-define(GSM48_MT_MM_CM_REEST_REQ,	16#28).
-define(GSM48_MT_MM_ABORT,		16#29).

-define(GSM48_MT_MM_NULL,		16#30).
-define(GSM48_MT_MM_STATUS,		16#31).
-define(GSM48_MT_MM_INFO,		16#32).

% Table 10.3/3GPP TS 04.08
-define(GSM48_MT_CC_ALERTING,		16#01).
-define(GSM48_MT_CC_CALL_CONF,		16#08).
-define(GSM48_MT_CC_CALL_PROC,		16#02).
-define(GSM48_MT_CC_CONNECT,		16#07).
-define(GSM48_MT_CC_CONNECT_ACK,	16#0f).
-define(GSM48_MT_CC_EMERG_SETUP,	16#0e).
-define(GSM48_MT_CC_PROGRESS,		16#03).
-define(GSM48_MT_CC_ESTAB,		16#04).
-define(GSM48_MT_CC_ESTAB_CONF,		16#06).
-define(GSM48_MT_CC_RECALL,		16#0b).
-define(GSM48_MT_CC_START_CC,		16#09).
-define(GSM48_MT_CC_SETUP,		16#05).

-define(GSM48_MT_CC_MODIFY,		16#17).
-define(GSM48_MT_CC_MODIFY_COMPL,	16#1f).
-define(GSM48_MT_CC_MODIFY_REJECT,	16#13).
-define(GSM48_MT_CC_USER_INFO,		16#10).
-define(GSM48_MT_CC_HOLD,		16#18).
-define(GSM48_MT_CC_HOLD_ACK,		16#19).
-define(GSM48_MT_CC_HOLD_REJ,		16#1a).
-define(GSM48_MT_CC_RETR,		16#1c).
-define(GSM48_MT_CC_RETR_ACK,		16#1d).
-define(GSM48_MT_CC_RETR_REJ,		16#1e).

-define(GSM48_MT_CC_DISCONNECT,		16#25).
-define(GSM48_MT_CC_RELEASE,		16#2d).
-define(GSM48_MT_CC_RELEASE_COMPL,	16#2a).

-define(GSM48_MT_CC_CONG_CTRL,		16#39).
-define(GSM48_MT_CC_NOTIFY,		16#3e).
-define(GSM48_MT_CC_STATUS,		16#3d).
-define(GSM48_MT_CC_STATUS_ENQ,		16#34).
-define(GSM48_MT_CC_START_DTMF,		16#35).
-define(GSM48_MT_CC_STOP_DTMF,		16#31).
-define(GSM48_MT_CC_STOP_DTMF_ACK,	16#32).
-define(GSM48_MT_CC_START_DTMF_ACK,	16#36).
-define(GSM48_MT_CC_START_DTMF_REJ,	16#37).
-define(GSM48_MT_CC_FACILITY,		16#3a).


%%%% INFORMATION ELEMENTS

% common elements
-define(GSM48_IE_LOCATION_AREA,	16#13). % 10.5.1.3
-define(GSM48_IE_MOBILE_ID,	16#17).	% 10.5.1.4

% MM elements
-define(GSM48_IE_NAME_LONG,	16#43).	% 10.5.3.5a
-define(GSM48_IE_NAME_SHORT,	16#45).	% 10.5.3.5a
-define(GSM48_IE_UTC,		16#46).	% 10.5.3.8
-define(GSM48_IE_NET_TIME_TZ,	16#47).	% 10.5.3.9
-define(GSM48_IE_LSA_IDENT,	16#48).	% 10.5.3.11
-define(GSM48_IE_FOLLOW_ON_PROC,	16#a1).
% cipher_key_seq
% classmark_1
% classmark_2
% spare_half
% loc_upd_type
% cong_lev
% rej_cause

% CC elements
-define(GSM48_IE_BEARER_CAP,	16#04).	% 10.5.4.5
-define(GSM48_IE_CAUSE,		16#08).	% 10.5.4.11
% call_state
-define(GSM48_IE_CC_CAP,	16#15).	% 10.5.4.5a
-define(GSM48_IE_ALERT,		16#19).	% 10.5.4.26
-define(GSM48_IE_FACILITY,	16#1c).	% 10.5.4.15
-define(GSM48_IE_PROGR_IND,	16#1e).	% 10.5.4.21
-define(GSM48_IE_AUX_STATUS,	16#24).	% 10.5.4.4
-define(GSM48_IE_NOTIFY,	16#27).	% 10.5.4.20
-define(GSM48_IE_KPD_FACILITY,	16#2c).	% 10.5.4.17
-define(GSM48_IE_SIGNAL,	16#34).	% 10.5.4.23
-define(GSM48_IE_CONN_BCD,	16#4c).	% 10.5.4.13
-define(GSM48_IE_CONN_SUB,	16#4d).	% 10.5.4.14
-define(GSM48_IE_CALLING_BCD,	16#5c).	% 10.5.4.9
-define(GSM48_IE_CALLING_SUB,	16#5d).	% 10.5.4.10
-define(GSM48_IE_CALLED_BCD,	16#5e).	% 10.5.4.7
-define(GSM48_IE_CALLED_SUB,	16#6d).	% 10.5.4.8
-define(GSM48_IE_REDIR_BCD,	16#74).	% 10.5.4.21a
-define(GSM48_IE_REDIR_SUB,	16#75).	% 10.5.4.21b
-define(GSM48_IE_LOWL_COMPAT,	16#7c).	% 10.5.4.18
-define(GSM48_IE_HIGHL_COMPAT,	16#7d).	% 10.5.4.16
-define(GSM48_IE_USER_USER,	16#7e).	% 10.5.4.25
-define(GSM48_IE_SS_VERS,	16#7f).	% 10.5.4.24
-define(GSM48_IE_MORE_DATA,	16#a0).	% 10.5.4.19
-define(GSM48_IE_CLIR_SUPP,	16#a1).	% 10.5.4.11a
-define(GSM48_IE_CLIR_INVOC,	16#a2).	% 10.5.4.11b
-define(GSM48_IE_REV_C_SETUP,	16#a3).	% 10.5.4.22a
-define(GSM48_IE_REPEAT_CIR,	16#d1).	% 10.5.4.22
-define(GSM48_IE_REPEAT_SEQ,	16#d3).	% 10.5.4.22
-define(GSM48_IE_PRIORITY_LEV,	16#80).
-define(GSM48_IE_CTS_PERMISSION,	16#a2).


% Section 10.5.4.11 / Table 10.5.122
-define(GSM48_CAUSE_CS_GSM,	16#60).

% Section 9.1.2 / Table 9.3
% RR elements
-define(GSM48_IE_VGCS_TARGET,	16#01).
-define(GSM48_IE_VGCS_T_MODE_I,	16#01).
-define(GSM48_IE_FRQSHORT_AFTER,	16#02).
-define(GSM48_IE_MUL_RATE_CFG,	16#03).	% 10.5.2.21aa
-define(GSM48_IE_FREQ_L_AFTER,	16#05).
-define(GSM48_IE_MSLOT_DESC,	16#10).
-define(GSM48_IE_CHANMODE_2,	16#11).
-define(GSM48_IE_FRQSHORT_BEFORE,	16#12).
-define(GSM48_IE_FRQSHORT_BEFOR,	16#12).
-define(GSM48_IE_CHANMODE_3,	16#13).
-define(GSM48_IE_CHANMODE_4,	16#14).
-define(GSM48_IE_CHANMODE_5,	16#15).
-define(GSM48_IE_CHANMODE_6,	16#16).
-define(GSM48_IE_CHANMODE_7,	16#17).
-define(GSM48_IE_CHANMODE_8,	16#18).
-define(GSM48_IE_CHANDESC_2,	16#64).
-define(GSM48_IE_FREQ_L_BEFORE,	16#19).
-define(GSM48_IE_FRQLIST_BEFORE,	16#19).
-define(GSM48_IE_CH_DESC_1_BEFORE,	16#1c).
-define(GSM48_IE_CHDES_1_BEFORE,	16#1c).
-define(GSM48_IE_CH_DESC_2_BEFORE,	16#1d).
-define(GSM48_IE_CHDES_2_BEFORE,	16#1d).
-define(GSM48_IE_F_CH_SEQ_BEFORE,	16#1e).
-define(GSM48_IE_FRQSEQ_BEFORE,	16#1e).
-define(GSM48_IE_CLASSMARK3,	16#20).
-define(GSM48_IE_MA_BEFORE,	16#21).
-define(GSM48_IE_RR_PACKET_UL,	16#22).
-define(GSM48_IE_RR_PACKET_DL,	16#23).
-define(GSM48_IE_CELL_CH_DESC,	16#62).
-define(GSM48_IE_CHANMODE_1,	16#63).
-define(GSM48_IE_CHDES_2_AFTER,	16#64).
-define(GSM48_IE_MODE_SEC_CH,	16#66).
-define(GSM48_IE_F_CH_SEQ_AFTER,	16#69).
-define(GSM48_IE_MA_AFTER,	16#72).
-define(GSM48_IE_BA_RANGE,	16#73).
-define(GSM48_IE_GROUP_CHDES,	16#74).
-define(GSM48_IE_BA_LIST_PREF,	16#75).
-define(GSM48_IE_MOB_OVSERV_DIF,	16#77).
-define(GSM48_IE_REALTIME_DIFF,	16#7b).
-define(GSM48_IE_START_TIME,	16#7c).
-define(GSM48_IE_TIMING_ADVANCE,	16#7d).
-define(GSM48_IE_GROUP_CIP_SEQ,	16#80).
-define(GSM48_IE_CIP_MODE_SET,	16#90).
-define(GSM48_IE_GPRS_RESUMPT,	16#c0).
-define(GSM48_IE_SYNC_IND,	16#d0).
% System Information 4 (types are equal IEs above)
-define(GSM48_IE_CBCH_CHAN_DESC,	16#64).
-define(GSM48_IE_CBCH_MOB_AL,	16#72).

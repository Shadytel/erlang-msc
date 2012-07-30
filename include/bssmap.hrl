% assignment
-define(BSSMAP_ASSIGN_REQ,	2#00000001).
-define(BSSMAP_ASSIGN_COMPL,	2#00000010).
-define(BSSMAP_ASSIGN_FAIL,	2#00000011).

% handover
-define(BSSMAP_HAND_REQUE,	2#00010000).
-define(BSSMAP_HAND_REQUI,	2#00010001).
-define(BSSMAP_HAND_REQ_ACK,	2#00010010).
-define(BSSMAP_HAND_CMD,	2#00010011).
-define(BSSMAP_HAND_COMPL,	2#00010100).
-define(BSSMAP_HAND_SUCCEED,	2#00010101).
-define(BSSMAP_HAND_FAIL,	2#00010110).
-define(BSSMAP_HAND_PERF,	2#00010111).
-define(BSSMAP_HAND_CAND_ENQ,	2#00011000).
-define(BSSMAP_HAND_CAND_RSP,	2#00011001).
-define(BSSMAP_HAND_REQ_REJ,	2#00011010).
-define(BSSMAP_HAND_DETECT,	2#00011011).

% release
-define(BSSMAP_CLR_CMD,		2#00100000).
-define(BSSMAP_CLR_COMPL,	2#00100001).
-define(BSSMAP_CLR_REQ,		2#00100010).
-define(BSSMAP_SAPI_REJ,	2#00100101).
-define(BSSMAP_CONFUSION,	2#00100110).

% other connection stuff
-define(BSSMAP_SUSPEND,		2#00101000).
-define(BSSMAP_RESUME,		2#00101001).

% general messages
-define(BSSMAP_RESET,		2#00110000).
-define(BSSMAP_RESET_ACK,	2#00110001).
-define(BSSMAP_OVERLOAD,	2#00110010).
-define(BSSMAP_RESET_CKT,	2#00110100).
-define(BSSMAP_RESET_CKT_ACK,	2#00110101).
-define(BSSMAP_MSC_INV_TRACE,	2#00110110).
-define(BSSMAP_BSS_INV_TRACE,	2#00110111).

% terrestrial resources
-define(BSSMAP_BLOCK,		2#01000000).
-define(BSSMAP_BLOCK_ACK,	2#01000001).
-define(BSSMAP_UNBLOCK,		2#01000010).
-define(BSSMAP_UNBLOCK_ACK,	2#01000011).
-define(BSSMAP_CGRP_BLOCK,	2#01000101).
-define(BSSMAP_CGRP_BLOCK_ACK,	2#01000101).
-define(BSSMAP_CGRP_UNBLOCK,	2#01000110).
-define(BSSMAP_CGRP_UNBLOCK_ACK,	2#01000111).
-define(BSSMAP_UNEQUIPPED_CKT,	2#01001000).
-define(BSSMAP_CHANGE_CKT,	2#01001110).
-define(BSSMAP_CHANGE_CKT_ACK,	2#01001111).

% radio resources
-define(BSSMAP_RSRC_REQ,	2#01010000).
-define(BSSMAP_RSRC_IND,	2#01010001).
-define(BSSMAP_PAGING,		2#01010010).
-define(BSSMAP_CIPHER_CMD,	2#01010011).
-define(BSSMAP_CLASSMARK_UPD,	2#01010100).
-define(BSSMAP_CIPHER_COMPL,	2#01010101).
-define(BSSMAP_QUEUEING_IND,	2#01010110).
-define(BSSMAP_COMPL_L3_INF,	2#01010111).
-define(BSSMAP_CLASSMARK_REQ,	2#01011000).
-define(BSSMAP_CIPHER_MODE_REJ,	2#01011001).
-define(BSSMAP_LOAD_IND,	2#01011010).

% voice group / voice broadcast
-define(BSSMAP_VBS_SETUP,	2#00000100).
-define(BSSMAP_VBS_SETUP_ACK,	2#00000101).
-define(BSSMAP_VBS_SETUP_REF,	2#00000110).
-define(BSSMAP_VBS_ASSN_REQ,	2#00000111).
-define(BSSMAP_VBS_ASSN_RSLT,	2#00011100).
-define(BSSMAP_VBS_ASSN_FAIL,	2#00011101).
-define(BSSMAP_VBS_QUEUE_IND,	2#00011110).
-define(BSSMAP_UPLINK_REQ,	2#00011111).
-define(BSSMAP_UPLINK_REQ_ACK,	2#00100111).
-define(BSSMAP_UPLINK_REQ_CONF,	2#01001001).
-define(BSSMAP_UPLINK_REL_IND,	2#01001010).
-define(BSSMAP_UPLINK_REJ_CMD,	2#01001011).
-define(BSSMAP_UPLINK_REL_CMD,	2#01001100).
-define(BSSMAP_UPLINK_SIEZE_CMD,	2#01001101).

% element type identifiers
-define(ELEM_CKT_ID,		2#00000001).
-define(ELEM_RSRC_AVAIL,	2#00000011).
-define(ELEM_CAUSE,		2#00000100).
-define(ELEM_CELL_ID,		2#00000101).
-define(ELEM_PRIORITY,		2#00000110).
-define(ELEM_L3_HEAD,		2#00000111).	%% which is info?
-define(ELEM_IMSI,		2#00001000).
-define(ELEM_TMSI,		2#00001001).
-define(ELEM_CRYPTO_INFO,	2#00001010).
-define(ELEM_CHAN_TYPE,		2#00001011).
-define(ELEM_PERIODICITY,	2#00001100).
-define(ELEM_EXT_RSRC_IND,	2#00001101).
-define(ELEM_MS_COUNT,		2#00001110).
-define(ELEM_CLASSMARK_IND_2,	2#00010010).
-define(ELEM_CLASSMARK_IND_3,	2#00010011).
-define(ELEM_INTERFER_BAND,	2#00010100).
-define(ELEM_RR_CAUSE,		2#00010101).
-define(ELEM_L3_MESSAGE,	2#00010111).
-define(ELEM_DLCI,		2#00011000).
-define(ELEM_DOWNLINK_DTX,	2#00011001).
-define(ELEM_CELL_ID_LIST,	2#00011010).
-define(ELEM_RSP_REQ,		2#00011011).
-define(ELEM_RSRC_IND_METH,	2#00011100).
-define(ELEM_CLASSMARK_IND_1,	2#00011101).
-define(ELEM_CIC_LIST,		2#00011110).
-define(ELEM_DIAG,		2#00011111).
-define(ELEM_L3_BODY,		2#00100000).
-define(ELEM_CHOSEN_CHAN,	2#00100001).
-define(ELEM_TOTAL_AVAIL,	2#00100010).
-define(ELEM_CIPHER_RSP_MODE,	2#00100011).
-define(ELEM_CHAN_NEEDED,	2#00100100).
-define(ELEM_TRACE_TYPE,	2#00100101).
-define(ELEM_TRIGGER_ID,	2#00100110).
-define(ELEM_TRACE_REF,		2#00100111).
-define(ELEM_TRANS_ID,		2#00101000).
-define(ELEM_MOBILE_ID,		2#00101001).
-define(ELEM_OMC_ID,		2#00101010).
-define(ELEM_FORWARD_IND,	2#00101011).
-define(ELEM_CHOSEN_CRYPTO,	2#00101100).
-define(ELEM_CKT_POOL,		2#00101101).
-define(ELEM_CKT_POOL_LIST,	2#00101110).
-define(ELEM_TIME_IND,		2#00101111).
-define(ELEM_RSRC_SITUATION,	2#00110000).
-define(ELEM_CURRENT_CHAN,	2#00110001).
-define(ELEM_QUEUE_IND,		2#00110010).
-define(ELEM_SPEECH_VERSION,	2#01000000).
-define(ELEM_ASSIGN_REQMT,	2#00110011).
-define(ELEM_TALKER_FLAG,	2#00110101).
-define(ELEM_CONN_REL_REQTED,	2#00110110).
-define(ELEM_GRP_CALL_REF,	2#00110111).
-define(ELEM_EMLPP_PRIO,	2#00111000).
-define(ELEM_CONF_EVOL_IND,	2#00111001).

-record(cell_id, { mcc, mnc, lac, cid }).
-record(circuit_id, { pcm, timeslot }).
-record(cause, { class, value }).
-record(imsi, { number }).
-record(tmsi, { number }).
-record(ms_count, { number }).
-record(l3, { protocol, transaction, body }).
-record(crypto_info, { a50, a51, a52, a53, a54, a55, a56, a57, key }).
-record(chan_type, { mode, ratetype, versions }).
-record(period, { period }).
-record(ext_rsrc, { tarr, mode }).
-record(rsrcs_available, { halfrate, fullrate }).
-record(priority, { can_preempt, priority, allow_queueing, vulnerable }).
-record(classmark1, { classmark }).
-record(classmark2, { classmark }).
-record(classmark3, { classmark }).
-record(interference, { band_mask }).
-record(dlci, { dlci }).
-record(dtx, { dtx }).
-record(rsrc_ind_method, { method }).
-record(cic_list, { circuits }).
-record(diag, { index, bit, message }).
-record(chan_kind, { mode, count }).
-record(cipher_rsp_mode, { mode }).
-record(chan_needed, { type }).
-record(trace_type, { type }).
-record(trigger_id, { id }).
-record(trace_ref, { ref }).
-record(trans_id, { id }).
-record(ms_id, { imei, imeisv }).
-record(omc_id, { id }).
-record(forward, { ind }).
-record(crypto_chosen, { a50, a51, a52, a53, a54, a55, a56, a57, key }).
-record(circuit_pool, { pool }).
-record(time_ind, { seconds }).
-record(queue_ind, { advice }).
-record(speech_type, { type }).
-record(assignment_require, { mode }).
-record(group_ref, { ref }).
-record(evol_ind, { smi_count }).

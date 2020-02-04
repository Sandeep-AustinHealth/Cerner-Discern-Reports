/*************************************************************************
        Source file name:   hei_astn_ed_ext_assign.prg
        Program purpose:    Request / Reply - Extracts the Austin ED tracking assign File.
 
**************************************************************************
                      GENERATED MODIFICATION CONTROL LOG                 *
**************************************************************************
 Mod Date     Engineer          Comment
 --- -------- ------------ --- -------------------------------------------
 001         Sandeep            Modification to Anthony's report
*************************************************************************/
 
drop program ahs_intr_deter_pt_rpt:dba go
create program ahs_intr_deter_pt_rpt:dba
 
prompt
	"Output to File/Printer/MINE" = "MINE"            ;* Enter or select the printer or file name to send this report to.
	, "Facility" = "    9568344.00"
	, "Report Range Begin Date" = "SYSDATE"
	, "Report Range End Date" = "SYSDATE"
	, "Current Patients Only" = "1"                   ;* Non-discharged pts. Ignores date qualification
	, "Nurse Unit Usage" = "A"
	, "Nurse Usage" = ""
	, "Encounter Type(s)" = VALUE("*             ")
	, "Clinical Unit(s)" = VALUE("*")
	, "Deceased Only Ind" = "0"
	, "Output Type" = "S"
 
with OUTDEV, FACILITY, BEG_DT, END_DT, CURRENT, NURSE_UNIT_USAGE, NURSE_UNIT,
	ENC_TYPE, MED_SERVICE, DECEASE_IND, OUTPUT_TYPE
 
declare iCurrentPt = i2 with constant(cnvtint($CURRENT)), protect
if (iCurrentPt = 0)
    Set aCurInPt = "e.disch_dt_tm >= cnvtdatetime(begDtTm) or e.disch_dt_tm = NULL"
                   ; e.reg_dt_tm between cnvtdatetime(RepBegDate) and cnvtdatetime(RepEndDate)"
elseif(iCurrentPt = 1)
    Set aCurInPt = "e.disch_dt_tm = NULL"
endif
 
 
declare dFacilityCd = f8 with constant(cnvtreal($FACILITY)), protect
declare begDtTm = dq8 with constant(cnvtdatetime($BEG_DT)), protect
declare endDtTm = dq8 with constant(cnvtdatetime($END_DT)), protect
;sandeep declare endDtTm = dq8 with constant(cnvtdatetime(cnvtdate2($END_DT,"DD-MMM-YYYY"),235959)), protect
declare sNurseUnitUsage = c1 with constant($NURSE_UNIT_USAGE), protect
declare iDeceasedInd = i2 with constant(cnvtint($DECEASE_IND)), protect
declare sOutputType = c1 with constant($OUTPUT_TYPE), protect
 
declare iCurrentPt = i2 with constant(cnvtint($CURRENT)), protect ;001
 
declare NURSE_UNIT_USAGE_ALL = c1 with constant("A"), protect
declare NURSE_UNIT_USAGE_INCLUDE = c1 with constant("I"), protect
declare NURSE_UNIT_USAGE_EXCLUDE = c1 with constant("E"), protect
 
declare OUTPUT_TYPE_SUMMARY = c1 with constant("S"), protect
declare OUTPUT_TYPE_DETAIL = c1 with constant("D"), protect
 
declare sEncNrsUnitFlex = vc with noconstant("1=1"), protect
declare sEncDmnNrsUnitFlex = vc with noconstant("1=1"), protect
declare sEncTypeFlex = vc with noconstant("1=1"), protect
declare sMedSrvFlex = vc with noconstant("1=1"), protect
declare sEncTypeELHFlex = vc with noconstant("1=1"), protect
declare sMedSrvELHFlex = vc with noconstant("1=1"), protect
declare sDeceasedFlex = vc with noconstant("1=1"), protect
 
declare dDeceasedYesCd = f8 with constant(uar_get_code_by("MEANING",268,"YES")), protect
declare dDPRConfigCd = f8 with constant(uar_get_code_by("DISPLAYKEY",100013,"DETERIORATINGPATIENTRPTEVENTS")), protect
declare dDPRConfigActionCd = f8 with constant(uar_get_code_by("DISPLAYKEY",100013,"DETERIORATINGPATIENTRPTACTIONS")), protect
declare dDPRConfigThresholdCd = f8
 with constant(uar_get_code_by("DISPLAYKEY",100013,"DETERIORATINGPATIENTRPTEVTTHRESHOLD")), protect
declare dDPRConfigRelapseCd = f8
 with constant(uar_get_code_by("DISPLAYKEY",100013,"DETERIORATINGPATIENTRPTEVTRELAPSE")), protect
declare dEncAliasMRNCd = f8 with constant(uar_get_code_by("MEANING",319,"MRN")), protect
declare dEncAliasFINCd = f8 with constant(uar_get_code_by("MEANING",319,"FIN NBR")), protect
 
 
declare GRAPH_RANGE_YELLOW = i2 with constant(2), protect
declare GRAPH_RANGE_RED = i2 with constant(3), protect
 
declare SUSPEND_ALTERED_SUSPENDED = i2 with constant(1), protect
declare SUSPEND_ALTERED_ALTERED = i2 with constant(2), protect
 
declare CRITERIA_CLINICAL_EVENT = i2 with constant(1), protect
declare CRITERIA_NO_CHANGE = i2 with constant(2), protect
declare CRITERIA_ALTERED = i2 with constant(3), protect
declare CRITERIA_RESET = i2 with constant(4), protect
 
declare sTmpGraphName = vc with noconstant(""), protect
declare sIdx = i4 with noconstant(0), protect
declare sLookAhead = vc with noconstant(""), protect
declare sTmpVitals = vc with noconstant(""), protect
declare sTmpActions = vc with noconstant(""), protect
 
declare sOutstring = vc with noconstant(""), protect
declare sDEL = c1 with constant(","), protect
declare sQTE = c1 with constant('"'), protect
declare CRLF = c2 with constant(concat(char(13),char(10))), protect
declare DEL2 = c1 with constant("|"), protect
declare DEL3 = c1 with constant("^"), protect
 
declare cIdx = i4 with noconstant(0), protect
declare tmpArriveDtTm = dq8 with protect
declare tmpDepartDtTm = dq8 with protect
 
declare HOURS_EXPIRE_THRESHOLD = i4 with constant(24), protect
 
 
declare iCurrentPt = i2 with constant(cnvtint($CURRENT)), protect
if (iCurrentPt = 0)
    Set aCurInPt = "e.disch_dt_tm >= cnvtdatetime(begDtTm) or e.disch_dt_tm = NULL"
                   ; e.reg_dt_tm between cnvtdatetime(RepBegDate) and cnvtdatetime(RepEndDate)"
elseif(iCurrentPt = 1)
    Set aCurInPt = "e.disch_dt_tm = NULL"
endif
 
 
free record enc
record enc(
  1 cnt = i4
  1 lst[*]
    2 encntr_id = f8
    2 person_id = f8
    2 pat_name = vc ;001
    2 reg_dt_tm = dq8
    2 disch_dt_tm = dq8
    2 dob = dq8
    2 encntr_type_cd = f8
    2 deceased_ind = i2
)
 
 
free record nrs_unit_list
record nrs_unit_list(
   1 arg_cnt = i4
   1 arg[*]
     2 double_val = f8
     2 inc_val = i4
     2 string_val = vc
) with protect
 
 
free record enc_type_list
record enc_type_list(
   1 arg_cnt = i4
   1 arg[*]
     2 double_val = f8
     2 inc_val = i4
     2 string_val = vc
) with protect
 
 
free record med_srv_list
record med_srv_list(
   1 arg_cnt = i4
   1 arg[*]
     2 double_val = f8
     2 inc_val = i4
     2 string_val = vc
) with protect
 
 
free record mp_range
record mp_range(
  1 param_cnt = i4
  1 param[*]
    2 param = vc
    2 rge_cnt = i4
    2 rge[*]
      3 age_range = i4
      3 nurse_unit_cd = f8
      3 specialty_cd = f8
      3 low_red_min = f8
      3 low_red_max = f8
      3 low_yellow_min = f8
      3 low_yellow_max = f8
      3 normal_low = f8
      3 normal_high = f8
      3 high_yellow_min = f8
      3 high_yellow_max = f8
      3 high_red_min = f8
      3 high_red_max = f8
)
 
 
free record evts
record evts(
  1 evt_cnt = i4
  1 evt[*]
    2 event_cd = f8
    2 graph_name = vc
  1 nom_cnt = i4
  1 nom[*]
    2 graph_name = vc
    2 nom_evt_cnt = i4
    2 nom_evt[*]
      3 event_cd = f8
    2 value_cnt = i4
    2 value[*]
      3 result_val = vc
      3 numeric_val = i4
  1 nbr_cnt = i4
  1 nbr[*]
    2 graph_name = vc
    2 nbr_evt_cnt = i4
    2 nbr_evt[*]
      3 event_cd = f8
)
 
 
free record cfg
record cfg(
  1 act_evt_cnt = i4
  1 act_evt[*]
    2 event_cd = f8
  1 evt_threshold_min = i4
  1 evt_relapse_hrs = i4
)
 
 
free record reply_obj
record reply_obj(
  1 qual_cnt = i4
  1 objArray[*]
    2 encntr_id = f8
    2 person_id = f8
    2 pat_name = vc ;001
    2 nurse_unit_disp = vc
    2 nurse_unit_cd = f8
    2 bed_disp = vc ;001
    2 bed_cd = f8   ;001
    2 in_dt_tm = dq8
    2 out_dt_tm = dq8
    2 in_qualifier_dt_tm = dq8
    2 out_qualifier_dt_tm = dq8
    2 reg_dt_tm = dq8
    2 disch_dt_tm = dq8
    2 birth_dt_tm = dq8
    2 med_srv_cd = f8
    2 med_srv_disp = vc
    2 mrn = vc
    2 fin = vc
    2 encntr_type_disp = vc
    2 ucr_event_cnt = i4
    2 met_event_cnt = i4
    2 ucr_action_cnt = i4
    2 met_action_cnt = i4
    2 ucr_dates = vc
    2 met_dates = vc
    2 ucr_vitals = vc
    2 met_vitals = vc
    2 ucr_actions = vc
    2 met_actions = vc
    2 acc_expire_cnt = i2
    2 disch_acc_ind = i2
    2 deceased_ind = i2
    2 max_review_dt_tm = dq8
    2 met_within_ucr_threshold_cnt = i2
    2 evt_cnt = i4
    2 evt[*]
      3 event_type = i2 ; 1 = UCR, 2 = MET
      3 yellow_cnt = i2
      3 red_cnt = i2
      3 event_dt_tm = dq8
      3 complete_dt_tm = dq8
      3 complete_thresh_dt_tm = dq8
      3 complete_ind = i2 ; set if this was complete other than the threshold.
      3 altered_ind = i2 ; actioned.
      3 suspended_ind = i2
      3 count_ind = i2 ; True if we need to count this one
      3 vit_cnt = i4
      3 vit[*]
        4 vital_evt = vc
        4 vital_value = vc
        4 normal_return_ind = i2
        4 altered_ind = i2
        4 suspended_ind = i2
      3 act_cnt = i4
      3 act[*]
        4 act_evt = vc
        4 act_value = vc
        4 act_dt_tm = dq8
        ;4 expire_dt_tm = dq8
    2 crit_cnt = i4
    2 crit[*]
      3 type_flag = i2
      3 crit_dt_tm = dq8
)
 
 
 
free record lyt
record lyt(
  1 facility = vc
  1 date_range_string = vc
  1 nurse_unit_string = vc
  1 clinical_unit_string = vc
  1 encntr_type_string = vc
  1 ucr_threshold = i2
  1 deceased_string = vc
  1 executed_by = vc
  1 unit_cnt = i4
  1 unit[*]
    2 nurse_unit_cd = f8
    2 nurse_unit_disp = vc
    2 tot_pt_ward = i4
    2 tot_pt_ucr = i4
    2 tot_ucr_evt = i4
    2 tot_ucr_act = i4
    2 ucr_action_rate_str = vc
    2 met_multi_evt = i4
    2 tot_pt_met = i4
    2 tot_met_evt = i4
    2 tot_met_act = i4
    2 met_action_rate_str = vc
    2 tot_met_evt_threshold = i4
    2 tot_met_evt_threshold_str = vc
    2 tot_acc_expired = i4
  1 pt_ward = i4
  1 pt_ucr = i4
  1 ucr_evt = i4
  1 ucr_act = i4
  1 final_ucr_action_rate_str = vc
  1 final_met_multi_evt = i4
  1 pt_met = i4
  1 met_evt = i4
  1 met_act = i4
  1 final_met_action_rate_str = vc
  1 met_evt_threshold = i4
  1 met_evt_threshold_str = vc
  1 acc_expired = i4
)
 
 
; **** Subs ***
declare getAgeRange(dobDtTm=dq8,vitalDtTm=dq8) = i2
subroutine getAgeRange(dobDtTm,vitalDtTm)
  declare years = f8 with protect
  declare months = f8 with protect
 
  set years = datetimediff(vitalDtTm,dobDtTm,10)
  if(years >= 16.0)
    return(5)
  elseif(years < 16.0 and years >= 12.0)
    return(4)
  elseif(years < 12.0 and years >= 5.0)
    return(3)
  elseif(years < 5.0 and years >= 1.0)
    return(2)
  else
    set months = datetimediff(vitalDtTm,dobDtTm,11)
    if(months < 12.0 and months >= 3.0)
      return(1)
    else
      return(0)
    endif
  endif
end
 
 
declare findEventGraph(evtCd=f8) = vc
subroutine findEventGraph(evtCd)
  declare evtIdx = i4 with protect
  declare evtPos = i4 with protect
 
  set evtPos = locateval(evtIdx,1,evts->evt_cnt,evtCd,evts->evt[evtIdx].event_cd)
  if(evtPos>0)
    return(evts->evt[evtPos].graph_name)
  else
    return ("UNKNOWN")
  endif
end
 
 
declare vitalNormal(_evtCd=f8,rVal=vc,ageRange=i2,nrsUnitCd=f8,specCd=f8)=i2
subroutine vitalNormal(_evtCd,rVal,ageRange,nrsUnitCd,specCd)
  declare rValFloat = f8 with protect
  declare rVitalGraph = vc with protect
  declare paramIdx = i4 with protect
  declare paramPos = i4 with protect
  declare rgePos = i4 with protect
 
  set rVitalGraph = findEventGraph(_evtCd)
  set rValFloat = makeResultFloat(rVitalGraph,rVal)
  set paramPos = locateval(paramIdx,1,mp_range->param_cnt,rVitalGraph,mp_range->param[paramIdx].param)
 
;  call echo("Here!")
;  call echo(rVitalGraph)
 
  if(paramPos<=0)
    return(-1)
  endif
  for(vnCnt=1 to mp_range->param[paramPos].rge_cnt)
    ; they are sorted in code assending order, so we can just go through them and take the last one
    if(mp_range->param[paramPos].rge[vnCnt].age_range = ageRange)
      if(mp_range->param[paramPos].rge[vnCnt].nurse_unit_cd = 0.0)
        if(mp_range->param[paramPos].rge[vnCnt].specialty_cd = 0.0)
          set rgePos = vnCnt
        elseif(mp_range->param[paramPos].rge[vnCnt].specialty_cd = specCd)
          set rgePos = vnCnt
        endif
      elseif(mp_range->param[paramPos].rge[vnCnt].nurse_unit_cd = nrsUnitCd)
        if(mp_range->param[paramPos].rge[vnCnt].specialty_cd = 0.0)
          set rgePos = vnCnt
        elseif(mp_range->param[paramPos].rge[vnCnt].specialty_cd = specCd)
          set rgePos = vnCnt
        endif
      endif
    endif
  endfor
 
  if(rgePos<=0)
    return(-2)
  endif
  if(rValFloat >= mp_range->param[paramPos].rge[rgePos].normal_low
   and rValFloat <= mp_range->param[paramPos].rge[rgePos].normal_high)
    return(TRUE)
  else
    return(FALSE)
  endif
end
 
 
declare makeResultFloat(rGraph=vc,rResultString=vc)=f8
subroutine makeResultFloat(rGraph,rResultString)
  declare nomIdx = i4 with protect
  declare nomPos = i4 with protect
  declare valIdx = i4 with protect
  declare valPos = i4 with protect
  declare dReturnResult = f8 with protect
 
  set nomPos = locateval(nomIdx,1,evts->nom_cnt,rGraph,evts->nom[nomIdx].graph_name)
  if(nomPos>0)
    set valPos = locateval(valIdx,1,evts->nom[nomPos].value_cnt,cnvtupper(rResultString),evts->nom[nomPos].value[valIdx].result_val)
    if(valPos>0)
      set dReturnResult = cnvtreal(evts->nom[nomPos].value[valPos].result_val)
    else
      set dReturnResult = -1.0
    endif
  else
    set dReturnResult = cnvtreal(rResultString)
  endif
  return(dReturnResult)
end
 
 
; Format outputs
declare formatPercentString(numerator=i4,denominator=i4)=vc with public
subroutine formatPercentString(numerator,denominator)
  declare sPercString = vc with noconstant(""), protect
  declare dPerc = f8 with noconstant(0.0), protect
 
  set dPerc = ((cnvtreal(numerator) / cnvtreal(denominator)) * 100)
  set sPercString = concat(trim(cnvtstring(dPerc)),"% - (",trim(cnvtstring(numerator)),")")
  return(sPercString)
end
 
 
execute ccl_prompt_api_dataset "all"
call parsecommandline(null)
execute hei_sub_drv_common
 
 
call parse_args(7, nrs_unit_list, "F8")
call parse_args(8, enc_type_list, "F8")
call parse_args(9, med_srv_list, "F8")
 
if(sNurseUnitUsage = NURSE_UNIT_USAGE_INCLUDE)
  set sEncDmnNrsUnitFlex = buildFilters(nrs_unit_list, "ed.loc_nurse_unit_cd IN (", TRUE, "F8")
elseif(sNurseUnitUsage = NURSE_UNIT_USAGE_EXCLUDE)
  set sEncDmnNrsUnitFlex = buildFilters(nrs_unit_list, "ed.loc_nurse_unit_cd NOT IN (", TRUE, "F8")
endif
 
set sEncTypeFlex = buildFilters(enc_type_list, "e.encntr_type_cd IN (", TRUE, "F8")
set sMedSrvFlex = buildFilters(med_srv_list, "e.med_service_cd IN (", TRUE, "F8")
set sEncTypeELHFlex = buildFilters(enc_type_list, "elh.encntr_type_cd IN (", TRUE, "F8")
set sMedSrvELHFlex = buildFilters(med_srv_list, "elh.med_service_cd IN (", TRUE, "F8")
if(iDeceasedInd = TRUE)
  set sDeceasedFlex = concat("p.deceased_cd = ",trim(cnvtstring(dDeceasedYesCd)),".00")
endif
 
 
; Load vital event config
select into "nl:"
     uar_get_code_display(cvg3.child_code_value)
     , cvg3.child_code_value
     , uar_get_code_display(cvg4.child_code_value)
     , cve2.field_value
     , cve4a.field_value
     , cve4b.field_value
from code_value_group cvg1 ; config item
   , code_value_group cvg2 ; event groupers
   , code_value_group cvg3 ; event codes
   , code_value_group cvg4 ; Nomen values
   , code_value_extension cve2 ; event graph name
   , code_value_extension cve4a ; Vital Result Value
   , code_value_extension cve4b ; Vital numeric Value
plan cvg1
  where cvg1.parent_code_value = dDPRConfigCd
join cvg2
  where cvg2.parent_code_value = cvg1.child_code_value
    and cvg2.code_set = 100013
join cve2
  where cve2.code_value = cvg2.child_code_value
    and cve2.field_name = "CONFIG_ITEM"
join cvg3
  where cvg3.parent_code_value = cvg2.child_code_value
    and cvg3.code_set = 72
join cvg4
  where cvg4.parent_code_value = outerjoin(cvg2.child_code_value)
    and cvg4.code_set = outerjoin(100013)
join cve4a
  where cve4a.code_value = outerjoin(cvg4.child_code_value)
    and cve4a.field_name = outerjoin("CONFIG_ITEM")
join cve4b
  where cve4b.code_value = outerjoin(cvg4.child_code_value)
    and cve4b.field_name = outerjoin("VALUE")
order by cve2.field_value ;
       , cvg3.child_code_value
       , cvg4.child_code_value
       , cve4a.field_value
       , cve4b.field_value
head report
  child_evt_cnt = 0
  child_idx = 0
  chile_pos = 0
head cve2.field_value
  if(cvg4.parent_code_value > 0.0)
    evts->nom_cnt = evts->nom_cnt+1
    stat = alterlist(evts->nom,evts->nom_cnt)
    evts->nom[evts->nom_cnt].graph_name = cve2.field_value
  else
    evts->nbr_cnt = evts->nbr_cnt+1
    stat = alterlist(evts->nbr,evts->nbr_cnt)
    evts->nbr[evts->nbr_cnt].graph_name = cve2.field_value
  endif
head cvg3.child_code_value
  evts->evt_cnt = evts->evt_cnt+1
  stat = alterlist(evts->evt,evts->evt_cnt)
  evts->evt[evts->evt_cnt].event_cd = cvg3.child_code_value
  evts->evt[evts->evt_cnt].graph_name = cve2.field_value
  if(cvg4.parent_code_value > 0.0)
    child_evt_cnt = evts->nom[evts->nom_cnt].nom_evt_cnt+1
    stat = alterlist(evts->nom[evts->nom_cnt].nom_evt,child_evt_cnt)
    evts->nom[evts->nom_cnt].nom_evt[child_evt_cnt].event_cd = cvg3.child_code_value
    evts->nom[evts->nom_cnt].nom_evt_cnt = child_evt_cnt
  else
    child_evt_cnt = evts->nbr[evts->nbr_cnt].nbr_evt_cnt+1
    stat = alterlist(evts->nbr[evts->nbr_cnt].nbr_evt,child_evt_cnt)
    evts->nbr[evts->nbr_cnt].nbr_evt[child_evt_cnt].event_cd = cvg3.child_code_value
    evts->nbr[evts->nbr_cnt].nbr_evt_cnt = child_evt_cnt
  endif
head cvg4.child_code_value
  if(cvg4.parent_code_value > 0.0)
    child_pos = locateval(child_idx,1,evts->nom[evts->nom_cnt].value_cnt
                 ,cve4a.field_value,evts->nom[evts->nom_cnt].value[child_idx].result_val
                 ,cnvtint(cve4b.field_value),evts->nom[evts->nom_cnt].value[child_idx].numeric_val)
    if(child_pos<=0)
      child_evt_cnt = evts->nom[evts->nom_cnt].value_cnt+1
      stat = alterlist(evts->nom[evts->nom_cnt].value,child_evt_cnt)
      evts->nom[evts->nom_cnt].value[child_evt_cnt].result_val = cve4a.field_value
      evts->nom[evts->nom_cnt].value[child_evt_cnt].numeric_val = cnvtint(cve4b.field_value)
      evts->nom[evts->nom_cnt].value_cnt = child_evt_cnt
    endif
  endif
foot cvg4.child_code_value
  null
foot cvg3.child_code_value
  null
foot cve2.field_value
  null
with nocounter
 
 
; Load other report config
select into "nl:"
from code_value cv
   , code_value_extension cve
   , code_value_group cvg
plan cv
  where cv.code_value in (dDPRConfigActionCd, dDPRConfigThresholdCd, dDPRConfigRelapseCd)
join cve
  where cve.code_value = outerjoin(cv.code_value)
    and cve.field_name = outerjoin("CONFIG_ITEM")
join cvg
  where cvg.parent_code_value = outerjoin(cv.code_value)
    and cvg.code_set = outerjoin(72)
detail
  if(cve.code_value > 0.0)
    case(cv.code_value)
      of dDPRConfigThresholdCd:
        cfg->evt_threshold_min = cnvtint(cve.field_value)
      of dDPRConfigRelapseCd:
        cfg->evt_relapse_hrs = cnvtint(cve.field_value)
    endcase
  elseif(cvg.parent_code_value > 0.0)
    cfg->act_evt_cnt = cfg->act_evt_cnt+1
    stat = alterlist(cfg->act_evt,cfg->act_evt_cnt)
    cfg->act_evt[cfg->act_evt_cnt].event_cd = cvg.child_code_value
  endif
with nocounter
 
;call echorecord(evts)
;call echorecord(cfg)
;go to exit_script
 
; load ranges
select into "nl:"
from cust_mp_custom_ref_range rr
plan rr
  where rr.facility_cd in (dFacilityCd, 0.0)
    and rr.active_ind = 1
order by rr.graph_name
       , rr.facility_cd
       , rr.location_cd
       , rr.specialty_cd
       , rr.age_range
head report
  param_cnt = 0
  rge_cnt = 0
head rr.graph_name
  param_cnt = param_cnt+1
  if(mod(param_cnt,10)=1)
    stat = alterlist(mp_range->param,param_cnt+9)
  endif
  mp_range->param[param_cnt].param = rr.graph_name
  rge_cnt = 0
detail
  rge_cnt = rge_cnt+1
  if(mod(rge_cnt,5)=1)
    stat = alterlist(mp_range->param[param_cnt].rge,rge_cnt+4)
  endif
  mp_range->param[param_cnt].rge[rge_cnt].age_range = rr.age_range
  mp_range->param[param_cnt].rge[rge_cnt].nurse_unit_cd = rr.location_cd
  mp_range->param[param_cnt].rge[rge_cnt].specialty_cd = rr.specialty_cd
  mp_range->param[param_cnt].rge[rge_cnt].normal_low = rr.info_number1
  mp_range->param[param_cnt].rge[rge_cnt].normal_high = rr.info_number2
  mp_range->param[param_cnt].rge[rge_cnt].low_red_min = rr.severe_low_min
  mp_range->param[param_cnt].rge[rge_cnt].low_red_max = rr.severe_low_max
  mp_range->param[param_cnt].rge[rge_cnt].low_yellow_min = rr.moderate_low_min
  mp_range->param[param_cnt].rge[rge_cnt].low_yellow_max = rr.moderate_low_max
  if(rr.graph_name = "RESPDISTRESS")
    mp_range->param[param_cnt].rge[rge_cnt].low_red_min = rr.severe_high_min
    mp_range->param[param_cnt].rge[rge_cnt].low_red_max = rr.severe_high_max
    mp_range->param[param_cnt].rge[rge_cnt].low_yellow_min = rr.moderate_high_min
    mp_range->param[param_cnt].rge[rge_cnt].low_yellow_max = rr.moderate_high_max
  else
    mp_range->param[param_cnt].rge[rge_cnt].high_yellow_min = rr.moderate_high_min
    mp_range->param[param_cnt].rge[rge_cnt].high_yellow_max = rr.moderate_high_max
    mp_range->param[param_cnt].rge[rge_cnt].high_red_min = rr.severe_high_min
    mp_range->param[param_cnt].rge[rge_cnt].high_red_max = rr.severe_high_max
  endif
foot rr.graph_name
  mp_range->param[param_cnt].rge_cnt = rge_cnt
  stat = alterlist(mp_range->param[param_cnt].rge,rge_cnt)
foot report
  mp_range->param_cnt = param_cnt
  stat = alterlist(mp_range->param,param_cnt)
with nocounter
 
 
select into "nl:"
from encounter e
   , person p
plan e
  where e.reg_dt_tm <= cnvtdatetime(endDtTm)
    and PARSER(aCurInPt);(e.disch_dt_tm >= cnvtdatetime(begDtTm) or e.disch_dt_tm = NULL)
    ;and e.encntr_id = 36461073.00 ;34494634.00 ;TESTING
    ;and parser(sEncNrsUnitFlex)
    and exists (select 1 from encntr_loc_hist elh
                where elh.encntr_id = e.encntr_id
                  and parser(sEncNrsUnitFlex)
                  and elh.active_ind = 1)
    and parser(sEncTypeFlex)
    and parser(sMedSrvFlex)
    and e.loc_facility_cd = dFacilityCd
    and e.active_ind = 1
join p
  where p.person_id = e.person_id
    and parser(sDeceasedFlex)
head report
  add_flag = 0
detail
  add_flag = 0
  if(iDeceasedInd = FALSE)
    add_flag = 1
  else
    if(p.deceased_dt_tm between e.reg_dt_tm and e.disch_dt_tm)
      add_flag = 1
    endif
  endif
 
  if(add_flag = 1)
    enc->cnt = enc->cnt+1
    if(mod(enc->cnt,100)=1)
      stat = alterlist(enc->lst,enc->cnt+99)
    endif
    enc->lst[enc->cnt].encntr_id = e.encntr_id
    enc->lst[enc->cnt].reg_dt_tm = e.reg_dt_tm
    enc->lst[enc->cnt].disch_dt_tm = e.disch_dt_tm
    enc->lst[enc->cnt].encntr_type_cd = e.encntr_type_cd
    enc->lst[enc->cnt].dob = p.birth_dt_tm
    enc->lst[enc->cnt].person_id = e.person_id
    enc->lst[enc->cnt].pat_name = substring(1,30,p.name_full_formatted)  ;001
    if(p.deceased_dt_tm between e.reg_dt_tm and e.disch_dt_tm)
      enc->lst[enc->cnt].deceased_ind = TRUE
    endif
  endif
foot report
  stat = alterlist(enc->lst,enc->cnt)
with nocounter, time = 600
 
;call echorecord(mp_range)
;call echorecord(enc)
;select into $outdev
;from (dummyt d with seq=value(enc->cnt))
;  head report
;  col 0 sEncTypeFlex row+1
;  col 0 sEncTypeELHFlex row+1
;detail
;  col 0 call print(substring(1,40,uar_get_code_display(enc->lst[d.seq].encntr_type_cd))) row+1
;with nocounter
;go to exit_script
;set enc->cnt = 100 ;TESTING
;set stat = alterlist(enc->lst,enc->cnt) ;TESTING
 
call echo(build("sEncTypeELHFlex-->",sEncTypeELHFlex))
call echo(build("sMedSrvELHFlex-->",sMedSrvELHFlex))
call echo(build("sEncNrsUnitFlex-->",sEncNrsUnitFlex))
 
; Load main record
;call echo("Load main record")
select into "nl:"
from (dummyt d with seq=value(enc->cnt))
   , encntr_loc_hist elh
plan d
join elh
  where elh.encntr_id = enc->lst[d.seq].encntr_id
    and elh.beg_effective_dt_tm <= cnvtdatetime(endDtTm)
    and elh.end_effective_dt_tm >= cnvtdatetime(begDtTm)
    and elh.loc_nurse_unit_cd > 0.0
;    and parser(sEncTypeELHFlex)
;    and parser(sMedSrvELHFlex)
    ;and parser(sEncNrsUnitFlex)
    and elh.active_ind = 1
order by elh.encntr_id, elh.beg_effective_dt_tm
head report
  cnt = 0
  include_flag = FALSE
  loc_cd = 0.0
head elh.encntr_id
  loc_cd = -1.0
  tmpArriveDtTm = NULL
  tmpDepartDtTm = NULL
head elh.beg_effective_dt_tm
  null
detail
;  call echo("*********************")
;  call echo(build("elh.encntr_id-->",elh.encntr_id))
;  call echo(build("loc_cd-->",uar_get_code_display(loc_cd)))
;  call echo(build("elh.loc_nurse_unit_cd-->",uar_get_code_display(elh.loc_nurse_unit_cd)))
;  call echo("*********************")
  include_flag = FALSE
  if(sNurseUnitUsage = NURSE_UNIT_USAGE_ALL)
    include_flag = TRUE
  else
    cPos = locateval(cIdx,1,nrs_unit_list->arg_cnt,elh.loc_nurse_unit_cd,nrs_unit_list->arg[cIdx].double_val)
    if(sNurseUnitUsage = NURSE_UNIT_USAGE_INCLUDE)
      if(cPos>0)
        include_flag = TRUE
      endif
    elseif(sNurseUnitUsage = NURSE_UNIT_USAGE_EXCLUDE)
      if(cPos=0)
        include_flag = TRUE
      endif
    endif
  endif
  ;call echo(build("include_flag-->",include_flag))
  if(include_flag = TRUE)
    call echo("Include on Location")
;    call echo("*********************")
;    call echo(elh.encntr_id)
;    call echo(uar_get_code_display(loc_cd))
;    call echo(uar_get_code_display(elh.loc_nurse_unit_cd))
;    call echo("*********************")
    if((loc_cd != elh.loc_nurse_unit_cd)
     and ((cnvtdatetime(elh.beg_effective_dt_tm) <= cnvtdatetime(endDtTm)
           and cnvtdatetime(elh.beg_effective_dt_tm) >= cnvtdatetime(begDtTm))
     or (cnvtdatetime(elh.end_effective_dt_tm) <= cnvtdatetime(endDtTm)
           and cnvtdatetime(elh.end_effective_dt_tm) >= cnvtdatetime(begDtTm))
     or (cnvtdatetime(elh.beg_effective_dt_tm) <= cnvtdatetime(begDtTm)
           and cnvtdatetime(elh.end_effective_dt_tm) >= cnvtdatetime(endDtTm))
          ))
      call echo("Include on Date")
      cnt = cnt+1
      if(mod(cnt,100)=1)
        stat = alterlist(reply_obj->objArray,cnt+99)
      endif
      tmpArriveDtTm = NULL
      tmpDepartDtTm = NULL
      reply_obj->objArray[cnt].encntr_id = enc->lst[d.seq].encntr_id
      reply_obj->objArray[cnt].person_id = enc->lst[d.seq].person_id
      reply_obj->objArray[cnt].pat_name = enc->lst[d.seq].pat_name   ;001
      reply_obj->objArray[cnt].nurse_unit_disp = substring(1,30,uar_get_code_display(elh.loc_nurse_unit_cd))
      reply_obj->objArray[cnt].nurse_unit_cd = elh.loc_nurse_unit_cd
      ;001++
      reply_obj->objArray[cnt].bed_disp = uar_get_code_display(elh.loc_bed_cd)
      reply_obj->objArray[cnt].bed_cd = elh.loc_bed_cd
      ;001--
      reply_obj->objArray[cnt].reg_dt_tm = enc->lst[d.seq].reg_dt_tm
      reply_obj->objArray[cnt].disch_dt_tm = enc->lst[d.seq].disch_dt_tm
      reply_obj->objArray[cnt].birth_dt_tm = enc->lst[d.seq].dob
      reply_obj->objArray[cnt].encntr_type_disp = uar_get_code_display(enc->lst[d.seq].encntr_type_cd)
      reply_obj->objArray[cnt].med_srv_cd = elh.med_service_cd
      reply_obj->objArray[cnt].med_srv_disp = uar_get_code_display(elh.med_service_cd)
      reply_obj->objArray[cnt].deceased_ind = enc->lst[d.seq].deceased_ind
 
 
 
    endif
;    if(cnvtdatetime(tmpArriveDtTm) != cnvtdatetime(elh.arrive_dt_tm) and elh.arrive_dt_tm > cnvtdatetime("01-JAN-1900"))
;      tmpArriveDtTm = elh.arrive_dt_tm
;    endif
;    if(cnvtdatetime(tmpDepartDtTm) != cnvtdatetime(elh.depart_dt_tm) and elh.depart_dt_tm > cnvtdatetime("01-JAN-1900"))
;      tmpDepartDtTm = elh.depart_dt_tm
;    endif
;    if(tmpArriveDtTm = NULL or cnvtdatetime(elh.beg_effective_dt_tm) < tmpArriveDtTm)
;      tmpArriveDtTm = cnvtdatetime(elh.beg_effective_dt_tm)
;    endif
;    if(cnvtdatetime(elh.end_effective_dt_tm) > cnvtdatetime(curdate,curtime3))
;      tmpDepartDtTm = cnvtdatetime(enc->lst[d.seq].disch_dt_tm)
;    else
;      tmpDepartDtTm = cnvtdatetime(elh.end_effective_dt_tm)
;    endif
    if(reply_obj->objArray[cnt].in_dt_tm = NULL
     or cnvtdatetime(elh.beg_effective_dt_tm) < cnvtdatetime(reply_obj->objArray[cnt].in_dt_tm))
      reply_obj->objArray[cnt].in_dt_tm = cnvtdatetime(elh.beg_effective_dt_tm)
    endif
    if(cnvtdatetime(elh.beg_effective_dt_tm) < cnvtdatetime(curdate,curtime3))
      reply_obj->objArray[cnt].out_dt_tm = cnvtdatetime(elh.end_effective_dt_tm)
      ;sandeep++ put NULL if end dt tm is 2100
      if(cnvtdatetime(elh.end_effective_dt_tm) = cnvtdatetime("31-DEC-2100 00:00"))
           reply_obj->objArray[cnt].out_dt_tm = NULL
      endif
      ;sandeep--
    else
      reply_obj->objArray[cnt].out_dt_tm = NULL
    endif
;    call echo(build("cnt->",cnt))
;    call echo(build("begDtTm-->",format(begDtTm,"dd-mmm-yyyy hh:mm;;q")))
;    call echo(build("inDtTm-->",format(reply_obj->objArray[cnt].in_dt_tm,"dd-mmm-yyyy hh:mm;;q")))
    if(cnvtdatetime(reply_obj->objArray[cnt].in_dt_tm) < cnvtdatetime(begDtTm))
      reply_obj->objArray[cnt].in_qualifier_dt_tm = cnvtdatetime(begDtTm)
    else
      reply_obj->objArray[cnt].in_qualifier_dt_tm = reply_obj->objArray[cnt].in_dt_tm
    endif
    if(cnvtdatetime(reply_obj->objArray[cnt].out_dt_tm) > cnvtdatetime(endDtTm) or reply_obj->objArray[cnt].out_dt_tm = NULL)
      reply_obj->objArray[cnt].out_qualifier_dt_tm = cnvtdatetime(endDtTm)
    else
      reply_obj->objArray[cnt].out_qualifier_dt_tm = reply_obj->objArray[cnt].out_dt_tm
    endif
;    if(tmpArriveDtTm <= cnvtdatetime("01-JAN-1900"))
;      reply_obj->objArray[cnt].in_dt_tm = enc->lst[d.seq].reg_dt_tm
;    else
;      reply_obj->objArray[cnt].in_dt_tm = tmpArriveDtTm
;    endif
;    ;reply_obj->objArray[cnt].in_dt_tm = tmpArriveDtTm
;    if(tmpDepartDtTm = cnvtdatetime("01-JAN-1900"))
;      reply_obj->objArray[cnt].out_dt_tm = enc->lst[d.seq].disch_dt_tm
;    else
;      reply_obj->objArray[cnt].out_dt_tm = tmpDepartDtTm
;    endif
;
;    if(reply_obj->objArray[cnt].in_dt_tm < cnvtdatetime(begDtTm))
;      reply_obj->objArray[cnt].in_qualifier_dt_tm = cnvtdatetime(begDtTm)
;    else
;      reply_obj->objArray[cnt].in_qualifier_dt_tm = reply_obj->objArray[cnt].in_dt_tm
;    endif
;    if(reply_obj->objArray[cnt].out_dt_tm > cnvtdatetime(endDtTm) or reply_obj->objArray[cnt].out_dt_tm = NULL)
;      reply_obj->objArray[cnt].out_qualifier_dt_tm = cnvtdatetime(endDtTm)
;    else
;      reply_obj->objArray[cnt].out_qualifier_dt_tm = reply_obj->objArray[cnt].out_dt_tm
;    endif
  endif
  loc_cd = elh.loc_nurse_unit_cd
foot elh.encntr_id
  null
foot report
  reply_obj->qual_cnt = cnt
  stat = alterlist(reply_obj->objArray,cnt)
with nocounter
 
;call echorecord(reply_obj)
;go to exit_script
 
 
; Get UCR & MET Events
call echo("Get UCR & MET Events")
;call echo("In:")
;call echo(format(reply_obj->objArray[1].in_qualifier_dt_tm,";;q"))
;call echo("Out:")
;call echo(format(reply_obj->objArray[1].out_qualifier_dt_tm,";;q"))
set sLookAhead = concat(trim(cnvtstring(cfg->evt_threshold_min)),',"MIN"')
call echo(sLookAhead)
select into "nl:"
from (dummyt d with seq=value(reply_obj->qual_cnt))
    , cust_mp_audit mpa
    , clinical_event ce
plan d
join mpa
  where mpa.person_id = reply_obj->objArray[d.seq].person_id
    and mpa.encntr_id = reply_obj->objArray[d.seq].encntr_id
    and mpa.alert_dt_tm >= cnvtdatetime(reply_obj->objArray[d.seq].in_qualifier_dt_tm)
    and mpa.alert_dt_tm <= cnvtdatetime(reply_obj->objArray[d.seq].out_qualifier_dt_tm)
    and mpa.active_ind = 1
join ce
  where ce.event_id = mpa.event_id
order by d.seq, mpa.alert_dt_tm, mpa.event_id
head d.seq
  evt_cnt = 0
  crit_cnt = 0
/*
;  cache_prev_alert_dt_tm = cnvtdatetime("01-JAN-1900")
;  cache_prev_alert_colour = 0
*/
  cache_prev_alert_dt_tm = cnvtdatetime("01-JAN-1900")
  cache_prev_alert_colour = 0
head mpa.alert_dt_tm
  evt_cnt = evt_cnt+1
  stat = alterlist(reply_obj->objArray[d.seq].evt,evt_cnt)
  reply_obj->objArray[d.seq].evt[evt_cnt].event_dt_tm = mpa.alert_dt_tm
  ;reply_obj->objArray[d.seq].evt[evt_cnt].complete_dt_tm = cnvtlookahead(sLookAhead,mpa.alert_dt_tm)
  reply_obj->objArray[d.seq].evt[evt_cnt].complete_thresh_dt_tm = cnvtlookahead(sLookAhead,mpa.alert_dt_tm)
  ; Add event to crit for 24 hr calculation
  crit_cnt = crit_cnt+1
  stat = alterlist(reply_obj->objArray[d.seq].crit,crit_cnt)
  reply_obj->objArray[d.seq].crit[crit_cnt].type_flag = CRITERIA_CLINICAL_EVENT
  reply_obj->objArray[d.seq].crit[crit_cnt].crit_dt_tm = cnvtdatetime(mpa.alert_dt_tm)
  ; declare vars
  vit_cnt = 0
  yellow_cnt = 0
  red_cnt = 0
head mpa.event_id
  vit_cnt = vit_cnt+1
  stat = alterlist(reply_obj->objArray[d.seq].evt[evt_cnt].vit,vit_cnt)
  reply_obj->objArray[d.seq].evt[evt_cnt].vit[vit_cnt].vital_evt = findEventGraph(ce.event_cd)
  reply_obj->objArray[d.seq].evt[evt_cnt].vit[vit_cnt].vital_value = mpa.value
  if(mpa.alert_color = GRAPH_RANGE_RED)
    red_cnt = red_cnt+1
  elseif(mpa.alert_color = GRAPH_RANGE_YELLOW)
    yellow_cnt = yellow_cnt+1
  endif
foot mpa.event_id
  null
foot mpa.alert_dt_tm
;  call echo("Here!")
;  call echo(format(mpa.alert_dt_tm,"dd-mmm-yyyy hh:mm:ss;;q"))
;  call echo(build("red_cnt-->",red_cnt))
;  call echo(build("yellow_cnt-->",yellow_cnt))
;  call echo("End Here!")
  reply_obj->objArray[d.seq].evt[evt_cnt].red_cnt = red_cnt
  reply_obj->objArray[d.seq].evt[evt_cnt].yellow_cnt = yellow_cnt
  if(red_cnt>0 or yellow_cnt>=3)
    if(datetimediff(mpa.alert_dt_tm,cache_prev_alert_dt_tm,4) > cfg->evt_threshold_min
     or cache_prev_alert_colour = GRAPH_RANGE_YELLOW)
      apply_flag = TRUE
      reply_obj->objArray[d.seq].evt[evt_cnt].event_type = GRAPH_RANGE_RED
      reply_obj->objArray[d.seq].met_event_cnt = reply_obj->objArray[d.seq].met_event_cnt+1
    endif
  elseif(yellow_cnt>0)
    if(datetimediff(mpa.alert_dt_tm,cache_prev_alert_dt_tm,4) > cfg->evt_threshold_min)
      apply_flag = TRUE
      reply_obj->objArray[d.seq].evt[evt_cnt].event_type = GRAPH_RANGE_YELLOW
      reply_obj->objArray[d.seq].ucr_event_cnt = reply_obj->objArray[d.seq].ucr_event_cnt+1
    endif
  endif
  reply_obj->objArray[d.seq].evt[evt_cnt].vit_cnt = vit_cnt
  if(apply_flag = TRUE)
    cache_prev_alert_dt_tm = mpa.alert_dt_tm
    cache_prev_alert_colour = reply_obj->objArray[d.seq].evt[evt_cnt].event_type
    reply_obj->objArray[d.seq].evt[evt_cnt].vit_cnt = vit_cnt
  else
    evt_cnt = evt_cnt-1
    stat = alterlist(reply_obj->objArray[d.seq].evt,evt_cnt)
  endif
/*
;  apply_flag = FALSE
 
  if(red_cnt>0 or yellow_cnt>=3)
;    if(datetimediff(mpa.alert_dt_tm,cache_prev_alert_dt_tm,4) > cfg->evt_threshold_min
;     or cache_prev_alert_colour = GRAPH_RANGE_YELLOW)
;      apply_flag = TRUE
      reply_obj->objArray[d.seq].evt[evt_cnt].event_type = GRAPH_RANGE_RED
;      reply_obj->objArray[d.seq].met_event_cnt = reply_obj->objArray[d.seq].met_event_cnt+1
;    endif
  elseif(yellow_cnt>0)
;    if(datetimediff(mpa.alert_dt_tm,cache_prev_alert_dt_tm,4) > cfg->evt_threshold_min)
;      apply_flag = TRUE
      reply_obj->objArray[d.seq].evt[evt_cnt].event_type = GRAPH_RANGE_YELLOW
;      reply_obj->objArray[d.seq].ucr_event_cnt = reply_obj->objArray[d.seq].ucr_event_cnt+1
;    endif
  endif
 reply_obj->objArray[d.seq].evt[evt_cnt].vit_cnt = vit_cnt
;  if(apply_flag = TRUE)
;    cache_prev_alert_dt_tm = mpa.alert_dt_tm
;    cache_prev_alert_colour = reply_obj->objArray[d.seq].evt[evt_cnt].event_type
;    reply_obj->objArray[d.seq].evt[evt_cnt].vit_cnt = vit_cnt
;  else
;    evt_cnt = evt_cnt-1
;    stat = alterlist(reply_obj->objArray[d.seq].evt,evt_cnt)
;  endif
*/
foot d.seq
  reply_obj->objArray[d.seq].evt_cnt = evt_cnt
  reply_obj->objArray[d.seq].crit_cnt = crit_cnt
with nocounter
 
;call echorecord(reply_obj)
;go to exit_script
;call echorecord(mp_range)
 
;declare sTmpGraphName = vc
; Load vitals and define actions
;call echo("Load vitals and define actions")
select into "nl:"
from (dummyt d1 with seq=value(reply_obj->qual_cnt))
   , (dummyt d2 with seq=1)
   , clinical_event ce
plan d1
  where maxrec(d2,reply_obj->objArray[d1.seq].evt_cnt)
join d2
join ce
  where ce.person_id = reply_obj->objArray[d1.seq].person_id
    and ce.encntr_id = reply_obj->objArray[d1.seq].encntr_id
    and ce.event_end_dt_tm > cnvtdatetime(reply_obj->objArray[d1.seq].evt[d2.seq].event_dt_tm)
    and ce.event_end_dt_tm < cnvtdatetime(reply_obj->objArray[d1.seq].evt[d2.seq].complete_thresh_dt_tm)
    and expand(sIdx,1,evts->evt_cnt,ce.event_cd,evts->evt[sIdx].event_cd)
    and ce.valid_until_dt_tm >= cnvtdatetime(curdate,curtime3)
order by d1.seq, d2.seq, ce.event_end_dt_tm
head d1.seq
  vitPos = 0
  vitIdx = 0
head d2.seq
  null
detail
  ; check if vital is breached
  sTmpGraphName = findEventGraph(ce.event_cd)
  vitPos = locateval(vitIdx,1,reply_obj->objArray[d1.seq].evt[d2.seq].vit_cnt,
    sTmpGraphName, reply_obj->objArray[d1.seq].evt[d2.seq].vit[vitIdx].vital_evt)
  if(vitPos > 0)
    if(vitalNormal(ce.event_cd
                  ,ce.result_val
                  ,getAgeRange(reply_obj->objArray[d1.seq].birth_dt_tm,ce.event_end_dt_tm)
                  ,reply_obj->objArray[d1.seq].nurse_unit_cd
                  ,reply_obj->objArray[d1.seq].med_srv_cd)=TRUE)
      reply_obj->objArray[d1.seq].evt[d2.seq].vit[vitPos].normal_return_ind = TRUE
    endif
  endif
foot d2.seq
  normalInd = TRUE
  for(vCnt=1 to reply_obj->objArray[d1.seq].evt[d2.seq].vit_cnt)
    if(reply_obj->objArray[d1.seq].evt[d2.seq].vit[vCnt].normal_return_ind = FALSE)
      normalInd = FALSE
    endif
  endfor
  if(normalInd = TRUE) ; override the complete date/time to the normal vital
    reply_obj->objArray[d1.seq].evt[d2.seq].complete_dt_tm = ce.event_end_dt_tm
    reply_obj->objArray[d1.seq].evt[d2.seq].complete_ind = TRUE
  endif
foot d1.seq
  null
with nocounter
 
;call echorecord(reply_obj)
;go to exit_script
 
 
;Check for complete ones to see if events after them now need to be counted
call echo("Check for complete ones to see if events after them now need to be counted")
select into "nl:"
from (dummyt d1 with seq=value(reply_obj->qual_cnt))
   , (dummyt d2 with seq=1)
plan d1
  where maxrec(d2,reply_obj->objArray[d1.seq].evt_cnt)
join d2
  where reply_obj->objArray[d1.seq].evt[d2.seq].event_type = 0
    and (reply_obj->objArray[d1.seq].evt[d2.seq].red_cnt > 0
      or reply_obj->objArray[d1.seq].evt[d2.seq].yellow_cnt > 0)
    and d2.seq > 1
detail
  ;if(reply_obj->objArray[d1.seq].evt[d2.seq-1].complete_ind = TRUE)
    if(reply_obj->objArray[d1.seq].evt[d2.seq].red_cnt>0
     or reply_obj->objArray[d1.seq].evt[d2.seq].yellow_cnt>=3)
      reply_obj->objArray[d1.seq].evt[d2.seq].event_type = GRAPH_RANGE_RED
      reply_obj->objArray[d1.seq].met_event_cnt = reply_obj->objArray[d1.seq].met_event_cnt+1
    elseif(reply_obj->objArray[d1.seq].evt[d2.seq].yellow_cnt>0)
      reply_obj->objArray[d1.seq].evt[d2.seq].event_type = GRAPH_RANGE_YELLOW
      reply_obj->objArray[d1.seq].ucr_event_cnt = reply_obj->objArray[d1.seq].ucr_event_cnt+1
    endif
  ;endif
with nocounter
 
;call echorecord(reply_obj)
;go to exit_script
 
 
; Check for criteria change
call echo("Check for criteria change")
select into "nl:"
 action_dt_tm = if(ac.suspend_alert_flag = 0) ac.altered_criteria_dt_tm else ac.updt_dt_tm endif
from (dummyt d1 with seq=value(reply_obj->qual_cnt))
   , (dummyt d2 with seq=1)
   , cust_mp_altered_criteria ac
plan d1
  where maxrec(d2,reply_obj->objArray[d1.seq].evt_cnt)
join d2
join ac
  where ac.person_id = reply_obj->objArray[d1.seq].person_id
    and ac.encntr_id = reply_obj->objArray[d1.seq].encntr_id
    and ((ac.updt_dt_tm >= cnvtdatetime(reply_obj->objArray[d1.seq].evt[d2.seq].event_dt_tm)
         and ac.updt_dt_tm <= cnvtdatetime(reply_obj->objArray[d1.seq].evt[d2.seq].complete_thresh_dt_tm)
         and ac.suspend_alert_flag = 1)
      or (ac.altered_criteria_dt_tm >= cnvtdatetime(reply_obj->objArray[d1.seq].evt[d2.seq].event_dt_tm)
         and ac.altered_criteria_dt_tm <= cnvtdatetime(reply_obj->objArray[d1.seq].evt[d2.seq].complete_thresh_dt_tm)
         and (ac.suspend_alert_flag = 0 or ac.suspend_alert_flag = NULL)))
order by d1.seq, d2.seq, action_dt_tm
head d1.seq
  null
head d2.seq
  vitIdx = 0
  vitPos = 0
  act_cnt = 0
  suspend_altered_flag = 0
detail
;  call echo("here!")
;  call echo(format(ac.create_dt_tm,"dd-mmm-yyyy hh:mm:ss;;q"))
  vitPos = locateval(vitIdx,1,reply_obj->objArray[d1.seq].evt[d2.seq].vit_cnt
      ,ac.modified_vital_name,reply_obj->objArray[d1.seq].evt[d2.seq].vit[vitIdx].vital_evt)
  if(vitPos>0)
    if(ac.suspend_alert_flag > 0)
      reply_obj->objArray[d1.seq].evt[d2.seq].vit[vitPos].suspended_ind = TRUE
      reply_obj->objArray[d1.seq].evt[d2.seq].complete_thresh_dt_tm = ac.suspend_alert_until_dt_tm
      if(suspend_altered_flag = 0)
        suspend_altered_flag = SUSPEND_ALTERED_SUSPENDED
      endif
    else
      reply_obj->objArray[d1.seq].evt[d2.seq].vit[vitPos].altered_ind = TRUE
      reply_obj->objArray[d1.seq].evt[d2.seq].complete_thresh_dt_tm = ac.altered_criteria_dt_tm
      if(suspend_altered_flag = 0)
        suspend_altered_flag = SUSPEND_ALTERED_ALTERED
      endif
    endif
    if(reply_obj->objArray[d1.seq].max_review_dt_tm < cnvtdatetime(ac.next_review_dt_tm)
     or reply_obj->objArray[d1.seq].max_review_dt_tm = NULL)
      reply_obj->objArray[d1.seq].max_review_dt_tm = ac.next_review_dt_tm
    endif
  endif
foot d2.seq
  if(suspend_altered_flag > 0)
    act_cnt = reply_obj->objArray[d1.seq].evt[d2.seq].act_cnt+1
    stat = alterlist(reply_obj->objArray[d1.seq].evt[d2.seq].act,act_cnt)
    reply_obj->objArray[d1.seq].evt[d2.seq].act_cnt = act_cnt
    reply_obj->objArray[d1.seq].evt[d2.seq].complete_ind = TRUE
    reply_obj->objArray[d1.seq].evt[d2.seq].complete_dt_tm = reply_obj->objArray[d1.seq].evt[d2.seq].complete_thresh_dt_tm
    reply_obj->objArray[d1.seq].evt[d2.seq].act[act_cnt].act_value =
     format(reply_obj->objArray[d1.seq].evt[d2.seq].complete_dt_tm,"dd/mm/yyyy hh:mm;;q")
    if(suspend_altered_flag = SUSPEND_ALTERED_ALTERED)
      reply_obj->objArray[d1.seq].evt[d2.seq].altered_ind = TRUE
      reply_obj->objArray[d1.seq].evt[d2.seq].act[act_cnt].act_evt = "Altered Calling Criteria"
    elseif(suspend_altered_flag = SUSPEND_ALTERED_SUSPENDED)
      reply_obj->objArray[d1.seq].evt[d2.seq].suspended_ind = TRUE
      reply_obj->objArray[d1.seq].evt[d2.seq].act[act_cnt].act_evt = "Calling Criteria Suspended"
    endif
    if(reply_obj->objArray[d1.seq].evt[d2.seq].event_type = GRAPH_RANGE_RED)
      reply_obj->objArray[d1.seq].met_action_cnt = reply_obj->objArray[d1.seq].met_action_cnt+1
    elseif(reply_obj->objArray[d1.seq].evt[d2.seq].event_type = GRAPH_RANGE_YELLOW)
      reply_obj->objArray[d1.seq].ucr_action_cnt = reply_obj->objArray[d1.seq].ucr_action_cnt+1
    endif
  endif
with nocounter
 
 
;call echorecord(reply_obj)
;go to exit_script
 
 
; Get altered, changes and resets for 24 hour column
select into "nl:"
from (dummyt d1 with seq=value(reply_obj->qual_cnt))
   , cust_mp_altered_criteria ac
plan d1
join ac
  where ac.person_id = reply_obj->objArray[d1.seq].person_id
    and ac.encntr_id = reply_obj->objArray[d1.seq].encntr_id
    and (ac.suspend_alert_flag = 0 or ac.suspend_alert_flag = NULL)
    and ac.altered_criteria_dt_tm >= cnvtdatetime(reply_obj->objArray[d1.seq].in_qualifier_dt_tm)
    and ac.altered_criteria_dt_tm <= cnvtdatetime(reply_obj->objArray[d1.seq].out_qualifier_dt_tm)
order by d1.seq, ac.altered_criteria_dt_tm
head d1.seq
  crit_cnt = reply_obj->objArray[d1.seq].crit_cnt
head ac.altered_criteria_dt_tm
  crit_cnt = crit_cnt+1
  stat = alterlist(reply_obj->objArray[d1.seq].crit,crit_cnt)
  reply_obj->objArray[d1.seq].crit[crit_cnt].crit_dt_tm = cnvtdatetime(ac.altered_criteria_dt_tm)
  if(ac.modified_vital_name = "RESET")
    reply_obj->objArray[d1.seq].crit[crit_cnt].type_flag = CRITERIA_RESET
  elseif(ac.modified_vital_name = "REVIEWED")
    reply_obj->objArray[d1.seq].crit[crit_cnt].type_flag = CRITERIA_NO_CHANGE
  else
    reply_obj->objArray[d1.seq].crit[crit_cnt].type_flag = CRITERIA_ALTERED
  endif
foot ac.altered_criteria_dt_tm
  null
foot d1.seq
  reply_obj->objArray[d1.seq].crit_cnt = crit_cnt
with nocounter
 
;call echorecord(reply_obj)
;go to exit_script
 
 
; Next query to check documenting the configured form.
call echo("Next query to check documenting the configured form.")
select into "nl:"
from (dummyt d1 with seq=value(reply_obj->qual_cnt))
   , clinical_event ce
plan d1
join ce
  where ce.person_id = reply_obj->objArray[d1.seq].person_id
    and ce.encntr_id = reply_obj->objArray[d1.seq].encntr_id
    and ce.event_end_dt_tm >= cnvtdatetime(reply_obj->objArray[d1.seq].in_qualifier_dt_tm)
    and ce.event_end_dt_tm <= cnvtdatetime(reply_obj->objArray[d1.seq].out_qualifier_dt_tm)
    and expand(sIdx,1,cfg->act_evt_cnt,ce.event_cd,cfg->act_evt[sIdx].event_cd)
    and ce.valid_until_dt_tm >= cnvtdatetime(curdate,curtime3)
detail
  ;call echo("Here!")
  ;call echo(format(ce.event_end_dt_tm,"dd-mmm-yyyy hh:mm:ss;;q"))
 
  ; Is within threshold?
  foundPos = 0
  foundCnt = 0
  tmpFoundPos = 0
  tmpFoundCompFlag = FALSE
  act_cnt = 0
  while(foundPos=0)
    foundCnt = foundCnt+1
    if(reply_obj->objArray[d1.seq].evt[foundCnt].event_dt_tm >= ce.event_end_dt_tm
     and datetimediff(ce.event_end_dt_tm,reply_obj->objArray[d1.seq].evt[foundCnt].event_dt_tm,4) <= cfg->evt_threshold_min
     and reply_obj->objArray[d1.seq].evt[foundCnt].act_cnt = 0)
      foundPos = foundCnt
    endif
    if(foundPos=0 and foundCnt >= reply_obj->objArray[d1.seq].evt_cnt)
      foundPos = -1
    endif
  endwhile
 
  if(foundPos>0)
    act_cnt = reply_obj->objArray[d1.seq].evt[foundPos].act_cnt+1
    stat = alterlist(reply_obj->objArray[d1.seq].evt[foundPos].act,act_cnt)
    reply_obj->objArray[d1.seq].evt[foundPos].act[act_cnt].act_evt = uar_get_code_display(ce.event_cd)
    reply_obj->objArray[d1.seq].evt[foundPos].act[act_cnt].act_evt = ce.result_val
    reply_obj->objArray[d1.seq].evt[foundPos].act[act_cnt].act_dt_tm = cnvtdatetime(ce.event_end_dt_tm)
    if((cnvtdatetime(ce.event_end_dt_tm) < reply_obj->objArray[d1.seq].evt[foundPos].complete_dt_tm)
     or reply_obj->objArray[d1.seq].evt[foundPos].complete_dt_tm = NULL)
      reply_obj->objArray[d1.seq].evt[foundPos].complete_dt_tm = cnvtdatetime(ce.event_end_dt_tm)
    endif
    reply_obj->objArray[d1.seq].evt[foundPos].act_cnt = act_cnt
    if(reply_obj->objArray[d1.seq].evt[foundPos].event_type = GRAPH_RANGE_RED)
      reply_obj->objArray[d1.seq].met_action_cnt = reply_obj->objArray[d1.seq].met_action_cnt+1
    elseif(reply_obj->objArray[d1.seq].evt[foundPos].event_type = GRAPH_RANGE_YELLOW)
      reply_obj->objArray[d1.seq].ucr_action_cnt = reply_obj->objArray[d1.seq].ucr_action_cnt+1
    else
      for(zz=1 to foundPos-1)
        if(tmpFoundCompFlag = FALSE)
          tmpFoundPos = foundPos-zz
          if(reply_obj->objArray[d1.seq].evt[tmpFoundPos].event_type = GRAPH_RANGE_RED)
            reply_obj->objArray[d1.seq].met_action_cnt = reply_obj->objArray[d1.seq].met_action_cnt+1
            tmpFoundCompFlag = TRUE
          elseif(reply_obj->objArray[d1.seq].evt[tmpFoundPos].event_type = GRAPH_RANGE_YELLOW)
            reply_obj->objArray[d1.seq].met_action_cnt = reply_obj->objArray[d1.seq].met_action_cnt+1
            tmpFoundCompFlag = TRUE
          endif
        endif
      endfor
    endif
  endif
with nocounter
 
;call echorecord(cfg)
;call echorecord(reply_obj)
;go to exit_script
 
 
; ACC actioned in 24 hours & Discharged while ACC
select into "nl:"
from (dummyt d1 with seq=value(reply_obj->qual_cnt))
   , (dummyt d2 with seq=1)
plan d1
  where maxrec(d2,reply_obj->objArray[d1.seq].evt_cnt)
join d2
  ;where reply_obj->objArray[d1.seq].evt[d2.seq].altered_ind = TRUE
order by d1.seq
head report
  sMETLookAhead = concat(trim(cnvtstring(cfg->evt_relapse_hrs)),",H")
head d1.seq
  last_ucr_evt_dt_tm = cnvtdatetime("01-JAN-1900 00:00:00")
detail
;  if(datetimediff(reply_obj->objArray[d1.seq].evt[d2.seq].complete_dt_tm
;   ,reply_obj->objArray[d1.seq].evt[d2.seq].event_dt_tm,3) > 24
;    or (reply_obj->objArray[d1.seq].evt[d2.seq].complete_dt_tm = NULL
;   and cnvtdatetime(curdate,curtime3) > cnvtlookahead("1,D",reply_obj->objArray[d1.seq].evt[d2.seq].event_dt_tm)))
;     ;and reply_obj->objArray[d1.seq].evt[d2.seq].altered_ind = TRUE)
;    reply_obj->objArray[d1.seq].acc_expire_cnt = reply_obj->objArray[d1.seq].acc_expire_cnt+1
;  endif
  ; Check hour threshold
  if(reply_obj->objArray[d1.seq].evt[d2.seq].event_type = GRAPH_RANGE_YELLOW)
    last_ucr_evt_dt_tm = cnvtdatetime(reply_obj->objArray[d1.seq].evt[d2.seq].event_dt_tm)
  elseif(reply_obj->objArray[d1.seq].evt[d2.seq].event_type = GRAPH_RANGE_RED)
    if(cnvtlookahead(sMETLookAhead,cnvtdatetime(last_ucr_evt_dt_tm))
     >= cnvtdatetime(reply_obj->objArray[d1.seq].evt[d2.seq].event_dt_tm))
      reply_obj->objArray[d1.seq].met_within_ucr_threshold_cnt = reply_obj->objArray[d1.seq].met_within_ucr_threshold_cnt + 1
      last_ucr_evt_dt_tm = cnvtdatetime("01-JAN-1900 00:00:00")
    endif
  endif
foot d1.seq
  if(reply_obj->objArray[d1.seq].disch_dt_tm != NULL
   and reply_obj->objArray[d1.seq].disch_dt_tm < reply_obj->objArray[d1.seq].max_review_dt_tm)
     reply_obj->objArray[d1.seq].disch_acc_ind = TRUE
  endif
with nocounter
 
 
; ACC actioned in 24 hours
select into "nl:"
 act_dt_tm = cnvtdatetime(reply_obj->objArray[d1.seq].crit[d2.seq].crit_dt_tm)
from (dummyt d1 with seq=value(reply_obj->qual_cnt))
   , (dummyt d2 with seq=1)
plan d1
  where maxrec(d2,reply_obj->objArray[d1.seq].crit_cnt)
join d2
  where reply_obj->objArray[d1.seq].crit[d2.seq].type_flag != CRITERIA_CLINICAL_EVENT
order by d1.seq, act_dt_tm
head report
  sFlexLookAhead = concat(trim(cnvtstring(HOURS_EXPIRE_THRESHOLD)),",H")
head d1.seq
  last_date = cnvtdatetime("01-JAN-2100")
  first = 0
detail
  if(first = 0)
    first = 1
    last_date = cnvtdatetime(reply_obj->objArray[d1.seq].crit[d2.seq].crit_dt_tm)
  endif
;  if(reply_obj->objArray[d1.seq].crit[d2.seq].type_flag = CRITERIA_CLINICAL_EVENT)
;    if(cnvtdatetime(last_date) < cnvtdatetime(reply_obj->objArray[d1.seq].crit[d2.seq].crit_dt_tm))
;      if(cnvtlookahead(sFlexLookAhead,cnvtdatetime(last_date)) < cnvtdatetime(reply_obj->objArray[d1.seq].crit[d2.seq].crit_dt_tm))
;        reply_obj->objArray[d1.seq].acc_expire_cnt = 1
;      endif
;    endif
;  elseif(reply_obj->objArray[d1.seq].crit[d2.seq].type_flag in (CRITERIA_NO_CHANGE, CRITERIA_ALTERED))
  if(reply_obj->objArray[d1.seq].crit[d2.seq].type_flag in (CRITERIA_NO_CHANGE, CRITERIA_ALTERED))
    if(cnvtlookahead(sFlexLookAhead,cnvtdatetime(last_date)) < cnvtdatetime(reply_obj->objArray[d1.seq].crit[d2.seq].crit_dt_tm))
      reply_obj->objArray[d1.seq].acc_expire_cnt = 1
    endif
    last_date = cnvtdatetime(reply_obj->objArray[d1.seq].crit[d2.seq].crit_dt_tm)
  elseif(reply_obj->objArray[d1.seq].crit[d2.seq].type_flag = CRITERIA_RESET)
    if(cnvtlookahead(sFlexLookAhead,cnvtdatetime(last_date)) < cnvtdatetime(reply_obj->objArray[d1.seq].crit[d2.seq].crit_dt_tm))
      reply_obj->objArray[d1.seq].acc_expire_cnt = 1
    endif
    first = 0
  endif
foot d1.seq
  if(cnvtlookahead(sFlexLookAhead,cnvtdatetime(last_date)) < cnvtdatetime(reply_obj->objArray[d1.seq].out_qualifier_dt_tm))
    reply_obj->objArray[d1.seq].acc_expire_cnt = 1
  endif
with nocounter
 
;call echorecord(reply_obj)
;go to exit_script
 
;TESTING
  select into "nl:"
  from (dummyt d1 with seq=value(reply_obj->qual_cnt))
       , encntr_alias ea
  plan d1
  join ea
    where ea.encntr_id = reply_obj->objArray[d1.seq].encntr_id
      and ea.active_ind = 1
      and ea.encntr_alias_type_cd in (dEncAliasMRNCd, dEncAliasFINCd)
      and ea.beg_effective_dt_tm <= cnvtdatetime(curdate,curtime3)
      and ea.end_effective_dt_tm >= cnvtdatetime(curdate,curtime3)
  detail
    if(ea.encntr_alias_type_cd = dEncAliasMRNCd)
      reply_obj->objArray[d1.seq].mrn = cnvtalias(ea.alias, ea.alias_pool_cd)
    else
      reply_obj->objArray[d1.seq].fin = cnvtalias(ea.alias, ea.alias_pool_cd)
    endif
  with nocounter
 
;call echorecord(reply_obj)
;call echo(sOutputType)
 
;select into "nl:"
;e_id = reply_obj->objArray[d1.seq].encntr_id
;from (dummyt d1 with seq=value(reply_obj->qual_cnt))
;plan d1
;  where reply_obj->objArray[d1.seq].ucr_event_cnt + reply_obj->objArray[d1.seq].met_event_cnt > 0
;order by e_id
;head e_id
; eIdCnt = 0
;detail
; eIdCnt = eIdCnt + 1
;foot e_id
; if(eIdCnt > 1)
;   call echo(build("e_id-->",e_id,"-->",eIdCnt))
; endif
 
 
 
;go to exit_script
 
; Query to summarise to expand (detail or summary)
if(sOutputType = OUTPUT_TYPE_SUMMARY)
  ;load labels
  set lyt->facility = uar_get_code_description(dFacilityCd)
  set lyt->date_range_string = concat("From: ",format(begDtTm,"dd/mm/yyyy HH:MM;;d")," To: ",format(endDtTm,"dd/mm/yyyy HH:MM;;d"))
  ; Nurse Unit
  if(sNurseUnitUsage = NURSE_UNIT_USAGE_ALL)
    set lyt->nurse_unit_string = "All"
  else
    if(sNurseUnitUsage = NURSE_UNIT_USAGE_INCLUDE)
      set lyt->nurse_unit_string = "Include: "
    elseif(sNurseUnitUsage = NURSE_UNIT_USAGE_EXCLUDE)
      set lyt->nurse_unit_string = "Exclude: "
    endif
    for(xCnt=1 to nrs_unit_list->arg_cnt)
      set lyt->nurse_unit_string = concat(lyt->nurse_unit_string,trim(uar_get_code_display(nrs_unit_list->arg[xCnt].double_val)))
      if(xCnt < nrs_unit_list->arg_cnt)
        set lyt->nurse_unit_string = concat(lyt->nurse_unit_string,", ")
      endif
    endfor
  endif
  ; Enc Type
  if(enc_type_list->arg_cnt = 0)
    set lyt->encntr_type_string = "All"
  else
    for(xCnt=1 to enc_type_list->arg_cnt)
      set lyt->encntr_type_string = concat(lyt->encntr_type_string,trim(uar_get_code_display(enc_type_list->arg[xCnt].double_val)))
      if(xCnt < nrs_unit_list->arg_cnt)
        set lyt->encntr_type_string = concat(lyt->encntr_type_string,", ")
      endif
    endfor
  endif
  ; Med Srv
  if(med_srv_list->arg_cnt = 0)
    set lyt->clinical_unit_string = "All"
  else
    for(xCnt=1 to med_srv_list->arg_cnt)
      set lyt->clinical_unit_string
       = concat(lyt->clinical_unit_string,trim(uar_get_code_display(med_srv_list->arg[xCnt].double_val)))
      if(xCnt < med_srv_list->arg_cnt)
        set lyt->clinical_unit_string = concat(lyt->clinical_unit_string,", ")
      endif
    endfor
  endif
 
  set lyt->deceased_string = evaluate(iDeceasedInd,TRUE,"Yes","No")
  set lyt->ucr_threshold = cfg->evt_relapse_hrs
 
  ; Get current User
  select into "nl:"
  from prsnl p
  where p.person_id = reqinfo->updt_id
  detail
    lyt->executed_by = p.name_full_formatted
  with nocounter
 
;  call echorecord(lyt)
;  call echorecord(cfg)
;  call echorecord(mp_range)
;  call echorecord(reply_obj)
;  go to exit_script
 
  ; Load layout record
  declare dupIndex= i4
  declare IsDupPat = i4
  declare mycnt = i4
  select into "nl:"
    nrs_unit_cd = reply_obj->objArray[d1.seq].nurse_unit_cd
    , nurse_unit_disp = substring(1,50,reply_obj->objArray[d1.seq].nurse_unit_disp)
  from (dummyt d1 with seq=value(reply_obj->qual_cnt))
  order by nurse_unit_disp, nrs_unit_cd
  head nrs_unit_cd
    lyt->unit_cnt = lyt->unit_cnt+1
    stat = alterlist(lyt->unit,lyt->unit_cnt)
    lyt->unit[lyt->unit_cnt].nurse_unit_cd = nrs_unit_cd
    lyt->unit[lyt->unit_cnt].nurse_unit_disp = nurse_unit_disp
    mycnt = 0
  detail
    IsDupPat = 0
    mycnt = mycnt + 1
    lyt->unit[lyt->unit_cnt].tot_pt_ward = lyt->unit[lyt->unit_cnt].tot_pt_ward+1
 
    IsDupPat = locateval(dupIndex,1,mycnt - 1,reply_obj->objArray[d1.seq].person_id,reply_obj->objArray[dupIndex].person_id)
    if(reply_obj->objArray[d1.seq].ucr_event_cnt > 0) ;and IsDupPat = 0)
      if(IsDupPat = 0 or reply_obj->objArray[dupIndex].ucr_event_cnt = 0)
          lyt->unit[lyt->unit_cnt].tot_pt_ucr = lyt->unit[lyt->unit_cnt].tot_pt_ucr+1
      ;if(reply_obj->objArray[d1.seq].ucr_event_cnt > 1)
      endif
    endif
    if(reply_obj->objArray[d1.seq].met_event_cnt > 0); and IsDupPat = 0)
      if(IsDupPat = 0 or reply_obj->objArray[dupIndex].met_event_cnt = 0)
          lyt->unit[lyt->unit_cnt].tot_pt_met = lyt->unit[lyt->unit_cnt].tot_pt_met+1
      endif
    endif
    ;if(reply_obj->objArray[d1.seq].ucr_event_cnt + reply_obj->objArray[d1.seq].met_event_cnt > 1)
    if(reply_obj->objArray[d1.seq].met_event_cnt > 1)
      lyt->unit[lyt->unit_cnt].met_multi_evt = lyt->unit[lyt->unit_cnt].met_multi_evt+1
    endif
    lyt->unit[lyt->unit_cnt].tot_ucr_evt = lyt->unit[lyt->unit_cnt].tot_ucr_evt + reply_obj->objArray[d1.seq].ucr_event_cnt
    lyt->unit[lyt->unit_cnt].tot_ucr_act = lyt->unit[lyt->unit_cnt].tot_ucr_act + reply_obj->objArray[d1.seq].ucr_action_cnt
 
    lyt->unit[lyt->unit_cnt].tot_met_evt = lyt->unit[lyt->unit_cnt].tot_met_evt + reply_obj->objArray[d1.seq].met_event_cnt
    lyt->unit[lyt->unit_cnt].tot_met_act = lyt->unit[lyt->unit_cnt].tot_met_act + reply_obj->objArray[d1.seq].met_action_cnt
    lyt->unit[lyt->unit_cnt].tot_met_evt_threshold = lyt->unit[lyt->unit_cnt].tot_met_evt_threshold
                                                     + reply_obj->objArray[d1.seq].met_within_ucr_threshold_cnt
    lyt->unit[lyt->unit_cnt].tot_acc_expired = lyt->unit[lyt->unit_cnt].tot_acc_expired
                                               + reply_obj->objArray[d1.seq].acc_expire_cnt
  foot nrs_unit_cd
    lyt->unit[lyt->unit_cnt].ucr_action_rate_str
     = formatPercentString(lyt->unit[lyt->unit_cnt].tot_ucr_act,lyt->unit[lyt->unit_cnt].tot_ucr_evt)
    lyt->unit[lyt->unit_cnt].met_action_rate_str
     = formatPercentString(lyt->unit[lyt->unit_cnt].tot_met_act,lyt->unit[lyt->unit_cnt].tot_met_evt)
    lyt->unit[lyt->unit_cnt].tot_met_evt_threshold_str
     = formatPercentString(lyt->unit[lyt->unit_cnt].tot_met_evt_threshold,lyt->unit[lyt->unit_cnt].tot_met_evt)
 
    ; do totals
    lyt->pt_ward = lyt->pt_ward + lyt->unit[lyt->unit_cnt].tot_pt_ward
    lyt->pt_ucr = lyt->pt_ucr + lyt->unit[lyt->unit_cnt].tot_pt_ucr
    lyt->ucr_evt = lyt->ucr_evt + lyt->unit[lyt->unit_cnt].tot_ucr_evt
    lyt->ucr_act = lyt->ucr_act + lyt->unit[lyt->unit_cnt].tot_ucr_act
    lyt->final_met_multi_evt = lyt->final_met_multi_evt + lyt->unit[lyt->unit_cnt].met_multi_evt
    lyt->pt_met = lyt->pt_met + lyt->unit[lyt->unit_cnt].tot_pt_met
    lyt->met_evt = lyt->met_evt + lyt->unit[lyt->unit_cnt].tot_met_evt
    lyt->met_act = lyt->met_act + lyt->unit[lyt->unit_cnt].tot_met_act
    lyt->met_evt_threshold = lyt->met_evt_threshold + lyt->unit[lyt->unit_cnt].tot_met_evt_threshold
    lyt->acc_expired = lyt->acc_expired + lyt->unit[lyt->unit_cnt].tot_acc_expired
  foot report
    lyt->final_ucr_action_rate_str = formatPercentString(lyt->ucr_act, lyt->ucr_evt)
    lyt->final_met_action_rate_str = formatPercentString(lyt->met_act, lyt->met_evt)
    lyt->met_evt_threshold_str = formatPercentString(lyt->met_evt_threshold, lyt->met_evt)
  with nocounter
 
;  call echorecord(reply_obj)
;  call echorecord(lyt)
;
;
;SELECT into $OUTDEV
;	UNIT_NURSE_UNIT_DISP = SUBSTRING(1, 50, LYT->unit[D1.SEQ].nurse_unit_disp)
;	, UNIT_TOT_PT_WARD = LYT->unit[D1.SEQ].tot_pt_ward
;	, UNIT_TOT_PT_UCR = LYT->unit[D1.SEQ].tot_pt_ucr
;	, UNIT_TOT_UCR_EVT = LYT->unit[D1.SEQ].tot_ucr_evt
;	, UNIT_TOT_UCR_ACT = LYT->unit[D1.SEQ].tot_ucr_act
;	, UNIT_UCR_ACTION_RATE_STR = SUBSTRING(1, 30, LYT->unit[D1.SEQ].ucr_action_rate_str)
;	, UNIT_UCR_MULTI_EVT = LYT->unit[D1.SEQ].ucr_multi_evt
;	, UNIT_TOT_PT_MET = LYT->unit[D1.SEQ].tot_pt_met
;	, UNIT_TOT_MET_EVT = LYT->unit[D1.SEQ].tot_met_evt
;	, UNIT_TOT_MET_ACT = LYT->unit[D1.SEQ].tot_met_act
;	, UNIT_MET_ACTION_RATE_STR = SUBSTRING(1, 30, LYT->unit[D1.SEQ].met_action_rate_str)
;	, UNIT_TOT_MET_EVT_THRESHOLD_STR = SUBSTRING(1, 30, LYT->unit[D1.SEQ].tot_met_evt_threshold_str)
;	, UNIT_TOT_ACC_EXPIRED = LYT->unit[D1.SEQ].tot_acc_expired
;
;FROM
;	(DUMMYT   D1  WITH SEQ = VALUE(SIZE(LYT->unit, 5)))
;
;PLAN D1
;
;WITH NOCOUNTER, SEPARATOR=" ", FORMAT
;  go to exit_script
 
  ;do layout
  execute astn_deter_pt_rpt_lyt $OUTDEV
else
  ; Load MRN & FIN
  select into "nl:"
  from (dummyt d1 with seq=value(reply_obj->qual_cnt))
       , encntr_alias ea
  plan d1
  join ea
    where ea.encntr_id = reply_obj->objArray[d1.seq].encntr_id
      and ea.active_ind = 1
      and ea.encntr_alias_type_cd in (dEncAliasMRNCd, dEncAliasFINCd)
      and ea.beg_effective_dt_tm <= cnvtdatetime(curdate,curtime3)
      and ea.end_effective_dt_tm >= cnvtdatetime(curdate,curtime3)
  detail
    if(ea.encntr_alias_type_cd = dEncAliasMRNCd)
      reply_obj->objArray[d1.seq].mrn = cnvtalias(ea.alias, ea.alias_pool_cd)
    else
      reply_obj->objArray[d1.seq].fin = cnvtalias(ea.alias, ea.alias_pool_cd)
    endif
  with nocounter
 
  ; Load out displays
  select into "nl:"
  from (dummyt d1 with seq=value(reply_obj->qual_cnt))
     , (dummyt d2 with seq=1)
  plan d1
    where maxrec(d2,reply_obj->objArray[d1.seq].evt_cnt)
  join d2
  order by d1.seq, d2.seq
  head d1.seq
    sTmpVitals = ""
    sTmpActions = ""
  head d2.seq
    null
  detail
    for(zz=1 to reply_obj->objArray[d1.seq].evt[d2.seq].vit_cnt)
      sTmpVitals = concat(sTmpVitals,trim(reply_obj->objArray[d1.seq].evt[d2.seq].vit[zz].vital_evt)
                         ,DEL3,trim(reply_obj->objArray[d1.seq].evt[d2.seq].vit[zz].vital_value),DEL2)
    endfor
    for(yy=1 to reply_obj->objArray[d1.seq].evt[d2.seq].act_cnt)
      sTmpActions = concat(sTmpActions,trim(reply_obj->objArray[d1.seq].evt[d2.seq].act[yy].act_evt)
                          ,DEL3,trim(reply_obj->objArray[d1.seq].evt[d2.seq].act[yy].act_value),DEL2)
 
    endfor
  foot d2.seq
    if(reply_obj->objArray[d1.seq].evt[d2.seq].event_type = GRAPH_RANGE_YELLOW)
      reply_obj->objArray[d1.seq].ucr_vitals = concat(reply_obj->objArray[d1.seq].ucr_vitals,replace(sTmpVitals,DEL2,"",2))
      reply_obj->objArray[d1.seq].ucr_actions = concat(reply_obj->objArray[d1.seq].ucr_actions,replace(sTmpActions,DEL2,"",2))
      if(d2.seq = 1)
        reply_obj->objArray[d1.seq].ucr_dates = format(reply_obj->objArray[d1.seq].evt[d2.seq].event_dt_tm,"dd/mm/yyyy hh:mm;;q")
      else
        reply_obj->objArray[d1.seq].ucr_dates = concat(reply_obj->objArray[d1.seq].ucr_dates,DEL2
                                                , format(reply_obj->objArray[d1.seq].evt[d2.seq].event_dt_tm,"dd/mm/yyyy hh:mm;;q"))
      endif
    else
      reply_obj->objArray[d1.seq].met_vitals = concat(reply_obj->objArray[d1.seq].met_vitals,replace(sTmpVitals,DEL2,"",2))
      reply_obj->objArray[d1.seq].met_actions = concat(reply_obj->objArray[d1.seq].met_actions,replace(sTmpActions,DEL2,"",2))
      if(d2.seq = 1)
        reply_obj->objArray[d1.seq].met_dates = format(reply_obj->objArray[d1.seq].evt[d2.seq].event_dt_tm,"dd/mm/yyyy hh:mm;;q")
      else
        reply_obj->objArray[d1.seq].met_dates = concat(reply_obj->objArray[d1.seq].met_dates,DEL2
                                                , format(reply_obj->objArray[d1.seq].evt[d2.seq].event_dt_tm,"dd/mm/yyyy hh:mm;;q"))
      endif
    endif
  foot d1.seq
    null
  with nocounter
 
 
  ; do Output
  select into $OUTDEV
    MRN = trim(substring(1,15,reply_obj->objarray[d1.seq].mrn))
    , Patient_Name = trim(substring(1,15,reply_obj->objarray[d1.seq].pat_name)) ;001
    , nurse_unit = trim(substring(1,60,reply_obj->objarray[d1.seq].nurse_unit_disp))
    , Bed = trim(substring(1,60,reply_obj->objarray[d1.seq].bed_disp))   ;001
    , clinical_unit = trim(substring(1,60,reply_obj->objarray[d1.seq].med_srv_disp))
    , ucr_events = trim(substring(1,500,reply_obj->objarray[d1.seq].ucr_dates))
    , met_events = trim(substring(1,200,reply_obj->objarray[d1.seq].met_dates))
    , ucr_vitals = trim(substring(1,500,reply_obj->objarray[d1.seq].ucr_vitals))
    , met_vitals = trim(substring(1,500,reply_obj->objarray[d1.seq].met_vitals))
    , ucr_actions = trim(substring(1,500,reply_obj->objarray[d1.seq].ucr_actions))
    , met_actions = trim(substring(1,500,reply_obj->objarray[d1.seq].met_actions))
    , ACC_expire_cnt = reply_obj->objArray[d1.seq].acc_expire_cnt
    , discharge_acc = evaluate(reply_obj->objarray[d1.seq].disch_acc_ind,TRUE,"yes","")
    , deceased = evaluate(reply_obj->objarray[d1.seq].deceased_ind,TRUE,"yes","")
    , encounter_type = trim(substring(1,60,reply_obj->objarray[d1.seq].encntr_type_disp))
    , visit_id = trim(substring(1,15,reply_obj->objarray[d1.seq].fin))
    , nurse_unit_in_dt_tm = format(reply_obj->objarray[d1.seq].in_dt_tm,"dd/mm/yyyy hh:mm;;q")
    , nurse_unit_out_dt_tm = format(reply_obj->objarray[d1.seq].out_dt_tm,"dd/mm/yyyy hh:mm;;q")
    , registration_dt_tm = format(reply_obj->objarray[d1.seq].reg_dt_tm,"dd/mm/yyyy hh:mm;;q")
    , discharge_dt_tm = format(reply_obj->objarray[d1.seq].disch_dt_tm,"dd/mm/yyyy hh:mm;;q")
  from (dummyt d1 with seq=value(reply_obj->qual_cnt))
  where (
           (TEXTLEN(reply_obj->objarray[d1.seq].ucr_dates) > 0) or
           (TEXTLEN(reply_obj->objarray[d1.seq].met_dates) > 0) or
           (TEXTLEN(reply_obj->objarray[d1.seq].ucr_vitals) > 0) or
           (TEXTLEN(reply_obj->objarray[d1.seq].met_vitals) > 0) or
           (TEXTLEN(reply_obj->objarray[d1.seq].ucr_actions) > 0) or
           (TEXTLEN(reply_obj->objarray[d1.seq].met_actions) > 0) or
           (cnvtint(reply_obj->objArray[d1.seq].acc_expire_cnt) > 0)
         )
  with format, separator = " "
 
endif
 
 
#exit_script
 
end
go
 

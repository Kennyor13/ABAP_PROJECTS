CLASS zcl_ps_project_versions DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_vsnmr_so TYPE RANGE OF vsnmr .
    TYPES:
      BEGIN OF  ty_object_costs_display,
        project         TYPE ps_pspid_edit,
        wbs             TYPE ps_posid_edit,
        network         TYPE aufnr,
        activity        TYPE vornr,
        description     TYPE char40,
        wrttp           TYPE wrttp,
        trgkz           TYPE bp_trgkz,
        gjahr           TYPE gjahr,
        versn           TYPE versn,
        twaer           TYPE twaer,
        plan_ver00      TYPE wertv8,
        act_ver00       TYPE wertv8,
        basic_st_ver00  TYPE datum,
        basic_fin_ver00 TYPE datum,
        sched_st_ver00  TYPE datum,
        sched_fin_ver00 TYPE datum,
        act_st_ver00    TYPE datum,
        act_fin_ver00   TYPE datum,
        plan_ver01      TYPE wertv8,
        act_ver01       TYPE wertv8,
        basic_st_ver01  TYPE datum,
        basic_fin_ver01 TYPE datum,
        sched_st_ver01  TYPE datum,
        sched_fin_ver01 TYPE datum,
        act_st_ver01    TYPE datum,
        act_fin_ver01   TYPE datum,
        plan_ver02      TYPE wertv8,
        act_ver02       TYPE wertv8,
        basic_st_ver02  TYPE datum,
        basic_fin_ver02 TYPE datum,
        sched_st_ver02  TYPE datum,
        sched_fin_ver02 TYPE datum,
        act_st_ver02    TYPE datum,
        act_fin_ver02   TYPE datum,
        plan_ver03      TYPE wertv8,
        act_ver03       TYPE wertv8,
        basic_st_ver03  TYPE datum,
        basic_fin_ver03 TYPE datum,
        sched_st_ver03  TYPE datum,
        sched_fin_ver03 TYPE datum,
        act_st_ver03    TYPE datum,
        act_fin_ver03   TYPE datum,
        plan_ver04      TYPE wertv8,
        act_ver04       TYPE wertv8,
        basic_st_ver04  TYPE datum,
        basic_fin_ver04 TYPE datum,
        sched_st_ver04  TYPE datum,
        sched_fin_ver04 TYPE datum,
        act_st_ver04    TYPE datum,
        act_fin_ver04   TYPE datum,
        plan_ver05      TYPE wertv8,
        act_ver05       TYPE wertv8,
        basic_st_ver05  TYPE datum,
        basic_fin_ver05 TYPE datum,
        sched_st_ver05  TYPE datum,
        sched_fin_ver05 TYPE datum,
        act_st_ver05    TYPE datum,
        act_fin_ver05   TYPE datum,
        plan_ver06      TYPE wertv8,
        act_ver06       TYPE wertv8,
        basic_st_ver06  TYPE datum,
        basic_fin_ver06 TYPE datum,
        sched_st_ver06  TYPE datum,
        sched_fin_ver06 TYPE datum,
        act_st_ver06    TYPE datum,
        act_fin_ver06   TYPE datum,
        plan_ver07      TYPE wertv8,
        act_ver07       TYPE wertv8,
        basic_st_ver07  TYPE datum,
        basic_fin_ver07 TYPE datum,
        sched_st_ver07  TYPE datum,
        sched_fin_ver07 TYPE datum,
        act_st_ver07    TYPE datum,
        act_fin_ver07   TYPE datum,
        plan_ver08      TYPE wertv8,
        act_ver08       TYPE wertv8,
        basic_st_ver08  TYPE datum,
        basic_fin_ver08 TYPE datum,
        sched_st_ver08  TYPE datum,
        sched_fin_ver08 TYPE datum,
        act_st_ver08    TYPE datum,
        act_fin_ver08   TYPE datum,
        plan_ver09      TYPE wertv8,
        act_ver09       TYPE wertv8,
        basic_st_ver09  TYPE datum,
        basic_fin_ver09 TYPE datum,
        sched_st_ver09  TYPE datum,
        sched_fin_ver09 TYPE datum,
        act_st_ver09    TYPE datum,
        act_fin_ver09   TYPE datum,
        plan_ver10      TYPE wertv8,
        act_ver10       TYPE wertv8,
        basic_st_ver10  TYPE datum,
        basic_fin_ver10 TYPE datum,
        sched_st_ver10  TYPE datum,
        sched_fin_ver10 TYPE datum,
        act_st_ver10    TYPE datum,
        act_fin_ver10   TYPE datum,
        plan_ver11      TYPE wertv8,
        act_ver11       TYPE wertv8,
        basic_st_ver11  TYPE datum,
        basic_fin_ver11 TYPE datum,
        sched_st_ver11  TYPE datum,
        sched_fin_ver11 TYPE datum,
        act_st_ver11    TYPE datum,
        act_fin_ver11   TYPE datum,
      END OF ty_object_costs_display .
    TYPES:
      ty_t_object_costs_display TYPE STANDARD TABLE OF ty_object_costs_display WITH DEFAULT KEY .

    METHODS constructor
      IMPORTING
        !iv_project  TYPE ps_pspid
        !iv_versions TYPE ty_vsnmr_so .
    METHODS create_tree_hierarchy .
    METHODS display .
    METHODS leave_program .
    METHODS prepare_alv .
    METHODS call_excel .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF  ty_subtree,
      node  TYPE lvc_nkey,
      value TYPE lvc_value,
    END OF ty_subtree .
  types:
    BEGIN OF  ty_object_costs,
      project     TYPE ps_pspid_edit,
      wbs         TYPE ps_posid_edit,
      order       TYPE aufnr,
      activity    TYPE vornr,
      version     TYPE vsnmr,
      description TYPE char40,
      object      TYPE j_objnr,
      wrttp       TYPE wrttp,
      trgkz       TYPE bp_trgkz,
      gjahr       TYPE gjahr,
      versn       TYPE versn,
      vorga       TYPE vorga,
      abkat       TYPE abkat,
      twaer       TYPE twaer,
      value       TYPE wertv8,
      basic_st    TYPE datum,
      basic_fin   TYPE datum,
      sched_st    TYPE datum,
      sched_fin   TYPE datum,
      act_st      TYPE datum,
      act_fin     TYPE datum,
    END OF ty_object_costs .
  types:
    ty_t_object_costs TYPE STANDARD TABLE OF ty_object_costs WITH DEFAULT KEY .
  types:
    ty_t_subtree      TYPE STANDARD TABLE OF ty_subtree WITH DEFAULT KEY .
  types:
    BEGIN OF  ty_objects,
      object      TYPE j_objnr,
      aufnr       TYPE aufnr,
      wbs         TYPE ps_pspid_edit,
      version     TYPE vsnmr,
      description TYPE char40,
      vornr       TYPE vornr,
      gstrp       TYPE datum,
      gltrp       TYPE datum,
      gstrs       TYPE datum,
      gltrs       TYPE datum,
      gstri       TYPE datum,
      getri       TYPE datum,
    END OF ty_objects .
  types:
    BEGIN OF ty_project_versions.
      INCLUDE TYPE c_projectwithversion.
TYPES END OF ty_project_versions .
  types:
    ty_t_project_versions TYPE STANDARD TABLE OF ty_project_versions WITH DEFAULT KEY .
  types:
    ty_t_hierarchie TYPE STANDARD TABLE OF bapi_wbs_hierarchie WITH DEFAULT KEY .
  types:
    ty_t_wbs_table TYPE STANDARD TABLE OF bapi_wbs_element_exp WITH DEFAULT KEY .

  data MV_PROJECT type PS_PSPID_EDIT .
  data MO_ALV_TREE type ref to CL_HRPAYNA_GUI_ALV_TREE .
  data MO_CUSTOM_CONTAINER type ref to CL_GUI_CUSTOM_CONTAINER .
  data MS_HIERARCHY_HEADER type TREEV_HHDR .
  data MT_PROJECT_VERSIONS type TY_T_PROJECT_VERSIONS .
  data MT_FIELDCATALOG type LVC_T_FCAT .
  data MT_OBJECT_COSTS_DISPLAY type TY_T_OBJECT_COSTS_DISPLAY .

  methods ADD_COMPLETE_LINE
    importing
      !IV_OBJECT_COSTS type TY_OBJECT_COSTS_DISPLAY
      !IV_RELAT_KEY type LVC_NKEY
    returning
      value(RV_NODE_KEY) type LVC_NKEY .
  methods ADD_FIRST_NODE
    importing
      !IV_NODE_TEXT type LVC_VALUE
      !IV_PROJECT_DESCRIPTION type CHAR40 .
  methods ADD_WBS
    importing
      !IV_WBS type C
      !IV_RELAT_KEY type LVC_NKEY
      !IV_WBS_DESCRIPTION type CHAR40
    returning
      value(RV_NODE_KEY) type LVC_NKEY .
  methods BUILD_ALV_TABLE
    importing
      !IT_OBJECT_COSTS type TY_T_OBJECT_COSTS
    returning
      value(RT_OBJECT_COSTS_DISPLAY) type TY_T_OBJECT_COSTS_DISPLAY .
  methods BUILD_FIELDCATALOG .
  methods BUILD_HIERARCHY_HEADER .
  methods GET_CORRECT_LINE
    importing
      !IS_HIERARCHIE type BAPI_WBS_HIERARCHIE
    returning
      value(RT_LINE) type LVC_NKEY .
  methods GET_OBJECT_COSTS
    returning
      value(RT_OBJECT_COSTS) type TY_T_OBJECT_COSTS .
  methods GET_PROJECT_STRUCTURE
    exporting
      value(ET_HIERARCHIE) type TY_T_HIERARCHIE
      !ET_WBS_TABLE type TY_T_WBS_TABLE
      !ES_PROJECT_INFO type BAPI_PROJECT_DEFINITION_EX .
  methods GET_PROJECT_VERSIONS
    importing
      !IV_VERSIONS type TY_VSNMR_SO .
  methods HANDLE_NODE_DOUBLE_CLICK
    for event NODE_DOUBLE_CLICK of CL_GUI_ALV_TREE
    importing
      !NODE_KEY
      !SENDER .
  methods REGISTER_EVENTS .
ENDCLASS.



CLASS ZCL_PS_PROJECT_VERSIONS IMPLEMENTATION.


  METHOD add_complete_line.
    DATA: l_node_text TYPE lvc_value.
    IF iv_object_costs-activity IS NOT INITIAL.
      l_node_text = |NWA { iv_object_costs-network ALPHA = OUT } { iv_object_costs-activity }|.
    ELSEIF iv_object_costs-network  IS INITIAL.
      l_node_text = |WBS { iv_object_costs-wbs }|.
    ELSEIF iv_object_costs-network+5(1) = '1'.
      l_node_text = |PRD { iv_object_costs-network ALPHA = OUT }|.
    ELSEIF iv_object_costs-network+5(1) = '5'.
      l_node_text = |NTW { iv_object_costs-network ALPHA = OUT }|.
    ENDIF.
    CALL METHOD mo_alv_tree->add_node
      EXPORTING
        i_relat_node_key = iv_relat_key
        i_relationship   = cl_gui_column_tree=>relat_last_child
        is_outtab_line   = iv_object_costs
        i_node_text      = l_node_text
      IMPORTING
        e_new_node_key   = rv_node_key.
  ENDMETHOD.


  METHOD add_first_node.
    DATA: ls_object_costs_display TYPE ty_object_costs_display.
    ls_object_costs_display-description = iv_project_description.
    CALL METHOD mo_alv_tree->add_node
      EXPORTING
        i_relat_node_key = ''
        i_relationship   = cl_gui_column_tree=>relat_last_child
        i_node_text      = iv_node_text
        is_outtab_line   = ls_object_costs_display
      IMPORTING
        e_new_node_key   = DATA(lv_node) ##NEEDED.
  ENDMETHOD.


  METHOD add_wbs.
    DATA: l_node_text             TYPE lvc_value,
          ls_object_costs_display TYPE ty_object_costs_display.

    l_node_text = iv_wbs.
    ls_object_costs_display-description = iv_wbs_description.

    CALL METHOD mo_alv_tree->add_node
      EXPORTING
        i_relat_node_key = iv_relat_key
        i_relationship   = cl_gui_column_tree=>relat_last_child
        i_node_text      = l_node_text
        is_outtab_line   = ls_object_costs_display
      IMPORTING
        e_new_node_key   = rv_node_key.
  ENDMETHOD.


  METHOD build_alv_table.
    DATA counter    TYPE num2.

    LOOP AT mt_project_versions INTO DATA(ls_projectversions). "kan dit niet slimmer...?
      DATA(plan)      = |LS_OBJECT_DISPLAY-PLAN_VER{ counter }|.
      DATA(act)       = |LS_OBJECT_DISPLAY-ACT_VER{ counter }|.
      DATA(basic_st)  = |LS_OBJECT_DISPLAY-BASIC_ST_VER{ counter }|.
      DATA(basic_fin) = |LS_OBJECT_DISPLAY-BASIC_FIN_VER{ counter }|.
      DATA(sched_st)  = |LS_OBJECT_DISPLAY-SCHED_ST_VER{ counter }|.
      DATA(sched_fin) = |LS_OBJECT_DISPLAY-SCHED_FIN_VER{ counter }|.
      DATA(act_st)    = |LS_OBJECT_DISPLAY-ACT_ST_VER{ counter }|.
      DATA(act_fin)   = |LS_OBJECT_DISPLAY-ACT_FIN_VER{ counter }|.
      ASSIGN (plan) TO FIELD-SYMBOL(<fs_plan>).
      ASSIGN (act) TO FIELD-SYMBOL(<fs_act>).
      ASSIGN (basic_st) TO FIELD-SYMBOL(<fs_basic_st>).
      ASSIGN (basic_fin) TO FIELD-SYMBOL(<fs_basic_fin>).
      ASSIGN (sched_st) TO FIELD-SYMBOL(<fs_sched_st>).
      ASSIGN (sched_fin) TO FIELD-SYMBOL(<fs_sched_fin>).
      ASSIGN (act_st) TO FIELD-SYMBOL(<fs_act_st>).
      ASSIGN (act_fin) TO FIELD-SYMBOL(<fs_act_fin>).

      LOOP AT it_object_costs INTO DATA(ls_objectcosts) WHERE version = ls_projectversions-version.

        IF line_exists( rt_object_costs_display[ wbs = ls_objectcosts-wbs  network = ls_objectcosts-order activity = ls_objectcosts-activity ] ).
          LOOP AT rt_object_costs_display INTO DATA(ls_object_display) WHERE wbs = ls_objectcosts-wbs AND network = ls_objectcosts-order  AND activity = ls_objectcosts-activity.
            DATA(ind) = sy-tabix.
          ENDLOOP.
        ENDIF.
        CASE ls_objectcosts-wrttp.
          WHEN '01'. "Plan
            IF ls_objectcosts-order IS NOT INITIAL.
              <fs_plan> = <fs_plan> + ls_objectcosts-value. "Add value in case of costs over multiple years.
            ENDIF.
          WHEN '04'. "Actual
*            IF ls_objectcosts-vorga NE 'KOAO'.
            <fs_act> = <fs_act> + ls_objectcosts-value. "Add value in case of costs over multiple years.
*            ENDIF.
          WHEN '29'. " Value from Sales Order
            <fs_plan> = - ls_objectcosts-value. "Add value in case of costs over multiple years.
          WHEN '32'. "Results Analysis and WIP Calculation
            CASE ls_objectcosts-abkat.
              WHEN '63'.
                "Cost of Sales with Requirement to Capitalize            COSR
                <fs_act> = <fs_act> + ls_objectcosts-value.
              WHEN '67'.
                "CLRV	Calculated Revenue                                 CLRV
                <fs_act> = <fs_act> + ls_objectcosts-value.
              WHEN '70'.
                "Calculated Profit (POC Method)                          POCP
                <fs_act> = <fs_act> - ls_objectcosts-value.
              WHEN '73'.
                "Valuated Costs                                          VLCO
                <fs_act> = <fs_act> - ls_objectcosts-value.
              WHEN '75'.
                "Valuated Actual Revenue/Revenue Affecting Net Income    VLRV
                <fs_act> = <fs_act> - ls_objectcosts-value.
              WHEN '81'.
                "Planned Revenue of Valuation                            PLRV
                "do nothing
              WHEN '82'.
                "Planned Costs of Valuation                              PLCV
                "do nothing
              WHEN OTHERS.
                "do nothing
            ENDCASE.
        ENDCASE.
        ls_object_display-project       = ls_objectcosts-project.
        ls_object_display-description   = ls_objectcosts-description.
        ls_object_display-wbs           = ls_objectcosts-wbs.
        ls_object_display-network       = ls_objectcosts-order.
        ls_object_display-activity      = ls_objectcosts-activity.
        ls_object_display-wrttp         = ls_objectcosts-wrttp.
        ls_object_display-trgkz         = ls_objectcosts-trgkz.
        ls_object_display-gjahr         = ls_objectcosts-gjahr.
        ls_object_display-versn         = ls_objectcosts-versn.
        ls_object_display-twaer         = ls_objectcosts-twaer.
        <fs_basic_st>                   = ls_objectcosts-basic_st.
        <fs_basic_fin>                  = ls_objectcosts-basic_fin.
        <fs_sched_st>                   = ls_objectcosts-sched_st.
        <fs_sched_fin>                  = ls_objectcosts-sched_fin.
        <fs_act_st>                     = ls_objectcosts-act_st.
        <fs_act_fin>                    = ls_objectcosts-act_fin.
        IF line_exists( rt_object_costs_display[ wbs = ls_objectcosts-wbs  network = ls_objectcosts-order activity = ls_objectcosts-activity ] ).
          MODIFY rt_object_costs_display FROM ls_object_display INDEX ind.
        ELSE.
          APPEND ls_object_display TO rt_object_costs_display.
        ENDIF.
        CLEAR ls_object_display.
      ENDLOOP.
      counter = counter + 1.
    ENDLOOP.
  ENDMETHOD.


  METHOD build_fieldcatalog.
    DATA: counter     TYPE lvc_colpos,
          c_plan_ver  TYPE lvc_colpos,
          c_act_ver   TYPE lvc_colpos,
          c_basic_st  TYPE lvc_colpos,
          c_basic_fin TYPE lvc_colpos,
          c_sched_st  TYPE lvc_colpos,
          c_sched_fin TYPE lvc_colpos,
          c_act_st    TYPE lvc_colpos,
          c_act_fin   TYPE lvc_colpos.

    DATA(lines) =  lines( mt_project_versions ).

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZZS_OBJECT_COSTS_DISPLAY'
      CHANGING
        ct_fieldcat      = mt_fieldcatalog.

    LOOP AT mt_fieldcatalog  ASSIGNING FIELD-SYMBOL(<ls_fieldcatalog>).
      <ls_fieldcatalog>-colddictxt = 'L'.
      CLEAR <ls_fieldcatalog>-no_out.
      CASE <ls_fieldcatalog>-fieldname(7).
        WHEN  'PLAN_VE'.
          c_plan_ver = c_plan_ver + 1.
          counter = c_plan_ver.
          DATA(type) = 'Plan' ##NO_TEXT.
        WHEN  'ACT_VER'.
          c_act_ver = c_act_ver + 1.
          counter = c_act_ver.
          type = 'Actual' ##NO_TEXT.
        WHEN 'BASIC_S'.
          c_basic_st = c_basic_st + 1.
          counter = c_basic_st.
          type = 'Basic Start' ##NO_TEXT.
          <ls_fieldcatalog>-outputlen = 20.
        WHEN 'BASIC_F'.
          c_basic_fin = c_basic_fin + 1.
          counter = c_basic_fin.
          type = 'Basic Finish' ##NO_TEXT.
          <ls_fieldcatalog>-outputlen = 20.
        WHEN 'SCHED_S'.
          c_sched_st = c_sched_st + 1.
          counter = c_sched_st.
          type = 'Sched Start' ##NO_TEXT.
          <ls_fieldcatalog>-outputlen = 20.
        WHEN 'SCHED_F'.
          c_sched_fin = c_sched_fin + 1.
          counter = c_sched_fin.
          type = 'Sched finish' ##NO_TEXT.
          <ls_fieldcatalog>-outputlen = 20.
        WHEN 'ACT_ST_'.
          c_act_st = c_act_st + 1.
          counter = c_act_st.
          type = 'Actual Start' ##NO_TEXT.
          <ls_fieldcatalog>-outputlen = 20.
        WHEN 'ACT_FIN'.
          c_act_fin = c_act_fin + 1.
          counter = c_act_fin.
          type = 'Actual Finish' ##NO_TEXT.
          <ls_fieldcatalog>-outputlen = 20.
        WHEN 'DESCRIP'.
          type = 'Description' ##NO_TEXT.
          <ls_fieldcatalog>-scrtext_l = 'Description' ##NO_TEXT.
          <ls_fieldcatalog>-scrtext_m = 'Description' ##NO_TEXT.
          <ls_fieldcatalog>-scrtext_s = 'Description' ##NO_TEXT.
          <ls_fieldcatalog>-outputlen = 40.
        WHEN OTHERS.
          <ls_fieldcatalog>-no_out = 'X'.
          CONTINUE.
      ENDCASE.
      IF counter > lines.
        <ls_fieldcatalog>-no_out = 'X'.
      ELSEIF <ls_fieldcatalog>-fieldname ne 'DESCRIPTION' .
        READ TABLE mt_project_versions INTO DATA(ls_projectversion) INDEX counter.
        <ls_fieldcatalog>-scrtext_l = COND #( WHEN ls_projectversion-version IS INITIAL THEN
                                                    |Curr. { type }| ##NO_TEXT
                                                    ELSE
                                                    |{ ls_projectversion-version } { ls_projectversion-projectversiondescription } { type }| ).
        <ls_fieldcatalog>-scrtext_m = COND #( WHEN ls_projectversion-version IS INITIAL THEN
                                                    |Curr. { type }| ##NO_TEXT
                                                    ELSE
                                                    |{ type } { ls_projectversion-projectversiondescription }| ).
        <ls_fieldcatalog>-scrtext_s = COND #( WHEN ls_projectversion-version IS INITIAL THEN
                                                    |Curr. { type }| ##NO_TEXT
                                                    ELSE
                                                    |{ ls_projectversion-version } { type }| ).
        <ls_fieldcatalog>-do_sum = 'X'.
        <ls_fieldcatalog>-h_ftype = 'SUM'.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.


  METHOD build_hierarchy_header.
    ms_hierarchy_header-heading = 'WBS structure' ##NO_TEXT.
    ms_hierarchy_header-tooltip = 'Plan/actuals' ##NO_TEXT.
    ms_hierarchy_header-width = 35.
    ms_hierarchy_header-width_pix = ''.
  ENDMETHOD.


  METHOD call_excel.
    CALL FUNCTION 'ZXLWB_CALLFORM' ##FM_SUBRC_OK
      EXPORTING
        iv_formname        = 'Z_PS_REPORT'
        iv_context_ref     = mo_alv_tree
      EXCEPTIONS
        process_terminated = 1
        OTHERS             = 2.
  ENDMETHOD.


  METHOD constructor.
    mv_project = iv_project.
    get_project_versions( iv_versions ).
  ENDMETHOD.


  METHOD create_tree_hierarchy.
    DATA: l_lvl_key      TYPE lvc_nkey,
          lt_subtree_val TYPE ty_t_subtree,
          lv_project     TYPE lvc_value.

    DATA(lt_object_costs) = get_object_costs( ).
    DATA(lt_object_costs_display) = build_alv_table( lt_object_costs ).
    get_project_structure( IMPORTING
                              et_hierarchie   = DATA(lt_hierarchie)
                              et_wbs_table    = DATA(lt_wbs)
                              es_project_info = DATA(ls_project) ).

    lv_project = mv_project.
    add_first_node( iv_node_text = lv_project iv_project_description = ls_project-description ). " First node.
    APPEND VALUE #( node = 1 value = lv_project ) TO lt_subtree_val." build table i couldnt fetch from CL_HRPAYNA_GUI_ALV_TREE

    WHILE lines( lt_subtree_val ) < lines( lt_hierarchie ).
      LOOP AT lt_hierarchie INTO DATA(ls_hierarchie).
        IF ls_hierarchie-up IS INITIAL.
          l_lvl_key = 1.
          DATA(l_wbs_key) = add_wbs( iv_wbs = ls_hierarchie-wbs_element iv_wbs_description = lt_wbs[ wbs_element = ls_hierarchie-wbs_element ]-description iv_relat_key = l_lvl_key ). "second node(s).
          APPEND VALUE #( node = l_wbs_key value = ls_hierarchie-wbs_element ) TO lt_subtree_val. " build table i couldnt fetch from CL_HRPAYNA_GUI_ALV_TREE
        ELSEIF line_exists( lt_subtree_val[ value = ls_hierarchie-wbs_element ] ). "first check already filled table.
          DATA(ls_subtree_val) =  lt_subtree_val[ value = ls_hierarchie-wbs_element ] .
          l_wbs_key = add_wbs( iv_wbs = ls_subtree_val-value iv_wbs_description = lt_wbs[ wbs_element = ls_hierarchie-wbs_element ]-description iv_relat_key = ls_subtree_val-node ).
        ELSE. "if UP is filled, and it's not yet in the subtree there, get the correct line
          DATA(lv_line) = get_correct_line( ls_hierarchie ).
          IF lv_line IS INITIAL.
            CONTINUE. " try again in next loop.
          ENDIF.
          l_wbs_key = add_wbs( iv_wbs = ls_hierarchie-wbs_element iv_wbs_description = lt_wbs[ wbs_element = ls_hierarchie-wbs_element ]-description iv_relat_key = lv_line ).
          APPEND VALUE #( node = l_wbs_key value = ls_hierarchie-wbs_element ) TO lt_subtree_val. " build table i couldnt fetch from CL_HRPAYNA_GUI_ALV_TREE
        ENDIF.
      ENDLOOP.
    ENDWHILE.

    "add the cost object lines to the correct node
    LOOP AT lt_object_costs_display INTO DATA(ls_object_costs).
      IF  line_exists( lt_subtree_val[ value =  ls_object_costs-wbs ] ). "Check if line is there.
        ls_subtree_val =  lt_subtree_val[ value = ls_object_costs-wbs ] .
        DATA(l_last_key) = add_complete_line( EXPORTING  iv_object_costs = ls_object_costs iv_relat_key = ls_subtree_val-node ) ##NEEDED.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD display.
    register_events( ).

    CALL METHOD mo_alv_tree->update_calculations.

    CALL METHOD mo_alv_tree->frontend_update.
  ENDMETHOD.


  METHOD get_correct_line.
    mo_alv_tree->get_subtree(
    EXPORTING
      i_node_key       = cl_alv_tree_base=>c_virtual_root_node
    IMPORTING
      et_subtree_nodes = DATA(lt_subtree)
    EXCEPTIONS
      node_key_not_found = 1
      OTHERS = 2 ) ##SUBRC_OK.

    LOOP AT lt_subtree INTO DATA(ls_subtree). "pretty stupid, but i can't get a table with value text from this class.
      mo_alv_tree->get_outtab_line(
      EXPORTING
         i_node_key     = ls_subtree
       IMPORTING
         e_node_text    = DATA(lv_wbs_text)
       EXCEPTIONS
         node_not_found = 1
         OTHERS         = 2 ) ##SUBRC_OK.
      IF lv_wbs_text = is_hierarchie-up.
        rt_line = ls_subtree.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_object_costs.
* Select data
    DATA: lt_objects TYPE TABLE OF ty_objects.
* get all objects assigned to the versions

    SELECT SINGLE pspnr FROM proj INTO @DATA(lv_project_internal) WHERE pspid = @mv_project.

    "all WBS elements with versions
    SELECT wbselementobject AS object, wbselement AS wbs, wbsdescription AS description, version, project, wbselementinternalid, basicstartdate AS gstrp, basicenddate AS gltrp, schedldbasicearlieststartdate AS gstrs,
                    schedldbasicearliestenddate AS gltrs, schedldbasiclateststartdate, scheduledbasiclatestenddate, actualstartdate AS gstri, actualenddate AS getri
      FROM c_wbselementwithversion
      INTO TABLE @DATA(lt_wbs_objects)
      FOR ALL ENTRIES IN @mt_project_versions
      WHERE project = @mt_project_versions-project
      AND version =  @mt_project_versions-version.

    MOVE-CORRESPONDING lt_wbs_objects TO lt_objects KEEPING TARGET LINES.

    " all Networks
    SELECT  objnr AS object, aufnr, plnbez AS description, pronr, pspel AS wbs, gstrp, gltrp, gstrs, gltrs, gstri, getri
      FROM caufv
      INTO TABLE @DATA(lt_network)
      WHERE pronr = @lv_project_internal.

    MOVE-CORRESPONDING lt_network TO lt_objects KEEPING TARGET LINES.

    " all Networks with versions
    SELECT  objnr AS object, aufnr, pronr, plnbez AS description, vsnmr, pspel AS wbs, gstrp, gltrp, gstrs, gltrs, gstri, getri
      FROM vscaufv_cn
     INTO TABLE @DATA(lt_network_vers)
     WHERE pronr = @lv_project_internal.

    MOVE-CORRESPONDING lt_network_vers TO lt_objects KEEPING TARGET LINES.

    " all Networks activities with versions
    SELECT  networkactivityobject AS object, projectnetwork AS aufnr, networkactivitydescription AS description, networkactivity AS vornr, superiorntwkactivityexternalid, project, version, wbselement AS wbs, actualstartdate AS gstri, actualenddate AS
getri
      FROM c_networkactivitywithversion
      INTO TABLE @DATA(lt_netw_act_objects)
      FOR ALL ENTRIES IN @mt_project_versions
    WHERE project = @mt_project_versions-project
    AND version =  @mt_project_versions-version.

    MOVE-CORRESPONDING  lt_netw_act_objects TO lt_objects KEEPING TARGET LINES.

* get all production orders assigned to the versions
    SELECT  vsaufk_cn~objnr AS object, vsafpo_cn~matnr AS description, vsaufk_cn~aufnr, vsaufk_cn~vsnmr, proj~pspid , prps~posid AS wbs, gstrp, gltrp, gstrs, gltrs, gstri, getri  FROM vsaufk_cn
                                                                                                              INNER JOIN vsafpo_cn ON ( vsaufk_cn~aufnr = vsafpo_cn~aufnr AND
                                                                                                                                        vsaufk_cn~vsnmr = vsafpo_cn~vsnmr )
                                                                                                              INNER JOIN vscaufv_cn ON ( vsaufk_cn~aufnr = vscaufv_cn~aufnr AND
                                                                                                                                         vsaufk_cn~vsnmr = vscaufv_cn~vsnmr )
                                                                                                              INNER JOIN prps      ON ( vsafpo_cn~projn = prps~pspnr )
                                                                                                              INNER JOIN proj      ON ( prps~psphi = proj~pspnr )
    INTO TABLE @DATA(lt_production_orders_vers)
    FOR ALL ENTRIES IN @lt_wbs_objects
    WHERE vsafpo_cn~projn = @lt_wbs_objects-wbselementinternalid
    AND vsafpo_cn~vsnmr =  @lt_wbs_objects-version.

    MOVE-CORRESPONDING  lt_production_orders_vers TO lt_objects KEEPING TARGET LINES.

* get all production orders without version
    SELECT  aufk~objnr AS object, aufk~aufnr, afpo~matnr AS description, proj~pspid, prps~posid AS wbs, gstrp, gltrp, gstrs, gltrs, gstri, getri  FROM aufk INNER JOIN afpo ON ( aufk~aufnr = afpo~aufnr )
                                                                              INNER JOIN caufv ON ( aufk~aufnr = caufv~aufnr )
                                                                              INNER JOIN prps ON ( afpo~projn = prps~pspnr )
                                                                              INNER JOIN proj ON ( prps~psphi = proj~pspnr )
    INTO TABLE @DATA(lt_production_orders)
    FOR ALL ENTRIES IN @lt_wbs_objects
    WHERE afpo~projn = @lt_wbs_objects-wbselementinternalid.
*    AND aufk~autyp NE '20'.

    MOVE-CORRESPONDING lt_production_orders TO lt_objects KEEPING TARGET LINES.
    SORT lt_objects.
    DELETE ADJACENT DUPLICATES FROM lt_objects COMPARING object.
*get all costs assigned to the objects per year
    SELECT * FROM rpsco INTO TABLE @DATA(lt_rpsco)
    FOR ALL ENTRIES IN @lt_objects
    WHERE objnr =  @lt_objects-object.

    LOOP AT lt_rpsco ASSIGNING FIELD-SYMBOL(<fs_rpsco>).

      APPEND VALUE #( project     = mv_project
                      wbs         = lt_objects[ object = <fs_rpsco>-objnr ]-wbs
                      version     = lt_objects[ object = <fs_rpsco>-objnr ]-version
                      description = lt_objects[ object = <fs_rpsco>-objnr ]-description
                      order       = lt_objects[ object = <fs_rpsco>-objnr ]-aufnr
                      activity    = lt_objects[ object = <fs_rpsco>-objnr ]-vornr
                      object      = <fs_rpsco>-objnr
                      wrttp       = <fs_rpsco>-wrttp
                      trgkz       = <fs_rpsco>-trgkz
                      vorga       = <fs_rpsco>-vorga
                      abkat       = <fs_rpsco>-abkat
                      gjahr       = <fs_rpsco>-gjahr
                      versn       = <fs_rpsco>-versn
                      twaer       = <fs_rpsco>-twaer
                      value       = ( <fs_rpsco>-wlp00 +
                                   <fs_rpsco>-wlp01 +
                                   <fs_rpsco>-wlp02 +
                                   <fs_rpsco>-wlp03 +
                                   <fs_rpsco>-wlp04 +
                                   <fs_rpsco>-wlp05 +
                                   <fs_rpsco>-wlp06 +
                                   <fs_rpsco>-wlp07 +
                                   <fs_rpsco>-wlp08 +
                                   <fs_rpsco>-wlp09 +
                                   <fs_rpsco>-wlp10 +
                                   <fs_rpsco>-wlp11 +
                                   <fs_rpsco>-wlp12 )
                      basic_st  = lt_objects[ object = <fs_rpsco>-objnr ]-gstrp
                      basic_fin = lt_objects[ object = <fs_rpsco>-objnr ]-gltrp
                      sched_st  = lt_objects[ object = <fs_rpsco>-objnr ]-gstrs
                      sched_fin = lt_objects[ object = <fs_rpsco>-objnr ]-gltrs
                      act_st    = lt_objects[ object = <fs_rpsco>-objnr ]-gstri
                      act_fin   = lt_objects[ object = <fs_rpsco>-objnr ]-getri   ) TO rt_object_costs. "append all costs objects

    ENDLOOP.

  ENDMETHOD.


  METHOD get_project_structure.
    CALL FUNCTION 'BAPI_PROJECT_GETINFO'
      EXPORTING
        project_definition     = mv_project
        with_activities        = 'X'
      IMPORTING
        e_project_definition   = es_project_info
      TABLES
        e_wbs_hierarchie_table = et_hierarchie
        e_wbs_element_table    = et_wbs_table.
  ENDMETHOD.


  METHOD get_project_versions.
    SELECT DISTINCT projectobject, project, version, projectversiondescription FROM c_projectwithversion
      INTO CORRESPONDING FIELDS OF TABLE @mt_project_versions
      WHERE project = @mv_project
      AND version IN @iv_versions
      ORDER BY version.
  ENDMETHOD.


  METHOD handle_node_double_click.
    DATA: lt_children TYPE lvc_t_nkey.

    CALL METHOD sender->get_children
      EXPORTING
        i_node_key  = node_key
      IMPORTING
        et_children = lt_children.

    IF NOT lt_children IS INITIAL.

      CALL METHOD sender->expand_node
        EXPORTING
          i_node_key    = node_key
          i_level_count = 2.
    ENDIF.
  ENDMETHOD.


  METHOD leave_program.
    CALL METHOD mo_custom_container->free.
  ENDMETHOD.


  METHOD prepare_alv.
    DATA(l_tree_container_name) = 'CCONTAINER1'.

    CREATE OBJECT mo_custom_container
      EXPORTING
        container_name              = l_tree_container_name
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc <> 0.
      MESSAGE x208(00) WITH 'ERROR' ##NO_TEXT.
    ENDIF.

* create tree control
    CREATE OBJECT mo_alv_tree
      EXPORTING
        parent                      = mo_custom_container
        node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
        item_selection              = 'X'
        no_html_header              = 'X'
        no_toolbar                  = ''
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        illegal_node_selection_mode = 5
        failed                      = 6
        illegal_column_name         = 7.

    IF sy-subrc <> 0.
      MESSAGE x208(00) WITH 'ERROR'.                        "#EC NOTEXT
    ENDIF.

    build_hierarchy_header( ).

    build_fieldcatalog( ).

    CALL METHOD mo_alv_tree->set_table_for_first_display
      EXPORTING
        is_hierarchy_header = ms_hierarchy_header
      CHANGING
        it_fieldcatalog     = mt_fieldcatalog
        it_outtab           = mt_object_costs_display.


  ENDMETHOD.


  METHOD register_events.
    DATA: lt_events TYPE cntl_simple_events,
          l_event   TYPE cntl_simple_event.

    CALL METHOD mo_alv_tree->get_registered_events
      IMPORTING
        events = lt_events.

    l_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
    APPEND l_event TO lt_events.

    CALL METHOD mo_alv_tree->set_registered_events
      EXPORTING
        events                    = lt_events
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3.

    IF sy-subrc <> 0.
      MESSAGE x208(00) WITH 'ERROR'.                        "#EC NOTEXT
    ENDIF.

    SET HANDLER handle_node_double_click FOR mo_alv_tree.
  ENDMETHOD.
ENDCLASS.

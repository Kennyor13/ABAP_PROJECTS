*&---------------------------------------------------------------------*
*& Report ZPS_PROJECT_VERSIONS_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zps_project_versions_report.
CLASS cl_gui_cfw DEFINITION LOAD.
DATA: save_ok          LIKE sy-ucomm,
      ok_code          LIKE sy-ucomm,
      p_vsnmr          TYPE vsnmr,
      g_project_report TYPE REF TO zcl_ps_project_versions.

SELECTION-SCREEN BEGIN OF BLOCK b01
  WITH FRAME.
  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME .
    PARAMETERS : p_proj LIKE proj-pspid OBLIGATORY.
    SELECT-OPTIONS: s_vers FOR p_vsnmr NO INTERVALS.
    SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-001.
      SELECTION-SCREEN COMMENT 1(71) TEXT-002.
      SELECTION-SCREEN SKIP.
      SELECTION-SCREEN COMMENT 1(71) TEXT-003.
      SELECTION-SCREEN SKIP.
      SELECTION-SCREEN COMMENT 1(71) TEXT-004.
      SELECTION-SCREEN SKIP.
      SELECTION-SCREEN COMMENT 1(71) TEXT-005.
    SELECTION-SCREEN END OF BLOCK b2.
  SELECTION-SCREEN END OF BLOCK b1 .

SELECTION-SCREEN END OF BLOCK b01.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_vers-low.

  PERFORM add_options USING 'S_VERS-LOW'.

START-OF-SELECTION.

AT SELECTION-SCREEN.

  PERFORM check_selection.

END-OF-SELECTION.

  CALL SCREEN 100.

MODULE pbo OUTPUT.
  DATA: g_alv_tree       TYPE REF TO cl_hrpayna_gui_alv_tree.

  SET PF-STATUS 'MAIN100'.
  SET TITLEBAR 'MAINTITLE'.

  IF g_alv_tree IS INITIAL.

    CREATE OBJECT g_project_report EXPORTING iv_project = p_proj iv_versions = s_vers[].

    g_project_report->prepare_alv(  ).

    g_project_report->create_tree_hierarchy(  ).

    g_project_report->display(  ).

    CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
        cntl_system_error = 1
        cntl_error        = 2.

    IF sy-subrc NE 0.
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = 'Automation Queue failure' ##no_text
          txt1  = 'Internal error:' ##no_text
          txt2  = 'A method in the automation queue' ##no_text
          txt3  = 'caused a failure.' ##no_text.
    ENDIF.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
*       process after input
*----------------------------------------------------------------------*
MODULE pai INPUT.
  save_ok = ok_code.
  CLEAR ok_code.
  CASE save_ok.
    WHEN 'EXIT' OR 'BACK' OR 'CANC'.
      g_project_report->leave_program( ).
      LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
    WHEN 'EXCEL'.
      g_project_report->call_excel(  ).
      LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0100.
    WHEN OTHERS.
      CALL METHOD cl_gui_cfw=>dispatch IMPORTING return_code = DATA(return_code).
      SET SCREEN 100.
  ENDCASE.
*  CALL METHOD cl_gui_cfw=>flush.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form add_options
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM add_options  USING string TYPE help_info-dynprofld.
  DATA : dynpread TYPE TABLE OF dynpread WITH HEADER LINE.

  REFRESH dynpread.
  CLEAR dynpread.
  dynpread-fieldname = 'P_PROJ'.
  APPEND dynpread.
  CLEAR dynpread.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-repid
      dynumb               = '1000'
    TABLES
      dynpfields           = dynpread
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.

  IF sy-subrc IS INITIAL.
    READ TABLE dynpread WITH KEY fieldname = 'P_PROJ'.
    TRANSLATE dynpread-fieldvalue TO UPPER CASE.
  ENDIF.

  SELECT DISTINCT project, version, projectversiondescription FROM c_projectwithversion
  INTO TABLE @DATA(mt_project_versions)
  WHERE project = @dynpread-fieldvalue
  ORDER BY version.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST' ##FM_SUBRC_OK
    EXPORTING
      retfield        = 'VERSION'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = string
      value_org       = 'S'
    TABLES
      value_tab       = mt_project_versions
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_selection
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_selection .
  SELECT SINGLE pspid FROM proj WHERE  pspid = @p_proj INTO @DATA(lv_project).
  IF sy-subrc NE 0.
    MESSAGE |Project { p_proj } does not exist| TYPE 'E'.
    RETURN.
  ENDIF.

  DATA versions TYPE RANGE OF vsnmr.
  LOOP AT s_vers INTO DATA(lv_options).
    SELECT DISTINCT project, version, projectversiondescription FROM c_projectwithversion
    INTO TABLE @DATA(mt_project_versions)
    WHERE project = @p_proj
      AND version = @lv_options-low.
    IF sy-subrc NE 0.
      DATA(error_flag) = abap_true.
      versions = VALUE #( BASE versions ( low = lv_options-low ) ).
    ENDIF.
  ENDLOOP.

  IF error_flag = abap_true.
    DATA(lines) = lines( versions ).
    IF lines > 1.
      DATA(version_string) = ||.
      DATA(komma) = |,|.
      LOOP AT versions INTO DATA(ls_versions).
        IF sy-tabix = 1.
          version_string = |{ ls_versions-low }, |.
        ELSEIF sy-tabix < lines.
          version_string = |{ version_string }{ ls_versions-low }, |.
        ELSE.
          version_string = |{ version_string }{ ls_versions-low }|.
        ENDIF.
      ENDLOOP.
    ELSE.
      version_string = | { versions[ 1 ]-low } |.
    ENDIF.
    MESSAGE |Project version(s) { version_string } do(es) not exist in project { p_proj }| TYPE 'E'.
  ENDIF.
ENDFORM.

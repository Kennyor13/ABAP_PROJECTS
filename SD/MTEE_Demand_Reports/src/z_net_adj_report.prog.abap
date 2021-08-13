*&---------------------------------------------------------------------*
*& Report  Z_NET_ADJ_REPORT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report  z_net_adj_report.

include z_net_types.

** ALV
define add_field.
  wa_field_tab-fieldname = &1.
  wa_field_tab-reptext_ddic = &2.
*  wa_field_tab-coltext = &2.
  wa_field_tab-no_zero = &3.
  append wa_field_tab to field_tab.
end-of-definition.

************************************************************************
* OBJECT VARIABLES
************************************************************************
* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
class lcl_event_receiver definition deferred.

data: o_alv_main        type ref to cl_gui_alv_grid,
      o_cont_alv_main   type ref to cl_gui_custom_container,
      o_error_container type ref to cl_gui_custom_container,
      o_event_receiver  type ref to lcl_event_receiver,
      o_protocol        type ref to cl_alv_changed_data_protocol.

*-----------------------------------------------------------------------

selection-screen begin of block b01
                 with frame.
*parameter: p_datum like syst-datum obligatory default sy-datum.
parameters: p_werks like marc-werks obligatory.
select-options: s_matnr for marc-matnr.
select-options: s_lgort for mard-lgort.
select-options: s_dispo for marc-dispo.
select-options: s_work  for p_work.
selection-screen skip.
selection-screen begin of line.
selection-screen comment 18(15) text-010 for field p_plant.
parameters : p_plant radiobutton group rb1.
selection-screen comment 46(12) text-011 for field p_cross.
parameters : p_cross radiobutton group rb1.
selection-screen end of line.
selection-screen skip.
select-options: s_yweek for p_yweek .
select-options: s_ymonth for p_ymonth .
selection-screen skip 2.
*selection-screen skip.
selection-screen begin of block b1 with frame title text-009.
selection-screen comment 1(75) text-001.
selection-screen skip.
selection-screen comment 1(75) text-002.
*selection-screen skip.
*selection-screen comment 1(75) text-003.
selection-screen skip.
selection-screen comment 1(75) text-004.
selection-screen skip.
selection-screen comment 1(75) text-005.
*selection-screen skip.
*selection-screen comment 1(75) text-005.
selection-screen skip.
selection-screen comment 1(75) text-006.
selection-screen skip.
selection-screen comment 1(75) text-012.
selection-screen skip.
selection-screen comment 1(75) text-013.
selection-screen skip.
selection-screen comment 1(75) text-014.
*selection-screen skip.
selection-screen end of block b1.
*select-options: s_week for p_week obligatory.


selection-screen end of block b01.

************************************************************************
* CLASSES DEFINITION
************************************************************************
class lcl_event_receiver definition.

  public section.

    methods:

      handle_top_of_page
        for event print_top_of_page of cl_gui_alv_grid,

      handle_end_of_list
        for event print_end_of_list of cl_gui_alv_grid,

      handle_double_click
                    for event double_click of cl_gui_alv_grid
        importing e_row e_column,

      handle_toolbar
                    for event toolbar of cl_gui_alv_grid
        importing e_object e_interactive,

      handle_user_command
                    for event user_command of cl_gui_alv_grid
        importing e_ucomm,

      handle_data_changed
                    for event data_changed of cl_gui_alv_grid
        importing er_data_changed.

endclass.                    "lcl_event_receiver DEFINITION

************************************************************************
* CLASSES IMPLEMENTATION
************************************************************************
class lcl_event_receiver implementation.

  method handle_double_click.
    perform handle_double_click
            using e_row
                  e_column.
  endmethod.                    "handle_double_click

  method handle_top_of_page.
  endmethod.                    "handle_top_of_page

  method handle_end_of_list.
  endmethod.                    "handle_end_of_list

  method handle_toolbar.
    perform create_toolbar
            changing e_object
                     e_interactive.
  endmethod.                    "handle_toolbar

  method handle_user_command.
    perform handle_alv_user_command
            using e_ucomm.
  endmethod.                    "handle_user_command

  method handle_data_changed.
    perform alv_data_changed
            using er_data_changed.
  endmethod.                    "handle_data_changed

endclass.                    "lcl_event_receiver IMPLEMENTATION

*-----------------------------------------------------------------------

start-of-selection.

  perform set_header.

  perform set_selection.

  check i_exit ne 'X'.

  perform get_mrp.

  perform show_output.

end-of-selection.

*-----------------------------------------------------------------------



*&--------------------------------------------------------------------*
*&      Form  show_output
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
form show_output.

  call screen 100.

endform.                    "show_output

*&--------------------------------------------------------------------*
*&      Form  handle_double_click
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->XS_ROW     text
*      -->XS_COLUMN  text
*---------------------------------------------------------------------*
form handle_double_click
     using xs_row    type lvc_s_row
           xs_column type lvc_s_col.

endform.                    "handle_double_click

*&--------------------------------------------------------------------*
*&      Form  create_toolbar
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->XY_OBJECT  text
*      -->XF_INTERACTtext
*---------------------------------------------------------------------*
form create_toolbar
     changing xy_object      type ref to cl_alv_event_toolbar_set
              xf_interactive type char01.

  data: ls_toolbar type stb_button.

** Separator in toolbar
*  CLEAR ls_toolbar.
*  MOVE 3 TO ls_toolbar-butn_type.
*  APPEND ls_toolbar TO xy_object->mt_toolbar.
*
** Define first button
*  CLEAR ls_toolbar.
*  MOVE: 'SELALL' TO ls_toolbar-function,
*        icon_select_all TO ls_toolbar-icon,
*        cntb_btype_button TO ls_toolbar-butn_type.
*  MOVE: 'Alle markeren' TO ls_toolbar-quickinfo,
*        'Alle markeren' TO ls_toolbar-text.
**        'X' TO ls_toolbar-disabled.
*  APPEND ls_toolbar TO xy_object->mt_toolbar.
*
** Define second button
*  CLEAR ls_toolbar.
*  MOVE: 'SELDEL' TO ls_toolbar-function,
*        icon_deselect_all TO ls_toolbar-icon,
*        cntb_btype_button TO ls_toolbar-butn_type.
*  MOVE: 'Alle demarkeren' TO ls_toolbar-quickinfo,
*        'Alle demarkeren' TO ls_toolbar-text.
**        'X' TO ls_toolbar-disabled.
*  APPEND ls_toolbar TO xy_object->mt_toolbar.
*
** Separator in toolbar
*  CLEAR ls_toolbar.
*  MOVE 3 TO ls_toolbar-butn_type.
*  APPEND ls_toolbar TO xy_object->mt_toolbar.

endform.                    " create_toolbar

*&--------------------------------------------------------------------*
*&      Form  handle_alv_user_command
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->XF_UCOMM   text
*---------------------------------------------------------------------*
form handle_alv_user_command using xf_ucomm type sy-ucomm.

  data: lt_row_no type lvc_t_roid,
        wa_row_no like line of lt_row_no.


* ALV Toolbar buttons trigger this method.
  case xf_ucomm.
    when 'SELALL'.
*      LOOP AT t_display.
*        t_display-selkz = 'X'.
*        MODIFY t_display INDEX sy-tabix.
*      ENDLOOP.

      perform create_grid_control.
    when 'SELDEL'.
*      LOOP AT t_display.
*        t_display-selkz = ' '.
*        MODIFY t_display INDEX sy-tabix.
*      ENDLOOP.

      perform create_grid_control.
    when others.
  endcase.

endform.                    "handle_alv_user_command

*&--------------------------------------------------------------------*
*&      Form  alv_data_changed
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->XS_DATA_CHAtext
*---
form alv_data_changed
     using xs_data_changed type ref to cl_alv_changed_data_protocol.

  data: lw_mod_cells type lvc_s_modi.

  field-symbols <fs_output> like line of t_display.
* This event is raised by the main ALV.
* All changes are checked and changed values transported to main itab.

* Table er_data_changed->mt_good_cells holds all cells that
* are valid according to checks against their DDIC data.

* Loop at changed data to check changes. Only the correct changes are
* checked here, DDIC errors are picked up by standard sap.

* In lw_mod_cells-value you find the new value, in <fs_output> the old
* one.
  loop at xs_data_changed->mt_good_cells
       into lw_mod_cells.
    read table t_display
         assigning <fs_output>
         index lw_mod_cells-row_id.
  endloop.

endform.                    " alv_data_changed

*&--------------------------------------------------------------------*
*&      Form  create_grid_control
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
form create_grid_control .

  data: alv_layout type slis_layout_alv,
        sort       type slis_t_sortinfo_alv,
        l_selhide  type slis_sel_hide_alv.
*  DATA: field_tab    TYPE slis_t_fieldcat_alv.
  data: wa_field_tab  type slis_fieldcat_alv,
        wa_t_fieldcat like line of t_fieldcat.
  data: l_repid      like sy-repid.
  data: ls_variant   type disvariant.
  data: lf_save      type char01.
  data: ls_layout    type lvc_s_layo.
  data: lt_exclude   type ui_functions.
  data: lt_sort      type lvc_t_sort.
  data: wa_sort      type lvc_s_sort.

  if o_alv_main is initial.
* Create Grid control
    create object o_alv_main
      exporting
        i_parent       = cl_gui_container=>screen0
        i_applogparent = o_error_container.

    clear:
*    field_tab[],
           t_fieldcat[].

*    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*      EXPORTING
*        i_program_name         = sy-repid
*        i_internal_tabname     = 'T_DISPLAY'
*        i_inclname             = sy-repid
*      CHANGING
*        ct_fieldcat            = field_tab[]
*      EXCEPTIONS
*        inconsistent_interface = 1
*        program_error          = 2
*        OTHERS                 = 3.

    loop at field_tab
         into wa_field_tab.
      move-corresponding wa_field_tab to wa_t_fieldcat.
      wa_t_fieldcat-coltext = wa_field_tab-reptext_ddic.
      append wa_t_fieldcat to t_fieldcat.
    endloop.

*    LOOP AT t_fieldcat INTO wa_t_fieldcat.
*      CASE wa_t_fieldcat-fieldname.
*        WHEN 'DISPO'.
*        WHEN 'PRGRP'.
*        WHEN 'COL1'.
*          wa_t_fieldcat-coltext = 'Part Number'.
*        WHEN 'MENGE'.
*          wa_t_fieldcat-coltext = 'Quantity'.
*        WHEN 'PAST'.
*          wa_t_fieldcat-coltext = 'Past due'.
*        WHEN 'WEEK1'.
*          wa_t_fieldcat-coltext = week1.
*        WHEN 'WEEK2'.
*          wa_t_fieldcat-coltext = week2.
*        WHEN 'WEEK3'.
*          wa_t_fieldcat-coltext = week3.
*        WHEN 'WEEK4'.
*          wa_t_fieldcat-coltext = week4.
*        WHEN 'WEEK5'.
*          wa_t_fieldcat-coltext = week5.
*        WHEN 'WEEK6'.
*          wa_t_fieldcat-coltext = week6.
*        WHEN 'WEEK7'.
*          wa_t_fieldcat-coltext = week7.
**        WHEN 'WEEK8'.
**          wa_t_fieldcat-coltext = week8.
**        WHEN 'WEEK9'.
**          wa_t_fieldcat-coltext = week9.
*        WHEN 'REST'.
*          wa_t_fieldcat-coltext = 'Beyond'.
*        WHEN OTHERS.
*          wa_t_fieldcat-no_out = 'X'.
*      ENDCASE.
*      MODIFY t_fieldcat FROM wa_t_fieldcat.
*    ENDLOOP.

* A (all layouts); X (only global); U (save only user specific layouts).
    ls_variant-report = sy-repid.
    lf_save           = 'A'.
    perform set_layout
            changing ls_layout.

* Restrict generic functions: no addition/moving/deleting lines.
*    perform exclude_toolbar changing lt_exclude.

* Set sort criteria.
*    wa_sort-fieldname = 'BUKRS'.
*    wa_sort-up = 'X'.
*    APPEND wa_sort TO lt_sort.

    call method o_alv_main->set_table_for_first_display
      exporting
        is_variant           = ls_variant
        i_save               = lf_save
        is_layout            = ls_layout
        it_toolbar_excluding = lt_exclude
      changing
        it_outtab            = t_display[]
        it_sort              = lt_sort
        it_fieldcatalog      = t_fieldcat.

* Create Object to receive events and link them to handler methods.
* When the ALV Control raises the event for the specified instance
* the corresponding method is automatically called.
    create object o_event_receiver.
    set handler o_event_receiver->handle_double_click for o_alv_main.
    set handler o_event_receiver->handle_user_command for o_alv_main.
    set handler o_event_receiver->handle_toolbar for o_alv_main.
    set handler o_event_receiver->handle_top_of_page for o_alv_main.
    set handler o_event_receiver->handle_end_of_list for o_alv_main.
* Each time user edits a field, method DATA_CHANGED needs to be
* triggered, so that all changed lines can be gathered in a table.
    set handler o_event_receiver->handle_data_changed for o_alv_main.

* Register commands that should raise event DATA_CHANGED: ENTER and
* modification of cell now raise it.
* (Per default the user may check data by using the check icon).
    call method o_alv_main->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    call method o_alv_main->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

* Call method 'set_toolbar_interactive' to raise event TOOLBAR.
    call method o_alv_main->set_toolbar_interactive.

** Set editable cells to ready for input
*    CALL METHOD o_alv_main->set_ready_for_input
*      EXPORTING
*        i_ready_for_input = 1.
  else.
    call method o_alv_main->refresh_table_display.
  endif.

endform.                    "create_grid_control

*&--------------------------------------------------------------------*
*&      Form  set_layout
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->YS_LAYOUT  text
*---------------------------------------------------------------------*
form set_layout
     changing ys_layout type lvc_s_layo.

*{   REPLACE        MDEK903925                                        1
*\  ys_layout-grid_title = 'MCi Net Adjust Report'.
  ys_layout-grid_title = 'MD04 Requirements Tabular Report'.
*}   REPLACE
  ys_layout-cwidth_opt = 'X'.
*  ys_layout-detailinit = 'X'.
  ys_layout-info_fname = 'LINE_COLOUR'. "Colour rows in internal table
*  ys_layout-zebra      = 'X'.
*  ys_layout-sel_mode   = 'X'.
*  ys_layout-no_toolbar = 'X'.
* ys_layout-edit       = 'X'. "Set the whole display to editable

endform.                    "set_layout

*&--------------------------------------------------------------------*
*&      Form  exclude_toolbar
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->YT_EXCLUDE_textTIONS
*---------------------------------------------------------------------*
form exclude_toolbar
     changing yt_exclude_functions type ui_functions.

  data lw_exclude       type ui_func.

  lw_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  append lw_exclude to yt_exclude_functions.

  lw_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  append lw_exclude to yt_exclude_functions.

  lw_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  append lw_exclude to yt_exclude_functions.

  lw_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  append lw_exclude to yt_exclude_functions.

  lw_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
  append lw_exclude to yt_exclude_functions.

  lw_exclude = cl_gui_alv_grid=>mc_fc_check.
  append lw_exclude to yt_exclude_functions.

  lw_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  append lw_exclude to yt_exclude_functions.

  lw_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  append lw_exclude to yt_exclude_functions.

  lw_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  append lw_exclude to yt_exclude_functions.

  lw_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  append lw_exclude to yt_exclude_functions.

  lw_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  append lw_exclude to yt_exclude_functions.

  lw_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
  append lw_exclude to yt_exclude_functions.

  lw_exclude = cl_gui_alv_grid=>mc_mb_sum.
  append lw_exclude to yt_exclude_functions.

  lw_exclude = cl_gui_alv_grid=>mc_mb_subtot.
  append lw_exclude to yt_exclude_functions.

  lw_exclude = cl_gui_alv_grid=>mc_mb_paste.
  append lw_exclude to yt_exclude_functions.

  lw_exclude = cl_gui_alv_grid=>mc_fc_average.
  append lw_exclude to yt_exclude_functions.

  lw_exclude = cl_gui_alv_grid=>mc_fc_f4.
  append lw_exclude to yt_exclude_functions.

  lw_exclude = cl_gui_alv_grid=>mc_fc_graph.
  append lw_exclude to yt_exclude_functions.

  lw_exclude = cl_gui_alv_grid=>mc_fc_info.
  append lw_exclude to yt_exclude_functions.

  exit.

  lw_exclude = cl_gui_alv_grid=>mc_mb_view.
  append lw_exclude to yt_exclude_functions.

  lw_exclude = cl_gui_alv_grid=>mc_fc_refresh.
  append lw_exclude to yt_exclude_functions.

  lw_exclude = cl_gui_alv_grid=>mc_mb_export.
  append lw_exclude to yt_exclude_functions.

endform.                    "exclude_toolbar

*&--------------------------------------------------------------------*
*&      Form  create_parent_container
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
form create_parent_container .

  data: li_int4 type int4.

* Create container on which to place the ALV control.
* NB not necessary if whole screen is used. In that case set
* 'i_parent = cl_gui_container=>screen0' as the parent for the ALV.
* So also screen 200 does not need a container any more!

  exit.

* Test whether in background mode.
  call method cl_gui_alv_grid=>offline
    receiving
      e_offline = li_int4.

  if li_int4 is initial.
    create object o_cont_alv_main
      exporting
        container_name = 'CONTAINER_ALV'.
  endif.

endform.                    "create_parent_container

*&--------------------------------------------------------------------*
*&      Form  free_objects
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
form free_objects .

  if not o_cont_alv_main is initial.
* Destroy container (destroys contained controls, too)
    call method o_cont_alv_main->free
      exceptions
        cntl_system_error = 1
        cntl_error        = 2.
    if not o_alv_main is initial.
      call method o_alv_main->free
        exceptions
          cntl_system_error = 1
          cntl_error        = 2.
      call method o_protocol->free.
    endif.

    call method cl_gui_cfw=>flush
      exceptions
        cntl_system_error = 1
        cntl_error        = 2.
  elseif not o_alv_main is initial.
* Container for alv control no longer needed -> clear alv control
    call method o_alv_main->free
      exceptions
        cntl_system_error = 1
        cntl_error        = 2.

    call method cl_gui_cfw=>flush
      exceptions
        cntl_system_error = 1
        cntl_error        = 2.
  endif.

  clear: o_cont_alv_main,
         o_alv_main,
         o_protocol,
         o_event_receiver,
         okcode.

endform.                    "free_objects

*&---------------------------------------------------------------------*
*&      Module  status_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.

  set pf-status '0100'.
  set titlebar '100'.

endmodule.                 " status_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  init_controls_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module init_controls_0100 output.

  if o_alv_main is initial.
    perform: create_parent_container,
             create_grid_control.
  endif.

endmodule.                 " init_controls_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.

  case okcode.
    when 'BACK' or
         'EXIT' or
         'E'.
      perform free_objects.
      set screen 0.
      leave screen.
    when others.
      call method o_alv_main->check_changed_data.
      call method o_alv_main->refresh_table_display.
      clear okcode.
  endcase.

endmodule.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Form  SET_HEADER
*&---------------------------------------------------------------------*
*       Sets the header
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_header .
  data: line        type num2,
        total_week  type num2,
        total_month type num2.
* set week numbers

  call function 'DATE_GET_WEEK'
    exporting
      date = sy-datum
    importing
      week = week0.

  if ( s_yweek is initial and s_ymonth is initial ) or ( s_yweek is not initial and s_ymonth is not initial ) .

    message 'Fill week or month in selection screen'  type 'I'.
    i_exit = 'X'.
    exit.

  elseif s_yweek is not initial.

    select calendarweek, yearweek from scal_tt_date into table @gt_weeks
      where yearweek in @s_yweek.
    sort gt_weeks by yearweek ascending.
    delete adjacent duplicates from gt_weeks comparing yearweek.

  elseif s_ymonth is not initial.

    select calendarmonth, yearmonth from scal_tt_date into table @gt_months
      where yearmonth in @s_ymonth.
    sort gt_months by yearmonth ascending.
    delete adjacent duplicates from gt_months comparing yearmonth.

  endif.

  describe table gt_months lines data(lv_lines_m).
  if lv_lines_m > 99.
    message | Select up to 99 months, current quantity of months is { lv_lines_m } |  type 'I'.
    i_exit = 'X'.
    exit.
  endif.

  describe table gt_weeks lines data(lv_lines).
  if lv_lines > 99.
    message | Select up to 99 weeks, current quantity of weeks is { lv_lines } |  type 'I'.
    i_exit = 'X'.
    exit.
  endif.

  add_field 'MATNR' 'Material' ' '.
  add_field 'MAKTX' 'Description' ' '.
  add_field 'DISPO' 'MRP Controller' ' '.
  add_field 'RESVP' 'DTF Value' ' '.
  add_field 'VRMOD' 'Consumption mode' ' '.
  add_field 'VINT2' 'Fwd consumption' ' '.
  add_field 'VINT1' 'Bwd consumption' ' '.
  add_field 'EISBE' 'Safety Stock' ' '.
  add_field 'COL1' 'Type' ' '.
  add_field 'MENGE' 'Current Stock' 'X'.
  add_field 'PAST' 'Past due' 'X'.
  line = 0.

  if gt_weeks is not initial.

    loop at gt_weeks assigning field-symbol(<fs_weeks>).

      line = line + 1.

      total_week = cond #( when line = 1
                                        then <fs_weeks>-calendarweek
                                        else total_week + 1
                                     ).

      <fs_weeks>-total_week = total_week.


      concatenate 'WEEK' <fs_weeks>-total_week into data(l_week).
      add_field l_week <fs_weeks>-yearweek 'X'.

      concatenate 'WEEK0_' line into data(lv_week).
      assign (lv_week) to field-symbol(<fs_week>).
      if <fs_weeks> is assigned.
        <fs_week> = <fs_weeks>-yearweek.
      endif.
      unassign <fs_week>.

    endloop.

  elseif gt_months is not initial.

    loop at gt_months assigning field-symbol(<fs_months>).
      line = line + 1.

      total_month = cond #( when line = 1
                                        then <fs_months>-calendarmonth
                                        else total_month + 1
                                     ).

      <fs_months>-total_month = total_month.


      concatenate 'WEEK' <fs_months>-total_month into data(l_month).
      add_field l_month <fs_months>-yearmonth 'X'.

      concatenate 'WEEK0_' line into data(lv_month).
      assign (lv_month) to field-symbol(<fs_month>).
      if <fs_month> is assigned.
        <fs_month> = <fs_months>-yearmonth.
      endif.
      unassign <fs_week>.
    endloop.

  endif.
  add_field 'REST' 'Beyond' 'X'.

endform.                    " SET_HEADER

form get_mrp .

* internal work fields
  data: w_aantal type p,
        lt_mdez  like mdez occurs 0 with header line.

  clear: w_dispo.

  sort it_marc by dispo matnr.

  loop at it_marc.

    if p_cross is not initial.
      read table t_display with key matnr = it_marc-matnr transporting no fields.
      if sy-subrc = 0.
        continue.
      endif.
    endif.

    clear: i_mdez[],
           lt_mdez,
           wa_display,
           wa_displayx,
           wa_display1,
           wa_display2,
           wa_display3,
           wa_display4,
           wa_display5,
           wa_display6,
           wa_display7,
           wa_display8.

    if p_cross is not initial.

      select werks from marc where matnr = @it_marc-matnr into table @data(lt_werks).

      loop at lt_werks into data(ls_plants).
        perform get_mrp_data using ls_plants-werks.
        perform get_original_pir using ls_plants-werks.
        perform get_original_sa  using ls_plants-werks.
      endloop.

    else.
      perform get_mrp_data using it_marc-werks.
      perform get_original_pir using it_marc-werks.
      perform get_original_sa  using it_marc-werks.
    endif.

    perform do_calculations.

    if sy-subrc eq 0.

      perform build_screen.

    endif.
  endloop.


endform.                    " GET_MRP
*&---------------------------------------------------------------------*
*&      Form  ADD_MNG_TO_WEEK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_MDEZ  text
*      <--P_WA_display4  text
*----------------------------------------------------------------------*
form add_mng_to_week  using    p_i_mdez type mdez
                      changing p_wa_display type ty_display.

  call function 'DATE_GET_WEEK'
    exporting
      date = p_i_mdez-dat00
    importing
      week = week0.

  read table gt_weeks into data(ls_week) with key yearweek = week0.

  if sy-subrc = 0.
    concatenate 'WEEK0_' ls_week-total_week into data(lv_week).
    assign (lv_week) to field-symbol(<fs_week>).

    if <fs_week> is assigned.
      concatenate 'p_wa_display-week' ls_week-total_week into data(lv_week_adjust).
      assign (lv_week_adjust) to field-symbol(<fs_week_adjust>).

      if <fs_week_adjust> is assigned.
        add p_i_mdez-mng01 to <fs_week_adjust>.
      endif.
    endif.

  else.
    read table gt_weeks into ls_week index 1.

    if week0 < ls_week-yearweek.
      add p_i_mdez-mng01 to p_wa_display-past.

    else.
      add p_i_mdez-mng01 to p_wa_display-rest.

    endif.
  endif.
endform.                    " ADD_MNG_TO_WEEK
*&---------------------------------------------------------------------*
*&      Form  ADD_MNG_TO_WEEK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_MDEZ  text
*      <--P_WA_display4  text
*----------------------------------------------------------------------*
form add_mng_to_month  using    p_i_mdez type mdez
                      changing p_wa_display type ty_display.
  data: month0 type char2,
        year0  type char4.

  call function 'CACS_DATE_GET_YEAR_MONTH'
    exporting
      i_date  = p_i_mdez-dat00
    importing
      e_month = month0
      e_year  = year0.

  data(yearmonth0) = |{ year0 }{ month0 }|.

  read table gt_months into data(ls_months) with key yearmonth = yearmonth0.

  if sy-subrc = 0.
    concatenate 'WEEK0_' ls_months-total_month into data(lv_month).
    assign (lv_month) to field-symbol(<fs_month>).

    if <fs_month> is assigned.
      concatenate 'p_wa_display-week' ls_months-total_month into data(lv_month_adjust).
      assign (lv_month_adjust) to field-symbol(<fs_month_adjust>).

      if <fs_month_adjust> is assigned.
        add p_i_mdez-mng01 to <fs_month_adjust>.
      endif.
    endif.

  else.
    read table gt_months into ls_months index 1.

    if yearmonth0 < ls_months-yearmonth.
      add p_i_mdez-mng01 to p_wa_display-past.

    else.
      add p_i_mdez-mng01 to p_wa_display-rest.

    endif.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  SET_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_selection.

  if s_work is not initial and p_cross is not initial.

    select marc~matnr makt~maktx marc~werks marc~dispo marc~vrmod marc~resvp marc~vint2 marc~vint1 marc~eisbe
      appending corresponding fields of table it_marc
       from marc
      inner join makt on ( makt~matnr = marc~matnr )
                     and ( makt~spras = 'E' )
      inner join mapl on ( mapl~matnr = marc~matnr )
      inner join plpo on ( plpo~plnty = mapl~plnty )
                     and ( plpo~plnnr = mapl~plnnr )
                     and ( plpo~zaehl = mapl~zaehl )
      inner join crhd on ( crhd~objid = plpo~arbid )
      where crhd~arbpl in s_work.

  elseif s_work is not initial and p_cross is  initial.

    select marc~matnr makt~maktx marc~werks marc~dispo marc~vrmod marc~resvp marc~vint2 marc~vint1 marc~eisbe
      appending corresponding fields of table it_marc
       from marc
      inner join makt on ( makt~matnr = marc~matnr )
                     and ( makt~spras = 'E' )
      inner join mapl on ( mapl~matnr = marc~matnr )
                     and ( mapl~werks = marc~werks )
      inner join plpo on ( plpo~plnty = mapl~plnty )
                     and ( plpo~plnnr = mapl~plnnr )
                     and ( plpo~zaehl = mapl~zaehl )
      inner join crhd on ( crhd~objid = plpo~arbid )
      where crhd~arbpl in s_work.

  elseif s_work is initial and p_cross is initial.

    select marc~matnr makt~maktx marc~werks marc~dispo marc~vrmod marc~resvp marc~vint2 marc~vint1 marc~eisbe
      appending corresponding fields of table it_marc
          from   marc
      inner join makt on ( makt~matnr = marc~matnr )
                         and ( makt~spras = 'E' )
          where marc~matnr in s_matnr
          and   marc~werks = p_werks
          and   marc~dispo in s_dispo.

  elseif s_work is initial and p_cross is not initial.

    select marc~matnr makt~maktx marc~werks marc~dispo marc~vrmod marc~resvp marc~vint2 marc~vint1 marc~eisbe
      appending corresponding fields of table it_marc
          from   marc
      inner join makt on ( makt~matnr = marc~matnr )
                         and ( makt~spras = 'E' )
          where marc~matnr in s_matnr
*          and   marc~werks = p_werks
          and   marc~dispo in s_dispo.
  endif.

  if sy-subrc <> 0.

    message 'Nothing found for this selection' type 'I'.
    i_exit = 'X'.
    exit.

  endif.

  sort it_marc by matnr.
  delete adjacent duplicates from it_marc comparing matnr werks.

endform.                    " SET_SELECTION
*&---------------------------------------------------------------------*
*&      Form  CALC_inventory
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_
*      -->P_WA_DISPLAY3_PAST  text
*      <--P_WA_DISPLAY_PAST  text
*----------------------------------------------------------------------*
form calc_inventory  using    p_1 type p
                              p_2 type p
                              p_3 type p
                              p_4 type p
                              p_5 type p
                              p_6 type p
                              p_7 type p
                              p_8 type p
                     changing p_9 type p.

  p_9 = p_1 + p_2 + p_3 + p_4 + p_5 + p_6 + p_7 + p_8.

endform.                    " CALC_inventory
*&---------------------------------------------------------------------*
*&      Form  CALCULATE_COMPLETE_INVENTORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calculate_complete_inventory_w .

  perform calc_inventory using    i_mdsta-labst
                                  wa_displayx-past
                                  wa_display7-past
                                  wa_display8-past
                                  wa_display4-past
                                  wa_display3-past
                                  wa_display2-past
                                  wa_display1-past
                        changing  wa_displayx-past.

  data: var    type c,
        var2   type num,
        week_1 type num2.

  loop at gt_weeks into data(ls_weeks).

    var2 = 0.

    do 8 times.
      var2 = var2 + 1.

      case var2.

        when 1.
          var = 'x'.
          if sy-tabix ne 1.
            week_1 = ls_weeks-total_week - 1.
            concatenate 'wa_display' var '-week' week_1 into data(weekx1).
            assign (weekx1) to field-symbol(<fs_weekx1>).
          else.
            concatenate 'wa_display' var '-past' into weekx1.
            assign (weekx1) to <fs_weekx1>.
          endif.

        when 2.
          var = 'x'.
          concatenate 'wa_display' var '-week' ls_weeks-total_week into data(weekx2).
          assign (weekx2) to field-symbol(<fs_weekx2>).

        when 3.
          var = '1'.
          concatenate 'wa_display' var '-week' ls_weeks-total_week into data(week12).
          assign (week12) to field-symbol(<fs_week12>).

        when 4.
          var = '2'.
          concatenate 'wa_display' var '-week' ls_weeks-total_week into data(week22).
          assign (week22) to field-symbol(<fs_week22>).

        when 5.
          var = '3'.
          concatenate 'wa_display' var '-week' ls_weeks-total_week into data(week32).
          assign (week32) to field-symbol(<fs_week32>).

        when 6.
          var = '4'.
          concatenate 'wa_display' var '-week' ls_weeks-total_week into data(week42).
          assign (week42) to field-symbol(<fs_week42>).
        when 7.
          var = '7'.
          concatenate 'wa_display' var '-week' ls_weeks-total_week into data(week72).
          assign (week72) to field-symbol(<fs_week72>).
        when 8.
          var = '8'.
          concatenate 'wa_display' var '-week' ls_weeks-total_week into data(week82).
          assign (week82) to field-symbol(<fs_week82>).

      endcase.
    enddo.
    if <fs_weekx1> is assigned.
      perform calc_inventory using    <fs_weekx1>
                                      <fs_weekx2>
                                      <fs_week72>
                                      <fs_week82>
                                      <fs_week42>
                                      <fs_week32>
                                      <fs_week22>
                                      <fs_week12>
                           changing   <fs_weekx2>.
    endif.
  endloop.

  perform calc_inventory using  <fs_weekx2>
                                wa_displayx-rest
                                wa_display8-rest
                                wa_display7-rest
                                wa_display4-rest
                                wa_display3-rest
                                wa_display2-rest
                                wa_display1-rest
                      changing  wa_displayx-rest.

endform.                    " CALCULATE_COMPLETE_INVENTORY
*---------------------------------------------------------------------*
*&      Form  CALCULATE_COMPLETE_INVENTORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calculate_complete_inventory_m.

  perform calc_inventory using    i_mdsta-labst
                                  wa_displayx-past
                                  wa_display8-past
                                  wa_display7-past
                                  wa_display4-past
                                  wa_display3-past
                                  wa_display2-past
                                  wa_display1-past
                        changing  wa_displayx-past.

  data: var    type c,
        var2   type num,
        week_1 type num2.

  loop at gt_months into data(ls_months).

    var2 = 0.

    do 8 times.
      var2 = var2 + 1.

      case var2.

        when 1.
          var = 'x'.
          if sy-tabix ne 1.
            week_1 = ls_months-total_month - 1.
            concatenate 'wa_display' var '-week' week_1 into data(weekx1).
            assign (weekx1) to field-symbol(<fs_weekx1>).
          else.
            concatenate 'wa_display' var '-past' into weekx1.
            assign (weekx1) to <fs_weekx1>.
          endif.

        when 2.
          var = 'x'.
          concatenate 'wa_display' var '-week' ls_months-total_month into data(weekx2).
          assign (weekx2) to field-symbol(<fs_weekx2>).

        when 3.
          var = '1'.
          concatenate 'wa_display' var '-week' ls_months-total_month into data(week12).
          assign (week12) to field-symbol(<fs_week12>).

        when 4.
          var = '2'.
          concatenate 'wa_display' var '-week' ls_months-total_month into data(week22).
          assign (week22) to field-symbol(<fs_week22>).

        when 5.
          var = '3'.
          concatenate 'wa_display' var '-week' ls_months-total_month into data(week32).
          assign (week32) to field-symbol(<fs_week32>).

        when 6.
          var = '4'.
          concatenate 'wa_display' var '-week' ls_months-total_month into data(week42).
          assign (week42) to field-symbol(<fs_week42>).
        when 7.
          var = '7'.
          concatenate 'wa_display' var '-week' ls_months-total_month into data(week72).
          assign (week72) to field-symbol(<fs_week72>).
        when 8.
          var = '7'.
          concatenate 'wa_display' var '-week' ls_months-total_month into data(week82).
          assign (week82) to field-symbol(<fs_week82>).
      endcase.
    enddo.
    if <fs_weekx1> is assigned.
      perform calc_inventory using    <fs_weekx1>
                                      <fs_weekx2>
                                      <fs_week82>
                                      <fs_week72>
                                      <fs_week42>
                                      <fs_week32>
                                      <fs_week22>
                                      <fs_week12>
                           changing   <fs_weekx2>.
    endif.
  endloop.

  perform calc_inventory using  <fs_weekx2>
                                wa_displayx-rest
                                wa_display8-rest
                                wa_display7-rest
                                wa_display4-rest
                                wa_display3-rest
                                wa_display2-rest
                                wa_display1-rest
                      changing  wa_displayx-rest.

endform.                    " CALCULATE_COMPLETE_INVENTORY
*&---------------------------------------------------------------------*
*& Form GET_ORIGINAL_PIR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form get_original_pir using werks type werks_d.

  select  pdatu, plnmg into table @data(lt_pbed) from pbed inner join pbim on ( pbed~bdzei = pbim~bdzei )
    where pbim~matnr = @it_marc-matnr and
          pbim~werks = @werks         and
          pbim~vervs = 'X'            and
          pbim~versb = '00'.

  loop at lt_pbed into data(ls_pbed).
    i_mdez[] = value #( base i_mdez[] (
                plaab = '02'
                dat00 = ls_pbed-pdatu
                delb0 = 'OrgPir'
                mng01 = ls_pbed-plnmg )
                ).
  endloop.

endform.
*&---------------------------------------------------------------------*
*& Form GET_ORIGINAL_SA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form get_original_sa using werks type werks_d.
  select auart, vbep~vbeln, vbep~posnr, vbak~gueen, mbdat, vbep~abart, ettyp, wmeng, abdis  from vbep inner join vbap on ( vbep~vbeln = vbap~vbeln )
                                                                                and ( vbep~posnr = vbap~posnr )
                                                                 inner join vbak on ( vbep~vbeln = vbak~vbeln )

    where vbap~matnr = @it_marc-matnr and
          vbap~werks = @werks and
          vbap~abgru is initial

    into table @data(lt_vbep).

  loop at lt_vbep into data(ls_vbep).
    if ls_vbep-auart = 'TAM' or ls_vbep-auart = 'ZNAV' or ( ls_vbep-gueen < sy-datum and ls_vbep-gueen is not initial ) .
      continue.
    endif.
    if ( ls_vbep-auart = 'LZJ' or ( ls_vbep-auart = 'LZ' and ( ls_vbep-abdis = 'E' or ls_vbep-abdis = 'D' ) ) ) and
         ls_vbep-abart = '1' and
         ls_vbep-ettyp = 'BN'.

      select single abhor  from vblb where vbeln = @ls_vbep-vbeln  and
                                           posnr = @ls_vbep-posnr and
                                           abhor is not initial and
                                           abrli is initial
          into @data(lv_abhor).

      if ls_vbep-mbdat <= lv_abhor.
        continue.
      endif.

    endif.
    i_mdez[] = value #( base i_mdez[] (
                plaab = '02'
                dat00 = ls_vbep-mbdat
                delb0 = 'OrgSA'
                mng01 = ls_vbep-wmeng )
                ).

  endloop.
endform.
*&---------------------------------------------------------------------*
*& Form ADD_WEEK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- WEEK1
*&      <-- WEEK0
*&---------------------------------------------------------------------*
form add_week  changing p_week1
                        p_week0.
  concatenate p_week0+4 '/' p_week0(4) into p_week1.
  concatenate 'W' p_week1 into p_week1 separated by ' '.

  call function 'NEXT_WEEK'
    exporting
      current_week = p_week0
    importing
      next_week    = p_week0.
endform.

*&---------------------------------------------------------------------*
*& Form GET_MRP_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form get_mrp_data using werks type werks_d.
  data: lt_mdez  like mdez occurs 0 with header line.
  call function 'MD_STOCK_REQUIREMENTS_LIST_API'
    exporting
*     plscn                    = p_plscn
      matnr                    = it_marc-matnr
      werks                    = werks
*     inper                    = p_inper
*     ergbz                    = p_ergbz
    importing
      e_mdsta                  = i_mdsta
*     e_mt61d                  = i_mt61d
    tables
      mdezx                    = lt_mdez
    exceptions
      material_plant_not_found = 1
      plant_not_found          = 2
      others                   = 3.
  append lines of lt_mdez to i_mdez.
endform.
*&---------------------------------------------------------------------*
*& Form DO_CALCULATIONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form do_calculations .
  loop at i_mdez
      where planr = ''
      and ( delb0 = 'PrdOrd'
      or    delb0 = 'DepReq'
      or    delb0 = 'SchLne'
      or    delb0 = 'CusOrd'
      or    delb0 = 'PldOrd'
      or    delb0 = 'SchAgr'
      or    delb0 = 'IndReq'
      or    delb0 = 'Delvry'
      or    delb0 = 'POitem'
      or    delb0 = 'ShpgNt'
      or    delb0 = 'InPlnt'
      or    delb0 = 'OrgSA'
      or    delb0 = 'OrgPir'
      or    delb0 = 'OrdRes'
      or    delb0 = 'Ord.DS'
    ).

* Calculate

    case i_mdez-delb0.

      when 'PrdOrd' or 'PldOrd' .
* Open work order
        if i_mdez-delb0 = 'PrdOrd'.
*            data(length) = strlen( i_mdez-extra ) - 4.
*            case i_mdez-extra+length(4).
          split i_mdez-extra at '/' into data(lv_number) data(lv_type) data(lv_number2).
          case lv_type.
            when 'PP01' or 'ZPCK' or 'ZRWK'.
*              Do nothing
            when others.
              continue.
          endcase.
        endif.

        if gt_weeks is not initial.
          perform add_mng_to_week using i_mdez
                                  changing wa_display1.
        else.
          perform add_mng_to_month using i_mdez
                                  changing wa_display1.
        endif.

*Open PO
      when 'POitem' or 'SchLne'.

        if gt_weeks is not initial.
          perform add_mng_to_week using i_mdez
                                  changing wa_display2.
        else.
          perform add_mng_to_month using i_mdez
                                  changing wa_display2.
        endif.

* in transit
      when 'ShpgNt'.

        if gt_weeks is not initial.
          perform add_mng_to_week using i_mdez
                                  changing wa_display3.
        else.
          perform add_mng_to_month using i_mdez
                                  changing wa_display3.
        endif.

*in Plant
      when 'InPlnt'.

        if gt_weeks is not initial.
          perform add_mng_to_week using i_mdez
                                  changing wa_display4.
        else.
          perform add_mng_to_month using i_mdez
                                  changing wa_display4.
        endif.

*Delivery
      when 'Delvry' or 'CusOrd'.
        if gt_weeks is not initial.
          perform add_mng_to_week using i_mdez
                                  changing wa_display7.
        else.
          perform add_mng_to_month using i_mdez
                                  changing wa_display7.
        endif.

*Dep. Requirements
      when 'DepReq' or 'CusOrd' or 'OrdRes' or 'Ord.DS'.
        if gt_weeks is not initial.
          perform add_mng_to_week using i_mdez
                                  changing wa_display8.
        else.
          perform add_mng_to_month using i_mdez
                                  changing wa_display8.
        endif.

* Open requirements
      when 'SchAgr' or  'IndReq'.

        if i_mdez-vrfkz is initial.
          continue.
        endif.

        if gt_weeks is not initial.
          perform add_mng_to_week using i_mdez
                                  changing wa_displayx.
        else.
          perform add_mng_to_month using i_mdez
                                  changing wa_displayx.
        endif.

*Original PIR
      when'OrgPir'.

        if gt_weeks is not initial.
          perform add_mng_to_week using i_mdez
                                  changing wa_display5.
        else.
          perform add_mng_to_month using i_mdez
                                  changing wa_display5.
        endif.
*Original SA
      when 'OrgSA'.

        if gt_weeks is not initial.
          perform add_mng_to_week using i_mdez
                                  changing wa_display6.
        else.
          perform add_mng_to_month using i_mdez
                                  changing wa_display6.
        endif.

    endcase.
  endloop.
endform.
*&---------------------------------------------------------------------*
*& Form BUILD_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form build_screen .

  w_dispo = it_marc-dispo.

* Line 1 Requirements & Information
  wa_display-maktx = it_marc-maktx.
  wa_display-vrmod = it_marc-vrmod.
  wa_display-resvp = it_marc-resvp.
  wa_display-vint2 = it_marc-vint2.
  wa_display-vint1 = it_marc-vint1.
  wa_display-eisbe = it_marc-eisbe.
  replace all occurrences of substring '.000' in wa_display-eisbe with ''.
  wa_display-dispo = it_marc-dispo.
  wa_display-matnr = it_marc-matnr.
  append wa_display to t_display.
  clear wa_display.

* Orginal PIR
  wa_display = wa_display5.
  wa_display-col1 = 'Orginal PIR'.
  append wa_display to t_display.
  clear wa_display.

* Original SA
  wa_display = wa_display6.
  wa_display-col1 = 'Original SA'.
  append wa_display to t_display.
  clear wa_display.

* Dep Requirement
  wa_display = wa_display8.
  wa_display-col1 = 'Dependent Requirements'.
  append wa_display to t_display.
  clear wa_display.

* Original Delvry
  wa_display = wa_display7.
  wa_display-col1 = 'Delivery'.
  append wa_display to t_display.
  clear wa_display.

*MD04 PIR + SA
  wa_display = wa_displayx.
  wa_display-line_colour = 'C700'.
  wa_display-col1 = 'Demand plan'.
  append wa_display to t_display.
  clear wa_display.

* Open Work Orders
  wa_display = wa_display1.
  wa_display-line_colour = 'C100'.
  wa_display-col1 = 'Open Production'.
  append wa_display to t_display.
  clear wa_display.

*in Transit
  wa_display = wa_display3.
  wa_display-col1 = 'In Transit'.
  append wa_display to t_display.
  clear wa_display.

* In Plant
  wa_display = wa_display4.
  wa_display-col1 = 'In Plant'.
  append wa_display to t_display.
  clear wa_display.

* In PO
  wa_display = wa_display2.
  wa_display-col1 = 'Open Purchase Order'.
  append wa_display to t_display.
  clear wa_display.


* Avail quantity
  if p_cross is not initial.
    select distinct sum( labst ) from mard
        into i_mdsta-labst
          where matnr = it_marc-matnr.
  else.
    select distinct sum( labst ) from mard
        into i_mdsta-labst
          where matnr = it_marc-matnr
          and   werks = it_marc-werks
          and   lgort in s_lgort.
  endif.
* Calculate totals for each week
  if gt_weeks is not initial.
    perform calculate_complete_inventory_w.
  else.
    perform calculate_complete_inventory_m.
  endif.

  wa_displayx-line_colour = 'C500'.
  wa_displayx-col1 = 'Available Quantity'. " If you change this please also change Form  DEL_POS_IVENT

  move i_mdsta-labst to wa_displayx-menge.
  append wa_displayx to t_display.
  clear wa_display.

endform.

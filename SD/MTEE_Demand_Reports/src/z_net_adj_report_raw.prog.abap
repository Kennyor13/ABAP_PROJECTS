*&---------------------------------------------------------------------*
*& Report Z_NET_ADJ_REPORT_RAW
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report z_net_adj_report_raw.

include z_net_types_raw.


selection-screen begin of block b01
                 with frame
                 title text-001.
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
parameters : p_week radiobutton group rb2.
parameters : p_month radiobutton group rb2.
selection-screen skip 2.
selection-screen begin of block b1 with frame title text-009.
selection-screen comment 1(75) text-002.
selection-screen skip.
selection-screen comment 1(75) text-003.
selection-screen skip.
selection-screen comment 1(75) text-004.
selection-screen skip.
selection-screen comment 1(75) text-006.
selection-screen skip.
selection-screen comment 1(75) text-005.
selection-screen skip.
selection-screen comment 1(75) text-006.
selection-screen skip.
selection-screen comment 1(75) text-012.
selection-screen skip.
selection-screen comment 1(75) text-013.
selection-screen skip.
selection-screen comment 1(75) text-014.
selection-screen end of block b1.
*select-options: s_prgrp for rmcp3-prgrp matchcode object mat2.
*parameter: p_sall type char1 as checkbox.
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

*  perform set_periods.

  check i_exit ne 'X'.

  perform get_mrp.

  perform show_output.

end-of-selection.

*-----------------------------------------------------------------------

form set_header .

  add_field 'MATNR' 'Material' ' '.
  add_field 'DISPO' 'MRP Controller' ' '.
  add_field 'RESVP' 'DTF Value' ' '.
  add_field 'VRMOD' 'Consumption mode' ' '.
  add_field 'VINT2' 'Fwd consumption' ' '.
  add_field 'VINT1' 'Bwd consumption' ' '.
  add_field 'EISBE' 'Safety Stock' ' '.
  if p_week is not initial.
    add_field 'PERIOD' 'Week Number' ' '.
  else.
    add_field 'PERIOD' 'Month Number' ' '.
  endif.
  add_field 'ORGPIR' 'Original PIR' 'X'.
  add_field 'PIR' 'Plnd Ind Req' 'X'.
  add_field 'ORGSA' 'Original Scheduling Agreement' 'X'.
  add_field 'DEPREQ' 'Dependent Requirements' 'X'.
  add_field 'SCHDA' 'Scheduling Agreement' 'X'.
  add_field 'DELVRY' 'Delivery' 'X'.
  add_field 'DEMN' 'Demand Plan' 'X'.
  add_field 'PROD' 'Production' 'X'.
  add_field 'SHPGNT' 'In Transit' 'X'.
  add_field 'INPLNT' 'In Plant' 'X'.
  add_field 'PURC' 'Open Purchase Order' 'X'.
  add_field 'AVAIL' 'Available Quantity' 'X'.


endform.                    " SET_HEADER

form set_selection.

  if s_work is not initial.

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

  else.

    select marc~matnr makt~maktx marc~werks marc~dispo marc~vrmod marc~resvp marc~vint2 marc~vint1 marc~eisbe
      appending corresponding fields of table it_marc
          from   marc
      inner join makt on ( makt~matnr = marc~matnr )
                         and ( makt~spras = 'E' )
          where marc~matnr in s_matnr
          and   marc~werks = p_werks
          and   marc~dispo in s_dispo.
  endif.

  if sy-subrc <> 0.
    message 'Nothing found for this selection' type 'I'.
    i_exit = 'X'.
    exit.
  endif.

endform.                    " SET_SELECTION

*form set_periods.
*  data: line         type num2,
*        total_period type num2.
*
*
*  if ( s_yweek is initial and s_ymonth is initial ) or ( s_yweek is not initial and s_ymonth is not initial ) .
*
*    message 'Fill week or month in selection screen'  type 'I'.
*    i_exit = 'X'.
*    exit.
*
*  elseif s_yweek is not initial.
*
*    select calendarweek, yearweek from scal_tt_date into table @gt_periods
*      where yearweek in @s_yweek.
*
*  elseif s_ymonth is not initial.
*
*    select calendarmonth, yearmonth from scal_tt_date into table @gt_periods
*      where yearmonth in @s_ymonth.
*
*  endif.
*
*  sort gt_periods by yearperiod ascending.
*  delete adjacent duplicates from gt_periods comparing yearperiod.
*
*  loop at gt_periods assigning field-symbol(<fs_periods>).
*
*    line = line + 1.
*
*    total_period = cond #( when line = 1
*                                      then <fs_periods>-calendarperiod
*                                      else total_period + 1
*                                   ).
*
*    <fs_periods>-total_period = total_period.
*  endloop.
*endform.

form get_mrp .

* internal work fields
  data: w_aantal     type p,
        var          type char6,
        pre_read_mrp type table of pph_matnr_werks_berid_sel.
  clear: w_dispo.
  sort it_marc by dispo matnr.

  move-corresponding it_marc[] to pre_read_mrp.
  call function 'PPH_STOCK_REQ_LISTS_PREREAD'
    exporting
      it_matnr_werks_berid = pre_read_mrp
*     IV_AGG_MODE          = ' '
*     IV_BUFFER_REFRESH    = 'X'.
    .
  loop at it_marc.

    clear i_mdez[].

    loop at gt_periods into data(ls_periods).

      t_display = value #( base t_display ( matnr = it_marc-matnr
                                            dispo = it_marc-dispo
                                            resvp  = it_marc-resvp
                                            vrmod = it_marc-vrmod
                                            vint2 = it_marc-vint2
                                            vint1 = it_marc-vint1
                                            eisbe = it_marc-eisbe
                                            period = ls_periods-yearperiod ) ).
    endloop.


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

      var = i_mdez-delb0.
      if i_mdez-delb0 = 'IndReq' and i_mdez-vrfkz is initial.
        continue.
      endif.

      perform add_mng_to_period using i_mdez
                                      var
                                      it_marc-matnr
                             changing t_display.


      clear var.

    endloop.
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

    sort t_display by matnr ascending period ascending.

    data(lv_prevavail) = i_mdsta-labst.
    loop at t_display reference into data(lr_display) where matnr = it_marc-matnr.
      lr_display->demn = lr_display->pir + lr_display->schda.
      lr_display->avail = lv_prevavail +
                          lr_display->schda +
                          lr_display->pir +
                          lr_display->depreq +
                          lr_display->shpgnt +
                          lr_display->prod +
                          lr_display->purc +
                          lr_display->inplnt +
                          lr_display->delvry.

      lv_prevavail = lr_display->avail.
    endloop.

  endloop.



endform.                    " GET_MRP

form add_mng_to_period using    p_i_mdez    type mdez
                                var         type char6
                                matnr       type matnr
                      changing p_wa_display like t_display.
  data: month0 type char2,
        year0  type char4,
        week0  like scal-week.
  if p_week is not initial.

    call function 'DATE_GET_WEEK'
      exporting
        date = p_i_mdez-dat00
      importing
        week = week0.

    period = week0.
  else.

    call function 'CACS_DATE_GET_YEAR_MONTH'
      exporting
        i_date  = p_i_mdez-dat00
      importing
        e_month = month0
        e_year  = year0.

    period = |{ year0 }{ month0 }|.

  endif.

  read table t_display assigning field-symbol(<fs_display>) with key period = period matnr = matnr.
  if <fs_display> is not assigned.
    t_display = value #( base t_display ( matnr = it_marc-matnr
                                          dispo = it_marc-dispo
                                          resvp = it_marc-resvp
                                          vrmod = it_marc-vrmod
                                          vint2 = it_marc-vint2
                                          vint1 = it_marc-vint1
                                          eisbe = it_marc-eisbe
                                          period = period ) ).
  endif.

  case var.
    when 'PrdOrd' or 'PldOrd'.

      if i_mdez-delb0 = 'PrdOrd'.
*        data(length) = strlen( i_mdez-extra ) - 4.
*        case i_mdez-extra+length(4).
        split i_mdez-extra at '/' into data(lv_number) data(lv_type) data(lv_number2).
        case lv_type.
          when 'PP01' or 'ZPCK' or 'ZRWK'.
*              Do nothing
          when others.
            return.
        endcase.
      endif.

      read table t_display assigning <fs_display> with key period = period matnr = matnr.
      if <fs_display> is assigned.
        add p_i_mdez-mng01 to <fs_display>-prod.
      endif.
      unassign <fs_display>.

    when 'Delvry' or 'CusOrd'.
      read table t_display assigning <fs_display> with key period = period matnr = matnr.
      if <fs_display> is assigned.
        add p_i_mdez-mng01 to <fs_display>-delvry.
      endif.
      unassign <fs_display>.

    when 'DepReq' or 'OrdRes' or 'Ord.DS'.
      read table t_display assigning <fs_display> with key period = period matnr = matnr.
      if <fs_display> is assigned.
        add p_i_mdez-mng01 to <fs_display>-depreq.
      endif.
      unassign <fs_display>.

    when 'POitem' or 'SchLne'.
      read table t_display assigning <fs_display> with key period = period matnr = matnr.
      if <fs_display> is assigned.
        add p_i_mdez-mng01 to <fs_display>-purc.
      endif.
      unassign <fs_display>.

    when 'ShpgNt'.
      read table t_display assigning <fs_display> with key period = period matnr = matnr.
      if <fs_display> is assigned.
        add p_i_mdez-mng01 to <fs_display>-shpgnt.
      endif.
      unassign <fs_display>.

    when 'InPlnt'.
      read table t_display assigning <fs_display> with key period = period matnr = matnr.
      if <fs_display> is assigned.
        add p_i_mdez-mng01 to <fs_display>-inplnt.
      endif.
      unassign <fs_display>.

    when 'SchAgr'.
      read table t_display assigning <fs_display> with key period = period matnr = matnr.
      if <fs_display> is assigned.
        add p_i_mdez-mng01 to <fs_display>-schda.
      endif.

    when 'IndReq'.
      read table t_display assigning <fs_display> with key period = period matnr = matnr.
      if <fs_display> is assigned.
        add p_i_mdez-mng01 to <fs_display>-pir.
      endif.
      unassign <fs_display>.

    when 'OrgPir'.
      read table t_display assigning <fs_display> with key period = period matnr = matnr.
      if <fs_display> is assigned.
        add p_i_mdez-mng01 to <fs_display>-orgpir.
      endif.
      unassign <fs_display>.

    when 'OrgSA'.
      read table t_display assigning <fs_display> with key period = period matnr = matnr.
      if <fs_display> is assigned.
        add p_i_mdez-mng01 to <fs_display>-orgsa.
      endif.
      unassign <fs_display>.

  endcase.

endform.

form get_original_pir using werks type werks_d..

  select  pdatu, plnmg into table @data(lt_pbed) from pbed inner join pbim on ( pbed~bdzei = pbim~bdzei )
    where pbim~matnr = @it_marc-matnr and
          pbim~werks = @werks and
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

form get_original_sa using werks type werks_d..
  select auart, vbep~vbeln, vbep~posnr, mbdat, vbak~gueen, vbep~abart, ettyp, wmeng, abdis  from vbep inner join vbap on ( vbep~vbeln = vbap~vbeln )
                                                                                and ( vbep~posnr = vbap~posnr )
                                                                 inner join vbak on ( vbep~vbeln = vbak~vbeln )

    where vbap~matnr = @it_marc-matnr and
          vbap~werks = @werks and
          vbap~abgru is initial

    into table @data(lt_vbep).

  loop at lt_vbep into data(ls_vbep).
    if ls_vbep-auart = 'TAM' or ls_vbep-auart = 'ZNAV' or ( ls_vbep-gueen < sy-datum and ls_vbep-gueen is not initial ).
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

form show_output.

  call screen 100.

endform.                    "show_output

form handle_double_click
     using xs_row    type lvc_s_row
           xs_column type lvc_s_col.

endform.                    "handle_double_click

form create_toolbar
     changing xy_object      type ref to cl_alv_event_toolbar_set
              xf_interactive type char01.

  data: ls_toolbar type stb_button.


endform.                    " create_toolbar


form handle_alv_user_command using xf_ucomm type sy-ucomm.

  data: lt_row_no type lvc_t_roid,
        wa_row_no like line of lt_row_no.


endform.                    "handle_alv_user_command

form alv_data_changed
     using xs_data_changed type ref to cl_alv_changed_data_protocol.


endform.                    " alv_data_changed

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


    loop at field_tab
         into wa_field_tab.
      move-corresponding wa_field_tab to wa_t_fieldcat.
      wa_t_fieldcat-coltext = wa_field_tab-reptext_ddic.
      append wa_t_fieldcat to t_fieldcat.
    endloop.


* A (all layouts); X (only global); U (save only user specific layouts).
    ls_variant-report = sy-repid.
    lf_save           = 'A'.
    perform set_layout
            changing ls_layout.

* Restrict generic functions: no addition/moving/deleting lines.
    perform exclude_toolbar changing lt_exclude.

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


form set_layout
     changing ys_layout type lvc_s_layo.

*{   REPLACE        MDEK903925                                        1
*\  ys_layout-grid_title = 'MCi Net Adjust Report'.
  ys_layout-grid_title = 'MD04 Requirements raw data Report'.
*}   REPLACE
  ys_layout-cwidth_opt = 'X'.
*  ys_layout-detailinit = 'X'.
  ys_layout-info_fname = 'LINE_COLOUR'. "Colour rows in internal table
*  ys_layout-zebra      = 'X'.
*  ys_layout-sel_mode   = 'X'.
*  ys_layout-no_toolbar = 'X'.
* ys_layout-edit       = 'X'. "Set the whole display to editable

endform.                    "set_layout


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

module status_0100 output.

  set pf-status '0100'.
  set titlebar '100'.

endmodule.                 " status_0100  OUTPUT

module init_controls_0100 output.

  if o_alv_main is initial.
    perform: create_parent_container,
             create_grid_control.
  endif.

endmodule.                 " init_controls_0100  OUTPUT

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
*& Form GET_MRP_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_PLANTS_WERKS
*&---------------------------------------------------------------------*
form get_mrp_data  using  werks type werks_d.
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

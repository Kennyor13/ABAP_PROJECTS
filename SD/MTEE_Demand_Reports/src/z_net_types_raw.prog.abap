*&---------------------------------------------------------------------*
*& Include          Z_NET_TYPES_RAW
*&---------------------------------------------------------------------*


tables:
  mara,
  marc,
  mard,
  rmcp3,
  mdez.

types: begin of ty_display,
         matnr(35) type c,
         dispo     type char4,    " MRP
         resvp     type char3,
         vrmod     type char1,
         vint2     type char3,
         vint1     type char3,
         eisbe     type p decimals 0,
         period    type char6,
         orgpir    type p decimals 0,
         pir       type p decimals 0,
         orgsa     type p decimals 0,
         schda     type p decimals 0,
         depreq    type p decimals 0,
         delvry    type p decimals 0,
         demn      type p decimals 0,
         prod      type p decimals 0,
         shpgnt    type p decimals 0,
         inplnt    type p decimals 0,
         purc      type p decimals 0,
         avail     type p decimals 0,
       end of ty_display.


types: begin of ty_marc,
         matnr like marc-matnr,
         werks like marc-werks,
         dispo like marc-dispo,
         vrmod like marc-vrmod,
         resvp like marc-resvp,
         vint2 like marc-vint2,
         vint1 like marc-vint1,
         eisbe like marc-eisbe,
         maktx like makt-maktx,

       end of ty_marc.

types: begin of ty_periods,
         calendarperiod type num2,
         yearperiod     type num6,
         total_period   type num2,
       end of ty_periods.

data: week(10) type c.

data: period   type char6,
      p_work   like crhd-arbpl,
      p_yweek  type num6,
      p_ymonth type num6.

data: w_dispo type dispo.

data: it_marc type ty_marc occurs 0 with header line,
      wa_marc type ty_marc.

field-groups: it_marc2.


data: t_display  type ty_display occurs 0,
      wa_display type ty_display,
      gt_periods type ty_periods occurs 0.


data: i_mdez  like mdez occurs 0 with header line,
      i_mdsta like mdsta occurs 0 with header line.

data i_exit.

include: <icon>.

type-pools: slis,
            cntb.

* ALV layout definities.
data: t_fieldcat   type lvc_t_fcat,
      field_tab    type slis_t_fieldcat_alv,
      wa_field_tab type slis_fieldcat_alv.

* Algemene data.
data: okcode            type syst-ucomm.

data: c_true  type boolean value 'X',
      c_false type boolean value ''.

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

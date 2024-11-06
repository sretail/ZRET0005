*&---------------------------------------------------------------------*
*& Include          ZRET0005_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS: gc_tabname     TYPE lvc_tname VALUE 'GT_ORDERS',
           gc_flag_change TYPE bapiflag  VALUE 'U',
           gc_lifnr_help  TYPE shlpname  VALUE 'KRED',
           gc_lifnr_name  TYPE fdname    VALUE 'LIFNR',
           gc_attyp_01    TYPE attyp     VALUE '01'. "Generic material

*----------------------------------------------------------------------*
* DICTIONARY TABLES
*----------------------------------------------------------------------*
TABLES: vbak, vbpa,mara,ekko,ekpo,t683.

*----------------------------------------------------------------------*
* GLOBAL TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF gtyp_s_orders,
         vbeln        TYPE vbak-vbeln,
         auart        TYPE vbak-auart,
         auartt       type TVAKT_BEZEI,
         kunnr        TYPE kunnr,
         name_sold    TYPE name1_gp,
         ship_to      TYPE kunnr,
         name_ship    TYPE name1_gp,
         bstnk        TYPE bstnk,
         bstdk        TYPE bstdk,
         audat        TYPE audat,
         posnr        TYPE vbap-posnr,
         matnr        TYPE matnr,
         maktx        TYPE maktx,
         inco1        TYPE inco1,
         inco2        TYPE inco2,
         zterm        TYPE dzterm,
         ztermt       TYPE TEXT1_052,
         edatu        TYPE edatu,
         lifsk        TYPE lifsk,
         lifskt       type BEZEI_LIFSP,
         eindt        TYPE eindt,
         agent        TYPE kunnr,
         name_agent   TYPE name1_gp,
         freight_ag   TYPE kunnr,
         name_freight TYPE name1_gp,
         werks        TYPE werks_d,
         name_werks   TYPE name1_gp,
         valdt        TYPE valdt,
         faksk        TYPE faksk,
         fakskt       type BEZEI_FAKSP,
         vsart        TYPE vsarttr,
         attyp        TYPE attyp,
         waerk        TYPE waerk,
         lgort        like vbap-lgort,
         lgortt       like t001l-lgobe,
         condh_01     type kbetr,
         condh_01_w   TYPE konwa,
         condh_02     type kbetr,
         condh_02_w   TYPE konwa,
         condh_03     type kbetr,
         condh_03_w   TYPE konwa,
         condh_04     type kbetr,
         condh_04_w   TYPE konwa,
         condh_05     type kbetr,
         condh_05_w   TYPE konwa,
         condh_06     type kbetr,
         condh_06_w   TYPE konwa,
         condh_07     type kbetr,
         condh_07_w   TYPE konwa,
         condh_08     type kbetr,
         condh_08_w   TYPE konwa,
         condh_09     type kbetr,
         condh_09_w   TYPE konwa,
         condh_10     type kbetr,
         condh_10_w   TYPE konwa,
         condh_11     type kbetr,
         condh_11_w   TYPE konwa,
         condh_12     type kbetr,
         condh_12_w   TYPE konwa,
         condh_13     type kbetr,
         condh_13_w   TYPE konwa,
         condh_14     type kbetr,
         condh_14_w   TYPE konwa,
         condh_15     type kbetr,
         condh_15_w   TYPE konwa,
         condp_01     type kbetr,
         condp_01_w   TYPE konwa,
         condp_02     type kbetr,
         condp_02_w   TYPE konwa,
         condp_03     type kbetr,
         condp_03_w   TYPE konwa,
         condp_04     type kbetr,
         condp_04_w   TYPE konwa,
         condp_05     type kbetr,
         condp_05_w   TYPE konwa,
         condp_06     type kbetr,
         condp_06_w   TYPE konwa,
         condp_07     type kbetr,
         condp_07_w   TYPE konwa,
         condp_08     type kbetr,
         condp_08_w   TYPE konwa,
         condp_09     type kbetr,
         condp_09_w   TYPE konwa,
         condp_10     type kbetr,
         condp_10_w   TYPE konwa,
         condp_11     type kbetr,
         condp_11_w   TYPE konwa,
         condp_12     type kbetr,
         condp_12_w   TYPE konwa,
         condp_13     type kbetr,
         condp_13_w   TYPE konwa,
         condp_14     type kbetr,
         condp_14_w   TYPE konwa,
         condp_15     type kbetr,
         condp_15_w   TYPE konwa,

       END OF gtyp_s_orders,
       gtyp_t_orders     TYPE STANDARD TABLE OF gtyp_s_orders,
       gtyp_t_orders_key TYPE STANDARD TABLE OF gtyp_s_orders WITH NON-UNIQUE DEFAULT KEY.


TYPES: BEGIN OF gtyp_s_vbpa,
         vbeln TYPE vbeln,
         posnr TYPE posnr,
         kunnr TYPE kunnr,
         parvw TYPE parvw,
         lifnr TYPE lifnr,
       END OF gtyp_s_vbpa,
       gtyp_t_vbpa TYPE STANDARD TABLE OF gtyp_s_vbpa.

TYPES: BEGIN OF gtyp_s_kna1,
         kunnr TYPE kunnr,
         name1 TYPE name1_gp,
       END OF gtyp_s_kna1,
       gtyp_t_kna1 TYPE HASHED TABLE OF gtyp_s_kna1 WITH UNIQUE KEY kunnr.

TYPES: BEGIN OF gtyp_s_lfa1,
         lifnr TYPE lifnr,
         name1 TYPE name1_gp,
       END OF gtyp_s_lfa1,
       gtyp_t_lfa1 TYPE HASHED TABLE OF gtyp_s_lfa1 WITH UNIQUE KEY lifnr.

TYPES: BEGIN OF gtyp_s_vbkd,
         vbeln TYPE vbeln,
         posnr TYPE posnr,
         inco1 TYPE inco1,
         inco2 TYPE inco2,
         zterm TYPE dzterm,
         valdt TYPE valdt,
         vsart TYPE vsarttr,
         ztermt type TEXT1_052,
       END OF gtyp_s_vbkd,
       gtyp_t_vbkd TYPE HASHED TABLE OF gtyp_s_vbkd WITH UNIQUE KEY vbeln posnr.

TYPES: BEGIN OF gtyp_s_vbep,
         vbeln TYPE vbeln_va,
         posnr TYPE posnr_va,
         etenr TYPE etenr,
         edatu TYPE edatu,
       END OF gtyp_s_vbep,
       gtyp_t_vbep TYPE STANDARD TABLE OF  gtyp_s_vbep.

TYPES: BEGIN OF gtyp_s_eket,
         ebeln TYPE ebeln,
         ebelp TYPE ebelp,
         eindt TYPE eindt,
       END OF gtyp_s_eket,
       gtyp_t_eket TYPE STANDARD TABLE OF gtyp_s_eket.

TYPES: BEGIN OF gtyp_s_ekpa,
         ebeln TYPE ebeln,
         ekorg TYPE ekorg,
         lifn2 TYPE lifnr,
       END OF gtyp_s_ekpa,
       gtyp_t_ekpa TYPE STANDARD TABLE OF gtyp_s_ekpa.

TYPES: BEGIN OF gtyp_s_makt,
         matnr TYPE matnr,
         maktx TYPE maktx,
         attyp TYPE attyp,
       END OF gtyp_s_makt,
       gtyp_t_makt TYPE STANDARD TABLE OF gtyp_s_makt.

TYPES: BEGIN OF gtyp_s_t001w,
         werks TYPE werks_d,
         name1 TYPE name1,
       END OF gtyp_s_t001w,
       gtyp_t_t001w TYPE STANDARD TABLE OF gtyp_s_t001w.

TYPES: BEGIN OF gtyp_s_change,
         bstnk      TYPE bstnk,
         bstdk      TYPE bstdk,
         inco1      TYPE vbkd-inco1,
         inco1t     type BEZEI30,
         inco2      TYPE inco2,
         zterm      TYPE dzterm,
         ztermt     TYPE TEXT1_052,
         edatu      TYPE edatu,
         eindt      TYPE eeind,
         lifsk      TYPE lifsk,
         lifskt     TYPE BEZEI_LIFSP,
         agent      like kna1-kunnr,
         AGENTt      like kna1-name1,
         freight_ag like lfa1-lifnr,
         freight_agt like lfa1-name1,
         zlsch      TYPE dzlsch,
         valdt      TYPE valdt,
         knprs      TYPE knprs,
         knprst     type text60,
         faksk      TYPE faksk,
         fakskt     type BEZEI_FAKSP,
         vsart      TYPE vsarttr,
         vsartt     TYPE VERSARTBEZ,
         shipto     like kna1-kunnr,
         shiptot     like kna1-name1,
       END OF gtyp_s_change.

TYPES: BEGIN OF gtyp_s_changex,
         bstnk      TYPE xfeld,
         bstdk      TYPE xfeld,
         inco1      TYPE xfeld,
         inco2      TYPE xfeld,
         zterm      TYPE xfeld,
         edatu      TYPE xfeld,
         lifsk      TYPE xfeld,
         eindt      TYPE xfeld,
         agent      TYPE xfeld,
         freight_ag TYPE xfeld,
         zlsch      TYPE xfeld,
         valdt      TYPE xfeld,
         knprs      TYPE xfeld,
         faksk      TYPE xfeld,
         vsart      TYPE xfeld,
         shipto     TYPE xfeld,
       END OF gtyp_s_changex.

TYPES: gtyp_s_log TYPE bapiret2,
       gtyp_t_log TYPE TABLE OF gtyp_s_log.


TYPES: BEGIN OF gtyp_s_job_exec,
         param_upd         TYPE char1,
         param_del         TYPE char1,
         param_test        TYPE char1,
         var_gvpos         TYPE char1,
         param_frei        TYPE parvw,
         param_agen        TYPE parvw,
         param_ship        TYPE parvw,
         struc_change      TYPE gtyp_s_change,
         struc_changex     TYPE gtyp_s_changex,
         tab_orders        TYPE gtyp_t_orders_key,
         tab_orders_change TYPE gtyp_t_orders_key,
         tab_index_rows    TYPE lvc_t_row.
TYPES: END OF gtyp_s_job_exec.

TYPES: BEGIN OF gtyp_s_material,
         matnr TYPE matnr,
         abgru TYPE abgru_va,
         abgrut TYPE BEZEI40,
       END OF gtyp_s_material.

TYPES: BEGIN OF gtyp_s_condition,
         kschl TYPE kscha,
         kbetr TYPE vfprc_element_amount,
       END OF gtyp_s_condition.

TYPES: gtyp_s_bapisdcond TYPE  bapisdcond,
       gtyp_t_bapisdcond TYPE STANDARD TABLE OF gtyp_s_bapisdcond.

TYPES: gtyp_s_bapisdhd TYPE bapisdhd,
       gtyp_t_bapisdhd TYPE STANDARD TABLE OF gtyp_s_bapisdhd.

TYPES: gtyp_s_bapisdit TYPE bapisdit,
       gtyp_t_bapisdit TYPE STANDARD TABLE OF gtyp_s_bapisdit.

TYPES: gtyp_s_bapicond TYPE  bapicond,
       gtyp_t_bapicond TYPE STANDARD TABLE OF gtyp_s_bapicond.

TYPES: gtyp_s_bapicondx TYPE bapicondx,
       gtyp_t_bapicondx TYPE STANDARD TABLE OF gtyp_s_bapicondx.

*===================================================================================================
* DEFINICIONES GLOBALES
*===================================================================================================
DATA:
  git_condh like ZRET0005P01 occurs 0 WITH HEADER LINE,
  git_condp like ZRET0005P01 occurs 0 WITH HEADER LINE,
  gf_cambiar_precio,
  gf_cambiar_descuento,
  gd_kschl_precio type kschl,

  gd_parvw_transportista type parvw,
  gd_parvw_agente type parvw,
  gd_parvw_direntrega type parvw.


DATA: gt_index_rows    TYPE lvc_t_row,
      gt_orders        TYPE gtyp_t_orders,
      gt_orders_change TYPE gtyp_t_orders,
      gt_kna1          TYPE gtyp_t_kna1.


DATA: gs_change    TYPE gtyp_s_change,
      gs_changex   TYPE gtyp_s_changex,
      gs_material  TYPE gtyp_s_material,
      gs_condition TYPE gtyp_s_condition.

DATA: gv_check    TYPE char1,
      gv_error    TYPE char1,
      gv_pos      TYPE char1,
      gv_exec_job TYPE char1,
      gv_id_job   TYPE char18.

" Control variables for dynpro
DATA: ok_code  TYPE sy-ucomm,
      ok_code2 TYPE sy-ucomm,
      ok_code4 TYPE sy-ucomm,
      ok_code5 TYPE sy-ucomm.



" ALV variables
DATA: gref_alvgrid   TYPE REF TO cl_gui_alv_grid,          " ALV object
      gref_container TYPE REF TO cl_gui_custom_container,  " ALV Container
      gt_fieldcat    TYPE lvc_t_fcat,                      " ALV catalog
      gt_exclude     TYPE ui_functions,                    " ALV exclude buttons
      gs_layout      TYPE lvc_s_layo,                      " ALV layout
      gs_variant     TYPE disvariant,                      " ALV variant
      gref_dock      TYPE REF TO cl_gui_docking_container. " ALV dock for background processing


* LOG variables
DATA: gv_log_handle TYPE balloghndl,  " Log
      gv_showlog    TYPE char1,       " Display log
      gt_log        TYPE gtyp_t_log.


*===================================================================================================
* RANGOS GLOBALES
*===================================================================================================
ranges: gran_kschl_desc for prcd_elements-kschl.

*-----------------------------------------------------------------------
* C L A S S E S
*-----------------------------------------------------------------------
CLASS gcl_event DEFINITION.
  PUBLIC SECTION.
    METHODS:
* New Toolbar buttons in ALV
      handle_toolbar
                    FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

* Actions for new toolbar buttons in ALV
      handle_user_command
                    FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.                    "gcl_event DEFINITION
* Implementacion de Metodos.
CLASS gcl_event IMPLEMENTATION.
* New Toolbar buttons in ALV
  METHOD handle_toolbar.
    PERFORM f_handle_toolbar_100 USING e_object.
  ENDMETHOD.                    "handle_toolbar
* Actions for new toolbar buttons in ALV
  METHOD handle_user_command.
    PERFORM f_user_command_toolbar_100 USING e_ucomm.
  ENDMETHOD.                           "handle_user_command
ENDCLASS.                    "gcl_event IMPLEMENTATION
* Class Instance of gcl_event.
DATA: gref_event TYPE REF TO gcl_event.

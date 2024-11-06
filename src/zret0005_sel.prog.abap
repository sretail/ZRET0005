*&---------------------------------------------------------------------*
*& Include          ZRET0005_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.

PARAMETERS:     p_vkorg TYPE vbak-vkorg      MODIF ID h1.
SELECT-OPTIONS: s_auart FOR vbak-auart       MODIF ID h1,
                s_vbeln FOR vbak-vbeln       MODIF ID h1,
                s_kunnr FOR vbak-kunnr       MODIF ID h1,
                s_kunwe FOR vbpa-kunnr       MODIF ID h1,
                s_kunze FOR vbpa-kunnr       MODIF ID h1.

SELECT-OPTIONS: s_matnr FOR mara-matnr,
                s_vtweg FOR vbak-vtweg,
                s_spart FOR vbak-spart.


*PARAMETERS: p_frei TYPE tpar-parvw,
*            p_agen TYPE tpar-parvw MODIF ID h1,
*            p_ship TYPE tpar-parvw MODIF ID h1.

PARAMETERS:     p_test  AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-003.
PARAMETERS:p_layout  TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4 .


*************** NO DISPLAY PARAMETERS TO RUN IN BACKGROUND ****************
PARAMETER: bgindx  TYPE indx_srtfd NO-DISPLAY.
***************************************************************************


*----------------------------------------------------------------------*
* AT SELECTION SCREEN
*----------------------------------------------------------------------*


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  PERFORM f_get_layout.

AT SELECTION-SCREEN OUTPUT.
  PERFORM f_change_selection_screen.

INITIALIZATION.
  perform f_get_parametrizacion.

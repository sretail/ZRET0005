*&---------------------------------------------------------------------*
*& Include          ZRET0005_FORMS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GENERATE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*----------------------------------------------------------------------*
FORM f_generate_log CHANGING pvc_log_handle TYPE balloghndl.

  DATA: ls_log TYPE bal_s_log.

  CLEAR: ls_log.

  "Define variables to create a log
  ls_log-extnumber = 'Application Log'.
  ls_log-aluser = sy-uname.
  ls_log-alprog = sy-repid.

  "Create log
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log      = ls_log
    IMPORTING
      e_log_handle = pvc_log_handle
    EXCEPTIONS
      OTHERS       = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " GENERATE_LOG
*&---------------------------------------------------------------------*
*&      Form  FREE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_free_log USING pvu_log_handle TYPE balloghndl.

  "Free log
  CALL FUNCTION 'BAL_LOG_REFRESH'
    EXPORTING
      i_log_handle  = pvu_log_handle
    EXCEPTIONS
      log_not_found = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " FREE_LOG
*&---------------------------------------------------------------------*
*&      Form  SHOW_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_show_log USING pvu_showlog TYPE char1.

  DATA: ls_display_profile TYPE bal_s_prof.

  CHECK pvu_showlog IS NOT INITIAL.

  CLEAR: ls_display_profile.

  ls_display_profile-disvariant-report = sy-repid.
  ls_display_profile-disvariant-handle = 'LOG'.

  "Show log
  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXPORTING
      i_s_display_profile = ls_display_profile
    EXCEPTIONS
      OTHERS              = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " SHOW_LOG
*&---------------------------------------------------------------------*
*&      Form  FILL_LOG_FROM_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_fill_log_from_bapi USING pvu_log_handle TYPE balloghndl
                                ptu_log_bapi   TYPE bapiret2_t
                       CHANGING pvc_showlog    TYPE char1.

  DATA: ls_log_bapi TYPE bapiret2.

  LOOP AT ptu_log_bapi INTO ls_log_bapi.
    PERFORM f_fill_log USING pvu_log_handle         ls_log_bapi-type
                             ls_log_bapi-id         ls_log_bapi-number
                             ls_log_bapi-message_v1 ls_log_bapi-message_v2
                             ls_log_bapi-message_v3 ls_log_bapi-message_v4
                    CHANGING pvc_showlog.
  ENDLOOP.

ENDFORM.                    " FILL_LOG_FROM_BAPI
*&---------------------------------------------------------------------*
*&      Form  FILL_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_fill_log USING pvu_log_handle TYPE balloghndl
                      pvu_type       TYPE symsgty
                      pvu_id         TYPE symsgid
                      pvu_number     TYPE symsgno
                      pvu_message_v1 TYPE symsgv
                      pvu_message_v2 TYPE symsgv
                      pvu_message_v3 TYPE symsgv
                      pvu_message_v4 TYPE symsgv
             CHANGING pvc_showlog    TYPE char1.

  DATA: ls_msg TYPE bal_s_msg.

  CLEAR: ls_msg.

  MOVE    pvu_type         TO  ls_msg-msgty.
  MOVE    pvu_id           TO  ls_msg-msgid.
  MOVE    pvu_number       TO  ls_msg-msgno.
  MOVE    pvu_message_v1   TO  ls_msg-msgv1.
  MOVE    pvu_message_v2   TO  ls_msg-msgv2.
  MOVE    pvu_message_v3   TO  ls_msg-msgv3.
  MOVE    pvu_message_v4   TO  ls_msg-msgv4.

  "add message to log
  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle = pvu_log_handle
      i_s_msg      = ls_msg
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    pvc_showlog = 'X'. "Define if log has any message
  ENDIF.

ENDFORM.                    " FILL_LOG

*&---------------------------------------------------------------------*
*&      Form  F_CHANGE_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_change_selection_screen .

  LOOP AT SCREEN.
    "Display fields
    CASE screen-group1.
      WHEN 'H1'.
        screen-invisible = 0.
        screen-active    = 1.
      WHEN 'H2'.
        screen-invisible = 1.
        screen-active    = 0.
      WHEN 'H3'.
        screen-invisible = 1.
        screen-active    = 0.
    ENDCASE.

    "obligatory fields
    CASE screen-name.
      WHEN 'P_VKORG'.
        screen-required = 2.
      WHEN 'S_AUART-LOW'.
        screen-required = 2.
      WHEN 'S_VTWEG-LOW'.
        screen-required = 2.
      WHEN 'S_SPART-LOW'.
        screen-required = 2.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_REQUIRED_FILES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_VKORG  text
*      -->P_S_AUART  text
*----------------------------------------------------------------------*
FORM f_check_required_files  USING  pvu_upd TYPE xfeld.


  IF p_vkorg IS INITIAL OR s_auart[] IS INITIAL
  OR s_vtweg[] IS INITIAL OR s_spart[] IS INITIAL.
    "Please complete all required fields for the selected option.
    MESSAGE s000(zret0005) DISPLAY LIKE 'E'.
    STOP.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_SALES_ORDERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_SALES_ORDERS  text
*----------------------------------------------------------------------*
FORM f_get_sales_orders  CHANGING ptc_orders TYPE gtyp_t_orders.

  DATA: lt_vbpa      TYPE gtyp_t_vbpa,
        ls_vbpa      TYPE gtyp_s_vbpa,
        lt_vbkd      TYPE gtyp_t_vbkd,
        ls_vbkd      TYPE gtyp_s_vbkd,
        ls_orders    TYPE gtyp_s_orders,
        lt_vbep      TYPE gtyp_t_vbep,
        ls_vbep      TYPE gtyp_s_vbep,
        ls_kna1      TYPE gtyp_s_kna1,
        lt_orders    TYPE gtyp_t_orders,
        lt_makt      TYPE gtyp_t_makt,
        ls_makt      TYPE gtyp_s_makt,
        lv_index     TYPE i VALUE '1',
        lv_index_aux TYPE i.

  REFRESH: lt_vbpa, lt_vbkd, ptc_orders,gt_kna1,lt_vbep.
  CLEAR: ls_vbpa, ls_vbkd,ls_orders,ls_kna1,ls_vbep.

  IF s_kunwe[] IS NOT INITIAL OR s_kunze[] IS NOT INITIAL.

    SELECT vbeln posnr kunnr parvw lifnr
    FROM vbpa
    INTO CORRESPONDING FIELDS OF TABLE lt_vbpa
    WHERE vbeln  IN s_vbeln
      AND  kunnr IN s_kunwe
      AND  kunnr IN s_kunze.

    IF lt_vbpa IS NOT INITIAL.
      SELECT vbak~vbeln,
             vbap~posnr,
             vbak~auart,
             tvakt~bezei AS auartt,
             vbak~lifsk,
             tvlst~vtext AS lifskt,
             vbak~kunnr,
             vbap~matnr,
             vbak~bstnk,
             vbak~audat,
             vbak~faksk,
             tvfst~vtext AS fakskt,
             vbak~waerk,
             vbap~werks,
             t001w~name1 AS name_werks,
             vbap~lgort,
             t001l~lgobe AS lgortt
        INTO CORRESPONDING FIELDS OF TABLE @ptc_orders
        FROM vbap INNER JOIN vbak ON vbap~vbeln = vbak~vbeln
                  LEFT JOIN t001w ON t001w~werks = vbap~werks
                  LEFT JOIN t001l ON t001l~werks = vbap~werks AND t001l~lgort = vbap~lgort
                  LEFT JOIN tvakt ON tvakt~spras = @sy-langu AND tvakt~auart = vbak~auart
                  LEFT JOIN tvlst ON tvlst~spras = @sy-langu AND tvlst~lifsp = vbak~lifsk
                  LEFT JOIN tvfst ON tvfst~spras = @sy-langu AND tvfst~faksp = vbak~faksk
         FOR ALL ENTRIES IN @lt_vbpa
       WHERE vbak~vbeln =  @lt_vbpa-vbeln
         AND vbap~matnr IN @s_matnr
         AND vbak~auart IN @s_auart
         AND vbak~vkorg EQ @p_vkorg
         AND vbak~kunnr IN @s_kunnr
         AND vbap~abgru EQ '' "  No cancel
         AND vbak~vtweg IN @s_vtweg
         AND vbak~spart IN @s_spart.

      lt_orders = ptc_orders.
      SORT lt_orders BY kunnr.
      DELETE ADJACENT DUPLICATES FROM lt_orders COMPARING kunnr.


      LOOP AT lt_orders INTO ls_orders.
        ls_vbpa-kunnr = ls_orders-kunnr.
        COLLECT ls_vbpa INTO lt_vbpa.
      ENDLOOP.

    ENDIF.
  ELSE.
    SELECT vbak~vbeln,
           vbap~posnr,
           vbak~auart,
           tvakt~bezei AS auartt,
           vbak~lifsk,
           tvlst~vtext AS lifskt,
           vbak~kunnr,
           vbap~matnr,
           vbak~bstnk,
           vbak~audat,
           vbak~faksk,
           tvfst~vtext AS fakskt,
           vbak~waerk,
           vbap~werks,
           t001w~name1 AS name_werks,
           vbap~lgort,
           t001l~lgobe AS lgortt
      INTO CORRESPONDING FIELDS OF TABLE @ptc_orders
      FROM vbap INNER JOIN vbak ON vbap~vbeln = vbak~vbeln
                LEFT JOIN t001w ON t001w~werks = vbap~werks
                LEFT JOIN t001l ON t001l~werks = vbap~werks AND t001l~lgort = vbap~lgort
                LEFT JOIN tvakt ON tvakt~spras = @sy-langu AND tvakt~auart = vbak~auart
                LEFT JOIN tvlst ON tvlst~spras = @sy-langu AND tvlst~lifsp = vbak~lifsk
                LEFT JOIN tvfst ON tvfst~spras = @sy-langu AND tvfst~faksp = vbak~faksk
     WHERE vbak~vbeln IN @s_vbeln
       AND vbap~matnr IN @s_matnr
       AND vbak~auart IN @s_auart
       AND vbak~vkorg EQ @p_vkorg
       AND vbak~kunnr IN @s_kunnr
       AND vbap~abgru EQ ' ' "  No cancel
       AND vbak~vtweg IN @s_vtweg
       AND vbak~spart IN @s_spart.

    IF ptc_orders IS NOT INITIAL.

      SORT ptc_orders BY vbeln posnr.

      SELECT vbeln posnr kunnr parvw lifnr
        FROM vbpa
        INTO CORRESPONDING FIELDS OF TABLE lt_vbpa
        FOR ALL ENTRIES IN ptc_orders
        WHERE vbeln = ptc_orders-vbeln.
    ENDIF.
  ENDIF.

  SORT ptc_orders BY vbeln posnr.
  SORT lt_vbpa    BY vbeln posnr.

  IF lt_vbpa IS NOT INITIAL.
    SELECT kunnr name1
      FROM kna1
      INTO CORRESPONDING FIELDS OF TABLE gt_kna1
      FOR ALL ENTRIES IN lt_vbpa
      WHERE  kunnr = lt_vbpa-kunnr.

    SELECT lifnr AS kunnr name1
      FROM lfa1
      APPENDING CORRESPONDING FIELDS OF TABLE gt_kna1
      FOR ALL ENTRIES IN lt_vbpa
      WHERE lifnr = lt_vbpa-lifnr.

  ENDIF.

  IF ptc_orders IS NOT INITIAL.
    SELECT vbeln,
           posnr,
           inco1,
           inco2,
           zterm,
           valdt,
           vsart
      from vbkd
      INTO CORRESPONDING FIELDS OF TABLE @lt_vbkd
      FOR ALL ENTRIES IN @ptc_orders
      WHERE vbeln = @ptc_orders-vbeln.

    loop at lt_vbkd into ls_vbkd.
      select single text1
        from t052u
        into ls_vbkd-ztermt
       where spras = sy-langu
         and zterm = ls_vbkd-zterm.

      MODIFY lt_vbkd from ls_vbkd.
    endloop.


    SELECT vbeln posnr etenr edatu
      FROM vbep
      INTO CORRESPONDING FIELDS OF TABLE lt_vbep
      FOR ALL ENTRIES IN ptc_orders
      WHERE vbeln = ptc_orders-vbeln
        AND posnr = ptc_orders-posnr.

    SELECT mara~matnr maktx attyp
      FROM mara
      INNER JOIN makt ON mara~matnr = makt~matnr
      INTO CORRESPONDING FIELDS OF TABLE lt_makt
      FOR ALL ENTRIES IN ptc_orders
      WHERE mara~matnr = ptc_orders-matnr
        AND spras = sy-langu.
  ENDIF.

  SORT lt_vbkd BY vbeln posnr.
  SORT lt_vbep BY vbeln posnr.

  " Fill alv
  LOOP AT ptc_orders INTO ls_orders.

    "Name sold to
    CLEAR ls_kna1.
    READ TABLE gt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_orders-kunnr.
    IF sy-subrc = 0.
      ls_orders-name_sold = ls_kna1-name1.
    ENDIF.

    "Ship to
    CLEAR: ls_vbpa.
    READ TABLE lt_vbpa INTO ls_vbpa WITH KEY vbeln = ls_orders-vbeln
                                             posnr = ls_orders-posnr
                                             parvw = gd_parvw_direntrega.
    IF sy-subrc = 0.
      ls_orders-ship_to = ls_vbpa-kunnr.
    ELSE.
      CLEAR: ls_vbpa.
      READ TABLE lt_vbpa INTO ls_vbpa WITH KEY vbeln = ls_orders-vbeln
                                               parvw = gd_parvw_direntrega.
      IF sy-subrc = 0.
        ls_orders-ship_to = ls_vbpa-kunnr.
      ENDIF.
    ENDIF.
    IF  ls_orders-ship_to IS NOT INITIAL.
      CLEAR ls_kna1.

      READ TABLE gt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_orders-ship_to.
      IF sy-subrc = 0.
        ls_orders-name_ship = ls_kna1-name1.
      ENDIF.
    ENDIF.

    "Agent
    CLEAR: ls_vbpa.
    READ TABLE lt_vbpa INTO ls_vbpa WITH KEY vbeln = ls_orders-vbeln
                                             posnr = ls_orders-posnr
                                             parvw = gd_parvw_agente.
    IF sy-subrc = 0.
      ls_orders-agent = ls_vbpa-kunnr.
    ELSE.
      CLEAR ls_vbpa.
      READ TABLE lt_vbpa INTO ls_vbpa WITH KEY vbeln = ls_orders-vbeln
                                               parvw = gd_parvw_agente.
      IF sy-subrc = 0.
        ls_orders-agent = ls_vbpa-kunnr.
      ENDIF.
    ENDIF.
    IF ls_orders-agent IS NOT INITIAL.
      CLEAR ls_kna1.
      READ TABLE gt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_orders-agent.
      IF sy-subrc = 0.
        ls_orders-name_agent = ls_kna1-name1.
      ENDIF.
    ENDIF.


    "Freight agent
    CLEAR: ls_vbpa.
    READ TABLE lt_vbpa INTO ls_vbpa WITH KEY vbeln = ls_orders-vbeln
                                             posnr = ls_orders-posnr
                                             parvw = gd_parvw_transportista .
    IF sy-subrc = 0.
      ls_orders-freight_ag = ls_vbpa-lifnr.
    ELSE.
      READ TABLE lt_vbpa INTO ls_vbpa WITH KEY vbeln = ls_orders-vbeln
                                               parvw = gd_parvw_transportista.
      IF sy-subrc = 0.
        ls_orders-freight_ag = ls_vbpa-lifnr.
      ENDIF.
    ENDIF.
    IF ls_orders-freight_ag IS NOT INITIAL.
      CLEAR ls_kna1.
      READ TABLE gt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_orders-freight_ag.
      IF sy-subrc = 0.
        ls_orders-name_freight = ls_kna1-name1.
      ENDIF.
    ENDIF.


    CLEAR: ls_vbkd.
    READ TABLE lt_vbkd INTO ls_vbkd WITH KEY vbeln = ls_orders-vbeln
                                             posnr = ls_orders-posnr.
    IF sy-subrc = 0.
      ls_orders-inco1 = ls_vbkd-inco1.
      ls_orders-inco2 = ls_vbkd-inco2.
      ls_orders-zterm = ls_vbkd-zterm.
      ls_orders-ztermt = ls_vbkd-ztermt.
      ls_orders-valdt = ls_vbkd-valdt.
      ls_orders-vsart = ls_vbkd-vsart.

    ELSE.
      CLEAR: ls_vbkd.
      READ TABLE lt_vbkd INTO ls_vbkd WITH KEY vbeln = ls_orders-vbeln.
      IF sy-subrc = 0.
        ls_orders-inco1 = ls_vbkd-inco1.
        ls_orders-inco2 = ls_vbkd-inco2.
        ls_orders-zterm = ls_vbkd-zterm.
        ls_orders-ztermt = ls_vbkd-ztermt.
        ls_orders-valdt = ls_vbkd-valdt.
        ls_orders-vsart = ls_vbkd-vsart.
      ENDIF.
    ENDIF.

    "Delivery date
    CLEAR: ls_vbep,lv_index_aux.
    LOOP AT lt_vbep INTO ls_vbep FROM lv_index
                                WHERE vbeln = ls_orders-vbeln
                                  AND posnr = ls_orders-posnr.

      lv_index_aux = sy-tabix.
      ls_orders-edatu = ls_vbep-edatu.
      EXIT.
    ENDLOOP.
    IF lv_index_aux IS NOT INITIAL.
      lv_index = lv_index_aux.
    ENDIF.

    READ TABLE lt_makt INTO ls_makt WITH KEY matnr = ls_orders-matnr.
    IF sy-subrc = 0.
      ls_orders-maktx = ls_makt-maktx.
      ls_orders-attyp = ls_makt-attyp.
    ENDIF.
    MODIFY ptc_orders FROM ls_orders.

  ENDLOOP.

*>Obtener columnas de condiciones de precio de cabecera y posición
  PERFORM f_get_columnas_cond CHANGING ptc_orders.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHANGE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_change_so_data USING pvu_pos TYPE char1.

  DATA: ls_index_rows TYPE LINE OF lvc_t_row,
        ls_orders     TYPE gtyp_s_orders.

  CLEAR: ls_index_rows.
  REFRESH: gt_index_rows,gt_orders_change.

  PERFORM f_get_selected_rows CHANGING gt_index_rows.

  CLEAR: gv_showlog.

  IF gt_index_rows[] IS INITIAL.
    MESSAGE i001(zret0005) DISPLAY LIKE 'W'. "Please select on line at least
  ELSE.

    CLEAR: gs_change, gs_changex,gv_check.

    CALL SCREEN 0200 STARTING AT 15 5.

    IF gv_check = '1'.
      LOOP AT gt_index_rows INTO ls_index_rows.
        READ TABLE gt_orders INTO ls_orders INDEX ls_index_rows-index.
        IF sy-subrc = 0.
          IF gs_changex-bstnk IS NOT INITIAL.
            ls_orders-bstnk = gs_change-bstnk.
          ENDIF.
          IF gs_changex-inco1 IS NOT INITIAL.
            ls_orders-inco1 = gs_change-inco1.
          ENDIF.
          IF gs_changex-inco2 IS NOT INITIAL.
            ls_orders-inco2 = gs_change-inco2.
          ENDIF.
          IF gs_changex-zterm IS NOT INITIAL.
            ls_orders-zterm = gs_change-zterm.
          ENDIF.
          IF gs_changex-edatu IS NOT INITIAL.
            ls_orders-edatu = gs_change-edatu.
          ENDIF.
          IF gs_changex-lifsk IS NOT INITIAL.
            ls_orders-lifsk = gs_change-lifsk.
          ENDIF.
          IF gs_changex-agent IS NOT INITIAL.
            ls_orders-agent = gs_change-agent.
          ENDIF.
          IF gs_changex-freight_ag IS NOT INITIAL.
            ls_orders-freight_ag = gs_change-freight_ag.
          ENDIF.
          IF gs_changex-valdt IS NOT INITIAL.
            ls_orders-valdt = gs_change-valdt.
          ENDIF.
          IF gs_changex-faksk IS NOT INITIAL.
            ls_orders-faksk =  gs_change-faksk.
          ENDIF.
          IF gs_changex-vsart IS NOT INITIAL.
            ls_orders-vsart = gs_change-vsart.
          ENDIF.
          IF gs_changex-shipto IS NOT INITIAL.
            ls_orders-ship_to = gs_change-shipto.
          ENDIF.
          APPEND ls_orders TO gt_orders_change.
        ENDIF.
      ENDLOOP.

      IF gv_exec_job IS INITIAL.
        PERFORM f_fill_bapi_so USING gt_orders_change
                                     pvu_pos.
      ELSE.
        PERFORM f_get_id_job        CHANGING gv_id_job.
        PERFORM f_set_cluster_data     USING gv_id_job      pvu_pos
                                             gt_index_rows  gt_orders_change.
        PERFORM f_run_job              USING gv_id_job.
        PERFORM f_update_flowtab_so    USING gt_orders_change pvu_pos.
      ENDIF.

    ENDIF.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_check_so_selection  USING psu_changex TYPE gtyp_s_changex
                                 psc_change  TYPE gtyp_s_change
                       CHANGING  pcv_error   TYPE char1.

  DATA: lv_inco  TYPE inco1,
        lv_lifsk TYPE lifsk,
        lv_kunnr TYPE kunnr,
        lv_vsart TYPE versart.

  CLEAR: pcv_error.

  IF    psu_changex-bstnk      IS INITIAL AND psu_changex-inco1   IS INITIAL
    AND psu_changex-inco2      IS INITIAL AND psu_changex-zterm   IS INITIAL
    AND psu_changex-edatu      IS INITIAL AND psu_changex-lifsk   IS INITIAL
    AND psu_changex-freight_ag IS INITIAL AND psu_changex-valdt   IS INITIAL
    AND psu_changex-knprs      IS INITIAL AND psu_changex-faksk   IS INITIAL
    AND psu_changex-vsart      IS INITIAL AND psu_changex-shipto  IS INITIAL
    and psu_changex-BSTDK      is initial and psu_changex-eindt   is initial                        "@APRADAS-29.11.2024 08:03:29
    and psu_changex-agent      is initial and psu_changex-ZLSCH   is initial.                       "@APRADAS-29.11.2024 08:03:29

    MESSAGE i005(zret0005)."Please select one field for mass change
    pcv_error = 'X'.

  ELSE.
    IF psu_changex-inco1 IS NOT INITIAL.
      SELECT SINGLE inco1
        INTO lv_inco
        FROM tinc
        WHERE inco1 = psc_change-inco1.
      IF sy-subrc NE 0.
        MESSAGE i002(zret0005) DISPLAY LIKE 'E' WITH TEXT-310. "The value for field Incoterm1 is incorrect
        pcv_error = 'X'.
      ENDIF.
    ENDIF.

    IF psu_changex-lifsk IS NOT INITIAL AND  psc_change-lifsk IS NOT INITIAL.
      SELECT SINGLE lifsp
        INTO lv_lifsk
        FROM tvls
        WHERE lifsp = psc_change-lifsk.
      IF sy-subrc NE 0.
        MESSAGE i002(zret0005) DISPLAY LIKE 'E' WITH TEXT-309 . "The value for field Delivery Block is incorrect
        pcv_error = 'X'.
      ENDIF.
    ENDIF.


    IF psu_changex-agent IS NOT INITIAL.
      PERFORM f_conversion_to_input USING   psc_change-agent
                                   CHANGING psc_change-agent.
      SELECT SINGLE kunnr
        INTO lv_kunnr
        FROM kna1
        WHERE kunnr = psc_change-agent.
      IF sy-subrc NE 0.
        MESSAGE i002(zret0005) DISPLAY LIKE 'E' WITH TEXT-306. "The value for field agent is incorrect
        pcv_error = 'X'.
      ENDIF.
    ENDIF.

    IF psu_changex-freight_ag IS NOT INITIAL.
      PERFORM f_conversion_to_input USING  psc_change-freight_ag
                                  CHANGING psc_change-freight_ag.
      SELECT SINGLE lifnr
        INTO lv_kunnr
        FROM lfa1
        WHERE lifnr = psc_change-freight_ag.
      IF sy-subrc NE 0.
        MESSAGE i002(zret0005) DISPLAY LIKE 'E' WITH TEXT-311. "The value for field freight agent is incorrect
        pcv_error = 'X'.
      ENDIF.
    ENDIF.

    IF psu_changex-knprs IS NOT INITIAL.
      psc_change-knprs = t683-knprs_v.
      CLEAR t683-knprs_v.
    ENDIF.

    "Billing block in SD document
    IF psu_changex-faksk IS NOT INITIAL.
      psc_change-faksk = vbak-faksk.
      CLEAR  vbak-faksk.
    ENDIF.

    IF psu_changex-vsart IS NOT INITIAL.
      IF psc_change-vsart IS NOT INITIAL. "Accept empty values
        SELECT SINGLE vsart
          INTO lv_vsart
          FROM t173
          WHERE vsart EQ psc_change-vsart.
        IF sy-subrc NE 0.
          MESSAGE i002(zret0005) DISPLAY LIKE 'E' WITH TEXT-313.  "The value for field vsart is incorrect
          pcv_error = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF psu_changex-shipto IS NOT INITIAL.
      PERFORM f_conversion_to_input USING psc_change-shipto
                                 CHANGING psc_change-shipto.
      SELECT SINGLE kunnr
        INTO lv_kunnr
        FROM kna1
        WHERE kunnr = psc_change-shipto.
      IF sy-subrc NE 0.
        MESSAGE i002(zret0005) DISPLAY LIKE 'E' WITH TEXT-305. "The value for field shipto is incorrect
        pcv_error = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CALL_POPUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_call_popup CHANGING pcv_check.



  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
*     TITLEBAR      = ' '
*     DIAGNOSE_OBJECT             = ' '
      text_question = TEXT-302
      text_button_1 = TEXT-303
*     ICON_BUTTON_1 = ' '
      text_button_2 = TEXT-304
*     ICON_BUTTON_2 = ' '
*     DEFAULT_BUTTON              = '1'
*     DISPLAY_CANCEL_BUTTON       = 'X'
*     USERDEFINED_F1_HELP         = ' '
*     START_COLUMN  = 25
*     START_ROW     = 6
*     POPUP_TYPE    =
*     IV_QUICKINFO_BUTTON_1       = ' '
*     IV_QUICKINFO_BUTTON_2       = ' '
    IMPORTING
      answer        = pcv_check
* TABLES
*     PARAMETER     =
* EXCEPTIONS
*     TEXT_NOT_FOUND              = 1
*     OTHERS        = 2
    .


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FILL_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_ORDERS_CHANGE  text
*----------------------------------------------------------------------*
FORM f_fill_bapi_so  USING ptu_orders_change TYPE gtyp_t_orders
                           pvu_pos           TYPE char1.

  DATA: ls_order   TYPE gtyp_s_orders,
        ls_return  TYPE gtyp_s_log,
        lv_message TYPE char200,
        lv_error   TYPE sy-subrc.

  DATA: ls_headx          TYPE          bapisdh1x,
        ls_head           TYPE          bapisdh1,
        lt_return         TYPE TABLE OF bapiret2,
        lt_item           TYPE TABLE OF bapisditm,
        ls_item           TYPE          bapisditm,
        lt_itemx          TYPE TABLE OF bapisditmx,
        ls_itemx          TYPE          bapisditmx,
        lt_schedulein     TYPE TABLE OF bapischdl,
        ls_schedulein     TYPE          bapischdl,
        lt_scheduleinx    TYPE TABLE OF bapischdlx,
        ls_scheduleinx    TYPE          bapischdlx,
        lt_partnerchanges TYPE TABLE OF bapiparnrc,
        ls_partnerchanges TYPE          bapiparnrc,
        lt_extensionin    TYPE TABLE OF bapiparex,
        ls_extensionin    TYPE          bapiparex,
        ls_bape_vbak      TYPE          bape_vbak,
        ls_bape_vbakx     TYPE          bape_vbakx,
        ls_logic_switch   TYPE          bapisdls.


  SORT ptu_orders_change BY vbeln posnr.

  LOOP AT ptu_orders_change INTO ls_order.

    ls_item-itm_number = ls_order-posnr.
    ls_itemx-updateflag = gc_flag_change.
    ls_itemx-itm_number = ls_order-posnr.
    ls_item-material = ls_order-matnr.
    ls_itemx-material = 'X'.


    IF gs_changex-vsart IS NOT INITIAL AND pvu_pos IS NOT INITIAL.
      ls_item-ship_type  = ls_order-vsart.
      ls_itemx-ship_type = 'X'.
    ENDIF.

    APPEND ls_item TO lt_item.
    CLEAR  ls_item.

    APPEND ls_itemx TO lt_itemx.
    CLEAR  ls_itemx.

    IF gs_changex-edatu IS NOT INITIAL AND pvu_pos IS NOT INITIAL.
      ls_schedulein-itm_number = ls_order-posnr.
      ls_schedulein-req_date   = ls_order-edatu.
      ls_schedulein-sched_line = '0001'.
      APPEND ls_schedulein TO lt_schedulein.
      CLEAR  ls_schedulein.

      ls_scheduleinx-itm_number  = ls_order-posnr.
      ls_scheduleinx-req_date    = 'X'. " Delivery Date
      ls_scheduleinx-updateflag  = gc_flag_change.
      ls_scheduleinx-sched_line = '0001'.
      APPEND ls_scheduleinx TO lt_scheduleinx.
      CLEAR  ls_scheduleinx.
    ENDIF.

    IF gs_changex-agent IS NOT INITIAL.
      ls_partnerchanges-document   = ls_order-vbeln.
*      ls_partnerchanges-itm_number = ls_order-posnr.
      ls_partnerchanges-updateflag = gc_flag_change.
      ls_partnerchanges-partn_role = gd_parvw_agente.
      ls_partnerchanges-p_numb_new = ls_order-agent.
      APPEND ls_partnerchanges TO lt_partnerchanges.
      CLEAR ls_partnerchanges.
    ENDIF.

    IF gs_changex-freight_ag IS NOT INITIAL.
      ls_partnerchanges-document   = ls_order-vbeln.
*      ls_partnerchanges-itm_number = ls_order-posnr.
      ls_partnerchanges-updateflag = gc_flag_change.
      ls_partnerchanges-partn_role = gd_parvw_transportista.
      ls_partnerchanges-p_numb_new = ls_order-freight_ag.
      APPEND ls_partnerchanges TO lt_partnerchanges.
      CLEAR ls_partnerchanges.
    ENDIF.

    IF gs_changex-shipto IS NOT INITIAL.
      ls_partnerchanges-itm_number = '000000'.
      ls_partnerchanges-document   = ls_order-vbeln.
      ls_partnerchanges-updateflag = gc_flag_change.
      ls_partnerchanges-partn_role = gd_parvw_direntrega.
      ls_partnerchanges-p_numb_new = ls_order-ship_to.
      APPEND ls_partnerchanges TO lt_partnerchanges.
      CLEAR ls_partnerchanges.
    ENDIF.

    IF gs_changex-bstnk IS NOT INITIAL.
      ls_head-purch_no_c = ls_order-bstnk.
      ls_headx-purch_no_c  = 'X'.

    ENDIF.

    IF gs_changex-inco1 IS NOT INITIAL.
      ls_head-incoterms1 = ls_order-inco1.
      ls_headx-incoterms1 = 'X'.
    ENDIF.
    IF gs_changex-inco2 IS NOT INITIAL.
      ls_head-incoterms2 = ls_order-inco2.
      ls_headx-incoterms2 = 'X'.
    ENDIF.
    IF gs_changex-zterm IS NOT INITIAL.
      ls_head-pmnttrms = ls_order-zterm.
      ls_headx-pmnttrms = 'X'.
    ENDIF.
    IF gs_changex-edatu IS NOT INITIAL AND pvu_pos IS INITIAL.
      ls_head-req_date_h = ls_order-edatu.
      ls_headx-req_date_h = 'X'.
    ENDIF.
    IF gs_changex-valdt IS NOT INITIAL.
      ls_head-fix_val_dy = ls_order-valdt.
      ls_headx-fix_val_dy = 'X'.
    ENDIF.

    "Billing block in SD document
    IF gs_changex-faksk IS NOT INITIAL.
      ls_head-bill_block = ls_order-faksk.
      ls_headx-bill_block = abap_true.
    ENDIF.

    "delivery block in SD document
    IF gs_changex-lifsk IS NOT INITIAL.
      ls_head-dlv_block = ls_order-lifsk.
      ls_headx-dlv_block = abap_true.
    ENDIF.

    IF gs_changex-vsart IS NOT INITIAL AND pvu_pos IS INITIAL.
      ls_head-ship_type  = ls_order-vsart.
      ls_headx-ship_type = 'X'.
    ENDIF.

    AT END OF vbeln.

      ls_headx-updateflag = gc_flag_change.

      IF gs_changex-knprs IS NOT INITIAL.
        ls_logic_switch-pricing = gs_change-knprs.
      ENDIF.


      lv_message = ls_order-vbeln.
      "Begin update of sales order &1
      PERFORM f_fill_log USING  gv_log_handle 'S' 'ZRET0005' '003' lv_message(50) lv_message+50(50)
                           lv_message+100(50) lv_message+150(50)
                          CHANGING gv_showlog.

      CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
        EXPORTING
          salesdocument    = ls_order-vbeln
          order_header_in  = ls_head
          order_header_inx = ls_headx
          logic_switch     = ls_logic_switch
        TABLES
          return           = lt_return[]
          order_item_in    = lt_item[]
          order_item_inx   = lt_itemx[]
          partnerchanges   = lt_partnerchanges[]
          schedule_lines   = lt_schedulein[]
          schedule_linesx  = lt_scheduleinx[]
          extensionin      = lt_extensionin[].

      APPEND LINES OF lt_return TO gt_log.

      IF p_test IS NOT INITIAL.
        PERFORM f_bapi_rollback CHANGING gt_log.
      ELSE.

        CLEAR: lv_error, ls_return.
        LOOP AT lt_return INTO ls_return WHERE type  = 'A'
                                           OR  type  = 'E'.
          lv_error = 1.
          EXIT.
        ENDLOOP.

        IF lv_error IS INITIAL.
          CLEAR: ls_return.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait   = 'X'
            IMPORTING
              return = ls_return.
          IF ls_return IS NOT INITIAL.
            APPEND ls_return TO gt_log.
          ENDIF.
          IF ls_return-type = 'E' OR ls_return-type = 'A'.
            PERFORM f_bapi_rollback CHANGING gt_log.
          ENDIF.

          PERFORM f_update_table_so USING    ls_order
                                             ptu_orders_change
                                             pvu_pos
                                    CHANGING gt_orders.
        ELSE.
          PERFORM f_bapi_rollback CHANGING gt_log.
        ENDIF .
      ENDIF.

      PERFORM f_fill_log_from_bapi USING gv_log_handle
                                         gt_log
                                 CHANGING gv_showlog.

      CLEAR:    ls_item, ls_itemx, ls_schedulein, ls_scheduleinx, ls_extensionin,
                ls_partnerchanges, ls_return, ls_headx, ls_head.
      REFRESH:  lt_item, lt_itemx, lt_schedulein, lt_scheduleinx, lt_extensionin,
                lt_partnerchanges, lt_return, lt_extensionin.
      REFRESH:  gt_log.

    ENDAT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BAPI_ROLLBACK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_PS_T_LOG  text
*----------------------------------------------------------------------*
FORM f_bapi_rollback  CHANGING  ptc_log TYPE gtyp_t_log.

  DATA : ls_return TYPE gtyp_s_log.

  CLEAR: ls_return.

  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
    IMPORTING
      return = ls_return.

  CHECK ls_return IS NOT INITIAL.
  APPEND ls_return TO ptc_log.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_layout .

  gs_variant-report = sy-repid.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = gs_variant
    IMPORTING
      es_variant    = gs_variant
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    p_layout  = gs_variant-variant.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_SALES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_ORDERS  text
*----------------------------------------------------------------------*
FORM f_get_sales.
  PERFORM f_get_sales_orders CHANGING gt_orders.
ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  F_UPDATE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_ORDER  text
*      <--P_GT_ORDER  text
*----------------------------------------------------------------------*
FORM f_update_table_so  USING psu_order TYPE gtyp_s_orders
                              ptu_order TYPE gtyp_t_orders
                              pvu_pos   TYPE char1
                     CHANGING ptc_order TYPE gtyp_t_orders.

  DATA: ls_orders  TYPE gtyp_s_orders,
        ls_ordersc TYPE gtyp_s_orders,
        ls_kna1    TYPE gtyp_s_kna1.


  LOOP AT ptc_order INTO ls_orders WHERE vbeln = psu_order-vbeln.

    "Item level
    READ TABLE ptu_order INTO ls_ordersc WITH KEY vbeln = ls_orders-vbeln
                                                  posnr = ls_orders-posnr.
    IF sy-subrc = 0.
      IF gs_changex-edatu IS NOT INITIAL.
        ls_orders-edatu = gs_change-edatu.
      ENDIF.
      IF gs_changex-agent IS NOT INITIAL.
        ls_orders-agent = gs_change-agent.
        CLEAR ls_kna1.
        READ TABLE gt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_orders-agent.
        IF sy-subrc = 0.
          ls_orders-name_agent = ls_kna1-name1.
        ELSE.
          PERFORM f_find_text_name USING    gs_change-agent
                                   CHANGING ls_orders-name_agent.
        ENDIF.
      ENDIF.
      IF gs_changex-vsart IS NOT INITIAL AND pvu_pos IS NOT INITIAL.
        ls_orders-vsart = gs_change-vsart.
      ENDIF.
      IF gs_changex-shipto IS NOT INITIAL AND pvu_pos IS NOT INITIAL.
        PERFORM f_conversion_to_input USING     gs_change-shipto
                                      CHANGING  ls_orders-ship_to.

        CLEAR ls_kna1.
        READ TABLE gt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_orders-ship_to.
        IF sy-subrc = 0.
          ls_orders-name_ship = ls_kna1-name1.
        ELSE.
          PERFORM f_find_text_name USING    gs_change-shipto
                                   CHANGING ls_orders-name_ship.
        ENDIF.
      ENDIF.
    ENDIF.

    IF gs_changex-bstnk IS NOT INITIAL.
      ls_orders-bstnk = gs_change-bstnk.
    ENDIF.
    IF gs_changex-lifsk IS NOT INITIAL.
      ls_orders-lifsk = gs_change-lifsk.
    ENDIF.

    IF pvu_pos IS INITIAL.
      IF gs_changex-agent IS NOT INITIAL.
        ls_orders-agent = gs_change-agent.
        READ TABLE gt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_orders-agent.
        IF sy-subrc = 0.
          ls_orders-name_agent = ls_kna1-name1.
        ELSE.
          PERFORM f_find_text_name USING gs_change-agent
                                  CHANGING ls_orders-name_agent.
        ENDIF.
      ENDIF.
      IF gs_changex-edatu IS NOT INITIAL.
        ls_orders-edatu = gs_change-edatu.
      ENDIF.
    ENDIF.
    IF gs_changex-freight_ag IS NOT INITIAL.
      ls_orders-freight_ag = gs_change-freight_ag.
      READ TABLE gt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_orders-freight_ag.
      IF sy-subrc = 0.
        ls_orders-name_freight = ls_kna1-name1.
      ELSE.
        PERFORM f_find_freight_name USING gs_change-freight_ag
                                 CHANGING ls_orders-name_freight.
      ENDIF.
    ENDIF.

    IF gs_changex-inco1 IS NOT INITIAL.
      ls_orders-inco1 = gs_change-inco1.
    ENDIF.
    IF gs_changex-inco2 IS NOT INITIAL.
      ls_orders-inco2 = gs_change-inco2.
    ENDIF.
    IF gs_changex-zterm IS NOT INITIAL.
      ls_orders-zterm = gs_change-zterm.
    ENDIF.
    IF gs_changex-valdt IS NOT INITIAL.
      ls_orders-valdt = gs_change-valdt.
    ENDIF.
    IF gs_changex-faksk IS NOT INITIAL.
      ls_orders-faksk = gs_change-faksk.
    ENDIF.

    MODIFY ptc_order FROM ls_orders.
  ENDLOOP.
*  CLEAR: pvu_pos.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FIND_TEXT_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_CHANGE_AGENT  text
*      <--P_LS_ORDERS_NAME_AGENT  text
*----------------------------------------------------------------------*
FORM f_find_text_name  USING    pvu_kunnr TYPE kunnr
                       CHANGING pvc_name1 TYPE name1_gp.

  SELECT SINGLE name1
    FROM kna1
    INTO pvc_name1
    WHERE kunnr =  pvu_kunnr.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FIND_FREIGHT_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_CHANGE_SHIP_TO  text
*      <--P_LS_ORDERS_NAME_SHIP  text
*----------------------------------------------------------------------*
FORM f_find_freight_name  USING pvu_kunnr TYPE kunnr
                       CHANGING pvc_name1 TYPE name1_gp.

  SELECT SINGLE name1
    FROM lfa1
    INTO pvc_name1
    WHERE lifnr =  pvu_kunnr.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CONVERSION_TO_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_conversion_to_input USING pvu_input    TYPE clike
                        CHANGING pvc_output   TYPE clike.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = pvu_input
    IMPORTING
      output = pvc_output.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHANGE_SO_DATA_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_change_so_data_item USING pvu_pos TYPE char1.

  DATA: ls_index_rows TYPE LINE OF lvc_t_row,
        ls_orders     TYPE gtyp_s_orders.

  CLEAR: ls_index_rows.
  REFRESH: gt_index_rows,gt_orders_change.

  PERFORM f_get_selected_rows CHANGING gt_index_rows.

  CLEAR: gv_showlog.

  IF gt_index_rows[] IS INITIAL.
    MESSAGE i001(zret0005) DISPLAY LIKE 'W'. "Please select on line at least
  ELSE.

    CLEAR: gs_change, gs_changex,gv_check.

    CALL SCREEN 0202 STARTING AT 15 5.

    IF gv_check = '1'.
      LOOP AT gt_index_rows INTO ls_index_rows.
        READ TABLE gt_orders INTO ls_orders INDEX ls_index_rows-index.
        IF sy-subrc = 0.
          IF gs_changex-edatu IS NOT INITIAL.
            ls_orders-edatu = gs_change-edatu.
          ENDIF.
          IF gs_changex-agent IS NOT INITIAL.
            ls_orders-agent = gs_change-agent.
          ENDIF.
          IF gs_changex-vsart IS NOT INITIAL.
            ls_orders-vsart = gs_change-vsart.
          ENDIF.
          IF gs_changex-shipto IS NOT INITIAL.
            ls_orders-ship_to = gs_change-shipto.
          ENDIF.
          APPEND ls_orders TO gt_orders_change.
        ENDIF.
      ENDLOOP.

      IF gv_exec_job IS INITIAL.
        PERFORM f_fill_bapi_so USING gt_orders_change
                                     pvu_pos.
      ELSE.
        PERFORM f_get_id_job        CHANGING gv_id_job.
        PERFORM f_set_cluster_data     USING gv_id_job      pvu_pos
                                             gt_index_rows  gt_orders_change.
        PERFORM f_run_job              USING gv_id_job.
        PERFORM f_update_flowtab_so    USING gt_orders_change pvu_pos.
      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_SELECTED_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_INDEX_ROWS  text
*----------------------------------------------------------------------*
FORM f_get_selected_rows  CHANGING ptc_index_rows TYPE lvc_t_row.

  REFRESH: ptc_index_rows.

  CALL METHOD gref_alvgrid->get_selected_rows
    IMPORTING
      et_index_rows = ptc_index_rows.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_SOITEM_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_CHANGEX  text
*      -->P_GS_CHANGE  text
*      <--P_GV_ERROR  text
*----------------------------------------------------------------------*
FORM f_check_soitem_selection USING psu_changex TYPE gtyp_s_changex
                                    psc_change TYPE gtyp_s_change
                           CHANGING pcv_error TYPE char1.

  DATA: lv_kunnr TYPE kunnr,
        lv_vsart TYPE versart.

  CLEAR: pcv_error.

  IF   psu_changex-edatu   IS INITIAL AND
       psu_changex-agent   IS INITIAL AND
       psu_changex-vsart   IS INITIAL AND
       psu_changex-shipto  IS INITIAL.

    MESSAGE i005(zret0005)."Please select one field for mass change
    pcv_error = 'X'.

  ELSE.
    IF psu_changex-agent IS NOT INITIAL.
      PERFORM f_conversion_to_input USING psc_change-agent
                                 CHANGING psc_change-agent.
      SELECT SINGLE kunnr
        INTO lv_kunnr
        FROM kna1
        WHERE kunnr = psc_change-agent.
      IF sy-subrc NE 0.
        MESSAGE i002(zret0005) WITH TEXT-306. "The value for field agent is incorrect
        pcv_error = 'X'.
      ENDIF.
    ENDIF.
    IF psu_changex-vsart IS NOT INITIAL.
      IF psc_change-vsart IS NOT INITIAL. "Accept empty values
        SELECT SINGLE vsart
          INTO lv_vsart
          FROM t173
          WHERE vsart EQ psc_change-vsart.
        IF sy-subrc NE 0.
          MESSAGE i002(zret0005) DISPLAY LIKE 'E' WITH TEXT-313.  "The value for field vsart is incorrect
          pcv_error = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.
    IF psu_changex-shipto IS NOT INITIAL.
      PERFORM f_conversion_to_input USING psc_change-shipto
                                 CHANGING psc_change-shipto.
      SELECT SINGLE kunnr
        INTO lv_kunnr
        FROM kna1
        WHERE kunnr = psc_change-shipto.
      IF sy-subrc NE 0.
        MESSAGE i002(zret0005) DISPLAY LIKE 'E' WITH TEXT-305. "The value for field shipto is incorrect
        pcv_error = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_ROITEM_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_CHANGEX  text
*      -->P_GS_CHANGE  text
*      <--P_GV_ERROR  text
*----------------------------------------------------------------------*
FORM f_check_roitem_selection  USING psu_changex TYPE gtyp_s_changex
                                     psc_change TYPE gtyp_s_change
                            CHANGING pcv_error TYPE char1.


  DATA:lv_inco  TYPE inco1,
       lv_saity TYPE saity.

  CLEAR:pcv_error.

  IF psu_changex-inco1 IS INITIAL AND psu_changex-inco2 IS INITIAL
    AND psu_changex-eindt IS INITIAL AND psu_changex-knprs IS INITIAL.

    MESSAGE i005(zret0005)."Please select one field for mass change
    pcv_error = 'X'.

  ELSE.
    IF psu_changex-inco1 IS NOT INITIAL.
      SELECT SINGLE inco1
        INTO lv_inco
        FROM tinc
        WHERE inco1 = psc_change-inco1.
      IF sy-subrc NE 0.
        MESSAGE i002(zret0005) DISPLAY LIKE 'E' WITH TEXT-310. "The value for field Incoterm1 is incorrect
        pcv_error = 'X'.
      ENDIF.

    ENDIF.
    IF psu_changex-knprs IS NOT INITIAL.
      psc_change-knprs = t683-knprs_v.
      CLEAR t683-knprs_v.
    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CONVERT_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_POSCHEDULE_DELIVERY_DATE  text
*      <--P_LV_DATE  text
*----------------------------------------------------------------------*
FORM f_convert_date  USING    pvu_date TYPE eeind
                     CHANGING pvc_date TYPE eindt.

  " Convert display data to internal format
  CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
    EXPORTING
      date_external            = pvu_date
*     ACCEPT_INITIAL_DATE      =
    IMPORTING
      date_internal            = pvc_date
    EXCEPTIONS
      date_external_is_invalid = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    CLEAR: pvc_date.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHANGE_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_change_header .
  CLEAR: gv_pos.
  PERFORM f_generate_log   CHANGING gv_log_handle.
  PERFORM f_change_so_data USING gv_pos.

  IF gv_showlog IS NOT INITIAL.

    PERFORM f_show_log  USING gv_showlog.
    CALL METHOD gref_alvgrid->refresh_table_display.

  ENDIF.
  PERFORM f_free_log  USING gv_log_handle.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHANGE_ITEMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_change_items .
  gv_pos = 'X'.
  PERFORM f_generate_log   CHANGING gv_log_handle.
  PERFORM f_change_so_data_item USING gv_pos.
  CLEAR gv_pos.
  IF gv_showlog IS NOT INITIAL.

    PERFORM f_show_log  USING gv_showlog.
    CALL METHOD gref_alvgrid->refresh_table_display.

  ENDIF.
  PERFORM f_free_log  USING gv_log_handle.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_UPDATE_FLOWTAB_SO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_ORDERS_CHANGE  text
*      -->P_PVU_POS  text
*----------------------------------------------------------------------*
FORM f_update_flowtab_so  USING ptu_orders_change TYPE gtyp_t_orders
                                pvu_pos           TYPE char1.

  DATA: ls_order   TYPE gtyp_s_orders.

  SORT ptu_orders_change BY vbeln posnr.
  LOOP AT ptu_orders_change INTO ls_order.
    AT END OF vbeln.
      PERFORM f_update_table_so USING    ls_order
                                         ptu_orders_change
                                         pvu_pos
                                CHANGING gt_orders.
    ENDAT.
  ENDLOOP.
  CALL METHOD gref_alvgrid->refresh_table_display.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_ID_JOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GV_ID_JOB  text
*----------------------------------------------------------------------*
FORM f_get_id_job  CHANGING pvc_id_job TYPE char18.

  CLEAR: pvc_id_job.
  CONCATENATE sy-datum sy-uzeit INTO pvc_id_job.
  CONDENSE: pvc_id_job NO-GAPS.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SET_CLUSTER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_ID_JOB  text
*----------------------------------------------------------------------*
FORM f_set_cluster_data   USING pvu_id_job        TYPE char18
                                pvu_pos           TYPE char1
                                ptu_index_rows    TYPE lvc_t_row
                                ptu_orders_change TYPE gtyp_t_orders.


  DATA: ls_job_exec TYPE gtyp_s_job_exec,
        lv_indxkey  TYPE indx_srtfd,
        lref_wf_obj TYPE REF TO cl_abap_expimp_db.

  CLEAR: ls_job_exec.
  ls_job_exec-param_upd           = 'X'.
  ls_job_exec-param_del           = ' '.
  ls_job_exec-param_test          = p_test.
  ls_job_exec-var_gvpos           = pvu_pos.
  ls_job_exec-param_frei          = gd_parvw_transportista.
  ls_job_exec-param_agen          = gd_parvw_agente.
  ls_job_exec-param_ship          = gd_parvw_direntrega.
  ls_job_exec-struc_change        = gs_change.
  ls_job_exec-struc_changex       = gs_changex.
  ls_job_exec-tab_orders[]        = gt_orders[].
  ls_job_exec-tab_orders_change[] = ptu_orders_change[].
  ls_job_exec-tab_index_rows[]    = ptu_index_rows[].


  IF ls_job_exec IS NOT INITIAL.

    CONCATENATE 'CAU' pvu_id_job INTO lv_indxkey.
    CONDENSE lv_indxkey NO-GAPS.
*Deleting any old key with similar to indxkey in cluster table
    CREATE OBJECT lref_wf_obj.
    TRY.
        CALL METHOD lref_wf_obj->delete
          EXPORTING
            tabname          = 'INDX'
            client           = sy-mandt
            area             = 'za'
            id               = lv_indxkey
            client_specified = abap_true.
      CATCH cx_sy_client .
      CATCH cx_sy_generic_key .
      CATCH cx_sy_incorrect_key .
    ENDTRY.

    EXPORT ls_job_exec FROM ls_job_exec TO DATABASE indx(za)
                                        ID lv_indxkey.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_RUN_JOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_ID_JOB  text
*----------------------------------------------------------------------*
FORM f_run_job  USING pvc_id_job TYPE char18.


  DATA: lv_jobcount         TYPE btcjobcnt,
        lv_jobname          TYPE btcjob,
        ls_print_parameters TYPE pri_params,
        lv_indxkey          TYPE indx_srtfd.


  CLEAR ls_print_parameters-primm.

  CONCATENATE '/BMG/CAU00005' sy-datum INTO lv_jobname.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = lv_jobname
    IMPORTING
      jobcount         = lv_jobcount
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.

  IF sy-subrc EQ 0.

    CLEAR: lv_indxkey.
    lv_indxkey = pvc_id_job.

    SUBMIT (sy-repid)
       TO SAP-SPOOL
        SPOOL PARAMETERS ls_print_parameters
        WITHOUT SPOOL DYNPRO
        VIA JOB lv_jobname NUMBER lv_jobcount
        WITH bgindx   EQ pvc_id_job
         AND RETURN.

    IF sy-subrc EQ 0.

      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = lv_jobcount
          jobname              = lv_jobname
          strtimmed            = 'X'
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          OTHERS               = 8.
      IF sy-subrc EQ 0.
        MESSAGE s127(zret0005) WITH lv_jobname. "Job &1 has been created
      ELSE.
        MESSAGE e126(zret0005). "Error, no job has been created
      ENDIF.
    ELSE.
      MESSAGE e126(zret0005). "Error, no job has been created
    ENDIF.
  ELSE.
    MESSAGE e126(zret0005). "Error, no job has been created
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_CLUSTER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_cluster_data .

  DATA: ls_job_exec TYPE gtyp_s_job_exec,
        lv_indxkey  TYPE indx_srtfd,
        lref_wf_obj TYPE REF TO cl_abap_expimp_db.

  CLEAR: ls_job_exec.

  " ************ Get DATABASE Data ************
  CLEAR: lv_indxkey.
  CONCATENATE 'CAU' bgindx INTO lv_indxkey.
  CONDENSE lv_indxkey NO-GAPS.


  IMPORT ls_job_exec TO ls_job_exec FROM DATABASE indx(za)
                                      ID lv_indxkey.

  IF sy-subrc = 0.
    CREATE OBJECT lref_wf_obj." type ref to CL_ABAP_EXPIMP_DB.
    TRY.
        CALL METHOD lref_wf_obj->delete
          EXPORTING
            tabname          = 'INDX'
            client           = sy-mandt
            area             = 'za'
            id               = lv_indxkey
            client_specified = abap_true.
      CATCH cx_sy_client .
      CATCH cx_sy_generic_key .
      CATCH cx_sy_incorrect_key .
    ENDTRY.

    " Mapping data from DATABASE to Internal variables
    CLEAR: p_test, gv_pos, gd_parvw_transportista, gd_parvw_agente, gd_parvw_direntrega, gs_change, gs_changex.
    REFRESH: gt_orders[], gt_orders_change[], gt_index_rows[].
    p_test              = ls_job_exec-param_test.           " Indicate Mode test execution
    gv_pos              = ls_job_exec-var_gvpos.            " Indicate Header or Item execution
    gd_parvw_transportista              = ls_job_exec-param_frei.           " Partner funtion Freight Agent
    gd_parvw_agente              = ls_job_exec-param_agen.           " Partner function Agent
    gd_parvw_direntrega              = ls_job_exec-param_ship.           " Partner function Ship to
    gs_change           = ls_job_exec-struc_change.         " Data to be changed from previous execution
    gs_changex          = ls_job_exec-struc_changex.        " Flags to change data from previous execution
    gt_orders[]         = ls_job_exec-tab_orders[].         " Global ALV data from previous execution
    gt_orders_change[]  = ls_job_exec-tab_orders_change[].  " Orders selected to be changed from previous execution
    gt_index_rows[]     = ls_job_exec-tab_index_rows[].     " ALV selected rows from previous execution

  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BAPI_BACKGROUND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_bapi_background .

  PERFORM f_generate_log   CHANGING gv_log_handle.

  PERFORM f_fill_bapi_so USING gt_orders_change
                               gv_pos.

  IF gv_showlog IS NOT INITIAL.
    PERFORM f_show_log  USING gv_showlog.
  ENDIF.
  PERFORM f_free_log    USING gv_log_handle.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_change_matnr
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_change_matnr.

  DATA: lt_orders TYPE gtyp_t_orders.


  PERFORM f_get_selected_rows CHANGING gt_index_rows.

  IF gt_index_rows[] IS INITIAL.
    MESSAGE i001(zret0005) DISPLAY LIKE 'W'. "Please select on line at least
  ELSE.

    LOOP AT gt_index_rows ASSIGNING FIELD-SYMBOL(<lfs_index_rows>).
      READ TABLE gt_orders ASSIGNING FIELD-SYMBOL(<lfs_orders>) INDEX <lfs_index_rows>-index.
      IF sy-subrc = 0.
        IF <lfs_orders>-attyp NE gc_attyp_01. "01
          APPEND <lfs_orders> TO lt_orders.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lt_orders IS NOT INITIAL.

      CLEAR gv_check.

      CALL SCREEN 0204 STARTING AT 15 5.

      IF gv_check = 1.

        PERFORM f_generate_log   CHANGING gv_log_handle.

        PERFORM f_change_material USING lt_orders gs_material.

        CLEAR gv_pos.
        IF gv_showlog IS NOT INITIAL.

          PERFORM f_show_log  USING gv_showlog.
          CALL METHOD gref_alvgrid->refresh_table_display.

        ENDIF.
        PERFORM f_free_log  USING gv_log_handle.
      ENDIF.

    ELSE.
      "No hay materiales para modificar
      MESSAGE i181(zret0005) DISPLAY LIKE 'W'.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_change_material
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_change_material USING ptu_orders TYPE gtyp_t_orders
                             psu_material TYPE gtyp_s_material.


  DATA: ls_order   TYPE gtyp_s_orders,
        ls_return  TYPE gtyp_s_log,
        lv_message TYPE char200,
        lv_error   TYPE sy-subrc,
        lv_posnr   TYPE posnr.

  DATA: ls_headx          TYPE          bapisdh1x,
        ls_head           TYPE          bapisdh1,
        lt_return         TYPE TABLE OF bapiret2,
        lt_item           TYPE TABLE OF bapisditm,
        ls_item           TYPE          bapisditm,
        lt_itemx          TYPE TABLE OF bapisditmx,
        ls_itemx          TYPE          bapisditmx,
        lt_schedulein     TYPE TABLE OF bapischdl,
        ls_schedulein     TYPE          bapischdl,
        lt_scheduleinx    TYPE TABLE OF bapischdlx,
        ls_scheduleinx    TYPE          bapischdlx,
        lt_partnerchanges TYPE TABLE OF bapiparnrc,
        ls_partnerchanges TYPE          bapiparnrc,
        lt_extensionin    TYPE TABLE OF bapiparex,
        ls_extensionin    TYPE          bapiparex,
        ls_bape_vbak      TYPE          bape_vbak,
        ls_bape_vbakx     TYPE          bape_vbakx,
        ls_logic_switch   TYPE          bapisdls.

  DATA: lt_order_items_out      TYPE TABLE OF bapisdit,
        lt_order_headers_out    TYPE TABLE OF bapisdhd,
        lt_order_conditions_out TYPE gtyp_t_bapisdcond.


  SORT ptu_orders BY vbeln posnr.

  LOOP AT ptu_orders INTO ls_order GROUP BY ( vbeln = ls_order-vbeln )
                                  ASSIGNING FIELD-SYMBOL(<lfs_group>).

    PERFORM f_get_so_detail_list USING <lfs_group>-vbeln
                                  CHANGING lt_order_conditions_out
                                           lt_order_headers_out
                                           lt_order_items_out.
    "get the last item number ot the sales order
    CLEAR lv_posnr.
    SORT lt_order_items_out BY itm_number DESCENDING.
    READ TABLE lt_order_items_out ASSIGNING FIELD-SYMBOL(<lfs_items_out>) INDEX 1.
    IF sy-subrc EQ 0.
      lv_posnr = <lfs_items_out>-itm_number.
    ENDIF.

    REFRESH: lt_item, lt_itemx.
    LOOP AT GROUP <lfs_group> ASSIGNING FIELD-SYMBOL(<lfs_order>).
      CLEAR  ls_item.
      CLEAR  ls_itemx.
      "The selected material will be marked with rejection reason
      ls_item-itm_number = <lfs_order>-posnr.
      ls_itemx-updateflag = gc_flag_change.
      ls_itemx-itm_number = <lfs_order>-posnr.
      ls_item-material = <lfs_order>-matnr.
      ls_itemx-material = abap_true.
      ls_item-reason_rej = psu_material-abgru.
      ls_itemx-reason_rej = abap_true.

      APPEND ls_item TO lt_item.
      APPEND ls_itemx TO lt_itemx.


      "append the new material
      ADD 10 TO lv_posnr.
      CLEAR ls_item.
      READ TABLE lt_order_items_out ASSIGNING <lfs_items_out> WITH KEY itm_number = <lfs_order>-posnr.
      IF sy-subrc EQ 0.

        ls_schedulein-itm_number = lv_posnr.
        ls_schedulein-req_qty    = <lfs_items_out>-req_qty .
        ls_schedulein-sched_line = '0001'.
        APPEND ls_schedulein TO lt_schedulein.
        CLEAR  ls_schedulein.

        ls_scheduleinx-itm_number  = lv_posnr.
        ls_scheduleinx-req_qty      = abap_true.
        ls_scheduleinx-updateflag  = 'I'.
        ls_scheduleinx-sched_line = '0001'.
        APPEND ls_scheduleinx TO lt_scheduleinx.
        CLEAR  ls_scheduleinx.
      ENDIF.
      ls_item-itm_number = lv_posnr.
      ls_itemx-updateflag = 'I'.
      ls_itemx-itm_number = lv_posnr.
      ls_item-material = psu_material-matnr.
      ls_itemx-material = abap_true.


      APPEND ls_item TO lt_item.
      APPEND ls_itemx TO lt_itemx.

    ENDLOOP.

    ls_headx-updateflag = gc_flag_change.

    lv_message = ls_order-vbeln.
    "Begin update of sales order &1
    PERFORM f_fill_log USING  gv_log_handle 'S' 'ZRET0005' '003' lv_message(50) lv_message+50(50)
                         lv_message+100(50) lv_message+150(50)
                        CHANGING gv_showlog.

    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
      EXPORTING
        salesdocument    = <lfs_group>-vbeln
        order_header_in  = ls_head
        order_header_inx = ls_headx
        logic_switch     = ls_logic_switch
      TABLES
        return           = lt_return[]
        order_item_in    = lt_item[]
        order_item_inx   = lt_itemx[]
        partnerchanges   = lt_partnerchanges[]
        schedule_lines   = lt_schedulein[]
        schedule_linesx  = lt_scheduleinx[]
        extensionin      = lt_extensionin[].

    APPEND LINES OF lt_return TO gt_log.

    IF p_test IS NOT INITIAL.
      PERFORM f_bapi_rollback CHANGING gt_log.
    ELSE.

      CLEAR: lv_error, ls_return.
      LOOP AT lt_return INTO ls_return WHERE type  = 'A'
                                         OR  type  = 'E'.
        lv_error = 1.
        EXIT.
      ENDLOOP.

      IF lv_error IS INITIAL.
        CLEAR: ls_return.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait   = 'X'
          IMPORTING
            return = ls_return.
        IF ls_return IS NOT INITIAL.
          APPEND ls_return TO gt_log.
        ENDIF.
        IF ls_return-type = 'E' OR ls_return-type = 'A'.
          PERFORM f_bapi_rollback CHANGING gt_log.
        ENDIF.

        PERFORM f_get_sales.
      ELSE.
        PERFORM f_bapi_rollback CHANGING gt_log.
      ENDIF .
    ENDIF.

    PERFORM f_fill_log_from_bapi USING gv_log_handle
                                       gt_log
                               CHANGING gv_showlog.

    CLEAR:    ls_item, ls_itemx, ls_schedulein, ls_scheduleinx, ls_extensionin,
              ls_partnerchanges, ls_return, ls_headx, ls_head.
    REFRESH:  lt_item, lt_itemx, lt_schedulein, lt_scheduleinx, lt_extensionin,
              lt_partnerchanges, lt_return, lt_extensionin.
    REFRESH:  gt_log.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_check_matnr_selection
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_MATERIAL
*&      <-- GV_ERROR
*&---------------------------------------------------------------------*
FORM f_check_matnr_selection  USING    psu_material TYPE gtyp_s_material
                              CHANGING pcv_error TYPE char1.

  CLEAR: pcv_error.

  IF   psu_material-matnr  IS INITIAL OR
       psu_material-abgru  IS INITIAL.

    MESSAGE i182(zret0005)."Por favor rellene todos los campos obligatorios
    pcv_error = 'X'.

  ELSE.

    SELECT SINGLE matnr
      FROM mara
      INTO @DATA(lv_matnr)
      WHERE matnr EQ @psu_material-matnr.
    IF sy-subrc NE 0.
      "El maerial introducido no existe
      MESSAGE i183(zret0005).
      pcv_error = 'X'.
    ENDIF.

    SELECT SINGLE abgru
      FROM tvag
      INTO @DATA(lv_abgru)
      WHERE abgru EQ @psu_material-abgru.
    IF sy-subrc NE 0.
      "El motivo de rechazo introducido no existe
      MESSAGE i184(zret0005).
      pcv_error = 'X'.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CHANGE_CONDITION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GC_KSCHL_ZP00
*&---------------------------------------------------------------------*
FORM f_change_condition  USING pvu_kschl TYPE kscha.

  DATA: lt_orders TYPE gtyp_t_orders.



  PERFORM f_get_selected_rows CHANGING gt_index_rows.

  IF gt_index_rows[] IS INITIAL.
    MESSAGE i001(zret0005) DISPLAY LIKE 'W'. "Please select on line at least
  ELSE.

    LOOP AT gt_index_rows ASSIGNING FIELD-SYMBOL(<lfs_index_rows>).
      READ TABLE gt_orders ASSIGNING FIELD-SYMBOL(<lfs_orders>) INDEX <lfs_index_rows>-index.
      IF sy-subrc = 0.
        IF <lfs_orders>-attyp NE gc_attyp_01. "01
          APPEND <lfs_orders> TO lt_orders.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lt_orders IS NOT INITIAL.

      CLEAR: gs_condition, gv_check.

      gs_condition-kschl = pvu_kschl.
      CALL SCREEN 0205 STARTING AT 15 5.

      IF gv_check EQ '1'.

        PERFORM f_generate_log   CHANGING gv_log_handle.

        PERFORM f_change_conditions USING lt_orders gs_condition.

        CLEAR gv_pos.
        IF gv_showlog IS NOT INITIAL.

          PERFORM f_show_log  USING gv_showlog.
          CALL METHOD gref_alvgrid->refresh_table_display.

        ENDIF.
        PERFORM f_free_log  USING gv_log_handle.
      ENDIF.
    ELSE.
      "No hay materiales para modificar
      MESSAGE i185(zret0005) DISPLAY LIKE 'W'.
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_change_conditions
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_ORDERS
*&      --> GS_CONDITION
*&---------------------------------------------------------------------*
FORM f_change_conditions  USING    ptu_orders    TYPE gtyp_t_orders
                                   psu_condition TYPE gtyp_s_condition.



  DATA: ls_order   TYPE gtyp_s_orders,
        ls_return  TYPE gtyp_s_log,
        lv_message TYPE char200,
        lv_error   TYPE sy-subrc.

  DATA: ls_headx        TYPE          bapisdh1x,
        ls_head         TYPE          bapisdh1,
        lt_return       TYPE TABLE OF bapiret2,
        lt_item         TYPE TABLE OF bapisditm,
        ls_item         TYPE          bapisditm,
        lt_itemx        TYPE TABLE OF bapisditmx,
        ls_itemx        TYPE          bapisditmx,
        ls_logic_switch TYPE          bapisdls.

  DATA: lt_order_items_out      TYPE gtyp_t_bapisdit,
        lt_order_headers_out    TYPE gtyp_t_bapisdhd,
        lt_order_conditions_out TYPE gtyp_t_bapisdcond,
        lt_order_conditions_inx TYPE gtyp_t_bapicondx,
        lt_order_conditions_in  TYPE gtyp_t_bapicond,
        ls_order_conditions_in  TYPE  bapicond,
        ls_order_conditions_inx TYPE  bapicondx.


  SORT ptu_orders BY vbeln posnr.

  LOOP AT ptu_orders INTO ls_order GROUP BY ( vbeln = ls_order-vbeln )
                                  ASSIGNING FIELD-SYMBOL(<lfs_group>).


    PERFORM f_get_so_detail_list USING <lfs_group>-vbeln
                                  CHANGING lt_order_conditions_out
                                           lt_order_headers_out
                                           lt_order_items_out.


    REFRESH: lt_item, lt_itemx,lt_order_conditions_inx,lt_order_conditions_in.
    LOOP AT GROUP <lfs_group> ASSIGNING FIELD-SYMBOL(<lfs_order>).
      CLEAR  ls_item.
      CLEAR  ls_itemx.

      ls_item-itm_number = <lfs_order>-posnr.
      ls_itemx-updateflag = gc_flag_change.
      ls_itemx-itm_number = <lfs_order>-posnr.
      ls_item-material = <lfs_order>-matnr.
      ls_itemx-material = abap_true.

      APPEND ls_item TO lt_item.
      APPEND ls_itemx TO lt_itemx.


      LOOP AT lt_order_conditions_out ASSIGNING FIELD-SYMBOL(<lfs_orders_cond>) WHERE cond_type = psu_condition-kschl
                                                                         AND itm_number = <lfs_order>-posnr.
        CLEAR: ls_order_conditions_in,ls_order_conditions_inx.
        ls_order_conditions_in-itm_number  = <lfs_order>-posnr.
        IF psu_condition-kschl = gd_kschl_precio.
          ls_order_conditions_in-cond_value  = psu_condition-kbetr / 10.
        ELSE.
          ls_order_conditions_in-cond_value  = psu_condition-kbetr.
        ENDIF.
*        ls_order_conditions_in-condvalue   = <lfs_orders_cond>-condvalue.
        ls_order_conditions_in-cond_st_no  = <lfs_orders_cond>-cond_st_no.
        ls_order_conditions_in-cond_count  = <lfs_orders_cond>-cond_count.
        ls_order_conditions_in-cond_type   = <lfs_orders_cond>-cond_type.
*        ls_order_conditions_in-cond_updat  = abap_true.
*        ls_order_conditions_in-currency    = <lfs_orders_cond>-currency.
*        ls_order_conditions_in-cond_unit   = <lfs_orders_cond>-cond_d_unt.
*        ls_order_conditions_in-accountkey  = <lfs_orders_cond>-accountkey.
*        ls_order_conditions_in-cond_p_unt  = <lfs_orders_cond>-cond_p_unt.
*        ls_order_conditions_in-conpricdat  = sy-datum.
*        ls_order_conditions_in-condchaman  = abap_true.
        ls_order_conditions_inx-itm_number = <lfs_order>-posnr.
        ls_order_conditions_inx-cond_st_no = <lfs_orders_cond>-cond_st_no.
        ls_order_conditions_inx-cond_count = <lfs_orders_cond>-cond_count.
        ls_order_conditions_inx-cond_type  = <lfs_orders_cond>-cond_type.
        ls_order_conditions_inx-cond_value = abap_true .
        ls_order_conditions_inx-updateflag = 'U'.
*        ls_order_conditions_inx-currency   = abap_true.
*        ls_order_conditions_inx-cond_unit  = abap_true.
*        ls_order_conditions_inx-cond_p_unt = abap_true.

        APPEND ls_order_conditions_in  TO lt_order_conditions_in.
        APPEND ls_order_conditions_inx TO lt_order_conditions_inx.

      ENDLOOP.
      IF sy-subrc NE 0.
        ls_order_conditions_in-itm_number  = <lfs_order>-posnr.
        IF psu_condition-kschl IN gran_kschl_desc.
          ls_order_conditions_in-cond_value  = psu_condition-kbetr * 10.
        ENDIF.
        ls_order_conditions_in-cond_type   = psu_condition-kschl.
        ls_order_conditions_in-currency    =  <lfs_order>-waerk.
        ls_order_conditions_inx-itm_number = <lfs_order>-posnr.
        ls_order_conditions_inx-cond_type  = psu_condition-kschl.
        ls_order_conditions_inx-cond_value = abap_true.
        ls_order_conditions_inx-updateflag = 'U'.
        ls_order_conditions_inx-currency   = abap_true.

        APPEND ls_order_conditions_in  TO lt_order_conditions_in.
        APPEND ls_order_conditions_inx TO lt_order_conditions_inx.
      ENDIF.
    ENDLOOP.

    ls_headx-updateflag = gc_flag_change.

    lv_message = ls_order-vbeln.
    "Begin update of sales order &1
    PERFORM f_fill_log USING  gv_log_handle 'S' 'ZRET0005' '003' lv_message(50) lv_message+50(50)
                         lv_message+100(50) lv_message+150(50)
                        CHANGING gv_showlog.

    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
      EXPORTING
        salesdocument    = <lfs_group>-vbeln
        order_header_in  = ls_head
        order_header_inx = ls_headx
        logic_switch     = ls_logic_switch
      TABLES
        return           = lt_return[]
        order_item_in    = lt_item[]
        order_item_inx   = lt_itemx[]
        conditions_in    = lt_order_conditions_in[]
        conditions_inx   = lt_order_conditions_inx[].

    APPEND LINES OF lt_return TO gt_log.

    IF p_test IS NOT INITIAL.
      PERFORM f_bapi_rollback CHANGING gt_log.
    ELSE.

      CLEAR: lv_error, ls_return.
      LOOP AT lt_return INTO ls_return WHERE type  = 'A'
                                         OR  type  = 'E'.
        lv_error = 1.
        EXIT.
      ENDLOOP.

      IF lv_error IS INITIAL.
        CLEAR: ls_return.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait   = 'X'
          IMPORTING
            return = ls_return.
        IF ls_return IS NOT INITIAL.
          APPEND ls_return TO gt_log.
        ENDIF.
        IF ls_return-type = 'E' OR ls_return-type = 'A'.
          PERFORM f_bapi_rollback CHANGING gt_log.
        ENDIF.

        PERFORM f_get_sales.
      ELSE.
        PERFORM f_bapi_rollback CHANGING gt_log.
      ENDIF .
    ENDIF.

    PERFORM f_fill_log_from_bapi USING gv_log_handle
                                       gt_log
                               CHANGING gv_showlog.

    CLEAR:    ls_item, ls_itemx, ls_headx, ls_head.
    REFRESH:  lt_item, lt_itemx,  lt_return.
    REFRESH:  gt_log.

  ENDLOOP.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_so_detail_list
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LFS_GROUP>_VBELN
*&      <-- LT_ORDERS_COND
*&      <-- LT_ORDER_HEADERS_OUT
*&---------------------------------------------------------------------*
FORM f_get_so_detail_list  USING    pvu_vbeln              TYPE vbeln
                           CHANGING ptc_orders_cond        TYPE gtyp_t_bapisdcond
                                    ptc_order_headers_out  TYPE gtyp_t_bapisdhd
                                    ptc_order_items_out    TYPE gtyp_t_bapisdit.


  DATA: ls_bapi_view TYPE order_view,
        lt_sales     TYPE TABLE OF sales_key,
        ls_sales     TYPE sales_key.

  ls_bapi_view-item     = 'X'.
  ls_bapi_view-sdcond   = 'X'.
  ls_bapi_view-business = 'X'.
  ls_bapi_view-text     = 'X'.
  ls_bapi_view-header   = 'X'.


  CLEAR ls_sales.
  REFRESH lt_sales.
  ls_sales-vbeln = pvu_vbeln.
  APPEND ls_sales TO lt_sales.

  CALL FUNCTION 'BAPISDORDER_GETDETAILEDLIST'
    EXPORTING
      i_bapi_view              = ls_bapi_view
*     I_MEMORY_READ            =
      i_with_header_conditions = 'X'
    TABLES
      sales_documents          = lt_sales
      order_headers_out        = ptc_order_headers_out
      order_items_out          = ptc_order_items_out
*     ORDER_SCHEDULES_OUT      =
*     order_business_out       = ptc_order_business_out
*     ORDER_PARTNERS_OUT       =
*     ORDER_ADDRESS_OUT        =
*     ORDER_STATUSHEADERS_OUT  =
*     ORDER_STATUSITEMS_OUT    =
      order_conditions_out     = ptc_orders_cond
*     ORDER_COND_HEAD          =
*     ORDER_COND_ITEM          =
*     ORDER_COND_QTY_SCALE     =
*     ORDER_COND_VAL_SCALE     =
*     ORDER_CONTRACTS_OUT      =
*     order_textheaders_out    = ptc_order_textheaders_out
*     order_textlines_out      = ptc_order_textlines_out
*     ORDER_FLOWS_OUT          =
*     ORDER_CFGS_CUREFS_OUT    =
*     ORDER_CFGS_CUCFGS_OUT    =
*     ORDER_CFGS_CUINS_OUT     =
*     ORDER_CFGS_CUPRTS_OUT    =
*     ORDER_CFGS_CUVALS_OUT    =
*     ORDER_CFGS_CUBLBS_OUT    =
*     ORDER_CFGS_CUVKS_OUT     =
*     ORDER_BILLINGPLANS_OUT   =
*     ORDER_BILLINGDATES_OUT   =
*     ORDER_CREDITCARDS_OUT    =
*     extensionout             = lt_extension.
    .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CREATE_ALV_100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_create_alv_100 .

  IF gref_container IS INITIAL.
    IF cl_gui_alv_grid=>offline( ) IS INITIAL.
      " Create a container
      CREATE OBJECT gref_container
        EXPORTING
          container_name    = 'CONTAINER'
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          create_error      = 3
          lifetime_error    = 4
          OTHERS            = 5.

      IF sy-subrc EQ 0.
      ENDIF.
      " Do ALV in frontend
      CREATE OBJECT gref_alvgrid
        EXPORTING
          i_parent = gref_container.

    ELSE.
      " Do ALV in background
      CREATE OBJECT gref_alvgrid
        EXPORTING
          i_parent = gref_dock.
    ENDIF.

    " Define the show fields in ALV
    PERFORM f_generate_catalog  USING gc_tabname
                                      gt_orders
                             CHANGING gt_fieldcat.
    PERFORM f_generate_layout          CHANGING gs_layout.     " Define ALV options
    PERFORM f_exclude_tb_functions_alv CHANGING gt_exclude.    " Excluded ALV buttons list

*    " Make object for control events
    CREATE OBJECT gref_event.
    SET HANDLER gref_event->handle_user_command  FOR gref_alvgrid.
    SET HANDLER gref_event->handle_toolbar       FOR gref_alvgrid.

    gs_variant-variant = p_layout.
    gs_variant-report = sy-repid.
    " Show ALV GRID in the first time
    CALL METHOD gref_alvgrid->set_table_for_first_display
      EXPORTING
        is_layout                     = gs_layout
        is_variant                    = gs_variant
*       i_default                     = 'X'
        i_save                        = 'A'
        it_toolbar_excluding          = gt_exclude " Exlude buttons
      CHANGING
        it_outtab                     = gt_orders
        it_fieldcatalog               = gt_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc NE 0.
    ENDIF.
  ELSE.
    CALL METHOD gref_alvgrid->refresh_table_display.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GENERATE_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM f_generate_catalog   USING  pcu_tabname  TYPE lvc_tname
                                 ptu_output   TYPE gtyp_t_orders "ALV table
                        CHANGING ptc_fieldcat TYPE lvc_t_fcat.

* Header data
  DATA : ls_fieldcat      LIKE LINE OF ptc_fieldcat,
         lo_ref_table_des TYPE REF TO cl_abap_tabledescr,
         lo_ref_table_aux TYPE REF TO cl_abap_structdescr,
         lo_ref_elem_des  TYPE REF TO cl_abap_elemdescr,
         ls_dfies         TYPE dfies,
         lt_idetails      TYPE abap_compdescr_tab,
         ls_xdetails      TYPE abap_compdescr,
         lt_typedesc      TYPE REF TO cl_abap_typedescr,
         lv_href          TYPE char62,
         ld_emphasize_h   LIKE ls_fieldcat-emphasize VALUE 'C311',
         ld_emphasize_h_c TYPE int4,
         ld_emphasize_p   LIKE ls_fieldcat-emphasize VALUE 'C511',
         ld_emphasize_p_c TYPE int4,
         ld_kschlt        LIKE t685t-vtext.

  CLEAR lt_idetails. REFRESH lt_idetails.

* Get the structure of the table
  lo_ref_table_des ?= cl_abap_tabledescr=>describe_by_data( ptu_output ).
  TRY.
      lo_ref_table_aux ?= lo_ref_table_des->get_table_line_type( ).
    CATCH cx_sy_move_cast_error .
  ENDTRY.
  lt_idetails[] = lo_ref_table_aux->components[].
  LOOP AT lt_idetails INTO ls_xdetails.

    TRY.
        lt_typedesc ?= lo_ref_table_aux->get_component_type( ls_xdetails-name ).
      CATCH cx_sy_move_cast_error .
    ENDTRY.
    TRY.
        lv_href = lt_typedesc->get_relative_name( ).
      CATCH cx_sy_move_cast_error .
    ENDTRY.
    IF NOT lv_href IS INITIAL.
      TRY.
          lo_ref_elem_des ?= cl_abap_typedescr=>describe_by_name( lv_href ).
        CATCH cx_sy_move_cast_error .
      ENDTRY.

      CALL METHOD lo_ref_elem_des->get_ddic_field
        EXPORTING
          p_langu    = sy-langu
        RECEIVING
          p_flddescr = ls_dfies.

      CLEAR ls_fieldcat.

      MOVE-CORRESPONDING ls_dfies TO ls_fieldcat.
      ls_fieldcat-fieldname  = ls_xdetails-name.
      ls_fieldcat-ref_field  = lv_href. "ls_xdetails-name.
      ls_fieldcat-tabname    = pcu_tabname.

      SELECT SINGLE entitytab
        INTO ls_fieldcat-ref_table
        FROM dd04l
        WHERE rollname  EQ ls_dfies-tabname
          AND entitytab NE space.

*     Columnas de condiciones de precio
      IF ls_fieldcat-fieldname(5) = 'CONDH'.
        READ TABLE git_condh WITH KEY conta = ls_fieldcat-fieldname+6(2).

        IF sy-subrc <> 0.
          CONTINUE.
        ELSE.
          PERFORM f_get_kschlt
            USING
              git_condh-valo1
            CHANGING
              ld_kschlt.

          IF strlen( ls_fieldcat-fieldname ) = 8.
            ls_fieldcat-reptext   = |{ git_condh-valo1 }| & | (| & |{ ld_kschlt }| & |)| & |[Valor]|.
            ls_fieldcat-scrtext_l = |{ git_condh-valo1 }| & || & |[V]|.
            ls_fieldcat-scrtext_m = |{ git_condh-valo1 }| & || & |[V]|.
            ls_fieldcat-scrtext_s = |{ git_condh-valo1 }| & || & |[V]|.
          ELSE.
            ls_fieldcat-reptext   = |{ git_condh-valo1 }| & | (| & |{ ld_kschlt }| & |)| & |[Moneda]|.
            ls_fieldcat-scrtext_l = |{ git_condh-valo1 }| & || & |[M]|.
            ls_fieldcat-scrtext_m = |{ git_condh-valo1 }| & || & |[M]|.
            ls_fieldcat-scrtext_s = |{ git_condh-valo1 }| & || & |[M]|.
          ENDIF.
        ENDIF.

        IF ld_emphasize_h = 'C311' AND ld_emphasize_h_c >= 2.
          ld_emphasize_h = 'C300'.
          ld_emphasize_h_c = 1.
        ELSEIF ld_emphasize_h = 'C300' AND ld_emphasize_h_c >= 2.
          ld_emphasize_h = 'C311'.
          ld_emphasize_h_c = 1.
        ELSE.
          ADD 1 TO ld_emphasize_h_c .
        ENDIF.
        ls_fieldcat-emphasize = ld_emphasize_h.
      ELSEIF ls_fieldcat-fieldname(5) = 'CONDP'.
        READ TABLE git_condp WITH KEY conta = ls_fieldcat-fieldname+6(2).

        IF sy-subrc <> 0.
          CONTINUE.
        ELSE.
          PERFORM f_get_kschlt
            USING
              git_condp-valo1
            CHANGING
              ld_kschlt.

          IF strlen( ls_fieldcat-fieldname ) = 8.
            ls_fieldcat-reptext   = |{ git_condp-valo1 }| & | (| & |{ ld_kschlt }| & |)| & |[Valor]|.
            ls_fieldcat-scrtext_l = |{ git_condp-valo1 }| & || & |[V]|.
            ls_fieldcat-scrtext_m = |{ git_condp-valo1 }| & || & |[V]|.
            ls_fieldcat-scrtext_s = |{ git_condp-valo1 }| & || & |[V]|.
          ELSE.
            ls_fieldcat-reptext   = |{ git_condp-valo1 }| & | (| & |{ ld_kschlt }| & |)| & |[Moneda]|.
            ls_fieldcat-scrtext_l = |{ git_condp-valo1 }| & || & |[M]|.
            ls_fieldcat-scrtext_m = |{ git_condp-valo1 }| & || & |[M]|.
            ls_fieldcat-scrtext_s = |{ git_condp-valo1 }| & || & |[M]|.
          ENDIF.

          IF ld_emphasize_p = 'C511' AND ld_emphasize_p_c >= 2.
            ld_emphasize_p = 'C500'.
            ld_emphasize_p_c = 1.
          ELSEIF ld_emphasize_p = 'C500' AND ld_emphasize_p_c >= 2.
            ld_emphasize_p = 'C511'.
            ld_emphasize_p_c = 1.
          ELSE.
            ADD 1 TO ld_emphasize_p_c .
          ENDIF.

          ls_fieldcat-emphasize = ld_emphasize_p.
        ENDIF.
      ENDIF.

*     Resto de columnas
      CASE ls_fieldcat-fieldname.
        WHEN 'VBELN'.
          ls_fieldcat-reptext    = 'Pedido de ventas'.
          ls_fieldcat-scrtext_l  = 'Pedido'.
          ls_fieldcat-scrtext_m  = 'Pedido'.
          ls_fieldcat-scrtext_s  = 'Pedido'.
          ls_fieldcat-emphasize  = 'C711'.
          ls_fieldcat-ref_field  = 'VBELN'.           "Reference field name for internal table field
          ls_fieldcat-ref_table  = 'VBAK'.           "Reference table name for internal table field
          ls_fieldcat-f4availabl = 'X'.          "Does the field have an input help
        WHEN 'AUART'.
          ls_fieldcat-reptext    = 'Clase de pedido'.
          ls_fieldcat-scrtext_l  = 'ClPed'.
          ls_fieldcat-scrtext_m  = 'ClPed'.
          ls_fieldcat-scrtext_s  = 'ClPed'.
          ls_fieldcat-emphasize  = 'C711'.
          ls_fieldcat-ref_field  = 'AUART'.           "Reference field name for internal table field
          ls_fieldcat-ref_table  = 'VBAK'.           "Reference table name for internal table field
          ls_fieldcat-f4availabl = 'X'.          "Does the field have an input help
        WHEN 'AUARTT'.
          ls_fieldcat-reptext    = 'Denom. clase de pedido'.
          ls_fieldcat-scrtext_l  = 'D.ClPed'.
          ls_fieldcat-scrtext_m  = 'D.ClPed'.
          ls_fieldcat-scrtext_s  = 'D.ClPed'.
          ls_fieldcat-emphasize  = 'C711'.
        WHEN 'KUNNR'.
          ls_fieldcat-reptext    = 'Solicitante'.
          ls_fieldcat-scrtext_l  = 'Solicitante'.
          ls_fieldcat-scrtext_m  = 'Solicitante'.
          ls_fieldcat-scrtext_s  = 'Solicitante'.
          ls_fieldcat-emphasize  = 'C711'.
        WHEN 'NAME_SOLD'.
          ls_fieldcat-reptext    = 'Nombre Solicitante'.
          ls_fieldcat-scrtext_l  = 'N.Solicitante'.
          ls_fieldcat-scrtext_m  = 'N.Solicitante'.
          ls_fieldcat-scrtext_s  = 'N.Solicitante'.
          ls_fieldcat-emphasize  = 'C711'.
        WHEN 'SHIP_TO'.
          ls_fieldcat-reptext    = 'Destinatario de mercancias'.
          ls_fieldcat-scrtext_l  = 'DestMerc'.
          ls_fieldcat-scrtext_m  = 'DestMerc'.
          ls_fieldcat-scrtext_s  = 'DestMerc'.
          ls_fieldcat-emphasize  = 'C711'.
        WHEN 'NAME_SHIP'.
          ls_fieldcat-reptext    = 'Nombre Dest. de mercancías'.
          ls_fieldcat-scrtext_l  = 'N.DestMerc'.
          ls_fieldcat-scrtext_m  = 'N.DestMerc'.
          ls_fieldcat-scrtext_s  = 'N.DestMerc'.
          ls_fieldcat-emphasize  = 'C711'.
        WHEN 'BSTNK'.
          ls_fieldcat-reptext    = 'Referencia cliente'.
          ls_fieldcat-scrtext_l  = 'Ref.Cliente'.
          ls_fieldcat-scrtext_m  = 'Ref.Cliente'.
          ls_fieldcat-scrtext_s  = 'Ref.Cliente'.
          ls_fieldcat-emphasize  = 'C711'.
        WHEN 'BSTDK'.
          ls_fieldcat-reptext    = 'Fecha referencia cliente'.
          ls_fieldcat-scrtext_l  = 'F.Ref.Clie'.
          ls_fieldcat-scrtext_m  = 'F.Ref.Clie'.
          ls_fieldcat-scrtext_s  = 'F.Ref.Clie'.
          ls_fieldcat-emphasize  = 'C711'.
        WHEN 'AUDAT'.
          ls_fieldcat-reptext    = 'Fecha Pedido'.
          ls_fieldcat-scrtext_l  = 'F.Pedido'.
          ls_fieldcat-scrtext_m  = 'F.Pedido'.
          ls_fieldcat-scrtext_s  = 'F.Pedido'.
          ls_fieldcat-emphasize  = 'C711'.
        WHEN 'POSNR'.
          ls_fieldcat-reptext    = 'Posición Pedido'.
          ls_fieldcat-scrtext_l  = 'Pos.Ped.'.
          ls_fieldcat-scrtext_m  = 'Pos.Ped.'.
          ls_fieldcat-scrtext_s  = 'Pos.Ped.'.
          ls_fieldcat-emphasize  = 'C700'.
          ls_fieldcat-ref_field  = 'POSNR'.           "Reference field name for internal table field
          ls_fieldcat-ref_table  = 'VBAP'.           "Reference table name for internal table field
          ls_fieldcat-f4availabl = 'X'.          "Does the field have an input help
        WHEN 'MATNR'.
          ls_fieldcat-reptext    = 'Artículo'.
          ls_fieldcat-scrtext_l  = 'Artículo'.
          ls_fieldcat-scrtext_m  = 'Artículo'.
          ls_fieldcat-scrtext_s  = 'Artículo'.
          ls_fieldcat-emphasize  = 'C700'.
        WHEN 'MAKTX'.
          ls_fieldcat-reptext    = 'Descripción Artículo'.
          ls_fieldcat-scrtext_l  = 'D.Artículo'.
          ls_fieldcat-scrtext_m  = 'D.Artículo'.
          ls_fieldcat-scrtext_s  = 'D.Artículo'.
          ls_fieldcat-emphasize  = 'C700'.
        WHEN 'INCO1'.
          ls_fieldcat-reptext    = 'Incoterms 1'.
          ls_fieldcat-scrtext_l  = 'Inco[1]'.
          ls_fieldcat-scrtext_m  = 'Inco[1]'.
          ls_fieldcat-scrtext_s  = 'Inco[1]'.
          ls_fieldcat-emphasize  = 'C700'.
          ls_fieldcat-ref_field  = 'INCO1'.           "Reference field name for internal table field
          ls_fieldcat-ref_table  = 'VBKD'.           "Reference table name for internal table field
          ls_fieldcat-f4availabl = 'X'.          "Does the field have an input help
        WHEN 'INCO2'.
          ls_fieldcat-reptext    = 'Incoterms 2'.
          ls_fieldcat-scrtext_l  = 'Inco[2]'.
          ls_fieldcat-scrtext_m  = 'Inco[2]'.
          ls_fieldcat-scrtext_s  = 'Inco[2]'.
          ls_fieldcat-emphasize  = 'C700'.
        WHEN 'ZTERM'.
          ls_fieldcat-reptext    = 'Clave condición de pago'.
          ls_fieldcat-scrtext_l  = 'CondPago'.
          ls_fieldcat-scrtext_m  = 'CondPago'.
          ls_fieldcat-scrtext_s  = 'CondPago'.
          ls_fieldcat-emphasize  = 'C700'.
          ls_fieldcat-ref_field  = 'ZTERM'.           "Reference field name for internal table field
          ls_fieldcat-ref_table  = 'VBKD'.           "Reference table name for internal table field
          ls_fieldcat-f4availabl = 'X'.          "Does the field have an input
        WHEN 'ZTERMT'.
          ls_fieldcat-reptext    = 'Denom.Clave cond. pago'.
          ls_fieldcat-scrtext_l  = 'D.CondPago'.
          ls_fieldcat-scrtext_m  = 'D.CondPago'.
          ls_fieldcat-scrtext_s  = 'D.CondPago'.
          ls_fieldcat-emphasize  = 'C700'.
        WHEN 'EDATU'.
          ls_fieldcat-reptext    = 'Fecha de reparto'.
          ls_fieldcat-scrtext_l  = 'F.Reparto'.
          ls_fieldcat-scrtext_m  = 'F.Reparto'.
          ls_fieldcat-scrtext_s  = 'F.Reparto'.
          ls_fieldcat-emphasize  = 'C700'.
        WHEN 'EINDT'.
          ls_fieldcat-reptext    = 'Fecha de entrega'.
          ls_fieldcat-scrtext_l  = 'F.Entrega'.
          ls_fieldcat-scrtext_m  = 'F.Entrega'.
          ls_fieldcat-scrtext_s  = 'F.Entrega'.
          ls_fieldcat-emphasize  = 'C700'.
        WHEN 'LIFSK'.
          ls_fieldcat-reptext    = 'Bloqueo de entrega'.
          ls_fieldcat-scrtext_l  = 'Bloq[E]'.
          ls_fieldcat-scrtext_m  = 'Bloq[E]'.
          ls_fieldcat-scrtext_s  = 'Bloq[E]'.
          ls_fieldcat-emphasize  = 'C711'.
          ls_fieldcat-ref_field  = 'LIFSK'.           "Reference field name for internal table field
          ls_fieldcat-ref_table  = 'VBAK'.           "Reference table name for internal table field
          ls_fieldcat-f4availabl = 'X'.          "Does the field have an input help
        WHEN 'LIFSKT'.
          ls_fieldcat-reptext    = 'Denom. Bloqueo de entrega'.
          ls_fieldcat-scrtext_l  = 'D.Bloq[E]'.
          ls_fieldcat-scrtext_m  = 'D.Bloq[E]'.
          ls_fieldcat-scrtext_s  = 'D.Bloq[E]'.
          ls_fieldcat-emphasize  = 'C711'.
        WHEN 'AGENT'.
          ls_fieldcat-reptext    = 'Agente'.
          ls_fieldcat-scrtext_l  = 'Agente'.
          ls_fieldcat-scrtext_m  = 'Agente'.
          ls_fieldcat-scrtext_s  = 'Agente'.
          ls_fieldcat-emphasize  = 'C711'.
        WHEN 'NAME_AGENT'.
          ls_fieldcat-reptext    = 'Nombre Agente'.
          ls_fieldcat-scrtext_l  = 'N.Agente'.
          ls_fieldcat-scrtext_m  = 'N.Agente'.
          ls_fieldcat-scrtext_s  = 'N.Agente'.
          ls_fieldcat-emphasize  = 'C711'.
        WHEN 'FREIGHT_AG'.
          ls_fieldcat-reptext    = 'Transportista'.
          ls_fieldcat-scrtext_l  = 'Transp.'.
          ls_fieldcat-scrtext_m  = 'Transp.'.
          ls_fieldcat-scrtext_s  = 'Transp.'.
          ls_fieldcat-emphasize  = 'C711'.
        WHEN 'NAME_FREIGHT'.
          ls_fieldcat-reptext    = 'Nombre Transportista'.
          ls_fieldcat-scrtext_l  = 'N.Transp.'.
          ls_fieldcat-scrtext_m  = 'N.Transp.'.
          ls_fieldcat-scrtext_s  = 'N.Transp.'.
          ls_fieldcat-emphasize  = 'C711'.
        WHEN 'VALDT'.
          ls_fieldcat-reptext    = 'Fecha de valor'.
          ls_fieldcat-scrtext_l  = 'F.Valor'.
          ls_fieldcat-scrtext_m  = 'F.Valor'.
          ls_fieldcat-scrtext_s  = 'F.Valor'.
          ls_fieldcat-emphasize  = 'C700'.
        WHEN 'FAKSK'.
          ls_fieldcat-reptext    = 'Bloqueo de factura'.
          ls_fieldcat-scrtext_l  = 'Bloq[F]'.
          ls_fieldcat-scrtext_m  = 'Bloq[F]'.
          ls_fieldcat-scrtext_s  = 'Bloq[F]'.
          ls_fieldcat-emphasize  = 'C711'.
        WHEN 'FAKSKT'.
          ls_fieldcat-reptext    = 'Denom. Bloqueo de factura'.
          ls_fieldcat-scrtext_l  = 'D.Bloq[F]'.
          ls_fieldcat-scrtext_m  = 'D.Bloq[F]'.
          ls_fieldcat-scrtext_s  = 'D.Bloq[F]'.
          ls_fieldcat-emphasize  = 'C711'.
        WHEN 'VSART'.
          ls_fieldcat-reptext    = 'Clase Expedición'.
          ls_fieldcat-scrtext_l  = 'ClExp'.
          ls_fieldcat-scrtext_m  = 'ClExp'.
          ls_fieldcat-scrtext_s  = 'ClExp'.
          ls_fieldcat-emphasize  = 'C700'.
        WHEN 'WAERK'.
          ls_fieldcat-reptext    = 'Moneda'.
          ls_fieldcat-scrtext_l  = 'Mon.'.
          ls_fieldcat-scrtext_m  = 'Mon.'.
          ls_fieldcat-scrtext_s  = 'Mon.'.
          ls_fieldcat-emphasize  = 'C700'.
        WHEN 'WERKS'.
          ls_fieldcat-reptext    = 'Centro'.
          ls_fieldcat-scrtext_l  = 'Centro'.
          ls_fieldcat-scrtext_m  = 'Centro'.
          ls_fieldcat-scrtext_s  = 'Centro'.
          ls_fieldcat-emphasize  = 'C700'.
        WHEN 'NAME_WERKS'.
          ls_fieldcat-reptext    = 'Nombre centro'.
          ls_fieldcat-scrtext_l  = 'N.Centro'.
          ls_fieldcat-scrtext_m  = 'N.Centro'.
          ls_fieldcat-scrtext_s  = 'N.Centro'.
          ls_fieldcat-emphasize  = 'C700'.
        WHEN 'LGORT'.
          ls_fieldcat-reptext    = 'Almacén'.
          ls_fieldcat-scrtext_l  = 'Almacén'.
          ls_fieldcat-scrtext_m  = 'Almacén'.
          ls_fieldcat-scrtext_s  = 'Almacén'.
          ls_fieldcat-emphasize  = 'C700'.
        WHEN 'LGORTT'.
          ls_fieldcat-reptext    = 'Nombre almacén'.
          ls_fieldcat-scrtext_l  = 'N.Almacén'.
          ls_fieldcat-scrtext_m  = 'N.Almacén'.
          ls_fieldcat-scrtext_s  = 'N.Almacén'.
          ls_fieldcat-emphasize  = 'C700'.
        WHEN 'ATTYP'.
          ls_fieldcat-reptext    = 'Categoría material'.
          ls_fieldcat-scrtext_l  = 'CatMat'.
          ls_fieldcat-scrtext_m  = 'CatMat'.
          ls_fieldcat-scrtext_s  = 'CatMat'.
          ls_fieldcat-emphasize  = 'C700'.
        WHEN ''.
          ls_fieldcat-reptext    = ''.
          ls_fieldcat-scrtext_l  = ''.
          ls_fieldcat-scrtext_m  = ''.
          ls_fieldcat-scrtext_s  = ''.
          ls_fieldcat-emphasize  = 'C700'.
      ENDCASE.

      APPEND ls_fieldcat TO ptc_fieldcat.
    ELSE.
      CLEAR ls_fieldcat.
      CASE ls_xdetails-name.
      ENDCASE.
      APPEND ls_fieldcat TO ptc_fieldcat.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GENERATE_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_LAYOUT  text
*----------------------------------------------------------------------*
FORM f_generate_layout  CHANGING psc_layout TYPE lvc_s_layo.

  "Screen options in ALV screen
  psc_layout-grid_title  = ' '.
  psc_layout-zebra       = 'X'.
*  psc_layout-no_toolbar  = ' '.
  psc_layout-cwidth_opt  = 'X'.
*  pSC_layout-no_rowmark = ' '.
  psc_layout-sel_mode    = 'D'.
*  pSC_layout-stylefname = 'CELLSTYLES'.
*  pSC_layout-ctab_fname = 'COLORCELL'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_EXCLUDE  text
*----------------------------------------------------------------------*
FORM f_exclude_tb_functions_alv  CHANGING ptc_exclude TYPE ui_functions.

  DATA: ls_exclude TYPE ui_func.

  REFRESH: ptc_exclude.

  ls_exclude = cl_gui_alv_grid=>mc_fc_sum.
  APPEND ls_exclude TO ptc_exclude.

  ls_exclude = cl_gui_alv_grid=>mc_fc_subtot.
  APPEND ls_exclude TO ptc_exclude.

  ls_exclude = cl_gui_alv_grid=>mc_fc_average.
  APPEND ls_exclude TO ptc_exclude.

  ls_exclude = cl_gui_alv_grid=>mc_fc_maximum.
  APPEND ls_exclude TO ptc_exclude.

  ls_exclude = cl_gui_alv_grid=>mc_fc_minimum.
  APPEND ls_exclude TO ptc_exclude.

  ls_exclude = cl_gui_alv_grid=>mc_fc_graph .
  APPEND ls_exclude TO ptc_exclude.

  ls_exclude = cl_gui_alv_grid=>mc_fc_help .
  APPEND ls_exclude TO ptc_exclude.

  ls_exclude = cl_gui_alv_grid=>mc_fc_info .
  APPEND ls_exclude TO ptc_exclude.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_user_command_100 .
  CASE ok_code.
    WHEN '&F03' OR '&F15'.
      REFRESH: gt_orders.
      LEAVE TO SCREEN 0.
    WHEN '&F12'.
      LEAVE PROGRAM.
  ENDCASE.
  CLEAR ok_code.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR_100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*----------------------------------------------------------------------*
FORM f_handle_toolbar_100  USING  p_object TYPE REF TO cl_alv_event_toolbar_set.

  DATA: ls_toolbar TYPE stb_button.

  CLEAR ls_toolbar.
  MOVE 'CHANGE'         TO ls_toolbar-function.
  MOVE icon_mass_change TO ls_toolbar-icon.
  MOVE TEXT-301         TO ls_toolbar-quickinfo.
  MOVE TEXT-301         TO ls_toolbar-text.
  MOVE ' '              TO ls_toolbar-disabled.
  APPEND ls_toolbar TO p_object->mt_toolbar.

  CLEAR ls_toolbar.
  MOVE 'CHANGE_POS'     TO ls_toolbar-function.
  MOVE icon_mass_change TO ls_toolbar-icon.
  MOVE TEXT-312         TO ls_toolbar-quickinfo.
  MOVE TEXT-312         TO ls_toolbar-text.
  MOVE ' '              TO ls_toolbar-disabled.
  APPEND ls_toolbar TO p_object->mt_toolbar.

  CLEAR ls_toolbar.
  MOVE 'CHANGE_MATNR'   TO ls_toolbar-function.
  MOVE icon_reject      TO ls_toolbar-icon.
  MOVE TEXT-314         TO ls_toolbar-quickinfo.
  MOVE TEXT-314         TO ls_toolbar-text.
  MOVE ' '              TO ls_toolbar-disabled.
  APPEND ls_toolbar TO p_object->mt_toolbar.

  CLEAR ls_toolbar.
  MOVE 'PRICE'          TO ls_toolbar-function.
  MOVE icon_price       TO ls_toolbar-icon.
  MOVE TEXT-315         TO ls_toolbar-quickinfo.
  MOVE TEXT-315         TO ls_toolbar-text.
  MOVE ' '              TO ls_toolbar-disabled.
  APPEND ls_toolbar TO p_object->mt_toolbar.

  CLEAR ls_toolbar.
  MOVE 'DISCOUNT'           TO ls_toolbar-function.
  MOVE icon_price_analysis  TO ls_toolbar-icon.
  MOVE TEXT-316             TO ls_toolbar-quickinfo.
  MOVE TEXT-316             TO ls_toolbar-text.
  MOVE ' '                  TO ls_toolbar-disabled.
  APPEND ls_toolbar TO p_object->mt_toolbar.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_TOOLBAR_100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM f_user_command_toolbar_100  USING  p_ucomm TYPE syucomm.

  CASE p_ucomm.
    WHEN 'CHANGE'.
      PERFORM f_change_header.
    WHEN 'CHANGE_POS'.
      PERFORM f_change_items.
    WHEN 'CHANGE_MATNR'.
      PERFORM f_change_matnr.
    WHEN 'PRICE'.
      gf_cambiar_precio = 'X'.
      gf_cambiar_descuento = ''.
      PERFORM f_change_condition USING gd_kschl_precio.
    WHEN 'DISCOUNT'.
      gf_cambiar_precio = ''.
      gf_cambiar_descuento = 'X'.
      READ TABLE gran_kschl_desc INDEX 1.
      PERFORM f_change_condition USING gran_kschl_desc-low.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_user_command_200 .

  CLEAR: gv_exec_job.
  CASE ok_code2.
    WHEN '&OK' OR '&OKJOB'.
      PERFORM f_check_so_selection USING gs_changex
                                      gs_change
                                CHANGING gv_error.
      IF gv_error IS INITIAL.                      .
        PERFORM f_call_popup CHANGING gv_check.
        IF ok_code2 EQ '&OKJOB'.
          gv_exec_job = 'X'.
        ENDIF.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN '&CANC'.
      CLEAR: ok_code2, vbak-faksk.
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR ok_code2.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_USER_COMMAND_202
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_user_command_202 .

  CLEAR: gv_exec_job.
  CASE ok_code2.
    WHEN '&OK' OR '&OKJOB'.
      PERFORM f_check_soitem_selection USING gs_changex
                                             gs_change
                                    CHANGING gv_error.
      IF gv_error IS INITIAL.                                      .
        PERFORM f_call_popup CHANGING gv_check.
        IF ok_code2 EQ '&OKJOB'.
          gv_exec_job = 'X'.
        ENDIF.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN '&CANC'.
      CLEAR: ok_code2.
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR ok_code2.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_HELP_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_help_f4 .

  DATA: lv_search_help   TYPE shlpname,
        lv_fieldname     TYPE fdname,
        lt_return_values TYPE TABLE OF ddshretval,
        ls_return_values TYPE ddshretval.

  lv_search_help = gc_lifnr_help."'KRED'.
  lv_fieldname   = gc_lifnr_name. "'LIFNR'.

* execute search help
  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname     = space
      fieldname   = space
      searchhelp  = lv_search_help
      dynprofield = 'X'
    TABLES
      return_tab  = lt_return_values.

* set tc field with the selected value
  READ TABLE lt_return_values INTO ls_return_values WITH KEY fieldname = lv_fieldname.
  gs_change-freight_ag = ls_return_values-fieldval.
* delete lead zeros ('0000012345')
  IF ( gs_change-freight_ag CO ' 0123456789' ) AND
    ( gs_change-freight_ag NE space ).
    PACK gs_change-freight_ag TO gs_change-freight_ag.
    CONDENSE gs_change-freight_ag NO-GAPS.
  ENDIF.

ENDFORM.

FORM f_0204_povr_kschl .
*===================================================================================================
* 0.- Declaración de variables
*===================================================================================================
  DATA: BEGIN OF lit_kschl OCCURS 0,
          kschl  TYPE kschl,
          kschlt TYPE text60,
        END OF lit_kschl.

*===================================================================================================
* 1.- Logica
*===================================================================================================
  IF gf_cambiar_descuento = 'X'.
    loop at gran_kschl_desc.
      lit_kschl-kschl = gran_kschl_desc-low.

      PERFORM f_get_kschlt
        USING
          lit_kschl-kschl
        CHANGING
          lit_kschl-kschlt.

      append lit_kschl.
    endloop.

  ELSEIF gf_cambiar_precio = 'X'.
    lit_kschl-kschl = gd_kschl_precio.

    PERFORM f_get_kschlt
      USING
        lit_kschl-kschl
      CHANGING
        lit_kschl-kschlt.

    append lit_kschl.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      dynpprog        = sy-repid
      dynpnr          = '0205'
      dynprofield     = 'GD_CONDITION-KSCHL'
      retfield        = 'KSCHL'
      value_org       = 'S'
    TABLES
      value_tab       = lit_kschl
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_USER_COMMAND_203
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_user_command_203 .
  CLEAR: gv_exec_job.
  CASE ok_code2.
    WHEN '&OK' OR '&OKJOB'.
      PERFORM f_check_roitem_selection USING gs_changex
                                             gs_change
                                    CHANGING gv_error.
      IF gv_error IS INITIAL.                                      .
        PERFORM f_call_popup CHANGING gv_check.
        IF ok_code2 EQ '&OKJOB'.
          gv_exec_job = 'X'.
        ENDIF.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN '&CANC'.
      CLEAR: ok_code2.
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR ok_code2.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_user_command_204
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_user_command_204 .

  CASE ok_code4.
    WHEN '&OK'.
      PERFORM f_check_matnr_selection USING gs_material
                                    CHANGING gv_error.
      IF gv_error IS INITIAL.
        gv_check = 1.                                  .
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN '&CANC'.
      CLEAR: ok_code4.
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR ok_code4.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_user_command_205
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_user_command_205 .

  CASE ok_code5.
    WHEN '&OK'.
      gv_check = 1.                                .
      LEAVE TO SCREEN 0.
    WHEN '&CANC'.
      CLEAR: ok_code5.
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR ok_code5.


ENDFORM.



*&---------------------------------------------------------------------*
*& Form f_get_parametrizacion
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_parametrizacion .
  DATA: lit_zret0005p01 LIKE zret0005p01 OCCURS 0 WITH HEADER LINE.

*>Cargar parametrización
  SELECT *
    FROM zret0005p01
    INTO TABLE lit_zret0005p01.

*>Organización de ventas por defecto
  LOOP AT lit_zret0005p01 WHERE param = 'VKORG_DEF'.
    p_vkorg = lit_zret0005p01-valo1.
    EXIT.
  ENDLOOP.

*>Clases de documento por defecto
  LOOP AT lit_zret0005p01 WHERE param = 'AUART_DEF'.
    s_auart-sign   = lit_zret0005p01-valo1.
    s_auart-option = lit_zret0005p01-valo2.
    s_auart-low    = lit_zret0005p01-valo3.
    s_auart-high   = lit_zret0005p01-valo4.

    APPEND s_auart.
  ENDLOOP.

*>Canales de distribución por defecto
  LOOP AT lit_zret0005p01 WHERE param = 'VTWEG_DEF'.
    s_vtweg-sign   = lit_zret0005p01-valo1.
    s_vtweg-option = lit_zret0005p01-valo2.
    s_vtweg-low    = lit_zret0005p01-valo3.
    s_vtweg-high   = lit_zret0005p01-valo4.

    APPEND s_vtweg.
  ENDLOOP.

*>Sectores por defecto
  LOOP AT lit_zret0005p01 WHERE param = 'SPART_DEF'.
    s_spart-sign   = lit_zret0005p01-valo1.
    s_spart-option = lit_zret0005p01-valo2.
    s_spart-low    = lit_zret0005p01-valo3.
    s_spart-high   = lit_zret0005p01-valo4.

    APPEND s_spart.
  ENDLOOP.

*>Funcion interlocutor: TRANSPORTISTA
  LOOP AT lit_zret0005p01 WHERE param = 'PARVW_TRANSPORTISTA'.
    gd_parvw_transportista = lit_zret0005p01-valo1.
    EXIT.
  ENDLOOP.

*>Funcion interlocutor: AGENTE
  LOOP AT lit_zret0005p01 WHERE param = 'PARVW_AGENTE'.
    gd_parvw_agente = lit_zret0005p01-valo1.
    EXIT.
  ENDLOOP.

*>Funcion interlocutor: DIR.ENTREGA
  LOOP AT lit_zret0005p01 WHERE param = 'PARVW_DIRENTREGA'.
    gd_parvw_direntrega = lit_zret0005p01-valo1.
    EXIT.
  ENDLOOP.

*>Clases de condición de cabecera a mostrar
  LOOP AT lit_zret0005p01 WHERE param = 'CONDH'.
    APPEND lit_zret0005p01 TO git_condh.
  ENDLOOP.

*>Clases de condición de posición a mostrar
  LOOP AT lit_zret0005p01 WHERE param = 'CONDP'.
    APPEND lit_zret0005p01 TO git_condp.
  ENDLOOP.

*>Clase de precio para el botón de "Cambiar precio"
  LOOP AT lit_zret0005p01 WHERE param = 'KSCHL_CAMBIAR_PRECIO'.
    gd_kschl_precio = lit_zret0005p01-valo1.
  ENDLOOP.

*>Clases de precio de descuentos
  gran_kschl_desc-sign   = 'I'.
  gran_kschl_desc-option = 'EQ'.
  LOOP AT lit_zret0005p01 WHERE param = 'KSCHL_CAMBIAR_DESCUENTO'.
    gran_kschl_desc-low = lit_zret0005p01-valo1.
    APPEND gran_kschl_desc.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_get_kschlt
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GIT_CONDH_VALO1
*&      <-- LD_KSCHLT
*&---------------------------------------------------------------------*
FORM f_get_kschlt  USING    pe_kschl
                   CHANGING ps_kschlt.

  CLEAR ps_kschlt.

  SELECT SINGLE vtext
    FROM t685t
    INTO ps_kschlt
   WHERE spras = sy-langu
     AND kappl = 'V'
     AND kschl = pe_kschl.

ENDFORM.

*===================================================================================================
*& Form f_get_columnas_cond
*===================================================================================================
* Obtiene los valores de las distintas clases de condición de cabecera y posición del pedido que
* estén parametrizadas para ser mostradas en el monitor.
*===================================================================================================
FORM f_get_columnas_cond CHANGING ptc_orders TYPE gtyp_t_orders.
*===================================================================================================
* 0.- Declaración de variables
*===================================================================================================
  DATA: ls_orders         TYPE gtyp_s_orders,
        lit_prcd_elements LIKE zret0005s01 OCCURS 0 WITH HEADER LINE.

*===================================================================================================
* 1.- Lógica
*===================================================================================================
*>Solo obtenemos valores si se han parametrizado condiciones para ser mostradas en el monitor y si
* se han seleccionado pedidos
  IF ( git_condh[] IS NOT INITIAL OR git_condp[] IS NOT INITIAL ) AND
       ptc_orders[] IS NOT INITIAL.

*  >Obtenemos todas las condiciones activas de cada uno de los pedidos seleccionados
    SELECT vbak~vbeln,
           prcd_elements~kposn,
           prcd_elements~kschl,
           prcd_elements~krech,
           prcd_elements~kbetr,
           prcd_elements~waers
      FROM vbak JOIN prcd_elements ON prcd_elements~knumv = vbak~knumv
      INTO CORRESPONDING FIELDS OF TABLE @lit_prcd_elements
      FOR ALL ENTRIES IN @ptc_orders
     WHERE vbak~vbeln = @ptc_orders-vbeln
       AND prcd_elements~kinak = ''.                                                                "Condición inactiva

*  >Completamos cada linea del monitor con los valores de las condiciones de precio parametrizadas
    LOOP AT ptc_orders INTO ls_orders.
      LOOP AT git_condh.
        READ TABLE lit_prcd_elements WITH KEY vbeln = ls_orders-vbeln
                                              kposn = '000000'
                                              kschl = git_condh-valo1.

        IF sy-subrc = 0.
          CASE git_condh-conta.
            WHEN 1.
              ls_orders-condh_01 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condh_01_w = '%'.
              ELSE.
                ls_orders-condh_01_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 2.
              ls_orders-condh_02 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condh_02_w = '%'.
              ELSE.
                ls_orders-condh_02_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 3.
              ls_orders-condh_03 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condh_03_w = '%'.
              ELSE.
                ls_orders-condh_03_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 4.
              ls_orders-condh_04 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condh_04_w = '%'.
              ELSE.
                ls_orders-condh_04_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 5.
              ls_orders-condh_05 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condh_05_w = '%'.
              ELSE.
                ls_orders-condh_05_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 6.
              ls_orders-condh_06 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condh_06_w = '%'.
              ELSE.
                ls_orders-condh_06_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 7.
              ls_orders-condh_07 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condh_07_w = '%'.
              ELSE.
                ls_orders-condh_07_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 8.
              ls_orders-condh_08 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condh_08_w = '%'.
              ELSE.
                ls_orders-condh_08_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 9.
              ls_orders-condh_09 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condh_09_w = '%'.
              ELSE.
                ls_orders-condh_09_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 10.
              ls_orders-condh_10 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condh_10_w = '%'.
              ELSE.
                ls_orders-condh_10_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 11.
              ls_orders-condh_11 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condh_11_w = '%'.
              ELSE.
                ls_orders-condh_11_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 12.
              ls_orders-condh_12 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condh_12_w = '%'.
              ELSE.
                ls_orders-condh_12_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 13.
              ls_orders-condh_13 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condh_13_w = '%'.
              ELSE.
                ls_orders-condh_13_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 14.
              ls_orders-condh_14 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condh_14_w = '%'.
              ELSE.
                ls_orders-condh_14_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 15.
              ls_orders-condh_15 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condh_15_w = '%'.
              ELSE.
                ls_orders-condh_15_w = lit_prcd_elements-waers.
              ENDIF.
          ENDCASE.
        ENDIF.
      ENDLOOP.

      LOOP AT git_condp.
        READ TABLE lit_prcd_elements WITH KEY vbeln = ls_orders-vbeln
                                              kposn = ls_orders-posnr
                                              kschl = git_condp-valo1.

        IF sy-subrc = 0.
          CASE git_condp-conta.
            WHEN 1.
              ls_orders-condp_01 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condp_01_w = '%'.
              ELSE.
                ls_orders-condp_01_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 2.
              ls_orders-condp_02 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condp_02_w = '%'.
              ELSE.
                ls_orders-condp_02_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 3.
              ls_orders-condp_03 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condp_03_w = '%'.
              ELSE.
                ls_orders-condp_03_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 4.
              ls_orders-condp_04 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condp_04_w = '%'.
              ELSE.
                ls_orders-condp_04_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 5.
              ls_orders-condp_05 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condp_05_w = '%'.
              ELSE.
                ls_orders-condp_05_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 6.
              ls_orders-condp_06 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condp_06_w = '%'.
              ELSE.
                ls_orders-condp_06_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 7.
              ls_orders-condp_07 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condp_07_w = '%'.
              ELSE.
                ls_orders-condp_07_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 8.
              ls_orders-condp_08 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condp_08_w = '%'.
              ELSE.
                ls_orders-condp_08_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 9.
              ls_orders-condp_09 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condp_09_w = '%'.
              ELSE.
                ls_orders-condp_09_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 10.
              ls_orders-condp_10 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condp_10_w = '%'.
              ELSE.
                ls_orders-condp_10_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 11.
              ls_orders-condp_11 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condp_11_w = '%'.
              ELSE.
                ls_orders-condp_11_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 12.
              ls_orders-condp_12 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condp_12_w = '%'.
              ELSE.
                ls_orders-condp_12_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 13.
              ls_orders-condp_13 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condp_13_w = '%'.
              ELSE.
                ls_orders-condp_13_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 14.
              ls_orders-condp_14 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condp_14_w = '%'.
              ELSE.
                ls_orders-condp_14_w = lit_prcd_elements-waers.
              ENDIF.
            WHEN 15.
              ls_orders-condp_15 = lit_prcd_elements-kbetr.
              IF lit_prcd_elements-krech = 'A'.
                ls_orders-condp_15_w = '%'.
              ELSE.
                ls_orders-condp_15_w = lit_prcd_elements-waers.
              ENDIF.
          ENDCASE.
        ENDIF.
      ENDLOOP.

      MODIFY ptc_orders FROM ls_orders.
    ENDLOOP.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_0200_pbo_init
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_0200_pbo_init .
  DATA: ld_domname  LIKE  dd07v-domname,
        ld_domvalue LIKE  dd07v-domvalue_l,
        ld_ddtext   LIKE dd07v-ddtext.

  IF gs_change-lifsk IS INITIAL.
    gs_change-lifskt = ''.
  ELSE.
    SELECT SINGLE vtext
      FROM tvlst
      INTO gs_change-lifskt
     WHERE spras = sy-langu
       AND lifsp = gs_change-lifsk.
  ENDIF.

  IF vbak-faksk IS INITIAL.
    gs_change-fakskt = ''.
  ELSE.
    SELECT SINGLE vtext
      FROM tvfst
      INTO gs_change-fakskt
     WHERE spras = sy-langu
       AND faksp = vbak-faksk.
  ENDIF.

  IF gs_change-zterm IS INITIAL.
    gs_change-ztermt = ''.
  ELSE.
    SELECT SINGLE text1
      FROM t052u
      INTO gs_change-ztermt
     WHERE spras = sy-langu
       AND zterm = gs_change-zterm.
  ENDIF.

  IF gs_change-vsart IS INITIAL.
    gs_change-vsartt = ''.
  ELSE.
    SELECT SINGLE bezei
      FROM t173t
      INTO gs_change-vsartt
     WHERE spras = sy-langu
       AND vsart = gs_change-vsart.
  ENDIF.

  IF t683-knprs_v IS INITIAL.
    gs_change-knprst = ''.
  ELSE.
    ld_domname = 'KNPRS'.
    ld_domvalue = t683-knprs_v.

    CALL FUNCTION 'DOMAIN_VALUE_GET'
      EXPORTING
        i_domname  = ld_domname
        i_domvalue = ld_domvalue
      IMPORTING
        e_ddtext   = ld_ddtext
      EXCEPTIONS
        not_exist  = 1
        OTHERS     = 2.
    IF sy-subrc <> 0. ENDIF.

    gs_change-knprst = ld_ddtext.
  ENDIF.

  IF gs_change-agent IS INITIAL.
    gs_change-agentt = ''.
  ELSE.
    SELECT SINGLE name1
      FROM kna1
      INTO gs_change-agentt
     WHERE kunnr = gs_change-agent.
  ENDIF.

  IF gs_change-shipto IS INITIAL.
    gs_change-shiptot = ''.
  ELSE.
    SELECT SINGLE name1
      FROM kna1
      INTO gs_change-shiptot
     WHERE kunnr = gs_change-shipto.
  ENDIF.

  IF gs_change-freight_ag IS INITIAL.
    gs_change-freight_agt = ''.
  ELSE.
    SELECT SINGLE name1
      FROM lfa1
      INTO gs_change-freight_agt
     WHERE lifnr = gs_change-freight_ag.
  ENDIF.

  IF gs_change-inco1 IS INITIAL.
    gs_change-inco1t = ''.
  ELSE.
    SELECT SINGLE bezei
      FROM tinct
      INTO gs_change-inco1t
     WHERE spras = sy-langu
       AND inco1 = gs_change-inco1.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_0200_pai_validar_zterm
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_0200_pai_validar_zterm .
  SELECT SINGLE zterm
    FROM t052u
    INTO gs_change-zterm
   WHERE zterm = gs_change-zterm.

  IF sy-subrc <> 0.
*   Msg: Condición de pago & no prevista.
    MESSAGE e172(zret0005) WITH gs_change-zterm DISPLAY LIKE 'I'.
  ENDIF.
ENDFORM.

FORM f_0200_pai_validar_inco1 .
  SELECT SINGLE inco1
    FROM tinc
    INTO gs_change-inco1
   WHERE inco1 = gs_change-inco1.

  IF sy-subrc <> 0.
*   Msg: Incoterms 1 & no previsto.
    MESSAGE e173(zret0005) WITH gs_change-inco1 DISPLAY LIKE 'I'.
  ENDIF.
ENDFORM.

FORM f_0200_pai_validar_lifsk .
  SELECT SINGLE lifsp
    FROM tvls
    INTO gs_change-lifsk
   WHERE lifsp = gs_change-lifsk.

  IF sy-subrc <> 0.
*   Msg: Bloqueo de entrega & no previsto.
    MESSAGE e174(zret0005) WITH gs_change-lifsk DISPLAY LIKE 'I'.
  ENDIF.
ENDFORM.

FORM f_0200_pai_validar_faksk .
  SELECT SINGLE faksp
    FROM tvfs
    INTO vbak-faksk
   WHERE faksp = vbak-faksk.

  IF sy-subrc <> 0.
*   Msg: Bloqueo de factura & no previsto.
    MESSAGE e175(zret0005) WITH vbak-faksk DISPLAY LIKE 'I'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_0200_pai_validar_edatu
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_0200_pai_validar_edatu .
  IF gs_change-edatu <= sy-datum.
    MESSAGE 'La fecha de reparto tiene que estar en el futuro' TYPE 'E' DISPLAY LIKE 'I'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_0200_pai_validar_agent
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_0200_pai_validar_agent .

  SELECT SINGLE kunnr
    FROM kna1
    INTO gs_change-agent
   WHERE kunnr = gs_change-agent.

  IF sy-subrc <> 0.
    MESSAGE e176(zret0005) WITH gs_change-agent DISPLAY LIKE 'I'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_0200_pai_validar_freight_ag
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_0200_pai_validar_freight_ag .
  SELECT SINGLE lifnr
    FROM lfa1
    INTO gs_change-freight_ag
   WHERE lifnr = gs_change-freight_ag.

  IF sy-subrc <> 0.
    MESSAGE e177(zret0005) WITH gs_change-freight_ag DISPLAY LIKE 'I'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_0200_pai_validar_shipto
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_0200_pai_validar_shipto .
  SELECT SINGLE kunnr
      FROM kna1
      INTO gs_change-shipto
     WHERE kunnr = gs_change-shipto.

  IF sy-subrc <> 0.
    MESSAGE e178(zret0005) WITH gs_change-shipto DISPLAY LIKE 'I'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_0200_pai_validar_knprs
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_0200_pai_validar_knprs .
  DATA: ld_domname  LIKE  dd07v-domname,
        ld_domvalue LIKE  dd07v-domvalue_l,
        ld_ddtext   LIKE dd07v-ddtext.

  ld_domname = 'KNPRS'.
  ld_domvalue = t683-knprs_v.

  CALL FUNCTION 'DOMAIN_VALUE_GET'
    EXPORTING
      i_domname  = ld_domname
      i_domvalue = ld_domvalue
    IMPORTING
      e_ddtext   = ld_ddtext
    EXCEPTIONS
      not_exist  = 1
      OTHERS     = 2.
  IF sy-subrc <> 0.
    MESSAGE e179(zret0005) WITH t683-knprs_v DISPLAY LIKE 'I'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_0200_pai_validar_vsart
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_0200_pai_validar_vsart .
  SELECT SINGLE vsart
    FROM t173
    INTO gs_change-vsart
   WHERE vsart = gs_change-vsart.

  IF sy-subrc <> 0.
    MESSAGE e180(zret0005) WITH gs_change-vsart DISPLAY LIKE 'I'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_0205_pbo_init
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_0205_pbo_init .
  IF gs_material-abgru IS INITIAL.
    gs_material-abgrut = ''.
  ELSE.
    SELECT SINGLE bezei
      FROM tvagt
      INTO gs_material-abgrut
     WHERE spras = sy-langu
       AND abgru = gs_material-abgru.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_0205_pbo_config
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_0205_pbo_config .
  IF gf_cambiar_descuento = ''.
    LOOP AT SCREEN.
      IF screen-name = 'GS_CONDITION-KSCHL'.
        screen-input = 0.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
ENDFORM.

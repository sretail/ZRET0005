*&---------------------------------------------------------------------*
*& Include          ZRET0005_FLOW
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  IF bgindx IS INITIAL.
    PERFORM f_check_required_files USING 'X'.
    PERFORM f_get_sales.
    IF gt_orders IS NOT INITIAL.
      CALL SCREEN 100.
    ELSE.
      MESSAGE i004(ZRET0005). ""No data found
    ENDIF.
  ELSE.
    PERFORM f_get_cluster_data.
    PERFORM f_bapi_background.
  ENDIF.

*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZRET0005P01.....................................*
DATA:  BEGIN OF STATUS_ZRET0005P01                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZRET0005P01                   .
CONTROLS: TCTRL_ZRET0005P01
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZRET0005P01                   .
TABLES: ZRET0005P01                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .

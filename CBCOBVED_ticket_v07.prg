*  PROGRAMA...: CBCOBVED.PRG (VIENE DE CBCODVEG.PRG)
*  OBJETIVO...: ACTUALIZA PAGOS EN VENTANILLA
*  FECHA......: 12/11/1997
*  ULT.MODIF..: 02/12/2013
*  AUTOR......: CARLOS LAMAS
*  CAMBIADO POR ROGER EL 8.5.2006
*  MODIFICADO POR CARLOS VALENTE 02-12-2013
*  MODIFICADO POR ROGER ARMOA 15.10.2018

*  SETEA AMBIENTE:
   SET DELE ON
   SET CONF ON
   SET INTE OFF
   SET TALK OFF
   SET EXCL OFF
   SET DATE BRIT
   SET CENT ON
   SET ESCA OFF
   SET INTE ON
   PROGRAM="CBCOBVED"
   @01,03 SAY PROGRAM
   SET COLOR TO I

*  DEFINE VARIABLES:
   V_FECCOB = DATE()                    && FECHA DE COBRANZA
   V_FECTOP = DATE()                    && FECHA TOPE P/ARMAR SALDOS
   V_CODSUC = "01"                      && CODIGO DE SUCURSAL
   V_CODPRO = SPACE(3)                  && CODIGO DE PRODUCTO
   V_NROCTA = 0                         && NRO DE CUENTA
   V_NROCON = 0                         && NRO DE CONTRATO
   V_NOMTIT = []                        && NOMBRE DEL TITULAR
   V_NOMBEN = []                        && NOMBRE DE BENEFICIARIO
   V_NROTAL = 0                         && NRO DE TALON CHEQUERA
   V_CITITU = []                        && NRO DE C.I. DE TITULAR
   V_RUCTIT = []                        && NRO DE R.U.C. DE TITULAR
   V_TIPACC = []                        && TIPO DE ACCESO
   V_CANFIL = 0                         && CANTIDAD DE FILAS EN ARRAY
   V_CANCON = 0                         && CANTIDAD DE CONTRATOS
   V_TOTIDO = 0                         && TOTAL DE CUOTAS VENCIDAS
   V_TOTCER = 0                         && TOTAL DE CUOTAS A VENCER
   V_TOTRAL = 0                         && TOTAL GENERAL
   V_TOTRGO = 0                         && TOTAL RECARGOS
   V_SUBNTO = 0                         && SUBTOTAL DE CUOTAS P/CONTRATO
   V_SUBRGO = 0                         && SUBTOTAL DE RECARGOS P/CONTRATO
   V_SUBTAL = 0                         && SUBTOTAL DE TOTALES P/CONTRATO
   V_NROFAC = 0                         && NRO DE FACTURA
   V_TIPFAC = []                        && TIPO DE FACTURA
   V_SERFAC = []                        && SERIE DE FACTURA
   V_CANVAL = 0                         && CANTIDAD DE VALORES DE COBRO
   V_CODPRO = []                        && CODIGO DE PRODUCTO O CENOPE
   V_TOTFAC = 0                         && TOTAL DE FACTURA
   V_PREIVA = 0                         && TOTAL SIN IVA
   V_VALIVA = 0                         && VALOR IVA
   V_IMES   = []                        && NOMBRE DEL MES DE COBRO
   VALNEL   = []                        && MONTO FACTURA EN LETRAS
   V_NROCI  = 0                         && NRO DE CEDULA DEL TITULAR
   V_NOMBEN = []                        && NOMBRE DE BENEFICIARIO
   V_FLAK01 = []                        && BANDERA DE CONTROL
   V_ARCAUX = []                        && NOMBRE DE ARCHIVO TEMPORAL
   V_CODCAJ = []                        && CODIGO DE CAJERO
   V_VENCIM = []                        && VARIABLE AUX. P/IMPRIMIR FACTURA
   V_DMES   = []                        && VARIABLE AUX. P/IMPRIMIR FACTURA
   V_LINEA  = 0                         && VARIABLE AUX. P/IMPRIMIR FACTURA
   V_FECALT = CTOD([//])                && FECHA AUX. ALTERNATIVA DE COBRANZA
   V_FECAUX = CTOD([//])                && FECHA AUX. DE COBRANZA
   V_CALIFA = 9                         && LINEAS DE DETALLES POR FACTURA
   V_LINFAC = 0                         && AUXILIAR LINEAS DE DETALLES POR FACTURA
   V_FORPAG = [         ]               && FORMA DE PAGO  MENSUAL, ANUAL, SEMESTRAL
   V_DETALLE = []                       && DETALLE DE IMPRECION DE FACTURA
   V_PREUNI = 0                         && PRECIO UNITARIO P/IMPRESION DE FACTURA
   V_SUBTOT = 0                         && SUBTOTAL DE ITEM P/IMPRESION DE FACTURA
   V_TOTDESCUE = 0                      && TOTAL DESCUENTO EN FACTURA

*  DEFINE MATRICES Y VECTORES
   DIMENSION ESTADCTA(300,17)
   DIMENSION SALDOACT(100,09)
   DIMENSION DETAVALO(100,07)

*  DEFINE VENTANAS:
   DEFINE WINDOW INTERIOR  FROM 07,01 TO 17,78 NONE COLOR I
   DEFINE WINDOW PIDEPAGO  FROM 02,01 TO 23,78 NONE COLOR I
   DEFINE WINDOW TITULARES FROM 10,05 TO 21,77 TITLE " " COLOR I,,I,I,,I,
*  DEFINE WINDOW DESVALCOB FROM 07,01 TO 18,78 NONE COLOR I
*  DEFINE WINDOW MENSAJES  FROM 06,04 TO 11,51 TITLE [ADVERTENCIA] COLOR SCHEME 5
   DEFINE WINDOW ACTICOBRO FROM 05,01 TO 17,78 NONE COLOR SCHEME 5
   DEFINE WINDOW SOLMODIF  FROM 06,04 TO 23,79 TITLE [ SOLICITUD DE MODIFICACION DE DATOS ] COLOR SCHEME 5
   DEFINE WINDOW TIPOMODI  FROM 10,27 TO 12,50 TITLE [QUE DESEA?] COLOR I,I,I,I
   DEFINE WINDOW CONFIRMA  FROM 20,27 TO 22,50 TITLE [CONFIRMA?] COLOR I,I,I,I
   DEFINE WINDOW ESTADCTA  FROM 01,00 TO 24,79 DOUBLE COLOR I,,I,I,,I,
   DEFINE WINDOW BUSQUEDA  FROM 16,06 TO 22,76 TITLE [ BUSQUEDA ] COLOR SCHEME 5
   DEFINE WINDOW DETACOBRO FROM 12,05 TO 22,75 TITLE [ DETALLE DE VALORES ]  COLOR SCHEME 5
   DEFINE WINDOW CON_TAR   FROM 17,27 TO 22,60 TITLE [ TARGETAS DE CREDITO ] DOUBLE GROW
   DEFINE WINDOW OBSER1    FROM 02,01 TO 06,78 TITLE [OBSERVACIONES ] DOUBLE
   DEFINE WINDOW OBSCAJERO FROM 07,01 TO 23,78 TITLE [DETALLE DE OBSERVACIONES ] DOUBLE FOOT "<ESC> PARA SALIR "

*  SELECCION DE ARCHIVOS:
   &&SELEC 1
   &&USE CLIENT_M ORDER TITULA
   SELEC 2
   USE SALDOS_M ORDER IAS003 SHARED
   SELEC 3
   USE MODIFI_M ORDER MODIFI_NUM
   SELEC 4
   USE TALONA_M ORDER TALONA_PEE
   SELEC 5
   USE FUNCIO_M ORDER FUNCIO_NUM
   SELEC 6
   USE BANCOS_M ORDER I01B01
   SELEC 7
   USE DERECH_M ORDER DERECH_HAB
   SELEC 8
   USE SUCURS_M ORDER SUCURSAL
   SELEC 9
   USE TARJET_M ORDER TARJET_COD
   SELEC 10
   USE MOVCAJ_M ORDER MOVCAJ_NUM
   SELEC 11
   USE VALCAJ_D ORDER VALCAJ_FAC
   SELEC 12
   USE CONTRA_M ORDER CON011
   &&SET RELATION TO (PADR(CONTRA_SUC,2) + TRANSFORM(CONTRA_TIT,[999999])) INTO CLIENT_M ADDITIVE
   SELEC 13
   USE BENEFI_D ORDER BENEFI_CON
   SET RELATION TO (BENEFI_PRO + TRANSFORM(BENEFI_CON,[999999]));
                INTO CONTRA_M ADDITIVE
   SELEC 14
   USE AUXCOB_D ORDER AUXCOB_001
   SELEC 15
   USE COBRAD_M ORDER COBRAD_PER
   SELE 16
   USE OBSCAJ_D ORDE CUENTA
   SET FILT TO OBSCAJ_FIN=CTOD('//')

*  CICLO PRINCIPAL:
   SAVE SCREEN TO PANTA1
   STORE "T" TO V_FLAK01
   DO VAL_DER WITH CLAVEUSU,CAPITULO,[AGREGAR]
   DO VALICAJE
   DO VALICTABAN
   DO WHILE V_FLAK01 = "T" .AND. HABILITA = SPACE(02)
      @	06,04 CLEAR TO 23,78
      @ 06,04 TO 23,78
      @ 06,05 SAY "COBROS EN VENTANILLA"
      DO VALIFACT
      IF V_FLAK01 = "F"
         EXIT
      ENDIF

******* TEMPORAL PARA EXONERAR CUOTAS ******

      DO AUXITEMP

******       FIN TEMPORAL             ******

      DO PANTAINI
      DO PIDEINIC
      IF LASTKEY() = 27
         EXIT
      ENDIF
      DO WHILE V_FLAK01 = "T"
          DO ELIGEACC
          IF LASTKEY() = 27
             EXIT
          ENDIF
          IF V_NROCTA > 0 .AND. V_NOMTIT <> SPACE(35)
             DO ARMAOBSC
             DO ARMAESTA
          ENDIF
          IF V_TOTRAL > 0
             DO DESPESTA
          ENDIF
          DO LIMPIAVAR

******* TEMPORAL PARA EXONERAR CUOTAS ******

      DO AUXITEMP

******       FIN TEMPORAL             ******

          ON KEY
          DO VALIFACT
      ENDDO
      ON KEY
   ENDDO
   RESTORE SCREEN FROM PANTA1
   CLOSE ALL
   RETURN

*  ARMA OBSERVACIONES PARA EL CAJERO:
   PROC ARMAOBSC
   SELE 16
   USE OBSCAJ_D ORDE CUENTA
   SET FILT TO OBSCAJ_FIN=CTOD('//')
   GO TOP
   SEEK TRAN(V_NROCTA,"999999")
   IF NOT EOF()
      ACTI WIND OBSER1
      @ 00,01 SAY [MENSAJE PARA:]+[ ]+USUARIOS
      @ 01,01 SAY [LA CUENTA NRO]+TRAN(V_NROCTA," 999999")+[ PERTENECIENTE A: ]+TRIM(V_NOMTIT)
      @ 02,01 SAY [REGISTRA LAS SIGUIENTES OBSERVACIONES:]
      DO WHIL .T.
         ACTI WIND OBSCAJERO
         BROW KEY TRAN(V_NROCTA,"999999"),TRAN(V_NROCTA,"999999");
         FIELDS;
         OBSCAJ_FEC     :H="FECHA",;
         OBSCAJ_DET     :H="OBSERVACION",;
         OBSCAJ_NOM     :H="OPERADOR";
         NOMO;
         NODE;
         NOAP;
         NOLG;
         NORG;
         IN WIND OBSCAJERO
         IF LAST()=27
            EXIT
         ENDI
      ENDD
   ENDI
   DEAC WIND OBSCAJERO, OBSER1
   SET FILT TO
   USE
   RETU

*  VALIDA NUMERO DE FACTURA
   PROCEDURE VALIFACT
   SET EXACT OFF
   SET NEAR  OFF
   SELECT TALONA_M
   GO TOP
   SEEK TRIM(USUARPER)+TRIM([EN USO])+TRIM([CONTADO])
   IF EOF()
      GO TOP
      SEEK TRIM(USUARPER)+TRIM([ENTREGADO])+TRIM([CONTADO])
      IF EOF()
         WAIT [NO EXISTE TALONARIO HABILITADO PARA ESTE USUARIO] WINDOW
         STORE "F" TO V_FLAK01
         RETURN
      ELSE
         IF TALONA_ULT = 0
            STORE TALONA_DES   TO V_NROFAC
         ENDIF
      ENDIF
   ELSE
      IF TALONA_ULT < TALONA_HAS
            STORE TALONA_ULT+1  TO V_NROFAC
      ELSE
         GO TOP
         SEEK TRIM(USUARPER)+TRIM([ENTREGADO])+TRIM([CONTADO])
         IF EOF()
            WAIT [NO EXISTE TALONARIO HABILITADO PARA ESTE USUARIO] WINDOW
            STORE "F" TO V_FLAK01
            RETURN
         ENDIF
         STORE TALONA_DES      TO V_NROFAC
      ENDIF
   ENDIF
   STORE TALONA_TIP            TO V_TIPFAC
   STORE TALONA_SER            TO V_SERFAC
   RETURN

*  VALIDA CAJERO CON EL USUARIO
   PROCEDURE VALICAJE
   SELECT COBRAD_M
   SEEK (USUARPER)
   IF .NOT. FOUND()
*     SEEK TRIM(USUARPER)+TRIM([ENTREGADO])
*     IF EOF()
         WAIT [ATENCION !!, USUARIO NO HABILITADO COMO COBRADOR ] WINDOW
         STORE    "F"     TO V_FLAK01
         RETURN
*     ENDIF
   ENDIF
   STORE COBRAD_NUM TO V_CODCAJ
   RETURN

* PANTALLA INICIAL
  PROCEDURE PANTAINI
     @ 07,06 SAY "FECHA DE COBRO......:"
     @ 07,45 SAY "SUCURSAL:"
     @ 08,06 SAY "USUARIO.............: " + USUARIOS
     @ 08,40 SAY "FECHA ALTERNATIVA...: "
  RETURN

* PIDE DATOS INICIALES
  PROCEDURE PIDEINIC
      @07,28 GET V_FECCOB VALID ! EMPTY(V_FECCOB) ERROR[NO PUEDE SER VACIO]
      &&@07,28 SAY V_FECCOB &&VALID ! EMPTY(V_FECCOB) ERROR[NO PUEDE SER VACIO]
      @07,54 GET V_CODSUC VALID ! EMPTY(V_CODSUC) ERROR[NO PUEDE SER VACIO]
      @08,61 GET V_FECALT
      READ
*     @07,28 SAY DTOC(V_FECCOB)
*     @07,54 SAY TRANSFORM(V_CODSUC,[99])
  RETURN

* ELIGE OPCION DE ACCESO
  PROCEDURE ELIGEACC
     @09,06 SAY "INGRESAR POR........:"
     @11,20 TO 18,55
     @12,23 PROMPT [\<A - NRO. DE CONTRATO        ]
     @13,23 PROMPT [\<B - NRO. DE CUENTA          ]
     @14,23 PROMPT [\<C - NRO. DE TALON CHEQUERA  ]
     @15,23 PROMPT [\<D - NRO. DE C.I. DEL TITULAR]
     @16,23 PROMPT [\<E - NOMBRE DEL TITULAR      ]
     @17,23 PROMPT [\<F - NOMBRE DE BENEFICIARIO  ]
     STORE 1 TO OPCIONACC
     MENU    TO OPCIONACC
     DO CASE
        CASE OPCIONACC = 1
             DO INNROCON
        CASE OPCIONACC = 2
             DO INNROCTA
        CASE OPCIONACC = 3
             DO INNROTAL
        CASE OPCIONACC = 4
             DO INNROCI
        CASE OPCIONACC = 5
             DO INNOMTIT
        CASE OPCIONACC = 6
             DO INNOMBEN
     ENDCASE
   RETURN

*  BUSQUEDA POR NRO. CTA.
   PROCEDURE INNROCTA
      @11,20 CLEAR TO 18,55
      @11,20 TO 18,55
      DO LIMPIAVAR
      DO WHILE LASTKEY() <> 27
         @13,22 SAY "NRO. DE CUENTA QUE DESEA:";
                GET V_NROCTA PICTURE "999999" VALID ! EMPTY(V_NROCTA) ERROR [NO PUEDE SER CERO]
         READ
         IF V_NROCTA > 0
         && SET EXACT ON
            SELECT CLIENT_M
            SET ORDER TO TITULA
            GO TOP
            SEEK (PADR(V_CODSUC,2)+TRANSFORM(V_NROCTA,"999999"))
            IF FOUND()
               IF CLIENT_EST <> "B"
                  STORE CLIENT_TIT TO V_NROCTA
                  STORE CLIENT_NOM TO V_NOMTIT
                  STORE CONTRA_M.CONTRA_NOM TO V_NOMTIT
                  STORE CONTRA_M.CONTRA_RUC TO V_RUCTIT
                  EXIT
               ELSE
                  WAIT [ESTA CUENTA FUE DADO DE BAJA EL DOA ] + DTOC(CLIENT_BAJ) + [, VERIFIQUE !!] WINDOW
                  DO LIMPIAVAR
               ENDIF
            ELSE
               IF V_NROCTA > 0
                  WAIT [ NO EXISTE NINGUNA CUENTA CON ESE NROMERO, FAVOR VERIFIQUE !!] WINDOW
               ENDIF
               DO LIMPIAVAR
            ENDIF
         ENDIF
      ENDDO
      @11,20 CLEAR TO 18,55
   RETURN

*  BUSQUEDA POR NRO. CONTRATO
   PROCEDURE INNROCON
      @11,20 CLEAR TO 18,58
      @11,20 TO 18,58
      DO LIMPIAVAR
      DO WHILE LASTKEY() <> 27
         @13     ,22 SAY "PRODUCTO QUE DESEA........:";
                     GET V_CODPRO PICTURE[@M PSM,PSV,UDS,CMS,SDS,INH,SDT,OTD,SDC,PSC,PSI]
         @ROW()+1,22 SAY "NRO. DE CONTRATO QUE DESEA:";
                     GET V_NROCON PICTURE "999999" VALID ! EMPTY(V_NROCON) ERROR [NO PUEDE SER CERO]
         READ
         IF V_NROCON > 0
         && SET EXACT ON
            SELECT CONTRA_M
            SET ORDER TO CONTRATO
            GO TOP
            SEEK (V_CODSUC + V_CODPRO + TRANSFORM(V_NROCON,[999999.99]))
            IF FOUND()
               IF CONTRA_EST = "V"

                  IF CONTRA_ASO > 0 .AND. CONTRA_EST <> "B"
                     WAIT [ESTE CONTRATO SE PAGA POR ASOC. EMPLEADOS, VERIFIQUE !!] WINDOW
                  ENDIF

                  STORE CONTRA_NUM TO V_NROCON
                  STORE CONTRA_CEN TO V_CODPRO
                  STORE CONTRA_TIT TO V_NROCTA
                  STORE CONTRA_NOM TO V_NOMTIT
                  STORE CONTRA_RUC TO V_RUCTIT
                  DO VALISALCON
                  EXIT
               ELSE
                  IF CONTRA_EST = "B"
                     WAIT [ESTE CONTRATO FUE DADO DE BAJA EL DOA ] + DTOC(CONTRA_BAJ) + [, VERIFIQUE !!] WINDOW
                  ENDIF
                  DO LIMPIAVAR
               ENDIF
            ELSE
               IF V_NROCON > 0
                  WAIT [ NO EXISTE NINGUN CONTRATO CON ESE NROMERO, FAVOR VERIFIQUE !!] WINDOW
               ENDIF
               DO LIMPIAVAR
            ENDIF
         ENDIF
         ENDDO
      @11,20 CLEAR TO 18,58
   RETURN

*  BUSQUEDA POR NRO. TALON DE CHEQUERA
   PROCEDURE INNROTAL
      @11,20 CLEAR TO 18,58
      @11,20 TO 18,58
      DO LIMPIAVAR
      DO WHILE LASTKEY() <> 27
         @13,22 SAY "INGRESE NRO DE TALON A COBRAR:";
                GET V_NROTAL PICTURE "999999" VALID ! EMPTY(V_NROTAL) ERROR [NO PUEDE SER CERO]
         READ
         IF V_NROTAL > 0
        &&  SET EXACT ON
            SELECT SALDOS_M
            SET ORDER TO SALDOS_HOJ
            GO TOP
            SEEK (TRANSFORM(V_NROTAL,[999999]))
            IF FOUND()
               STORE SALDOS_NUM TO V_NROCON
               STORE SALDOS_PRO TO V_CODPRO
               STORE SALDOS_TIT TO V_NROCTA

               SELECT CONTRA_M
               SET ORDER TO CONTRA_001
               SEEK (V_CODPRO + TRANSFORM(V_NROCON,[999999]))
               IF FOUND()
                  IF CONTRA_EST = "V"
                     SELECT SALDOS_M
                     IF SALDOS_ASO = 0 .AND. SALDOS_EST = " "
                    &&  SET EXACT ON
                        SELECT CLIENT_M
                        SET ORDER TO TITULA
                        GO TOP
                        SEEK (PADR(V_CODSUC,2)+TRANSFORM(V_NROCTA,"999999"))
                        IF FOUND()
                           STORE CLIENT_NOM TO V_NOMTIT
                           STORE CONTRA_M.CONTRA_NOM TO V_NOMTIT
                           STORE CONTRA_M.CONTRA_RUC TO V_RUCTIT
                           EXIT
                        ELSE
                           DO LIMPIAVAR
                        ENDIF
                     ELSE
                        IF SALDOS_EST = "B"
                           WAIT [ESTE CONTRATO FUE DADO DE BAJA EL DOA ] + DTOC(CONTRA_BAJ) + [, VERIFIQUE !!] WINDOW
                        ENDIF
                        IF SALDOS_ASO > 0 .AND. SALDOS_EST <> "B"
                           WAIT [ESTE CONTRATO SE PAGA POR ASOC. EMP. NRO] + ALLTRIM(SALDOS_ASO) + [, VERIFIQUE !!] WINDOW
                        ENDIF
                        DO LIMPIAVAR
                     ENDIF
                  ELSE
                     IF CONTRA_EST = "B"
                        WAIT [ESTE CONTRATO FUE DADO DE BAJA EL DOA ] + DTOC(CONTRA_M.CONTRA_BAJ) + [, VERIFIQUE !!] WINDOW
                     ENDIF
                     DO LIMPIAVAR
                  ENDIF
               ENDIF
            ELSE
               IF V_NROCON > 0
                  WAIT [ NO EXISTE NINGUN TALON CON ESE NROMERO, FAVOR VERIFIQUE !!] WINDOW
               ENDIF
               DO LIMPIAVAR
            ENDIF
         ENDIF
      ENDDO
      @11,20 CLEAR TO 18,58
   RETURN

*  BUSQUEDA POR NRO. C.I. DEL TITULAR
   PROCEDURE INNROCI
      @11,20 CLEAR TO 18,55
      @11,20 TO 18,55
      DO LIMPIAVAR
      DO WHILE LASTKEY() <> 27
         @13,22 SAY "NRO. DE C.I. QUE DESEA:";
                GET V_NROCI  PICTURE "99999999" VALID ! EMPTY(V_NROCI) ERROR [NO PUEDE SER CERO]
         READ
         IF V_NROCI > 0
        &&  SET EXACT ON
            SELECT CLIENT_M
            SET ORDER TO CLIENT_NDO
            GO TOP
            SEEK (PADR(V_CODSUC,2) + TRANSFORM(V_NROCI,"99999999"))
            IF FOUND()
               IF CLIENT_EST <> "B"
                  STORE CLIENT_TIT TO V_NROCTA
                  STORE CLIENT_NOM TO V_NOMTIT
                  STORE CONTRA_M.CONTRA_NOM TO V_NOMTIT
                  STORE CONTRA_M.CONTRA_RUC TO V_RUCTIT
                  EXIT
               ELSE
                  WAIT [ESTE CONTRATO FUE DADO DE BAJA EL DOA ] + DTOC(CLIENT_BAJ) + [, VERIFIQUE !!] WINDOW
                  DO LIMPIAVAR
               ENDIF
            ELSE
               IF V_NROCI > 0
                  WAIT [ NO EXISTE NINGUN TITULAR CON ESE NRO DE C.I., FAVOR VERIFIQUE !!] WINDOW
               ENDIF
               DO LIMPIAVAR
            ENDIF
         ENDIF
      ENDDO
      @11,20 CLEAR TO 18,55
   RETURN

*  BUSQUEDA POR NOMBRE DEL TITULAR
   PROCEDURE INNOMTIT
      SAVE SCREEN TO PANTA5
      SET COLO TO I
      @ 11,05 CLEA TO 22,76
      SET COLO TO W+/B
      @ 22,07 SAY [ <F2>BUSCAR     <F3>ACTUALIZACIONES     <ESC>CANCELAR ]
      SET COLO TO I
      &&SELE CLIENT_M
      &&SET ORDE TO CLIENT_NOM
      SELECT CONTRA_M
      SET ORDE TO NOMBRE
      GO TOP
      DO LIMPIAVAR
      DO TECLA_CLIENTE
      ACTI WINDOW TITULARES
      IF .NOT. EOF()
         BROW FIEL;
         CONTRA_NOM      :H="APELLIDO, NOMBRE" :35    ,;
         CONTRA_NDO      :H="IDENTIDAD"  :P=[99999999],;
         CONTRA_TIT      :H="NRO CUENTA"  :P=[999999]   ;
         NOMODIFY			 						   ;
         NOAPPEND									   ;
         NODELETE									   ;
         NORGRID									   ;
         IN WINDOW TITULARES
         IF LASTKEY() <> 27
            IF CONTRA_EST <> "B"
               STORE CONTRA_M.CONTRA_NOM TO V_NOMTIT
               STORE CONTRA_TIT TO V_NROCTA
               STORE CONTRA_RUC TO V_RUCTIT
            ELSE
               WAIT [ ESTA CUENTA FUE DADO DE BAJA EL DOA ] + DTOC(CONTRA_BAJ) + [ , FAVOR VERIFIQUE !!! ] WINDOW NOWAIT
               DO LIMPIAVAR
            ENDIF
         ENDIF
      ELSE
         WAIT [ NO EXISTE NINGUN TITULAR CON ESE NOMBRE, FAVOR VERIFIQUE !!! ] WINDOW NOWAIT
         IF LASTKEY() = 27
            EXIT
         ENDIF
      ENDIF
      SET ORDER TO NOMBRE
      DEACTIVATE WINDOW TITULARES
      RESTORE SCREEN FROM PANTA5
      SET COLOR TO I
      DO TECLA_PAGO
   RETURN

*  BUSQUEDA POR NOMBRE DEL BENEFICIARIO
   PROCEDURE INNOMBEN
      SAVE SCREEN TO PANTA5
      SET COLOR TO I
      @ 11,05 CLEAR TO 22,76
      SET COLOR TO W+/B
      @ 22,15 SAY [ <F2>BUSCAR     <F3>ACTUALIZACIONES     <ESC>CANCELAR ]
      SET COLOR TO I
      SELECT BENEFI_D
      SET ORDER TO BENEFI_NOM
      GO TOP
      DO LIMPIAVAR
      DO TECLA_BENEFI
      ACTIVATE WINDOW TITULARES
      IF .NOT. EOF()
         BROWSE FIELDS;
         BENEFI_NOM      :H="APELLIDO, NOMBRE" :35    ,;
         BENEFI_NDO      :H="IDENTIDAD"  :P=[99999999],;
         BENEFI_PRO      :H=" "                       ,;
         BENEFI_CON      :H="CONTRATO "  :P=[999999]   ;
         NOMODIFY									   ;
         NOAPPEND									   ;
         NODELETE									   ;
         NORGRID									   ;
         IN WINDOW TITULARES
         IF LASTKEY() <> 27
            IF  CONTRA_M.CONTRA_ASO = 0   .AND. ;
                CONTRA_M.CONTRA_EST = "V"
                IF BENEFI_EST <> "B"
                   STORE CONTRA_M.CONTRA_TIT TO V_NROCTA
                   STORE CLIENT_M.CLIENT_NOM TO V_NOMTIT
                   STORE CONTRA_M.CONTRA_NOM TO V_NOMTIT
                   STORE CONTRA_M.CONTRA_RUC TO V_RUCTIT
                   STORE BENEFI_PRO          TO V_CODPRO
                   STORE BENEFI_CON          TO V_NROCON
                ELSE
                   IF CONTRA_M.CONTRA_EST = "B"
                       WAIT [ESTE BENEFICIARIO FUE DADO DE BAJA EL DOA ] + DTOC(BENEFI_BAJ) + [, VERIFIQUE !!] WINDOW
                   ENDIF
                ENDIF
            ELSE
               IF CONTRA_M.CONTRA_EST = "B"
                    WAIT [ESTE CONTRATO FUE DADO DE BAJA EL DOA ] + DTOC(CONTRA_M.CONTRA_BAJ) + [, VERIFIQUE !!] WINDOW
               ENDIF
               IF  CONTRA_M.CONTRA_ASO > 0 .AND. CONTRA_M.CONTRA_EST <> "B"
                    WAIT [ESTE CONTRATO SE PAGA POR ASOC. EMPLEADOS, VERIFIQUE !!] WINDOW
               ENDIF
               DO LIMPIAVAR
            ENDIF
         ENDIF
      ELSE
         WAIT [ NO EXISTE NINGUN BENEFICIARIO CON ESE NOMBRE, FAVOR VERIFIQUE !!! ] WINDOW NOWAIT
         IF LASTKEY() = 27
            EXIT
         ENDIF
      ENDIF
      SET ORDER TO BENEFI_NOM
      DEACTIVATE WINDOW TITULARES
      RESTORE SCREEN FROM PANTA5
      SET COLOR TO I
      DO TECLA_PAGO
   RETURN

*  CARGA ESTADO DE CUENTA DEL CLIENTE EN LA MATRIZ POR
   PROCEDURE ARMAESTA

   FOR I = 1 TO 300

       STORE   0        TO ESTADCTA(I,01)     && POSICION FISICA REGISTRO SALDOS_M
       STORE SPACE(03)  TO ESTADCTA(I,02)     && CODIGO DEL PRODUCTO
       STORE   0        TO ESTADCTA(I,03)     && NRO. CONTRATO
       STORE SPACE(05)  TO ESTADCTA(I,04)     && NRO. CUOTA
       STORE CTOD([//]) TO ESTADCTA(I,05)     && FECHA VENCIMIENTO CUOTA
       STORE   0        TO ESTADCTA(I,06)     && CANT. DIAS DE ATRASO
       STORE   0        TO ESTADCTA(I,07)     && SALDO DE CUOTA
       STORE   0        TO ESTADCTA(I,08)     && MONTO RECARGO
       STORE   0        TO ESTADCTA(I,09)     && MONTO CUOTA + RECARGO
       STORE   0        TO ESTADCTA(I,10)     && ACUMULADO A PAGAR
       STORE   0        TO ESTADCTA(I,11)     && NUMERO DE TALON
       STORE   0        TO ESTADCTA(I,12)     && MONTO INTERES COBRADO
       STORE   0        TO ESTADCTA(I,13)     && MONTO CUOTA COBRADO
       STORE   0        TO ESTADCTA(I,14)     && MONTO CUOTA ORIGINAL
       STORE   0        TO ESTADCTA(I,15)     && NRO. DE FACTURA ASIGNADA
       STORE CTOD([//]) TO ESTADCTA(I,16)     && PERIODO INICIAL CMS.
       STORE CTOD([//]) TO ESTADCTA(I,17)     && PERIODO FINAL CMS.

   ENDFOR

   FOR I = 1 TO 100

       STORE SPACE(03)  TO SALDOACT(I,01)     && CODIGO PRODUCTO
       STORE   0        TO SALDOACT(I,02)     && NRO. CONTRATO
       STORE   0        TO SALDOACT(I,03)     && MONTO CUOTA SIN RECARGO
       STORE   0        TO SALDOACT(I,04)     && MONTO A COBRAR
       STORE   0        TO SALDOACT(I,05)     && MONTO COBRADO
       STORE   0        TO SALDOACT(I,06)     && MONTO CUOTA CON RECARGO
       STORE SPACE(03)  TO SALDOACT(I,07)     && MARCA DE FORMA DE PAGO
       STORE   0        TO SALDOACT(I,08)     && MONTO DE DESCUENTO P/SEM. O ANU
       STORE   0        TO SALDOACT(I,09)     && PORCENTAJE P/RECARGO POR PRODUCTO

       STORE SPACE(03)  TO DETAVALO(I,01)     && CODIGO DEL VALOR
       STORE   0        TO DETAVALO(I,02)     && MONTO  DEL VALOR
       STORE CTOD([//]) TO DETAVALO(I,03)     && FECHA DE VENCIMIENTO
       STORE SPACE(16)  TO DETAVALO(I,04)     && NRO. DEL VALOR
       STORE SPACE(02)  TO DETAVALO(I,05)     && CODIGO ENTIDAD EMISORA
       STORE SPACE(02)  TO DETAVALO(I,06)     && CODIGO TARJETA DE CREDITO
       STORE   0        TO DETAVALO(I,07)     && CANTIDAD DE FACTURAS UTILIZADAS

   ENDFOR

**************** TEMPORAL PARA EXONERAR RECARGOS **********

   IF V_FECALT <> CTOD([//])
      STORE V_FECCOB TO V_FECAUX
      STORE V_FECALT TO V_FECCOB
   ENDIF

***************  FIN TEMPORAL  *****************************

   STORE  0            TO V_NROCON
   STORE "   "         TO V_CODPRO
   STORE V_FECCOB + 30 TO V_FECTOP
   SET EXACT OFF
   SET NEAR  ON
   SELECT SALDOS_M
   SET ORDER TO SAL001
   GO TOP
   SEEK (V_CODSUC+TRANSFORM(V_NROCTA,"999999")+[   ]+TRANSFORM(000000,"999999")+DTOS(V_FECCOB))
   IF .NOT. EOF() .AND. SALDOS_TIT = V_NROCTA
      DO WHILE V_NROCTA = SALDOS_TIT
         IF (TRANSFORM(YEAR(SALDOS_VEN),"9999")+TRANSFORM(MONTH(SALDOS_VEN),"99"));
         <= (TRANSFORM(YEAR(V_FECTOP),"9999")  +TRANSFORM(MONTH(V_FECTOP),"99")) .AND.;
            (SALDOS_HAB - SALDOS_DEB) < 0 .AND. SALDOS_EST <> "B"

            STORE (V_CANFIL + 1)    TO V_CANFIL
            IF SALDOS_PRO  <>  V_CODPRO  .OR.  SALDOS_NUM  <>  V_NROCON
               IF V_CANFIL > 1
                  STOR V_CODPRO   TO ESTADCTA(V_CANFIL,02)
                  STOR V_NROCON   TO ESTADCTA(V_CANFIL,03)
                  STOR V_SUBNTO   TO ESTADCTA(V_CANFIL,07)
                  STOR V_SUBRGO   TO ESTADCTA(V_CANFIL,08)
                  STOR V_SUBTAL   TO ESTADCTA(V_CANFIL,09)
                  STOR   0        TO V_SUBNTO, V_SUBRGO, V_SUBTAL
                  V_CANFIL = V_CANFIL + 1
               ENDIF
               V_CANCON = V_CANCON + 1
               STORE SALDOS_PRO     TO ESTADCTA(V_CANFIL,02),;
                V_CODPRO,;
 								       SALDOACT(V_CANCON,01)
               STORE SALDOS_NUM     TO ESTADCTA(V_CANFIL,03),;
					                   V_NROCON			    ,;
 								       SALDOACT(V_CANCON,02)

*********** BUSCA INTERES P/CALCULO DE RECARGO POR MORA ***********
               SELE CONTRA_M
               SET ORDE TO SUCURSAL
               SEEK V_CODSUC+V_CODPRO+TRAN(V_NROCON,"999999")
               IF FOUND()
                  IF CONTRA_INT > 0
                     STORE CONTRA_INT TO SALDOACT(V_CANCON,09)
                  ELSE
                      WAIT "ATENCION !!!, EL " + V_CODPRO + " " + TRANSFORM(V_NROCON,"999999") + " NO TIENE CARGADO INTERES P/MORA" WINDOW
                  ENDIF
               ENDIF
               SELE SALDOS_M

***********          FIN DE BUSQUEDA DE INTERES P/MORA          *********

			ENDIF
            ESTADCTA(V_CANFIL,01)  =  RECNO()
            ESTADCTA(V_CANFIL,02)  =  SALDOS_PRO
            ESTADCTA(V_CANFIL,03)  =  SALDOS_NUM
            ESTADCTA(V_CANFIL,04)  =  SALDOS_NCU
            ESTADCTA(V_CANFIL,05)  =  SALDOS_VEN
            ESTADCTA(V_CANFIL,06)  =  V_FECCOB   - SALDOS_VEN
            ESTADCTA(V_CANFIL,07)  =  SALDOS_DEB - SALDOS_HAB
            ESTADCTA(V_CANFIL,14)  =  SALDOS_DEB
            ESTADCTA(V_CANFIL,16)  =  SALDOS_PDE
            ESTADCTA(V_CANFIL,17)  =  SALDOS_PHA
            V_SUBNTO               =  ESTADCTA(V_CANFIL,07) +  V_SUBNTO

************  CALCULO DE RECARGO DE CUOTAS VENCIDAS  **********

            IF (V_FECCOB - SALDOS_VEN) > 0
               IF SALDOS_INT = SALDOS_DEB * ((SALDOACT(V_CANCON,09)*ESTADCTA(V_CANFIL,06)) / 100)
                  ESTADCTA(V_CANFIL,08) = 0
               ELSE
                  IF V_CODPRO=[UDS] OR V_CODPRO=[PSV] OR V_CODPRO=[SDS] OR V_CODPRO=[SDC] OR V_CODPRO=[PSC] OR V_CODPRO=[PSI]
                     IF SALDOS_NCU # [GA/28] AND SALDOS_NCU # [GA/29] AND SALDOS_NCU # [GA/AD]
                        IF SALDOS_FEP # CTOD('//')
                           V_MESATR = V_FECCOB-SALDOS_FEP
                        ELSE
                           V_MESATR = V_FECCOB-ESTADCTA(V_CANFIL,05)
                        ENDI
                        V_PORREC = V_MESATR*(SALDOACT(V_CANCON,09)/30)
                        ESTADCTA(V_CANFIL,08)=ROUN((SALDOS_DEB - SALDOS_HAB)*(V_PORREC)/100,0)
                     ENDI
                  ELSE
                     IF SALDOS_NCU # [GA/28] AND SALDOS_NCU # [GA/29] AND SALDOS_NCU # [GA/AD]
                        ESTADCTA(V_CANFIL,08) = ROUND(((SALDOS_DEB - SALDOS_HAB) * (SALDOACT(V_CANCON,09) / 100)),0)
                     ENDI
                  ENDI
               ENDIF
               V_SUBRGO              = (V_SUBRGO + ESTADCTA(V_CANFIL,08))
               V_TOTIDO              = (V_TOTIDO + ESTADCTA(V_CANFIL,07))
               V_SUBTAL              = (V_SUBTAL + ESTADCTA(V_CANFIL,07) + ESTADCTA(V_CANFIL,08))
               V_TOTRGO              = (V_TOTRGO + ESTADCTA(V_CANFIL,08))
               SALDOACT(IIF(V_CANCON > 0,V_CANCON,1),03) = ESTADCTA(V_CANFIL,07)
               SALDOACT(IIF(V_CANCON > 0,V_CANCON,1),06) = ESTADCTA(V_CANFIL,07) + ESTADCTA(V_CANFIL,08)
               SALDOACT(IIF(V_CANCON > 0,V_CANCON,1),07) = "***"
            ELSE
               ESTADCTA(V_CANFIL,08) = 0
               V_SUBTAL              = (V_SUBTAL + ESTADCTA(V_CANFIL,07))
               V_TOTCER              = (V_TOTCER + SALDOS_DEB)
            ENDIF

************      FIN DE CALCULO RECARGO     ***********

            IF V_CANCON = 0
               V_CANCON = 1
               STORE SALDOS_PRO TO SALDOACT(V_CANCON,01)
               STORE SALDOS_NUM TO SALDOACT(V_CANCON,02)
            ENDIF

            IF SALDOACT(V_CANCON,03) = 0
               SALDOACT(V_CANCON,03) = ESTADCTA(V_CANFIL,07)
               SALDOACT(V_CANCON,06) = ESTADCTA(V_CANFIL,07) + ESTADCTA(V_CANFIL,08)
            ENDIF

*           SALDOACT(V_CANCON,03)     = SALDOACT(V_CANCON,03) + ESTADCTA(V_CANFIL,07)
            SALDOACT(V_CANCON,04)     = SALDOACT(V_CANCON,04) + (ESTADCTA(V_CANFIL,07) + ESTADCTA(V_CANFIL,08))

            ESTADCTA(V_CANFIL,09)     = (ESTADCTA(V_CANFIL,07) + ESTADCTA(V_CANFIL,08))
            IF ESTADCTA(V_CANFIL,10)  =  0  .AND.  ESTADCTA(IIF(V_CANFIL=1,1,V_CANFIL-1),10) = 0
               ESTADCTA(V_CANFIL,10)  =  ESTADCTA(V_CANFIL,09)
            ELSE
               ESTADCTA(V_CANFIL,10)  =  ESTADCTA(V_CANFIL,09) + ESTADCTA(IIF(V_CANFIL=1,1,V_CANFIL-1),10)
            ENDIF
            ESTADCTA(V_CANFIL,11)     =  SALDOS_HOJ
         ELSE
            IF (TRANSFORM(YEAR(SALDOS_VEN),"9999")+TRANSFORM(MONTH(SALDOS_VEN),"99"));
            >  (TRANSFORM(YEAR(V_FECTOP),"9999")  +TRANSFORM(MONTH(V_FECTOP),"99")) .AND.;
               (SALDOS_HAB - SALDOS_DEB) < 0 .AND. SALDOS_EST <> "B"
               STORE (V_CANFIL + 1)    TO V_CANFIL
               IF SALDOS_PRO  <>  V_CODPRO  .OR.  SALDOS_NUM  <>  V_NROCON
                  IF V_CANFIL > 1
                      STORE V_CODPRO   TO ESTADCTA(V_CANFIL,02)
                      STORE V_NROCON   TO ESTADCTA(V_CANFIL,03)
                      STORE V_SUBNTO   TO ESTADCTA(V_CANFIL,07)
                      STORE V_SUBRGO   TO ESTADCTA(V_CANFIL,08)
                      STORE V_SUBTAL   TO ESTADCTA(V_CANFIL,09)
                      V_SUBNTO = 0
                      V_SUBRGO = 0
                      V_SUBTAL = 0
                      V_CANFIL = V_CANFIL + 1
                  ENDIF
                  V_CANCON = V_CANCON + 1
                  STORE SALDOS_PRO TO ESTADCTA(V_CANFIL,02), V_CODPRO, SALDOACT(V_CANCON,01)
                  STORE SALDOS_NUM TO ESTADCTA(V_CANFIL,03), V_NROCON, SALDOACT(V_CANCON,02)
               ENDIF
               ESTADCTA(V_CANFIL,01)  =  RECNO()
               ESTADCTA(V_CANFIL,02)  =  SALDOS_PRO
               ESTADCTA(V_CANFIL,03)  =  SALDOS_NUM
               ESTADCTA(V_CANFIL,04)  =  SALDOS_NCU
               ESTADCTA(V_CANFIL,05)  =  SALDOS_VEN
               ESTADCTA(V_CANFIL,06)  =  V_FECCOB   - SALDOS_VEN
               ESTADCTA(V_CANFIL,07)  =  SALDOS_DEB - SALDOS_HAB
               ESTADCTA(V_CANFIL,14)  =  SALDOS_DEB
               ESTADCTA(V_CANFIL,16)  =  SALDOS_PDE
               ESTADCTA(V_CANFIL,17)  =  SALDOS_PHA
               V_TOTCER               = (V_TOTCER + SALDOS_DEB)

               IF V_CANCON = 0
                  V_CANCON = 1
                  STORE SALDOS_PRO TO SALDOACT(V_CANCON,01)
                  STORE SALDOS_NUM TO SALDOACT(V_CANCON,02)
               ENDI

               IF SALDOACT(V_CANCON,03) = 0
                  SALDOACT(V_CANCON,03) = ESTADCTA(V_CANFIL,07)
                  SALDOACT(V_CANCON,06) = ESTADCTA(V_CANFIL,07) + ESTADCTA(V_CANFIL,08)
               ENDI
*              SALDOACT(V_CANCON,03)     = SALDOACT(V_CANCON,03) + ESTADCTA(V_CANFIL,07)
               SALDOACT(V_CANCON,04)     = SALDOACT(V_CANCON,04) + (ESTADCTA(V_CANFIL,07) + ESTADCTA(V_CANFIL,08))
               ESTADCTA(V_CANFIL,09)     = (ESTADCTA(V_CANFIL,07) + ESTADCTA(V_CANFIL,08))
               IF ESTADCTA(V_CANFIL,10)  =  0  .AND.  ESTADCTA(IIF(V_CANFIL=1,1,V_CANFIL-1),10) = 0
                  ESTADCTA(V_CANFIL,10)  =  ESTADCTA(V_CANFIL,09)
               ELSE
                  ESTADCTA(V_CANFIL,10)  =  ESTADCTA(V_CANFIL,09) + ESTADCTA(IIF(V_CANFIL=1,1,V_CANFIL-1),10)
               ENDI
               ESTADCTA(V_CANFIL,11)     =  SALDOS_HOJ
            ENDI
         ENDI
         SKIP
      ENDD
  &&  SET EXACT ON
      SET NEAR OFF
      V_CANFIL = V_CANFIL + 1
      ESTADCTA(V_CANFIL,07)    = V_SUBNTO
      ESTADCTA(V_CANFIL,02)    = V_CODPRO
      ESTADCTA(V_CANFIL,03)    = V_NROCON
      ESTADCTA(V_CANFIL,08)    = V_SUBRGO
      ESTADCTA(V_CANFIL,09)    = V_SUBTAL
      V_TOTRAL 				   = V_TOTIDO + V_TOTCER
      IF V_TOTRAL > 0
         DO IMPESTCTA
      ELSE
         WAIT "NO TIENE CUOTAS VENCIDAS NI A VENCER, FAVOR VERIFIQUE" WINDOW
      ENDI
   ELSE
      WAIT "NO EXISTE FICHA DE NINGUN CONTRATO PARA LA CUENTA " + TRANSFORM(V_NROCTA,[999999]) + " - " + TRIM(V_NOMTIT)+", VERIFIQUE !!!" WINDOW
      DO LIMPIAVAR
   ENDI
   RETU

*  DESPLIEGA ESTADO DE CTA. DEL CLIENTE
   PROCEDURE DESPESTA
      DO TECLA_PAGO
      SAVE SCRE TO PANTA2
      ACTI WIND ESTADCTA
      CLEAR
      @00,00 SAY "CUENTA.: " + TRANSFORM(V_NROCTA,"999999")
      @00,60 SAY "FECHA: "   + DTOC(V_FECCOB)
      @01,00 SAY "TITULAR: " + ALLTRIM(V_NOMTIT)
      @02,00 SAY REPLICATE("O",78)
      @03,00 SAY " CONTRATO   CUOTA   VENCE   ATRASO  MONTO  RECARGO    TOTAL     ACUMUL.  TALON"
      @04,00 SAY REPLICATE("O",78)
      @16,00 SAY REPLICATE("O",78)
      @17,00 SAY SPACE(16)+"TOTAL VENCIDO.:  " + TRANSFORM(V_TOTIDO,[99,999,999]) + SPACE(01) + TRANSFORM(V_TOTRGO,[999,999]) + SPACE(01)+TRANSFORM(V_TOTIDO+V_TOTRGO,[9,999,999])
      @18,00 SAY SPACE(16)+"TOTAL A VENCER:  " + TRANSFORM(V_TOTCER,[99,999,999]) + SPACE(09) + TRANSFORM(V_TOTCER,[9,999,999])
      @19,00 SAY SPACE(16)+"TOTAL GENERAL.:  " + TRANSFORM(V_TOTRAL,[99,999,999]) + SPACE(01) + TRANSFORM(V_TOTRGO,[999,999]) + SPACE(01)+TRANSFORM(V_TOTRAL+V_TOTRGO,[9,999,999])
      @20,00 SAY REPLICATE("O",78)
      SET COLO TO W+/B
      @21,03 SAY " <F4>COBRAR  <F5>ACT. COBROS  <F6>RECLAMOS  <F7>MENSAJES  <ESC>CANCELAR "
      SET COLO TO I
      MODI COMM M:\-DATOS\SICMADAT\CCE\&V_ARCAUX NOEDIT WINDOW INTERIOR
      DELE FILE M:\-DATOS\SICMADAT\CCE\&V_ARCAUX
      DEAC WIND ESTADCTA
      REST SCRE FROM PANTA2
   RETU

* IMPRIME ESTADO DE CUENTA:
  PROCEDURE IMPESTCTA
  V_ARCAUX = LEFT(USUARIOS,2) + SUBSTR(TIME(),1,2) + SUBSTR(TIME(),4,2) + SUBSTR(TIME(),7,2) + ".TXT"
  SET PRIN TO M:\-DATOS\SICMADAT\CCE\&V_ARCAUX
  SET DEVI TO PRIN
  FOR I = 1 TO V_CANFIL
      IF (TRANSFORM(YEAR(ESTADCTA(I,05)),"9999") + TRANSFORM(MONTH(ESTADCTA(I,05)),"99")) <=  ;
         (TRANSFORM(YEAR(V_FECTOP),"9999")       + TRANSFORM(MONTH(V_FECTOP),"99"))      .OR. ;
         ESTADCTA(I,04) = SPACE(05)
         IF I > 1
            IF ESTADCTA(I,02) <>  ESTADCTA(I-1,02) .OR.;
               ESTADCTA(I,03) <>  ESTADCTA(I-1,03)
               @PROW()+1,00   SAY IIF(V_FECCOB - ESTADCTA(I,05) > 0,""," ")
               @PROW(),PCOL() SAY ESTADCTA(I,02)
               @PROW(),PCOL() SAY TRANSFORM(ESTADCTA(I,03),[999999])
            ELSE
               @PROW()+1,00   SAY IIF(V_FECCOB - ESTADCTA(I,05) > 0,""," ")
               @PROW(),PCOL() SAY SPACE(03)
               @PROW(),PCOL() SAY SPACE(06)
            ENDIF
         ELSE
            @PROW()+1,00   SAY IIF(V_FECCOB - ESTADCTA(I,05) > 0,""," ")
            @PROW(),PCOL() SAY ESTADCTA(I,02)
            @PROW(),PCOL() SAY TRANSFORM(ESTADCTA(I,03),[999999])
         ENDI
         @PROW(),PCOL()+1 SAY ESTADCTA(I,04)
         @PROW(),PCOL()+1 SAY IIF(ESTADCTA(I,05)<>CTOD([//]),DTOC(ESTADCTA(I,05)),SPACE(010))
         @PROW(),PCOL()+1 SAY IIF(ESTADCTA(I,06)>0,TRANSFORM(ESTADCTA(I,06),[9999]),SPACE(04))
         @PROW(),PCOL()+1 SAY IIF(ESTADCTA(I,07)>0,TRANSFORM(ESTADCTA(I,07),[9,999,999]),SPACE(09))
         @PROW(),PCOL()+1 SAY IIF(ESTADCTA(I,08)>0,TRANSFORM(ESTADCTA(I,08),[999,999]),SPACE(07))
         @PROW(),PCOL()+1 SAY IIF(ESTADCTA(I,09)>0,TRANSFORM(ESTADCTA(I,09),[9,999,999]),SPACE(09))
         @PROW(),PCOL()+1 SAY IIF(ESTADCTA(I,10)>0,TRANSFORM(ESTADCTA(I,10),[9,999,999]),SPACE(09))
         @PROW(),PCOL()+1 SAY IIF(ESTADCTA(I,11)>0,TRANSFORM(ESTADCTA(I,11),[999999]),SPACE(06))
      ENDI
  ENDF
  SET PRIN OFF
  SET CONS ON
  SET DEVI TO SCRE
  SET PRIN TO PRN
  RETU

*  PIDE PAGOS POR CONTRATO
   PROCEDURE PIDEPAGO
      ON KEY
      V_MENSAJE = []
      SAVE SCREEN TO PANTA3
      CLEAR
      ACTIVATE WINDOW PIDEPAGO
      DO WHILE .T.
         CLEAR
         @00,00 SAY "CUENTA.: " + TRANSFORM(V_NROCTA,[999999])
         @00,55 SAY "FECHA.....: " + DTOC(V_FECCOB)
         @01,00 SAY "TITULAR: " + ALLTRIM(V_NOMTIT)
         @01,55 SAY "FACTURA NRO: " + TRANSFORM(V_NROFAC,[999999])
         @02,00 SAY REPLICATE("O",77)
         @03,00 SAY "CONTRATO NRO  MONTO CUOTA  CON RECARGO  SALDO ACTUAL  FORMA PAGO  MONTO A COBRAR "
         @04,00 SAY REPLICATE("O",77)
         V_TOTCOBRAR = 0
         V_BRUCOBRAR = 0
         V_NETCOBRAR = 0
         V_TOTDESCUE = 0
         FOR I = 1 TO V_CANCON
             SALDOACT(I,08) = 0
             V_FORPAG       = "MENSUAL  "
             V_BANDERA      = .F.
             @ROW()+1,00     SAY SALDOACT(I,01)
             @ROW(),COL()+1  SAY TRANSFORM(SALDOACT(I,02),[999999])
             @ROW(),COL()+5  SAY TRANSFORM(SALDOACT(I,03),[9,999,999])
             @ROW(),COL()+4  SAY TRANSFORM(SALDOACT(I,06),[9,999,999])
             @ROW(),COL()+5  SAY TRANSFORM(SALDOACT(I,04),[9,999,999])
             DO WHILE V_BANDERA = .F. .AND. LASTKEY() <> 27
                @ROW(),COL()+2  GET V_FORPAG PICTURE[@M MENSUAL,SEMESTRAL,ANUAL] VALID VALFORPAG(V_FORPAG) ERROR V_MENSAJE
                READ
             ENDDO
             DO CASE
                CASE SALDOACT(I,07) = "MEN"
                   V_MONCOB  = IIF(SALDOACT(I,06)>0,SALDOACT(I,06),SALDOACT(I,04))
                CASE SALDOACT(I,07) = "SEM"
                   V_MONCOB  = SALDOACT(I,03) * 6
                   SALDOACT(I,08) = SALDOACT(I,03) * (-1)
                CASE SALDOACT(I,07) = "ANU"
                   V_MONCOB  = SALDOACT(I,03) * 12
                   SALDOACT(I,08) = SALDOACT(I,03) * (-2)
             ENDCASE
             V_BANDERA = .F.
             V_TOTDESCUE = V_TOTDESCUE + SALDOACT(I,08)
             DO WHILE V_BANDERA = .F. .AND. LASTKEY() <> 27
                @ROW(),COL()+5  GET V_MONCOB PICTURE[99,999,999] VALID VERIPAGO(V_MONCOB) ERROR V_MENSAJE
                READ
             ENDDO
             IF LASTKEY() = 27
                EXIT
             ENDIF
             IF SALDOACT(I,05) > 0
                 V_BRUCOBRAR = V_BRUCOBRAR + SALDOACT(I,05)
             ENDIF
         ENDFOR
         V_NETCOBRAR = V_BRUCOBRAR + V_TOTDESCUE
         SET COLOR TO B+/W
         @ROW()+1,64 SAY "============="
         @ROW()+1,37 SAY "MONTO BRUTO A COBRAR ------>> " + TRANSFORM(V_BRUCOBRAR,[99,999,999])
         @ROW()+1,37 SAY "DESCUENTO            ------>> " + TRANSFORM(V_TOTDESCUE,[99,999,999])
         @ROW()+1,64 SAY "============="
         @ROW()+1,37 SAY "MONTO NETO A COBRAR  ------>> " + TRANSFORM(V_NETCOBRAR,[99,999,999])
         V_OPCION = 1
         SET COLOR TO N+/W,W/N
         @20,05 PROMP "\<ACEPTAR PAGO"
         @20,30 PROMP "\<CORREGIR    "
         @20,50 PROMP "CA\<NCELAR    "
         MENU TO V_OPCION
         SET COLOR TO I
         DO CASE
            CASE V_OPCION = 1
               DO ACEPTAPAGO
               IF V_NETCOBRAR = 0
                  EXIT
               ENDI
            CASE V_OPCION = 2
               LOOP
            CASE V_OPCION = 3
               EXIT
         ENDC
      ENDD
      DEACTIVATE WINDOW PIDEPAGO
      RESTORE SCREEN FROM PANTA3
      DO TECLA_PAGO
   RETURN

*  CARGA DETALLES DE VALORES DE COBRO
   PROCEDURE ACEPTAPAGO
   SAVE SCREEN TO PANTA4
   ACTIVATE WINDOW DETACOBRO
   V_CANVAL = 1
   V_VUELTO = 0
   V_AUXNETCOB = V_NETCOBRAR
   V_TIPVALOR  = SPACE(15)
   DO WHILE .T. .AND. LASTKEY() <> 27 .AND. V_NETCOBRAR > 0
      V_VUELTO = 0
      DETAVALO(V_CANVAL,02) = V_NETCOBRAR
      CLEAR
      @00,00       SAY "MONTO A COBRAR......: " + TRANSFORM(V_NETCOBRAR,[99,999,999])
      @ROW()+2,00  SAY "TIPO DE VALOR.......: "
      @ROW(),COL() GET V_TIPVALOR PICTURE[@M EFECTIVO,CHEQUE,TARJETA] VALID VALTIPVAL(V_TIPVALOR)
      @ROW()+1,00  SAY "MONTO VALOR.........: "
      @ROW(),COL() GET DETAVALO(V_CANVAL,02) PICTURE[99,999,999] VALID VALMONVAL(DETAVALO(V_CANVAL,02));
                                      ERROR V_MENSAJE
      READ
      IF DETAVALO(V_CANVAL,01) <> "EFE" .AND. LASTKEY() <> 27
         STORE DATE()  TO DETAVALO(V_CANVAL,03)
         @ROW()+1,00  SAY "FECHA VENCIMIENTO...: "
         @ROW(),COL() GET DETAVALO(V_CANVAL,03) VALID !EMPTY(DETAVALO(V_CANVAL,03)) ERROR [DEBE CARGAR DE VENCIMIENTO DEL CHEQUE O TARJETA]
         @ROW()+1,00  SAY "NRO DEL VALOR........: "
         @ROW(),COL() GET DETAVALO(V_CANVAL,04) PICTURE[@!] VALID !EMPTY(DETAVALO(V_CANVAL,04)) ERROR [DEBE CARGAR EL NRO DEL CHEQUE O TARJETA]
         IF DETAVALO(V_CANVAL,01) = "CHE"
            @ROW()+1,00  SAY "ENTIDAD EMISORA.....: "
            @ROW(),COL() GET DETAVALO(V_CANVAL,05) VALID VALBAN(DETAVALO(V_CANVAL,05))
            READ
         ENDIF
         IF DETAVALO(V_CANVAL,01) = "TAR" .AND. LASTKEY() <> 27
            @ROW()+1,00  SAY "TARJETA DE CREDITO..: "
            @ROW(),COL() GET DETAVALO(V_CANVAL,06) VALID VALTAR(DETAVALO(V_CANVAL,06))
            READ
         ENDIF
      ENDIF
      V_OPCION2 = 1
      @08,15       PROMP "\<ACEPTAR "
      @08,COL()+05 PROMP "\<CORREGIR"
      @08,COL()+05 PROMP "CA\<NCELAR"
      MENU TO V_OPCION2
      DO CASE
         CASE V_OPCION2 = 1
              V_NETCOBRAR = V_NETCOBRAR - (DETAVALO(V_CANVAL,02) + V_VUELTO)
              IF V_NETCOBRAR > 0
                 V_CANVAL = V_CANVAL + 1
              ENDIF
              LOOP
  		 CASE V_OPCION2 = 2
		      LOOP
		 CASE V_OPCION2 = 3
		      V_NETCOBRAR = V_AUXNETCOB
		      EXIT
	  ENDCASE
   ENDDO

********** SI PAGO EN CHEQUE Y TIENE VUELTO **********
   IF V_VUELTO < 0
      V_CANVAL              = V_CANVAL + 1
      DETAVALO(V_CANVAL,01) = "EFE"
      DETAVALO(V_CANVAL,02) = V_VUELTO
      DETAVALO(V_CANVAL,04) = "VUELTO"
   ENDIF

**********     FIN VUELTO DE CHEQUE     **********

   IF V_NETCOBRAR = 0
      V_OPCION2 = 1
      @08,10       SAY SPACE(65)
      @08,15       PROMP "\<GRABAR"
      @08,COL()+10 PROMP "CA\<NCELAR"
      MENU TO V_OPCION2
      DO CASE
         CASE V_OPCION2 = 1
			  *DO IMPRIFACTU
			  DO ACTUAFICHA
              DO ACTUACAJA
              DO ACTUAVALOR
    	      DO ACTUATALON
    	      DO IMPRIFACTU
         CASE V_OPCION2 = 2
              V_NETCOBRAR = V_AUXNETCOB
      ENDCASE
   ENDIF
   DEACTIVATE WINDOW DETACOBRO
   RESTORE SCREEN FROM PANTA4
   RETURN

* ACTUALIZA MATRIZ DE FICHA CON COBROS
  PROCEDURE ACTUAFICHA
	 **********************
	 * MOVIMIENTO DE CAJA *
	 **********************
     V_LINFAC  = 1
     V_CANFAC  = 1
     V_PRIMERO = ""
     V_PAGCON  = 0
     FOR I = 1 TO V_CANCON
        IF SALDOACT(I,05) > 0
           V_PAGCON = SALDOACT(I,05)
           FOR J = 1 TO V_CANFIL
              IF ESTADCTA(J,02) =  SALDOACT(I,01) .AND.;
                 ESTADCTA(J,03) =  SALDOACT(I,02) .AND.;
                 ESTADCTA(J,04) <> SPACE(05)      .AND.;
                 SALDOACT(I,05)  > 0

                 IF SALDOACT(I,05) >= (ESTADCTA(J,07) + ESTADCTA(J,08))
                    ESTADCTA(J,12) =  ESTADCTA(J,08)
                    ESTADCTA(J,13) =  ESTADCTA(J,07)
                    SALDOACT(I,05) =  SALDOACT(I,05) - ESTADCTA(J,09)
                 ELSE
                    IF SALDOACT(I,05) >= ESTADCTA(J,08)
                       ESTADCTA(J,12) =  ESTADCTA(J,08)
                       SALDOACT(I,05) =  SALDOACT(I,05) - ESTADCTA(J,08)
                       ESTADCTA(J,13) =  SALDOACT(I,05)
                       SALDOACT(I,05) =  SALDOACT(I,05) - ESTADCTA(J,13)
                    ELSE
                       ESTADCTA(J,12) =  SALDOACT(I,05)
                       SALDOACT(I,05) =  SALDOACT(I,05) - ESTADCTA(J,12)
                    ENDIF
                 ENDIF
                    ESTADCTA(J,15) = V_NROFAC
                    IF SALDOACT(I,7) = "MEN"
                       IF V_LINFAC >= V_CALIFA
                       DO ACTUAVALOR
*                      DO VALIFACT
                       V_NROFAC = V_NROFAC + 1
                       V_CANFAC = V_CANFAC + 1
                       V_LINFAC = 0
                    ELSE
                       V_LINFAC = V_LINFAC + 1
                       V_PRIMERO = "T"
                    ENDIF
                 ELSE
                    IF V_PRIMERO = "T"
                       IF V_LINFAC >= V_CALIFA
                          DO ACTUAVALOR
                          V_NROFAC = V_NROFAC + 1
                          V_CANFAC = V_CANFAC + 1
                          V_LINFAC = 0
                       ELSE
                          V_LINFAC = V_LINFAC + 1
                          V_PRIMERO = "F"
                       ENDIF
                    ENDIF
                 ENDIF
	          ENDIF
 	       ENDFOR
        ENDIF
        SALDOACT(I,05) = V_PAGCON
	 ENDFOR
  RETURN

* ACTUALIZA MOVIMIENTO DE CAJA
  PROCEDURE ACTUACAJA
     V_PRIVEZ = "SI"
     FOR H = 1 TO V_CANCON

************     ACTUALIZA AUXILIAR DE COBRANZA     **********
        DO ACTUAUXCOB
************	     FIN DE ACTUALIZACION           **********

        IF SALDOACT(H,05) > 0
           FOR J = 1 TO V_CANFIL
              IF (ESTADCTA(J,12) >  0 .OR. ESTADCTA(J,13) >  0) .AND. ;
                 SALDOACT(H,01) = ESTADCTA(J,02)                .AND. ;
                 SALDOACT(H,02) = ESTADCTA(J,03)

                 SELEC MOVCAJ_M
                 SCATTER MEMVAR BLANK
                 GO BOTTOM
                 SELE SUCURS_M
                 SEEK V_CODSUC
                 M.MOVCAJ_NUM = SUCURS_ULM + 1
                 REPL SUCURS_ULM WITH SUCURS_ULM+1
                 M.MOVCAJ_SUC = V_CODSUC
                 SELE MOVCAJ_M

*************** TEMPORAL PARA EXONERAR RECARGOS *********

                 IF V_FECALT <> CTOD([//])
                    M.MOVCAJ_FEC = V_FECAUX
                 ELSE
                    M.MOVCAJ_FEC = V_FECCOB
                 ENDIF

*************** FIN TEMPORAL ***************

                 M.MOVCAJ_COB = V_CODCAJ
                 M.MOVCAJ_PNA = USUARPNA
                 M.MOVCAJ_PER = USUARPER
                 M.MOVCAJ_CLI = V_NROCTA
                 M.MOVCAJ_FAC = ESTADCTA(J,15)
                 M.MOVCAJ_TIP = V_TIPFAC
                 M.MOVCAJ_SER = V_SERFAC
                 M.MOVCAJ_PRO = ESTADCTA(J,02)
                 M.MOVCAJ_CON = ESTADCTA(J,03)
                 M.MOVCAJ_CUO = ESTADCTA(J,04)
                 M.MOVCAJ_VEN = ESTADCTA(J,05)
                 M.MOVCAJ_MON = ESTADCTA(J,13)
                 M.MOVCAJ_INT = ESTADCTA(J,12)
                 M.MOVCAJ_TAL = ESTADCTA(J,11)
                 M.MOVCAJ_DAT = DATE()
                 M.MOVCAJ_TIM = TIME()
                 M.MOVCAJ_TRE = [1]
                 APPEND BLANK
                 GATHER MEMVAR
                 IF ESTADCTA(J,01) > 1
                    DO ACTUASALDO WITH J
                 ENDIF
              ENDIF
           ENDFOR

*********** ACTUALIZA DESCUENTO P/PAGO SEMES. O ANUAL EN MOVIM. DE CAJA *********

           IF SALDOACT(H,08) < 0
              DO CASE
                 CASE UPPE(LEFT(V_FORPAG,3))=[SEM]
                    SELE SALDOS_M
                    REPL SALDOS_OBH WITH [DESC.PAGO SEMESTRAL]
                    REPL SALDOS_ADS WITH 1
                 CASE UPPE(LEFT(V_FORPAG,3))=[ANU]
                    SELE SALDOS_M
                    SKIP -1
                    REPL SALDOS_OBH WITH [DESC.PAGO ANUAL]
                    REPL SALDOS_ADS WITH 2
                    SKIP
                    REPL SALDOS_OBH WITH [DESC.PAGO ANUAL]
                    REPL SALDOS_ADS WITH 2
              ENDC
              SELE MOVCAJ_M
              GO BOTT
              SCAT MEMV BLAN
              IF V_FECALT <> CTOD([//])
                 M.MOVCAJ_FEC = V_FECAUX
              ELSE
                 M.MOVCAJ_FEC = V_FECCOB
              ENDIF
              M.MOVCAJ_SUC = V_CODSUC
              M.MOVCAJ_NUM = MOVCAJ_NUM + 1
              M.MOVCAJ_COB = V_CODCAJ
              M.MOVCAJ_PNA = USUARPNA
              M.MOVCAJ_PER = USUARPER
              M.MOVCAJ_CLI = V_NROCTA
              M.MOVCAJ_FAC = V_NROFAC
              M.MOVCAJ_TIP = V_TIPFAC
              M.MOVCAJ_SER = V_SERFAC
              M.MOVCAJ_PRO = SALDOACT(H,01)
              M.MOVCAJ_CON = SALDOACT(H,02)
              M.MOVCAJ_CUO = [DESCU]
              M.MOVCAJ_MON = SALDOACT(H,08)
              M.MOVCAJ_DAT = DATE()
              M.MOVCAJ_TIM = TIME()
              M.MOVCAJ_TRE = [1]
              APPE BLAN
              GATH MEMV
           ENDIF

***********       FIN ACTUALIZACION DE DESCUENTO      *********

        ENDIF
     ENDFOR
    DO ACTUATALON
   RETURN

* ACTUALIZA ARCHIVO DE SALDO DE CLIENTES
  PROCEDURE ACTUASALDO
  PARAMETERS J
     SELEC SALDOS_M
     GO ESTADCTA(J,01)
     SCATTER MEMVAR
     IF M.SALDOS_NCU = ESTADCTA(J,04)  .AND.;
        M.SALDOS_TIT = V_NROCTA        .AND.;
        M.SALDOS_PRO = ESTADCTA(J,02)  .AND.;
        M.SALDOS_NUM = ESTADCTA(J,03)  .AND.;
        M.SALDOS_VEN = ESTADCTA(J,05)
        M.SALDOS_SUC  =  V_CODSUC
        M.SALDOS_MOV  =  MOVCAJ_M.MOVCAJ_NUM
        M.SALDOS_CAJ  =  V_CODCAJ
        M.SALDOS_INT  =  (M.SALDOS_INT + ESTADCTA(J,12))
        M.SALDOS_FAC  =  ESTADCTA(J,15)
        M.SALDOS_TIP  =  V_TIPFAC
        M.SALDOS_SER  =  V_SERFAC
*********** TEMPORAL PARA EXONERAR RECARGOS **********
        IF V_FECALT <> CTOD([//])
           M.SALDOS_FEP  =  V_FECAUX
        ELSE
           M.SALDOS_FEP  =  V_FECCOB
        ENDIF
***********   FIN TEMPORAL **************
        M.SALDOS_HAB  =  (M.SALDOS_HAB + ESTADCTA(J,13))
        M.SALDOS_CDC  =  V_CODCAJ
        M.SALDOS_PNA  =  USUARPNA
        M.SALDOS_PER  =  USUARPER
        M.SALDOS_DAT  =  DATE()
        M.SALDOS_TIM  =  TIME()
        GATHER MEMVAR
     ENDIF
   RETURN

* ACTUALIZA DETALLE VALORES DE CAJA
  PROCEDURE ACTUAVALOR
     SELEC VALCAJ_D
     GO BOTTOM
     FOR K = 1 TO V_CANVAL
         SCATTER MEMVAR BLANK
         M.VALCAJ_SUC = V_CODSUC
         M.VALCAJ_FAC = V_NROFAC
         M.VALCAJ_TIP = V_TIPFAC
         M.VALCAJ_SER = V_SERFAC
         M.VALCAJ_TVA = DETAVALO(K,01)
         M.VALCAJ_IMP = DETAVALO(K,02)
         M.VALCAJ_EEM = DETAVALO(K,05)
         M.VALCAJ_TAR = DETAVALO(K,06)
         M.VALCAJ_NRO = DETAVALO(K,04)
         IF DETAVALO(K,01) = "CHE" .OR. DETAVALO(K,01) = "TAR"
            M.VALCAJ_VEN = DETAVALO(K,03)
         ELSE
            M.VALCAJ_VEN = V_FECCOB
         ENDIF
         IF DETAVALO(K,07) > 1
            M.VALCAJ_DUP ="S"
         ENDIF
         M.VALCAJ_PNA = USUARPNA
         M.VALCAJ_PER = USUARPER
         M.VALCAJ_DAT = DATE()
         M.VALCAJ_TIM = TIME()
         M.VALCAJ_TRE = [1]

*********** TEMPORAL PARA EXONERAR RECARGOS **********

         IF V_FECALT <> CTOD([//])
            M.VALCAJ_FEC = V_FECAUX
         ELSE
            M.VALCAJ_FEC = V_FECCOB
         ENDIF

***********   FIN TEMPORAL **************

         M.VALCAJ_NCD = ALLTRIM(V_CTABAN)

         APPEND BLANK
         GATHER MEMVAR
     ENDFOR
   RETURN

* ACTUALIZA NRO FACTURA EN ARCHIVO DE TALONARIOS
  PROCEDURE ACTUATALON
     SELEC TALONA_M

     DO CASE
        CASE V_NROFAC = TALONA_DES
             REPLACE TALONA_EST WITH [EN USO]
        CASE V_NROFAC = TALONA_HAS
             REPLACE TALONA_EST WITH [UTILIZADO]
     ENDCASE

     REPLACE TALONA_ULT WITH V_NROFAC

********** TEMPORAL PARA EXONERACION DE RECARGO **********

     IF V_FECALT <> CTOD([//])
        REPLACE TALONA_FUM WITH V_FECAUX
     ELSE
        REPLACE TALONA_FUM WITH V_FECCOB
     ENDIF

**********       FIN TEMPORAL         **********

   RETURN

*  ACTUALIZA ARCHIVO AUXILIAR DE COBRANZA
   PROCEDURE ACTUAUXCOB
      SELEC AUXCOB_D
      GO TOP

*********** TEMPORAL PARA EXONERAR RECARGOS **********

            IF V_FECALT <> CTOD([//])
                SEEK (DTOS(V_FECAUX)+V_CODCAJ+SALDOACT(H,01))
            ELSE
                SEEK (DTOS(V_FECCOB)+V_CODCAJ+SALDOACT(H,01))
            ENDIF

***********   FIN TEMPORAL **************

      IF FOUND()
         SCATTER MEMVAR
         M.AUXCOB_MOP = M.AUXCOB_MOP + SALDOACT(H,05) + SALDOACT(H,08)
         IF V_PRIVEZ = "SI"
             M.AUXCOB_CAP = M.AUXCOB_CAP + 1
             V_PRIVEZ = "NO"
         ENDIF
      ELSE
         GO BOTTOM
         SCATTER MEMVAR BLANK
         M.AUXCOB_COB = V_CODCAJ
         M.AUXCOB_CEN = SALDOACT(H,01)
         M.AUXCOB_CAP = 1
         M.AUXCOB_MOP = SALDOACT(H,05) + SALDOACT(H,08)
         M.AUXCOB_CAA = 0
         M.AUXCOB_MOA = 0
         M.AUXCOB_COD = "C"
*********** TEMPORAL PARA EXONERAR RECARGOS **********

            IF V_FECALT <> CTOD([//])
                M.AUXCOB_FEC = V_FECAUX
            ELSE
                M.AUXCOB_FEC = V_FECCOB
            ENDIF

***********   FIN TEMPORAL **************

         APPEND BLANK
         V_PRIVEZ = "NO"
      ENDIF
      GATHER MEMVAR
      SELEC MOVCAJ_M
   RETURN

*  VERIFICA CARGA DE COBRO
   PROCEDURE VERIPAGO
   PARAMETERS V_MONCOB
      DO CASE
         CASE SALDOACT(I,07) = "MEN"
              IF V_MONCOB > SALDOACT(I,04) .AND. V_MONCOB > SALDOACT(I,05)
                 V_MONCOB = SALDOACT(I,04)
                 V_MENSAJE = [MONTO A COBRAR NO PUEDE SER MAYOR AL VALOR TOTAL DE CUOTAS PENDIENTES]
                 RETURN .F.
              ENDIF
         CASE SALDOACT(I,07) = "SEM"
              IF V_MONCOB > (SALDOACT(I,03) * 6) .OR. V_MONCOB < (SALDOACT(I,03) * 6)
                 V_MONCOB = SALDOACT(I,03) * 6
                 V_MENSAJE = [MONTO A COBRAR NO PUEDE SER DIFERENTE AL VALOR DE 6 CUOTAS PENDIENTES]
                 RETURN .F.
              ENDIF
         CASE SALDOACT(I,07) = "ANU"
              IF V_MONCOB > (SALDOACT(I,03) * 12) .OR. V_MONCOB < (SALDOACT(I,03) * 12)
                 V_MONCOB = SALDOACT(I,03) * 12
                 V_MENSAJE = [MONTO A COBRAR NO PUEDE SER DIFERENTE AL VALOR DE 12 CUOTAS PENDIENTES]
                 RETURN .F.
              ENDIF
         ENDCASE
         SALDOACT(I,05) = V_MONCOB
         V_BANDERA = .T.
   RETURN .T.

*  VALIDA TIPO DE VALOR
   PROCEDURE VALTIPVAL
   PARAMETERS V_TIPOVALOR
      DETAVALO(V_CANVAL,01) = (UPPER(LEFT(V_TIPOVALOR,3)))
   RETURN

*  VALIDA FORMA DE PAGO
   PROCEDURE VALFORPAG
   PARAMETERS V_FORPAG
      IF V_FORPAG <> "MENSUAL  " .AND. SALDOACT(I,07) = "***"
         V_MENSAJE = "ATENCION!!!, CONTRATO CON MORA, IMPOSIBLE COBRAR SEMESTRAL O ANUAL"
         RETURN .F.
      ELSE
        IF V_FORPAG <> "MENSUAL  "
           IF ESTADCTA(I,02) = "UDS" .OR. ESTADCTA(I,02) = "PSV"
               V_MENSAJE = "ATENCION !!!, NO SE PUEDE REALIZAR PAGO SEMESTRAL O ANUAL A LOS 'PSV' Y 'UDS'"
               RETURN .F.
           ENDIF
           IF V_FORPAG = "SEMESTRAL" .AND. ((SALDOACT(I,03)*6) >  SALDOACT(I,04))
               V_MENSAJE = "ATENCION !!!, CONTRATO NO TIENE 6 CUOTAS PENDIENTES P/PAGO SEMESTRAL"
              RETURN .F.
           ENDIF
           IF V_FORPAG = "ANUAL    " .AND. ((SALDOACT(I,03)*12) >  SALDOACT(I,04))
              V_MENSAJE = "ATENCION !!!, CONTRATO NO TIENE 12 CUOTAS PENDIENTES P/PAGO ANUAL"
              RETURN .F.
           ENDIF
         ENDIF
      ENDIF
      SALDOACT(I,07) = (UPPER(LEFT(V_FORPAG,3)))
      V_BANDERA = .T.
   RETURN

*  VALIDA MONTO DE VALOR
   PROCEDURE VALMONVAL
   PARAMETERS V_VALORMONTO
      DO CASE
         CASE DETAVALO(V_CANVAL,01) = "EFE"
            IF DETAVALO(V_CANVAL,02) <= 0 .OR. DETAVALO(V_CANVAL,02) > V_NETCOBRAR
               V_MENSAJE = [DEBE CARGAR MONTO A COBRAR O <ESC> PARA CANCELAR]
               RETURN .F.
            ENDIF
         CASE DETAVALO(V_CANVAL,01) = "CHE"
            IF DETAVALO(V_CANVAL,02) > V_NETCOBRAR
               V_VUELTO = V_NETCOBRAR - DETAVALO(V_CANVAL,02)
            ENDIF
         CASE DETAVALO(V_CANVAL,01) = "TAR"
            IF DETAVALO(V_CANVAL,02) <= 0 .OR. DETAVALO(V_CANVAL,02) > V_NETCOBRAR
               V_MENSAJE = [DEBE CARGAR MONTO A COBRAR O <ESC> PARA CANCELAR]
               RETURN .F.
			ENDIF
      ENDCASE
   RETURN

*  VALIDA SALDO DE CONTRATO
   PROCEDURE VALISALCON
      V_TOTDEB = 0
      V_TOTCRE = 0
      V_FECCAN = CTOD("  /  /  ")
      SET EXACT OFF
      SELECT SALDOS_M
      SET ORDER TO SAL005
      GO TOP
      SEEK V_CODPRO + TRANSFORM(V_NROCON,[999999])
      IF FOUND()
         DO WHILE SALDOS_PRO = V_CODPRO .AND. SALDOS_NUM = V_NROCON
            STORE V_TOTDEB + SALDOS_DEB TO V_TOTDEB
            STORE V_TOTCRE + SALDOS_HAB TO V_TOTCRE
            STORE SALDOS_FEP            TO V_FECCAN
            IF EOF()
               EXIT
            ENDIF
            SKIP
         ENDDO
      ENDIF
      IF V_TOTDEB - V_TOTCRE = 0
         WAIT "EL " + V_CODPRO + " " + TRANSFORM(V_NROCON,[999999]) + " FUE CANCELADO EL " + DTOC(V_FECCAN) WINDOW
      ENDIF
   RETURN

*  TECLAS PARA CARGA DE PAGOS
   PROCEDURE TECLA_PAGO
      ON KEY
      ON KEY LABEL F4  DO PIDEPAGO
      ON KEY LABEL F5  DO ACTIVACOBROS
      ON KEY LABEL F6  DO RECLACLIEN
      ON KEY LABEL F7  DO MENSACLIEN
      ON KEY LABEL F8  DO AVISOCOBRA
      ON KEY LABEL F11 ON KEY
   RETURN

  PROCEDURE TECLA_CLIENTE
     ON KEY
     ON KEY LABEL ENTER KEYB CHR(23)
*    ON KEY LABEL INS   DO ALTAS_PERSON
*    ON KEY LABEL F1    DO CONSU_HISTOR
     ON KEY LABEL F2    DO BUSCATITU
     ON KEY LABEL F3    DO SOLIC_MODIFI WITH [T]
     ON KEY LABEL F11   ON KEY
  RETURN

  PROCEDURE TECLA_BENEFI
     ON KEY
     ON KEY LABEL ENTER KEYB CHR(23)
     ON KEY LABEL F2    DO BUSCABENE
     ON KEY LABEL F3    DO SOLIC_MODIFI WITH [B]
     ON KEY LABEL F11   ON KEY
  RETURN

*  LIMPIA VARIABLES DEL PROGRAMA
   PROCEDURE LIMPIAVAR
       STORE      0      TO V_NROCTA, V_CANCON, V_CANFIL, V_CANVAL, V_NROCON
       STORE   SPACE(35) TO V_NOMTIT, V_NOMBEN
       STORE   SPACE(03) TO V_CODPRO
       STORE   SPACE(10) TO V_IMES

       STORE      0      TO V_SUBNTO,V_SUBRGO,V_SUBTAL,V_CANFIL,V_CANCON
       STORE      0      TO V_TOTIDO,V_TOTCER,V_TOTRAL,V_TOTRGO,V_TOTFAC
       STORE      0      TO V_PREIVA,V_VALIVA,V_NROTAL,V_NROCI

   RETURN

****************************************************************
*  PROCEDIMIENTO DE IMPRESION DE FACTURA:
****************************************************************
*  TICKETFELIZ 2018.10.15
   PROCEDURE IMPRIFACTU
   SET DEVICE TO PRINTER
   SET PRINTER TO LPT1
   SET PRINTER ON

   *@ 1,1 SAY "IMPRESION CON ARROBAS"
     DO VALMES
      V_LINEA  = 08
     DO IMPCABFAC
      V_LINEA = V_LINEA + 4
      *@ PROW()+4, 00  SAY [ ]

	  FOR K = 1 TO V_CANCON
         IF SALDOACT(K,05) > 0
            DO CASE
               CASE SALDOACT(K,07) = "SEM"
                  DO IMPPAGPER
               CASE SALDOACT(K,07) = "ANU"
                  DO IMPPAGPER
               CASE SALDOACT(K,07) = "MEN"
                  DO IMPPAGMEN
            ENDCASE
         ENDIF
      ENDFOR

      IF V_TOTDESCUE < 0
         V_LINEA = V_LINEA  + 1
         @ PROW()+1, 32  SAY "DESCUENTO..."
         @ PROW()  , 142 SAY V_TOTDESCUE PICT  "999,999,999"
      ENDIF

      IF V_LINEA < 22
         FOR I = V_LINEA TO 22
             @ PROW()+1,00 SAY [ ]
         ENDFOR
         V_LINEA = 22
      ENDIF

*******************************************************************
*	IMPRIME TOTALES DE LA FACTURA
*******************************************************************
      DO A0OPEAGS WITH V_TOTFAC
      V_PREIVA = ROUND(V_TOTFAC/1.1,0)
      V_VALIVA = V_TOTFAC - V_PREIVA

      @ PROW()+1,142 SAY V_TOTFAC  PICT "999,999,999"
      @ PROW()+1, 38 SAY SUBS(VALNEL,1,60) &&( IMPORTE EN LETRAS )
      @ PROW()  ,142 SAY V_TOTFAC  PICT "999,999,999"

      @ PROW()+2,80 SAY V_VALIVA PICT "99,999,999"
      @ PROW()  ,113 SAY V_VALIVA PICT "99,999,999"

      V_LINEA  = V_LINEA + 8
	  EJECT
      SET DEVICE  TO SCREEN
      SET PRINTER OFF
      SET PRINTER TO
   RETURN

********************************************************************
*  IMPRIME CABECERA DE FACTURA
********************************************************************
* IMPRESION DE TICKETS 2018.10.15A
   PROCEDURE IMPCABFAC
      *??? CHR(27)+CHR(64)
      *??? CHR(27)+CHR(67)+CHR(33)
      ??? CHR(15)
      ??? CHR(27)+CHR(77)
      DO VALMES
      * @ PROW() + V_LINEA,5 SAY [PARQUE SERENIDAD S.R.L.]
      @ PROW() + 1 ,0 SAY [VERSION 7]
      @ PROW() + 1 ,0 SAY [PARQUE SERENIDAD S.R.L.]
      @ PROW() + 1,0 SAY "CAPITAL: GS 1.000.000 "
      @ PROW() + 1,0 SAY [SERVICIOS FUNERALES Y CAJONERIA]
      @ PROW() + 1,0 SAY "MATRIZ: AV.ESPANHA 693"
      @ PROW() + 1,0 SAY "ESQ.BOQUERON"
      @ PROW() + 1,0 SAY "TEL1: 207013(R.A.)"
      @ PROW() + 1,0 SAY "TEL2: 211452 (R.A.)"
      @ PROW() + 1,0 SAY "SUC1. ADMINISTRACION"
      @ PROW() + 1,0 SAY "PERU C/ESPANA"
      @ PROW() + 1,0 SAY "TEL: 207013 (RA)"
      @ PROW() + 1,0 SAY "TEL: 211452 (RA)"
      @ PROW() + 1,0 SAY "SUC2. MEMORIAL MCAL.LOPEZ"
      @ PROW() + 1,0 SAY "AVDA. MCAL LOPEZ 5353"
      @ PROW() + 1,0 SAY "TEL: 613767/9"
      @ PROW() + 1,0 SAY "SUC3. BOQUERON"
      @ PROW() + 1,0 SAY "CALLE BOQUERON 491"
      @ PROW() + 1,0 SAY "JUAN DE SALAZAR"
      @ PROW() + 1,0 SAY "SUC4. MEMORIAL SAN LORENZO"
      @ PROW() + 1,0 SAY "RUTA MCAL ESTIGARRIBIA Y AZARA"
      @ PROW() + 1,0 SAY "TEL: 585030 - 585111"
      @ PROW() + 1,0 SAY "SUC5. PARQUE CEMENTERIO"
      @ PROW() + 1,0 SAY "AV TTE. AMERICO PICO"
      @ PROW() + 1,0 SAY "Y TTE OJEDA 4300"
      @ PROW() + 1,0 SAY "TEL: 940260"
      @ PROW() + 1,0 SAY "R.U.C.: 80001620-3"
      @ PROW() + 1,0 SAY "SUC6. MEMORIAL SAJONIA"
      @ PROW() + 1,0 SAY "AV.CARLOS ANTONIO LOPEZ"
      @ PROW() + 1,0 SAY "E/PARIS Y DR. CORONEL"
      @ PROW() + 1,0 SAY "TEL: 480481"
      @ PROW() + 1,0 SAY "DEP. FABRICA M.R.ALONSO"
      @ PROW() + 1,0 SAY "CALLE CAMPO VIA 1850"
      @ PROW() + 1,0 SAY "TEL: 751305"
      @ PROW() + 1,0 SAY "R.U.C.: 80001620-3"
      @ PROW() + 1,0 SAY "TIMBRADO: 12506778"
      @ PROW() + 1,0 SAY "VALIDO DESDE: 18/12/2017"
      @ PROW() + 1,0 SAY "VALIDO HASTA: 31/12/2017"
      @ PROW() + 1,0 SAY "CODIGO DESCRIPCION PRECIO"
      @ PROW() + 1,0 SAY [FECHA ]
	IF V_FECALT <> CTOD("//")
		@ PROW()+1, 10  SAY DAY(DATE()) PICT [99]
		@ PROW() , 13  SAY V_IMES PICT [@!]
		@ PROW() , 15  SAY YEAR(DATE()) PICT [9999]
	ELSE
		IF V_FECCOB <> DATE()
			@ PROW()+1 , 10  SAY DAY(V_FECCOB) PICT [99]
			@ PROW()   , 13  SAY V_IMES PICT [@!]
			@ PROW()   , 15  SAY YEAR(V_FECCOB) PICT [9999]
		ELSE
			@ PROW()+1 , 10  SAY DAY(DATE()) PICT [99]
			@ PROW()   , 13  SAY V_IMES PICT [@!]
			@ PROW()   , 15  SAY YEAR(DATE()) PICT [9999]
		ENDIF
	ENDIF
*  	  @ PROW()   , 135 SAY "X"

      V_LINEA  = V_LINEA  + 1
      @ PROW()+1, 0  SAY [CLIENTE:]
      @ PROW()+0, 11 SAY TRIM(V_RUCTIT)
      @ PROW()+0, 32  SAY TRIM(V_NOMTIT)+[ - ]+[(]+TRAN(V_NROCTA,"999,999")+[)]
      V_LINEA = V_LINEA + 1
      @ PROW()+4, 21  SAY [ ]
   RETURN

*********************************************************************
*  IMPRIME LINEA DE DETALLE FACTURA
   PROCEDURE IMPRIDETA
      V_LINEA = V_LINEA  + 1
      @ PROW()  , 27  SAY V_DETALLE
      @ PROW()  , 92  SAY V_PREUNI  PICT  "999,999,999"
      @ PROW()  , 142 SAY V_SUBTOT  PICT  "999,999,999"
      @ PROW()+1, 21  SAY [ ]
   RETURN


*********************************************************************
*  IMPRIME DETALLE DE FACTURA POR PAGO SEMESTRAL O ANUAL
   PROCEDURE IMPPAGPER
      V_DETALLE   = []
      V_CUOMEN    = SPACE(05)
      V_ANOMEN    = 0
      V_CUOMAY    = SPACE(05)
      V_ANOMAY    = 0
      V_TOTDESCUE = 0
      FOR Q = 1 TO V_CANFIL
         IF ESTADCTA(Q,02) =  SALDOACT(K,01) .AND.;
            ESTADCTA(Q,03) =  SALDOACT(K,02) .AND.;
            ESTADCTA(Q,04) <> SPACE(05)      .AND.;
            ESTADCTA(Q,15) >   0

*************  ELIGE EL PRIMER MES DEL PERIODO A PAGAR  *************

            IF V_CUOMEN = SPACE(05)
               STORE ESTADCTA(Q,04)       TO V_CUOMEN
               STORE YEAR(ESTADCTA(Q,05)) TO V_ANOMEN
            ELSE
               IF VAL(SUBS(ESTADCTA(Q,04),1,2)) <  VAL(SUBS(V_CUOMEN,1,2)) .AND. ;
                  YEAR(ESTADCTA(Q,05))          <= V_ANOMEN
                  STORE ESTADCTA(Q,04)       TO V_CUOMEN
                  STORE YEAR(ESTADCTA(Q,05)) TO V_ANOMEN
               ENDIF
            ENDIF

*************  FIN DE ELECCION DEL PRIMER MES  **************

*************  ELIGE EL ULTIMO MES DEL PERIODO A PAGAR  ************

            IF V_CUOMAY = SPACE(05)
               STORE ESTADCTA(Q,04)       TO V_CUOMAY
               STORE YEAR(ESTADCTA(Q,05)) TO V_ANOMAY
            ELSE
               IF VAL(SUBS(ESTADCTA(Q,04),1,2)) > VAL(SUBS(V_CUOMAY,1,2)) .OR. ;
                  YEAR(ESTADCTA(Q,05))          > V_ANOMAY
                  STORE ESTADCTA(Q,04)       TO V_CUOMAY
                  STORE YEAR(ESTADCTA(Q,05)) TO V_ANOMAY
               ENDIF
            ENDIF
************   FIN DE ELECCION DEL ULTIMO MES  *************
            V_TOTFAC       = V_TOTFAC + ESTADCTA(Q,12) + ESTADCTA(Q,13)
         ENDIF
      ENDFOR
      V_PREUNI    = SALDOACT(K,05)
      V_SUBTOT    = SALDOACT(K,05)
      V_TOTFAC    = V_TOTFAC + SALDOACT(K,08)
      V_TOTDESCUE = V_TOTDESCUE + SALDOACT(K,08)
      V_VENCIM    = V_CUOMEN
      DO VALMES
      V_CUOMEN    = ALLTRIM(V_DMES)
      V_VENCIM    = V_CUOMAY
      DO VALMES
      V_CUOMAY    = ALLTRIM(V_DMES)
      DO CASE
         CASE SALDOACT(K,07) = "SEM"
            V_DETALLE = "PAGO SEMESTRAL " + SALDOACT(K,01) + TRANSFORM(SALDOACT(K,02),[999999])  +;
                        ", DE " + V_CUOMEN + "/" + TRANSFORM(V_ANOMEN,[9999]) + " A " + V_CUOMAY +;
                        "/" + TRANSFORM(V_ANOMAY,[9999])
         CASE SALDOACT(K,07) = "ANU"
            V_DETALLE = "PAGO ANUAL " + SALDOACT(K,01) + TRANSFORM(SALDOACT(K,02),[999999])      +;
                        ", DE " + V_CUOMEN + "/" + TRANSFORM(V_ANOMEN,[9999]) + " A " + V_CUOMAY +;
                        "/" + TRANSFORM(V_ANOMAY,[9999])
      ENDCASE
      DO IMPRIDETA
   RETURN

*************************************************
*  IMPRIME DETALLE DE FACTURA POR PAGO MENSUAL

   PROCEDURE IMPPAGMEN
      V_DETALLE = []
      FOR Q = 1 TO V_CANFIL
         IF SALDOACT(K,01) = ESTADCTA(Q,02) .AND. ;
            SALDOACT(K,02) = ESTADCTA(Q,03) .AND. ;
            (ESTADCTA(Q,12) + ESTADCTA(Q,13)) > 0
               IF ESTADCTA(Q,02) = "PSM"
                  V_VENCIM = ESTADCTA(Q,04)
                  DO VALMES
                  V_DMES = ALLTRIM(V_DMES)+[/]+ TRANSFORM(YEAR(ESTADCTA(Q,05)),[9999])
               ENDIF
               DO CASE
                  CASE (ESTADCTA(Q,12) + ESTADCTA(Q,13)) <  ESTADCTA(Q,09)
                     V_DETALLE = [PAGO ]+ESTADCTA(Q,02)+[ ]+TRAN(ESTADCTA(Q,03),"999999")+[, A CUENTA DE ]
                  CASE (ESTADCTA(Q,12) + ESTADCTA(Q,13)) =  ESTADCTA(Q,09)
                     IF ESTADCTA(Q,09) < ESTADCTA(Q,14)
                        V_DETALLE = [PAGO ]+ESTADCTA(Q,02)+[ ]+TRAN(ESTADCTA(Q,03),"999999")+[, COMPLEMENTO ]
                     ELSE
                        V_DETALLE = [PAGO ]+ESTADCTA(Q,02)+[ ]+TRAN(ESTADCTA(Q,03),"999999")+[, ]
                     ENDIF
               ENDCASE
               V_DETALLE = V_DETALLE + IIF(ESTADCTA(Q,02)="PSM","CUOTA DE " + V_DMES,"CUOTA NRO " + ESTADCTA(Q,04))
               IF ESTADCTA(Q,02) = "CMS"
                  V_DETALLE = V_DETALLE +  " DE " + TRANSFORM(YEAR(ESTADCTA(Q,05)),[9999])
               ENDIF
               V_PREUNI  = ESTADCTA(Q,12)+ESTADCTA(Q,13)
               V_SUBTOT  = ESTADCTA(Q,12)+ESTADCTA(Q,13)
               V_TOTFAC  = V_TOTFAC+ESTADCTA(Q,12)+ESTADCTA(Q,13)
               IF ESTADCTA(Q,04)=[GA/29]
                  V_DETALLE = [GASTOS ADMINISTRATIVOS-029]
               ENDI
               IF ESTADCTA(Q,04)=[GA/AD]
                  V_DETALLE = [GASTOS ADMINISTRATIVOS-DOCUMENTOS]
               ENDI
               IF ESTADCTA(Q,02)=[CMS] &&AND LEFT(ESTADCTA(Q,04),2)#[GA]
                  V_DETALLE = [PAGO ]+ESTADCTA(Q,02)+" "+TRAN(ESTADCTA(Q,03),"999999")+" PER: "+DTOC(ESTADCTA(Q,16))+" A "+DTOC(ESTADCTA(Q,17))
               &&ELSE
               &&   V_DETALLE = [PAGO ]+ESTADCTA(Q,02)+" "+TRAN(ESTADCTA(Q,03),"999999")+" GASTOS ADMINISTRATIVOS "
               ENDI
               DO IMPRIDETA
         ENDIF
      ENDFOR
      V_TOTFAC    = V_TOTFAC - SALDOACT(K,08)
      V_TOTDESCUE = V_TOTDESCUE + SALDOACT(K,08)
   RETURN


**************************************
*  VALIDA CODIGO DE BANCO:
   FUNCTION VALBAN
   PARAMETERS PBAN
   SELE BANCOS_M
   SEEK PADR(PBAN,2)
   IF EOF()
      DEFINE POPUP POPBANCO FROM 01,05 TO 10,62;
      PROMPT FIELDS;
      BANCOS_DEN+" "+;
      BANCOS_NUM;
      TITLE " BANCOS ";
      COLOR  I,W+/B,W+/N,I,,B/W,
      ON SELE POPU POPBANCO DEAC POPU
      SET ORDE TO TAG NOMBCO
      GO TOP
      ON KEY LABEL ENTER KEYB CHR(23)
      ACTI POPU POPBANCO
      M.BANCOS_DEN=BANCOS_DEN
      CODBAN      =BANCOS_NUM
   ELSE
      M.BANCOS_DEN=BANCOS_DEN
      CODBAN      =BANCOS_NUM
   ENDI
   ON KEY
   M.BANCOS_DEN          = BANCOS_DEN
   DETAVALO(V_CANVAL,05) = BANCOS_NUM
   SET ORDE TO I01B01
   @ROW(),COL()+05 SAY M.BANCOS_DEN FONT FUENTE,12
   RETU

****************************************
*  VALIDA CODIGO DE TARJETA DE CREDITO:
   FUNCTION VALTAR
   PARA PTAR
   SELE TARJET_M
   SEEK PADR(PTAR,2)
   IF EOF()
      SET ORDE TO TARJET_NOM
      GO TOP
      ON KEY LABEL ENTER KEYB CHR(23)
      ACTI WIND CON_TAR
      BROW FIELDS TARJET_DEN:H="DENOMINACION",TARJET_COD:H="COD." IN WIND CON_TAR NODE NOMO NOAP
      M.TARJET_DEN=TARJET_DEN
      CODTAR      =TARJET_COD
   ENDI
   ON KEY
   M.TARJET_DEN          = TARJET_DEN
   DETAVALO(V_CANVAL,06) = TARJET_COD
   DEACTIVATE WINDOW CON_TAR
   SET ORDER TO TARJET_COD
   @ROW(),COL()+05 SAY TRIM(M.TARJET_DEN)
   RETURN

***** PROCEDIMIENTO DE VALIDACION DE MES:
  PROCEDURE VALMES
  DO CASE
     CASE SUBS(V_VENCIM,1,2)=[01]
        V_DMES=[ENERO    ]
     CASE SUBS(V_VENCIM,1,2)=[02]
        V_DMES=[FEBRERO  ]
     CASE SUBS(V_VENCIM,1,2)=[03]
        V_DMES=[MARZO    ]
     CASE SUBS(V_VENCIM,1,2)=[04]
        V_DMES=[ABRIL    ]
     CASE SUBS(V_VENCIM,1,2)=[05]
        V_DMES=[MAYO     ]
     CASE SUBS(V_VENCIM,1,2)=[06]
        V_DMES=[JUNIO    ]
     CASE SUBS(V_VENCIM,1,2)=[07]
        V_DMES=[JULIO    ]
     CASE SUBS(V_VENCIM,1,2)=[08]
        V_DMES=[AGOSTO   ]
     CASE SUBS(V_VENCIM,1,2)=[09]
        V_DMES=[SETIEMBRE]
     CASE SUBS(V_VENCIM,1,2)=[10]
        V_DMES=[OCTUBRE  ]
     CASE SUBS(V_VENCIM,1,2)=[11]
        V_DMES=[NOVIEMBRE]
     CASE SUBS(V_VENCIM,1,2)=[12]
        V_DMES=[DICIEMBRE]
     CASE TRIM(V_VENCIM)=[CU/IN]
        V_DMES=[CUOTA DE INGRESO]
  ENDCASE
  DO CASE
     CASE MONT(DATE())=1
        V_IMES=[ENERO    ]
     CASE MONT(DATE())=2
        V_IMES=[FEBRERO  ]
     CASE MONT(DATE())=3
        V_IMES=[MARZO    ]
     CASE MONT(DATE())=4
        V_IMES=[ABRIL    ]
     CASE MONT(DATE())=5
        V_IMES=[MAYO     ]
     CASE MONT(DATE())=6
        V_IMES=[JUNIO    ]
     CASE MONT(DATE())=7
        V_IMES=[JULIO    ]
     CASE MONT(DATE())=8
        V_IMES=[AGOSTO   ]
     CASE MONT(DATE())=9
        V_IMES=[SETIEMBRE]
     CASE MONT(DATE())=10
        V_IMES=[OCTUBRE  ]
     CASE MONT(DATE())=11
        V_IMES=[NOVIEMBRE]
     CASE MONT(DATE())=12
        V_IMES=[DICIEMBRE]
  ENDCASE
IF V_FECCOB<>DATE()
  DO CASE
     CASE MONT(V_FECCOB)=1
        V_IMES=[ENERO    ]
     CASE MONT(V_FECCOB)=2
        V_IMES=[FEBRERO  ]
     CASE MONT(V_FECCOB)=3
        V_IMES=[MARZO    ]
     CASE MONT(V_FECCOB)=4
        V_IMES=[ABRIL    ]
     CASE MONT(V_FECCOB)=5
        V_IMES=[MAYO     ]
     CASE MONT(V_FECCOB)=6
        V_IMES=[JUNIO    ]
     CASE MONT(V_FECCOB)=7
        V_IMES=[JULIO    ]
     CASE MONT(V_FECCOB)=8
        V_IMES=[AGOSTO   ]
     CASE MONT(V_FECCOB)=9
        V_IMES=[SETIEMBRE]
     CASE MONT(V_FECCOB)=10
        V_IMES=[OCTUBRE  ]
     CASE MONT(V_FECCOB)=11
        V_IMES=[NOVIEMBRE]
     CASE MONT(V_FECCOB)=12
        V_IMES=[DICIEMBRE]
  ENDCASE
ENDIF
IF V_FECALT<>CTOD("//")
  DO CASE
     CASE MONT(DATE())=1
        V_IMES=[ENERO    ]
     CASE MONT(DATE())=2
        V_IMES=[FEBRERO  ]
     CASE MONT(DATE())=3
        V_IMES=[MARZO    ]
     CASE MONT(DATE())=4
        V_IMES=[ABRIL    ]
     CASE MONT(DATE())=5
        V_IMES=[MAYO     ]
     CASE MONT(DATE())=6
        V_IMES=[JUNIO    ]
     CASE MONT(DATE())=7
        V_IMES=[JULIO    ]
     CASE MONT(DATE())=8
        V_IMES=[AGOSTO   ]
     CASE MONT(DATE())=9
        V_IMES=[SETIEMBRE]
     CASE MONT(DATE())=10
        V_IMES=[OCTUBRE  ]
     CASE MONT(DATE())=11
        V_IMES=[NOVIEMBRE]
     CASE MONT(DATE())=12
        V_IMES=[DICIEMBRE]
  ENDCASE
ENDIF

  RETURN

*****  CONVERSION DE NUMEROS EN TEXTO:
   PROC A0OPEAGS
   PARAMETERS NUM
   PUBLIC MONTO1,MONTO2,MONTO3,MONTO4
   NUME=' '
   IF NUM=0
      MONTO1=' CERO'+REPLICATE('-',60)
      MONTO2=REPLICATE('-',63)
      RETURN
   ENDIF
   NUMS=STR(NUM,12)
   IF AT('-',NUMS)>0
      POS=AT('-',NUMS)
      NUMS=SPACE(POS)+SUBSTR(NUMS,(POS+1))
   ENDIF
   C1=SUBSTR(NUMS,10,3)
   C2=SUBSTR(NUMS,7,3)
   C3=SUBSTR(NUMS,4,3)
   C4=SUBSTR(NUMS,1,3)
   *
   CICLO=0
   DO WHILE .T.
      CICLO=CICLO+1
      DO CASE
         CASE CICLO=1
              C=C1
              IF C='   '
                 LOOP
              ENDIF
         CASE CICLO=2
              C=C2
              IF C='   '
                 LOOP
              ENDIF
              IF VAL(C)>0
                 NUME=' MIL'+NUME
              ENDIF
         CASE CICLO=3
              C=C3
              IF C='   ' .OR. C='000'
                 LOOP
              ENDIF
              IF VAL(C)>1
                 NUME=' MILLONES'+NUME
              ELSE
                 NUME=' MILLON'+NUME
              ENDIF
         CASE CICLO=4
              C=C4
              IF C='   ' .OR. C='000'
                 LOOP
              ENDIF
              NUME=' MIL'+NUME
         OTHERWISE
     EXIT
   ENDCASE
   *
   IF SUBSTR(C,2,1)='1'
      AUX=VAL(SUBSTR(C,2,2))
      DO CASE
         CASE AUX=10
              NUME=' DIEZ'+NUME
         CASE AUX=11
              NUME=' ONCE'+NUME
         CASE AUX=12
              NUME=' DOCE'+NUME
         CASE AUX=13
              NUME=' TRECE'+NUME
         CASE AUX=14
              NUME=' CATORCE'+NUME
         CASE AUX=15
              NUME=' QUINCE'+NUME
         CASE AUX=16
              NUME=' DIEZ Y SEIS'+NUME
         CASE AUX=17
              NUME=' DIEZ Y SIETE'+NUME
         CASE AUX=18
              NUME=' DIEZ Y OCHO'+NUME
         CASE AUX=19
              NUME=' DIEZ Y NUEVE'+NUME
      ENDCASE
   ELSE
      AUX=VAL(SUBSTR(C,3,1))
      DO CASE
         CASE AUX=1
              IF CICLO>1
                 NUME= ' UN'+NUME
              ELSE
                 NUME= ' UNO'+NUME
              ENDIF
         CASE AUX=2
              NUME=' DOS'+NUME
         CASE AUX=3
              NUME= ' TRES'+NUME
         CASE AUX=4
              NUME=' CUATRO'+NUME
         CASE AUX=5
              NUME= ' CINCO'+NUME
         CASE AUX=6
              NUME=' SEIS'+NUME
         CASE AUX=7
              NUME= ' SIETE'+NUME
         CASE AUX=8
              NUME=' OCHO'+NUME
         CASE AUX=9
              NUME= ' NUEVE'+NUME
      ENDCASE

      *  D E C E N A S
      AUX=VAL(SUBSTR(C,2,1))
      IF VAL(SUBSTR(C,3,1))>0 AND AUX>1
         NUME=' Y'+NUME
      ENDIF
      DO CASE
         CASE AUX=2
              NUME=' VEINTE'+NUME
         CASE AUX=3
              NUME= ' TREINTA'+NUME
         CASE AUX=4
              NUME=' CUARENTA'+NUME
         CASE AUX=5
              NUME= ' CINCUENTA'+NUME
         CASE AUX=6
              NUME=' SESENTA'+NUME
         CASE AUX=7
              NUME= ' SETENTA'+NUME
         CASE AUX=8
              NUME=' OCHENTA'+NUME
         CASE AUX=9
              NUME= ' NOVENTA'+NUME
      ENDCASE
   ENDIF

   **************** C E N T E N A S
   AUX=VAL(SUBSTR(C,1,1))
   DO CASE
     CASE AUX=1
     IF VAL(C)<>100
     NUME=' CIENTO'+NUME
     ELSE
     NUME=' CIEN'+NUME
     ENDIF
     CASE AUX=2
     NUME=' DOSCIENTOS'+NUME
     CASE AUX=3
     NUME= ' TRESCIENTOS'+NUME
     CASE AUX=4
     NUME=' CUATROCIENTOS'+NUME
     CASE AUX=5
     NUME= ' QUINIENTOS'+NUME
     CASE AUX=6
     NUME=' SEISCIENTOS'+NUME
     CASE AUX=7
     NUME= ' SETECIENTOS'+NUME
     CASE AUX=8
     NUME=' OCHOCIENTOS'+NUME
     CASE AUX=9
     NUME= ' NOVECIENTOS'+NUME
   ENDCASE
   *
   ENDDO
   IF SUBSTR(NUME,2,1)='Y'
   NUME=SUBSTR(NUME,4)
   ENDIF
   IF NUM<0
   NUME='MENOS '+NUME
   ENDIF
   MONTO4=NUME
   *NUME='SON GUARANIES:'+NUME
   IF LEN(NUME)>75
   CON=75
   *
   DO WHILE .T.
	   IF SUBSTR(NUME,CON,1)=' '
		   MONTO1=SUBSTR(NUME,1,CON)
		   MONTO2=SUBSTR(NUME,CON+1)
		   EXIT
	   ELSE
		   CON=CON-1
		   LOOP
	   ENDIF
	EXIT
   ENDDO
   *
   ELSE
   MONTO1=NUME
   MONTO2=REPLICATE('-',75)
   ENDIF

   NUME1=MONTO2
   IF LEN(NUME1)>75
   CON=75
   *
   DO WHILE .T.
   IF SUBSTR(NUME1,CON,1)=' '
   MONTO2=SUBSTR(NUME1,1,CON)
   MONTO3=SUBSTR(NUME1,CON+1)
   MONTO3=MONTO3+REPLICATE('-',(75-LEN(MONTO3)))
   EXIT
   ELSE
   CON=CON-1
   LOOP
   ENDIF
   EXIT
   ENDDO
   *
   ELSE
   MONTO3=REPLICATE('-',75)
   ENDIF
   MONTO1=MONTO1+REPLICATE('-',(75-LEN(MONTO1)))
   MONTO2=MONTO2+REPLICATE('-',(75-LEN(MONTO2)))
   STORE MONTO1 TO VALNEL
   RETURN

* BUSQUEDA DE CLIENTES POR NOMBRE
  PROCEDURE BUSCATITU
     ON KEY
     ACTIVATE WINDOW BUSQUEDA
     @ 00     ,01 CLEAR TO 05,48
     IF LASTKEY()<>27
        STORE SPACE(35) TO V_BUSCAR
        @ 00     ,01 SAY [ NOMBRE?      :]
        @ ROW()+2,01 SAY [ESTA OPCION LE ENCONTRARA EL TITULAR CON EL CUAL DESEA TRABAJAR.]
        @ 00     ,16 GET V_BUSCAR PICT[@!]
        READ
        SET ORDER TO NOMBRE
        GO TOP
        SET NEAR ON
        SET EXACT OFF
        SEEK ALLT(V_BUSCAR)
     ENDIF
     DEACTIVATE WINDOW BUSQUEDA
     SET NEAR OFF
  && SET EXACT ON
     DO TECLA_CLIENTE
  RETURN

* BUSQUEDA DE BENEFICIARIOS POR NOMBRE
  PROCEDURE BUSCABENE
     ON KEY
     ACTIVATE WINDOW BUSQUEDA
     @ 00     ,01 CLEAR TO 05,48
     IF LASTKEY()<>27
        STORE SPACE(35) TO V_BUSCAR
        @ 00     ,01 SAY [ NOMBRE?      :]
        @ ROW()+2,01 SAY [ESTA OPCION LE ENCONTRARA EL BENEFICIARIO CON EL CUAL DESEA TRA-    ]
        @ ROW()+1,01 SAY [BAJAR.                                                              ]
        @ 00     ,16 GET V_BUSCAR PICT[@!]
        READ
        SET ORDER TO BENEFI_NOM
        GO TOP
        SET NEAR ON
        SET EXACT OFF
        SEEK (V_CODSUC + V_BUSCAR)
     ENDIF
     DEACTIVATE WINDOW BUSQUEDA
     SET NEAR OFF
  && SET EXACT ON
     DO TECLA_BENEFI
  RETURN

* SOLICITUD DE MODIFICACION DE DATOS:
  PROCEDURE SOLIC_MODIFI
  PARAMETER ORIDATOS
  STORE SPACE(30)TO SOLICITA
  DO CASE
     CASE ORIDATOS =[T]
          STORE [EL TITULAR]        TO ITEMMODI
          STORE []                  TO ORIGIAGR
          &&STORE TRANSFORM(CLIENT_TIT,[999999])+[ ]+ALLTRIM(CLIENT_NOM)+ [ - ]+ [C.I.: ]+TRANSFORM(CLIENT_NDO,[99999999]);
                                    TO ORIGICAM
          STORE []                  TO ORIGICOD
          STORE [CLIENT_M]          TO SALEBUSQ,ARCHIMOD
          STORE [TECLA_CLIENTE]     TO TECLBUSQ
     CASE ORIDATOS =[B]
          STORE [EL BENEFICIARIO]   TO ITEMMODI
          STORE BENEFI_PRO+[ ]+TRANSFORM(BENEFI_CON,[999999]);
                                    TO ORIGIAGR
          STORE TRANSFORM(BENEFI_NUM,[9999999])+[ ]+ALLTRIM(BENEFI_NOM)+[ - ]+ORIGIAGR;
                                    TO ORIGICAM
          STORE [BENEFI_D]          TO SALEBUSQ,ARCHIMOD
          STORE [TECLA_BENEFI]      TO TECLBUSQ
     CASE ORIDATOS =[E]
          STORE [LA CALLE]          TO ITEMMODI
          STORE CIUCIU+[, ]+CIUPAI  TO ORIGIAGR
          STORE ALLTRIM(CALLES_NOM)+[, ]+CIUCIU+[, ]+CIUPAI TO ORIGICAM
          STORE [CALLES_M]          TO SALEBUSQ,ARCHIMOD
          STORE [TECLA_CALLES]      TO TECLBUSQ
  ENDCASE
  ON KEY
  SELECT MODIFI_M
  GO BOTTOM
  STORE VAL(MODIFI_NUM)+1 TO REGISNUM
  STORE STR(REGISNUM,5)   TO REGISNUM
  STORE ALLTRIM(REGISNUM) TO REGISNUM
  STORE DTOC(DATE())      TO FECHASOL
  ACTIVATE WINDOW SOLMODIF
  @ 00,00 SAY [SOLICITUD NO. ]+REGISNUM+[ DEL ]+FECHASOL
  @ 01,00 SAY [POR MEDIO DE LA PRESENTE, SOLICITO...]
  @ 07,14 SAY [HAGA SU ELECCION O SI DESISTE, <ESC>ESCAPE]
  ACTIVATE WINDOW TIPOMODI
  @ 00,00 PROMPT [ \<CAMBIAR ]
  @ 00,11 SAY    [O]
  @ 00,13 PROMPT [ \<AGREGAR ]
  MENU TO OPCTIP
  DEACTIVA WINDOW TIPOMODI
  IF LASTKEY()<>27
     @ 04,00 SAY [ADVERTENCIA: POR FAVOR ESCRIBA EXACTAMENTE COMO DIROA EL NUEVO DATO]
     @ 07,14 SAY [DIGOTE EL DATO  O  SI DESISTE, <ESC>ESCAPE]
     IF OPCTIP=1
        STORE [CAMBIAR] TO TIPOMODI
        @ 01,34 SAY [ QUE SE CAMBIE ]+UPPER(ITEMMODI)
        @ 02,00 SAY [DE: ]+ORIGICAM
        @ 03,00 SAY [A :]GET SOLICITA PICT[@!!!!!!!!!!!!!!!!!!!!!!!!!!!!!]
        READ
        @ 03,00 SAY SPACE(72)
        @ 03,00 SAY [A : ]+SOLICITA
        STORE TIPOMODI+[ ]+SUBSTR(ITEMMODI,4)+[ A ]+ALLTRIM(SOLICITA) TO TEXTMODI
        STORE ORIGICAM                                                TO ORIGMODI
     ELSE
        STORE [AGREGAR] TO TIPOMODI
        @ 01,34 SAY [ QUE SE AGREGE ]+UPPER(ITEMMODI)
        IF ITEMMODI<>[EL TITULAR]
           @ 02,00 SAY [DENTRO DE: ]+ORIGIAGR
        ENDIF
        @ 03,00 GET SOLICITA PICT[@!]
        READ
        @ 03,00 SAY SPACE(72)
        IF .NOT. EMPTY (SOLICITA)
           @ 03,00 SAY ALLTRIM(SOLICITA)
        ELSE
           @ 03,00 SAY ALLTRIM(SOLICITA)+[, ]+ORIGIAGR
        ENDIF
        STORE TIPOMODI+[ ]+SUBSTR(ITEMMODI,4)+[ ]+ALLTRIM(SOLICITA) TO TEXTMODI
        STORE ORIGIAGR                                              TO ORIGMODI
     ENDIF
     IF LASTKEY()<>27
        SCATTER MEMVAR BLANK
        @ 04,00 SAY SPACE(72)
        @ 04,00 SAY [DEBIDO A QUE:]
        @ 12,00 SAY [ADVERTENCIA!: SI USTED NO COMENTA JUSTIFICANDO ADECUADAMENTE SU SOLICI-]
        @ 13,00 SAY [              TUD OSTA NO SERO GRABADA ASI COMO TAMPOCO SERO PROCESADA.]
        @ 15,22 SAY [<CTRL-W>GRABA <ESC>ESCAPA]
        SCATTER MEMVAR MEMO BLANK
        @ 05,00 EDIT M.MODIFI_DET SIZE 06,72
        READ
        IF LASTKEY()<>27
           IF EMPTY(M.MODIFI_DET)
              WAIT [ADVERTENCIA!: NO PUEDE SER VACIO, SU DATO NO SERO PROCESADO] WIND NOWAIT
           ELSE
              @ 12,00 CLEAR TO 15,72
              ACTIVATE WINDOW CONFIRMA
              @ 00,03 PROMPT [   \<NO   ]
              @ 00,11 PROMPT [   \<SI   ]
              MENU TO OPCCON
              DEACTIVA WINDOW CONFIRMA
              IF OPCCON=2
                 STORE REGISNUM TO M.MODIFI_NUM
                 STORE DATE()   TO M.MODIFI_ALT
                 STORE TEXTMODI TO M.MODIFI_TXT
                 STORE ORIGMODI TO M.MODIFI_ORI
                 STORE ARCHIMOD TO M.MODIFI_ARC
                 STORE SOLICITA TO M.MODIFI_CAM
                 STORE ORIGICOD TO M.MODIFI_COD
                 STORE USUARIOS TO M.MODIFI_USU
                 STORE USUARPAI TO M.MODIFI_UPA
                 STORE USUARPER TO M.MODIFI_UNU
                 APPEND BLANK
                 GATHER MEMVAR
                 GATHER MEMVAR MEMO
              ENDIF
           ENDIF
        ENDIF
     ENDIF
  ENDIF
  DEACTIVA WINDOW SOLMODIF
  SELECT &SALEBUSQ
  DO     &TECLBUSQ
  RETURN

* LISTA DE LLAMADAS POR CLIENTES.
  PROCEDURE ACTIVACOBROS

  SELEC 15
  USE ACTIVA_M

  ******************************************************
  *  VERIFICA SI EL CLIENTE TIENE LLAMADOS             *
  ******************************************************
  ON KEY
  SELEC ACTIVA_M
  SET ORDER TO ACTIVA_TIT
  GO TOP
  SEEK (V_CODSUC + TRANSFORM(V_NROCTA,[999999]))
  IF EOF()
     WAIT " ESTE CLIENTE NO REGISTRA LLAMADOS - PRESIONE UNA TECLA... " WINDOW
     SELEC ACTIVA_M
     USE
     DO TECLA_PAGO
     RETURN
  ENDIF
  STORE USUARIOS + [.TXT] TO V_DISACCO
  SAVE SCREEN TO PANTA07
  SET PRINTER TO M:\TEMP\&V_DISACCO
  SET DEVICE  TO PRINTER

  ************************
  *  INICIO DEL LISTADO  *
  ************************
  SCATTER MEMVAR
  DO TITULO
  V_TOTLLA = 0  									&& TOTAL DE LLAMADOS.
  V_TOTACO = 0  									&& TOTAL A COBRAR.
  DO WHILE .T.
     *********************
     *  ACUMULA TOTALES  *
     *********************
     V_TOTLLA = V_TOTLLA + 1       					&& TOTAL DE LLAMADOS.
     V_TOTACO = V_TOTACO + M.ACTIVA_MON  			&& TOTAL A COBRAR.
     *******************
     *  IMPRIME LINEA  *
     *******************
     @PROW()+1,01     SAY M.ACTIVA_PRO  + ": " + TRAN(M.ACTIVA_CON,"999999") + " "+ M.ACTIVA_NOM
     @PROW()  ,PCOL() SAY SPACE(05) + "(" + TRAN(M.ACTIVA_NLL,"999999") + ")"
     @PROW()+1,01     SAY "DIRECCION...: " + M.ACTIVA_DIR
     @PROW()+1,01     SAY "INTERSECCION: " + M.ACTIVA_INT + "   (" + M.ACTIVA_COB + ")"
     @PROW()+1,01     SAY "TELEFONOS: PARTICULAR: " + M.ACTIVA_TPA
     IF LEN(ALLT(M.ACTIVA_TCM)) # 0
        @PROW()+1,01 SAY "           COMERCIAL.: " + M.ACTIVA_TCM
     ENDIF
     IF LEN(ALLT(M.ACTIVA_TCB)) # 0
        @PROW()+1,01 SAY "           DE COBRO..: " + M.ACTIVA_TCB
     ENDIF
     @PROW()+1,01 SAY  "VOLVER A LLAMAR EL DIA: " + DTOC(M.ACTIVA_LLA)
     IF LEN(ALLT(M.ACTIVA_OB1)) # 0
        @PROW()+1,01 SAY  SPAC(05) + M.ACTIVA_OB1
     ENDIF
     IF LEN(ALLT(M.ACTIVA_OB2)) # 0
        @PROW()+1,01 SAY  SPAC(05) + M.ACTIVA_OB2
     ENDIF
     IF LEN(ALLT(M.ACTIVA_OB3)) # 0
        @PROW()+1,01 SAY  SPAC(05) + M.ACTIVA_OB3
     ENDIF
     IF LEN(ALLT(M.ACTIVA_SE1)) # 0 .OR. LEN(ALLT(M.ACTIVA_SE2)) # 0;
        LEN(ALLT(M.ACTIVA_SE3)) # 0 .OR. LEN(ALLT(M.ACTIVA_SE4)) # 0;
        LEN(ALLT(M.ACTIVA_SE5)) # 0 .OR. LEN(ALLT(M.ACTIVA_SE6)) # 0
        @PROW()+1,01 SAY SPACE(01)
        @PROW()+1,01 SAY "SEGUIMIENTO: "
        IF LEN(ALLT(M.ACTIVA_SE1)) # 0
          @PROW()+1,01 SAY  SPAC(05) + M.ACTIVA_SE1
        ENDIF
        IF LEN(ALLT(M.ACTIVA_SE2)) # 0
          @PROW()+1,01 SAY  SPAC(05) + M.ACTIVA_SE2
        ENDIF
        IF LEN(ALLT(M.ACTIVA_SE3)) # 0
          @PROW()+1,01 SAY  SPAC(05) + M.ACTIVA_SE3
        ENDIF
        IF LEN(ALLT(M.ACTIVA_SE4)) # 0
          @PROW()+1,01 SAY  SPAC(05) + M.ACTIVA_SE4
        ENDIF
        IF LEN(ALLT(M.ACTIVA_SE5)) # 0
          @PROW()+1,01 SAY  SPAC(05) + M.ACTIVA_SE5
        ENDIF
        IF LEN(ALLT(M.ACTIVA_SE6)) # 0
          @PROW()+1,01 SAY  SPAC(05) + M.ACTIVA_SE6
        ENDIF
     ENDIF
     @PROW()+1,01  SAY  REPLI("O",79)
     IF .NOT. EOF()
        SKIP
        SCATTER MEMVAR
     ENDIF
     IF V_NROCTA # M.ACTIVA_TIT .OR. EOF()
        EXIT
     ENDIF
  ENDDO
  @PROW()+1,01 SAY "O" + REPLI("O",77) + "O"
  @PROW()+1,01 SAY "O" + SPAC(10) + "** " + "TOTAL DE LLAMADOS:" + TRAN(V_TOTLLA,"9999") + " ** " + "TOTAL A COBRAR:" + TRAN(V_TOTACO,"999,999,999") + " **" + SPAC(09) + "O"
  @PROW()+1,01 SAY "O" + REPLI("O",77) + "O"
  SET DEVICE  TO SCREEN
  *SET PRINTER TO PRN
  SET CONSOLE ON
  MODIFY COMM M:\TEMP\&V_DISACCO NOEDIT WINDOW ACTICOBRO
  DELETE FILE M:\TEMP\&V_DISACCO
  SET COLOR TO I
  RESTORE SCREEN FROM PANTA07
  SELEC ACTIVA_M
  USE
  DO TECLA_PAGO
  RETURN

*  TITULO DEL LISTADO DEL ACTIVADOR DE COBROS
   PROCEDURE TITULO
      @00      ,01 SAY "O" + REPLI("O",77) + "O"
      @PROW()+1,01 SAY "O  " + "LLAMADOS DEL ACTIVADOR DE COBROS AL " + DTOC(DATE()) + SPAC(28) + " O"
      @PROW()+1,01 SAY "O" + REPLI("O",77) + "O"
   RETURN

* SEGUIMIENTO DE RECLAMOS DE CLIENTES.
  PROCEDURE RECLACLIEN

   SELEC 16
   USE RECLAM_M
   SELEC 17
   USE FORREC_M ORDER FORREC_COD
   SELEC 18
   USE TIPREC_M ORDER TIPREC_COD

  ******************************************************
  *  VERIFICA SI EL CLIENTE TIENE RECLAMOS             *
  ******************************************************
  ON KEY
  SELEC RECLAM_M
  SET ORDER TO RECLAM_TIT
  GO TOP
  SEEK (V_CODSUC + TRANSFORM(V_NROCTA,[999999]))
  IF EOF()
     WAIT " ESTE CLIENTE NO TIENE RECLAMOS - PRESIONE UNA TECLA... " WINDOW
     DO FINRECLAMOS
     RETURN
  ENDIF
  STORE USUARIOS + [.TXT] TO V_DISACCO
  SAVE SCREEN TO PANTA07
  *SET PRINTER TO M:\-DATOS\SICMADAT\CCE\&V_DISACCO
  SET PRINTER TO M:\TEMP\&V_DISACCO
  SET DEVICE  TO PRINTER
  ************************
  *  INICIO DEL LISTADO  *
  ************************
  SCATTER MEMVAR
  V_TOTACO = 0  									&& TOTAL A COBRAR.
  V_CANREC = 0 							            && CANTIDAD DE RECLAMOS.
  DO TITURECLA
  DO WHILE .T.
     *********************
     *  ACUMULA TOTALES  *
     *********************
     V_CANREC = V_CANREC + 1       					&& CANTIDAD DE RECLAMOS.
     V_TOTACO = V_TOTACO + M.RECLAM_MON  			&& TOTAL A COBRAR.
     *******************
     *  IMPRIME LINEA  *
     *******************
      @PROW()+1,01 SAY  " RECLAMO NRO.: " + TRANSFORM(RECLAM_NUM,"999999")
      @PROW()+1,01 SAY  " " + M.RECLAM_PRO + "........: " + TRANSFORM(M.RECLAM_CON,"999999") + " " + V_NOMTIT;
                                           + "  " + "COBRADOR..: " + M.RECLAM_COB
      SELEC FORREC_M
      GO TOP
      SEEK (TRANSFORM(M.RECLAM_ATR,[999]))
      @PROW()+1,01 SAY " SE RECIBIO EL " + DTOC(M.RECLAM_FEC) + " A LAS " + M.RECLAM_HOR + " HS. - " + FORREC_DES
      @PROW()+1,01 SAY " DIRECCION...: " + M.RECLAM_DIR
      @PROW()+1,01 SAY " INTERSECCION: " + M.RECLAM_INT
      SELEC TIPREC_M
      GO TOP
      SEEK (TRANSFORM(M.RECLAM_TIP,[999]))
      @PROW()+1,01 SAY " RECLAMANDO..: " + TIPREC_DES + SPAC(08) + "A COBRAR...:" + TRAN(M.RECLAM_MON,"999,999,999")
      SELEC RECLAM_M
      IF M.RECLAM_OB1 # SPACE(69)
         @PROW()+1,01   SAY SPACE(10) + M.RECLAM_OB1
      ENDIF
      IF M.RECLAM_OB2 # SPACE(69)
         @PROW()+1,01   SAY SPACE(10) + M.RECLAM_OB2
      ENDIF
      IF M.RECLAM_OB3 # SPACE(69)
         @PROW()+1,01   SAY SPACE(10) + M.RECLAM_OB3
      ENDIF
      IF M.RECLAM_OB4 # SPACE(69)
         @PROW()+1,01   SAY SPACE(10) + M.RECLAM_OB4
      ENDIF
      IF M.RECLAM_SE1 # SPACE(69) .OR. M.RECLAM_SE2 # SPACE(69);
         M.RECLAM_SE3 # SPACE(69) .OR. M.RECLAM_SE4 # SPACE(69);
         M.RECLAM_SE5 # SPACE(69) .OR. M.RECLAM_SE6 # SPACE(69)

         @PROW()+1,01 SAY "SEGUIMIENTO: "
         IF M.RECLAM_SE1 # SPACE(69)
            @PROW()+1,01 SAY  SPACE(10) + M.RECLAM_SE1
         ENDIF
         IF M.RECLAM_SE2 # SPACE(69)
            @PROW()+1,01 SAY  SPACE(10) + M.RECLAM_SE2
         ENDIF
         IF M.RECLAM_SE3 # SPACE(69)
            @PROW()+1,01 SAY  SPACE(10) + M.RECLAM_SE3
         ENDIF
         IF M.RECLAM_SE4 # SPACE(69)
            @PROW()+1,01 SAY  SPACE(10) + M.RECLAM_SE4
         ENDIF
         IF M.RECLAM_SE5 # SPACE(69)
            @PROW()+1,01 SAY  SPACE(10) + M.RECLAM_SE5
         ENDIF
         IF M.RECLAM_SE6 # SPACE(69)
            @PROW()+1,01 SAY  SPACE(10) + M.RECLAM_SE6
         ENDIF
      ENDIF
      @PROW()+1,01 SAY " SE FINIQUITO EL " + DTOC(M.RECLAM_FFI) + " A LAS " + M.RECLAM_HFI + " HS."
      @PROW()+1,01 SAY REPLICATE("O",79)
      IF .NOT. EOF()
         SKIP
         SCATTER MEMVAR
      ENDIF
      IF V_NROCTA # M.RECLAM_TIT .OR. EOF()
         EXIT
      ENDIF
  ENDDO
  @PROW()+1,01 SAY "O" + REPLI("O",77) + "O"
  @PROW()+1,01 SAY "O" + SPAC(10) + "** " + "TOTAL DE RECLAMOS:" + TRAN(V_CANREC,"9999") + " ** " + "TOTAL A COBRAR:" + TRAN(V_TOTACO,"999,999,999") + " **" + SPAC(09) + "O"
  @PROW()+1,01 SAY "O" + REPLI("O",77) + "O"
  SET DEVICE  TO SCREEN
  SET PRINTER TO PRN
  SET CONSOLE ON

  *MODIFY COMM M:\-DATOS\SICMADAT\CCE\&V_DISACCO NOEDIT WINDOW ACTICOBRO
  *DELETE FILE M:\-DATOS\SICMADAT\CCE\&V_DISACCO
  MODIFY COMM M:\TEMP\&V_DISACCO NOEDIT WINDOW ACTICOBRO
  DELETE FILE M:\TEMP\&V_DISACCO

  SET COLOR TO I
  RESTORE SCREEN FROM PANTA07
  DO FINRECLAMOS
  RETURN

* TITULO DEL LISTADO DE RECLAMOS.
  PROCEDURE TITURECLA
     @00      ,01  SAY "O"   + REPLI("O",77) + "O"
     @PROW()+1,01  SAY "O"   + "RECLAMOS DE CLIENTES" + SPACE(57) + "O"
     @PROW()+1,01  SAY "O"   + REPLI("O",77) + "O"
  RETURN

* FIN DE RECLAMOS
  PROCEDURE FINRECLAMOS
     SELEC RECLAM_M
     USE
     SELEC FORREC_M
     USE
     SELEC TIPREC_M
     USE
     DO TECLA_PAGO
  RETURN

* SEGUIMIENTO DE MENSAJES A CLENTES.
  PROCEDURE MENSACLIEN

   SELEC 19
   USE MENMEN_M
   SET ORDER TO MENMEN_COD
   SELEC 20
   USE MENMOT_M
   SELEC 21
   USE MENENC_M
   SELEC 22
   USE MENSAJ_M
   SELEC 23
   USE TIPENT_M

  ******************************************************
  *  VERIFICA SI EL CLIENTE TIENE MENSAJES             *
  ******************************************************
  ON KEY
  SELEC MENSAJ_M
  SET ORDER TO MENSAJ_TIT
  GO TOP
  SEEK (V_CODSUC + TRANSFORM(V_NROCTA,[999999]))
  IF EOF()
     WAIT " ESTE CLIENTE NO REGISTRA MENSAJES - PRESIONE UNA TECLA... " WINDOW
     DO FINMENSAJES
     RETURN
  ENDIF
  STORE USUARIOS + [.TXT] TO V_DISACCO
  SAVE SCREEN TO PANTA07
  *SET PRINTER TO M:\-DATOS\SICMADAT\CCE\&V_DISACCO
  SET PRINTER TO M:\TEMP\&V_DISACCO
  SET DEVICE  TO PRINTER
  ************************
  *  INICIO DEL LISTADO  *
  ************************
  SCATTER MEMVAR
  V_TOTENT = 0  									&& CANTIDAD DE MENSAJES.
  DO TITUMENSA
  DO WHILE .T.
     *********************
     *  ACUMULA TOTALES  *
     *********************
     V_TOTENT = V_TOTENT + 1       					&& CANTIDAD DE MENSAJES.
     SELEC MENMOT_M
     SET ORDER TO MENMOT_COD
     GO TOP
     SEEK (M.MENSAJ_MOT)
     *******************
     *  IMPRIME LINEA  *
     *******************
     @PROW()+1,01 SAY M.MENSAJ_PRO + ": " + TRAN(M.MENSAJ_CON,[999999]) + " " + M.MENSAJ_NOM + " "
     IF M.MENSAJ_URG = "SI"
        @PROW(),PCOL() SAY "! URGENTE ! "
     ELSE
        @PROW(),PCOL() SAY "            "
     ENDIF
     IF (DATE() - M.MENSAJ_FEN) > 1
        @PROW(),PCOL() SAY "ATRASO:" + TRAN((DATE() - M.MENSAJ_FEN),[999]) + " DIAS"
     ELSE
        @PROW(),PCOL() SAY "              "
     ENDIF
     @PROW(),PCOL()    SAY "(" + TRAN(M.MENSAJ_ENC,[99]) + ")"
     @PROW()+1,01      SAY "CALLE: " + ALLT(M.MENSAJ_DIR) + " " + M.MENSAJ_INT
     IF LEN(ALLT(M.MENSAJ_OB1)) # 0
        @PROW()+1,01   SAY SPAC(05) + M.MENSAJ_OB1
     ENDIF
     IF LEN(ALLT(M.MENSAJ_OB2)) # 0
        @PROW()+1,01   SAY SPAC(05) + M.MENSAJ_OB2
     ENDIF
     IF LEN(ALLT(M.MENSAJ_OB3)) # 0
        @PROW()+1,01   SAY SPAC(05) + M.MENSAJ_OB3
     ENDIF
     IF LEN(ALLT(M.MENSAJ_OB4)) # 0
        @PROW()+1,01   SAY SPAC(05) + M.MENSAJ_OB4
     ENDIF
     SELEC TIPENT_M
     IF M.MENSAJ_L01 = "SI"
        GO 1
        @PROW()+1,01     SAY SPAC(17) + TIPENT_CON + " " + M.MENSAJ_O01
     ENDIF
     IF M.MENSAJ_L02 = "SI"
        GO 2
        @PROW()+1,01     SAY SPAC(17) + TIPENT_CON + " " + M.MENSAJ_O02
     ENDIF
     IF M.MENSAJ_L03 = "SI"
        GO 3
        @PROW()+1,01     SAY SPAC(17) + TIPENT_CON + " " + M.MENSAJ_O03
     ENDIF
     IF M.MENSAJ_L04 = "SI"
        GO 4
        @PROW()+1,01     SAY SPAC(17) + TIPENT_CON + " " + M.MENSAJ_O04
     ENDIF
     IF M.MENSAJ_L05 = "SI"
        GO 5
        @PROW()+1,01     SAY SPAC(17) + TIPENT_CON + " " + M.MENSAJ_O05
     ENDIF
     IF M.MENSAJ_L06 = "SI"
        GO 6
        @PROW()+1,01      SAY SPAC(17) + TIPENT_CON + " " + M.MENSAJ_O06
     ENDIF
     IF M.MENSAJ_L07 = "SI"
        GO 7
        @PROW()+1,01     SAY SPAC(17) + TIPENT_CON + " " + M.MENSAJ_L07
     ENDIF
     IF M.MENSAJ_L08 = "SI"
        GO 8
        @PROW()+1,01     SAY SPAC(17) + TIPENT_CON + " " + M.MENSAJ_L08
     ENDIF
     IF M.MENSAJ_L09 = "SI"
        GO 9
        @PROW()+1,01     SAY SPAC(17) + TIPENT_CON + " " + M.MENSAJ_L09
     ENDIF
     IF M.MENSAJ_L10 = "SI"
        @PROW()+1,01     SAY SPAC(17) + TIPENT_CON + " " + M.MENSAJ_L10
     ENDIF
     SELEC MENSAJ_M
     IF .NOT. EOF()
        SKIP
        SCATTER MEMVAR
     ENDIF
     IF V_NROCTA # M.MENSAJ_TIT .OR. EOF()
        EXIT
     ENDIF
  ENDDO
  @PROW()+1,01 SAY  REPLI("O",79)
  @PROW()+1,01 SAY  "O" + REPLI("O",77) + "O"
  @PROW()+1,01 SAY  "O" + SPAC(25) + "** " + "TOTAL DE ENTREGAS:" + TRAN(V_TOTENT,[9999]) + " **" + SPAC(24) + "O"
  @PROW()+1,01 SAY  "O" + REPLI("O",77) + "O"
  SET DEVICE  TO SCREEN
  *SET PRINTER TO PRN
  SET CONSOLE ON
  *MODIFY COMM M:\-DATOS\SICMADAT\CCE\&V_DISACCO NOEDIT WINDOW ACTICOBRO
  *DELETE FILE M:\-DATOS\SICMADAT\CCE\&V_DISACCO
  MODIFY COMM M:\TEMP\&V_DISACCO NOEDIT WINDOW ACTICOBRO
  DELETE FILE M:\TEMP\&V_DISACCO

  SET COLOR TO I
  RESTORE SCREEN FROM PANTA07
  DO FINMENSAJE
  RETURN

* TITULO DEL LISTADO DE MENSAJES.
  PROCEDURE TITUMENSA
     @01,01       SAY "O"   + REPLI("O",77) + "O"
     @PROW()+1,01 SAY "O  " + "ENTREGAS A CLIENTE  AL " + DTOC(DATE()) + SPAC(32)
     @PROW()+1,01 SAY "O  " +  "TODOS LOS MENSAJEROS"   + SPAC(55) + "O"
     @PROW()+1,01 SAY "O" + REPLI("O",77) + "O"
  RETURN

* FINALIZAR MENSAJES
  PROCEDURE FINMENSAJES
     SELEC MENMEN_M
     USE
     SELEC MENMOT_M
     USE
     SELEC MENENC_M
     USE
     SELEC MENSAJ_M
     USE
     SELEC TIPENT_M
     USE
     DO TECLA_PAGO
  RETURN

*****************************************************
* VERIFICA SI EL CLIENTE TIENE AVISOS DE COBRADORES *
*****************************************************
PROCEDURE AVISOCOBRA
  SELEC 19
  USE AVISOS_M
  SET ORDER TO AVISOS_NUM
  SELEC 20
  USE CODAVI_M

  ****************************************************
  *  VERIFICA SI EL CLIENTE TIENE AVISOS PENDIENTES  *
  ****************************************************
  SELEC AVISOS_M
  SET ORDER TO AVISOS_TIT
  GO TOP
  SEEK (V_CODSUC + TRANSFORM(V_NROCTA,[999999]))
  IF EOF()
     WAIT " ESTE CLIENTE NO TIENE AVISOS PENDIENTES " WINDOW
     DO FINAVISOS
     RETURN
  ENDIF

  STORE USUARIOS + [.TXT] TO V_DISACCO
  SAVE SCREEN TO PANTA07
  *SET PRINTER TO M:\-DATOS\SICMADAT\CCE\&V_DISACCO
  SET PRINTER TO M:\TEMP\&V_DISACCO
  SET DEVICE  TO PRINTER

  ************************
  *  INICIO DEL LISTADO  *
  ************************
  V_TOTAVI = 0  							&& TOTAL DE AVISOS.
  V_TOTACO = 0  							&& TOTAL A COBRAR.

  SCATTER MEMVAR
  DO TITUAVISO
  DO WHILE .T.
     *********************
     *  ACUMULA TOTALES  *
     *********************
     V_TOTAVI = V_TOTAVI + 1   			    	&& TOTAL DE AVISOS.
     V_MONACO = M.AVISOS_PSM + M.AVISOS_UDS + M.AVISOS_CMS + M.AVISOS_PSV + M.AVISOS_OTR
     V_TOTACO = V_TOTACO + V_MONACO         	&& TOTAL A COBRAR.
     *******************
     *  IMPRIME LINEA  *
     *******************
     @ PROW()+1,01 SAY DTOC(M.AVISOS_FCA)+SPACE(01)+M.AVISOS_PRO+": "+TRAN(M.AVISOS_CON,"999999")+" "+M.AVISOS_NOM
     @ PROW()  ,PCOL() SAY SPAC(08)+" "+M.AVISOS_COB + " (" + TRAN(M.AVISOS_NUM,"999999") + ")"
     @ PROW()+1,01 SAY "  DIRECCION...: " + AVISOS_DIR
     @ PROW()+1,01 SAY "  INTERSECCION: " + AVISOS_INT
     IF M.AVISOS_PSM # 0
        @ PROW()+1,01 SAY SPAC(07) + "PSM " + TRAN(M.AVISOS_PSM,"999,999,999") + " " + M.AVISOS_OSM
     ENDIF
     IF M.AVISOS_UDS # 0
        @ PROW()+1,01 SAY SPAC(07) + "UDS " + TRAN(M.AVISOS_UDS,"999,999,999") + " " + M.AVISOS_ODS
     ENDIF
     IF M.AVISOS_CMS # 0
        @ PROW()+1,01 SAY SPAC(07) + "CMS " + TRAN(M.AVISOS_CMS,"999,999,999") + " " + M.AVISOS_OMS
     ENDIF
     IF M.AVISOS_PSV # 0
        @ PROW()+1,01 SAY SPAC(07) + "PSV " + TRAN(M.AVISOS_PSV,"999,999,999") + " " + M.AVISOS_PSV
     ENDIF
     IF M.AVISOS_OTR # 0
        @ PROW()+1,01 SAY SPAC(07) + "OTR " + TRAN(M.AVISOS_OTR,"999,999,999") + " " + M.AVISOS_OBS
     ENDIF
     @ PROW(),PCOL()+1 SAY TRAN(V_MONACO,"999,999,999")
     @ PROW()+1,01     SAY REPLI("O",79)
     @ PROW()+1,01     SAY REPLI("O",79)

     SELEC AVISOS_M

     IF .NOT. EOF()
        SKIP
        SCATTER MEMVAR
     ENDIF
     IF V_NROCTA # M.AVISOS_TIT .OR. EOF()
        EXIT
     ENDIF
  ENDDO
  @ PROW()+1,01 SAY "O" + REPLI("O",78) + "O"
  @ PROW()+1,01 SAY "O" + SPAC(11) + "** " + "TOTAL DE AVISOS:" + TRAN(V_TOTAVI,"9999") + " ** " + "TOTAL A COBRAR:"
  @ PROW(),PCOL()+1 SAY TRAN(V_TOTACO,"999,999,999") + " **" + SPAC(10) + "O"
  @ PROW()+1,01 SAY  "O" + REPLI("O",78) + "O"
  SET DEVICE  TO SCREEN
  SET PRINTER TO PRN
  SET CONSOLE ON

  *MODIFY COMM M:\-DATOS\SICMADAT\CCE\&V_DISACCO NOEDIT WINDOW ACTICOBRO
  *DELETE FILE M:\-DATOS\SICMADAT\CCE\&V_DISACCO
  MODIFY COMM M:\TEMP\&V_DISACCO NOEDIT WINDOW ACTICOBRO
  DELETE FILE M:\TEMP\CCE\&V_DISACCO

  SET COLOR TO I
  RESTORE SCREEN FROM PANTA07
  DO FINAVISOS
RETU

* TITULO DEL AVISOS.
  PROCEDURE TITUAVISO
     @ 01,01       SAY "O"   + REPLI("O",77) + "O"
     @ PROW()+1,01 SAY "O  " + "AVISOS POR CLIENTES AL " + DTOC(DATE()) + SPAC(30) + "            O"
     @ PROW()+1,01 SAY "O  " + "CLIENTE: " + M.AVISOS_PRO + TRAN(M.AVISOS_CON," 999999") + " "+ M.AVISOS_NOM + SPAC(20) + "O"
     @ PROW()+1,01 SAY "O" + REPLI("O",77) + "O"
  RETURN

* FINALIZAR AVISOS
  PROCEDURE FINAVISOS
     SELEC AVISOS_M
     USE
     DO TECLA_PAGO
  RETURN

* RUTINA DE VALIDACION DE DERECHOS.
  PROCEDURE VAL_DER
  PARAMETER CLAVEUSU,CAPITULO,DERECHOS
  STORE ALLTRIM(CLAVEUSU)+ALLTRIM(CAPITULO)+ALLTRIM(DERECHOS) TO PETICION
  STORE SPACE(02) TO HABILITA
  SELECT DERECH_M
  SEEK PETICION
  IF EOF()
     STORE [NO] TO HABILITA
     WAIT [ATENCION !!, USUARIO NO HABILITADO COMO CAJERO ] WINDOW
  ENDIF
  RETURN

 * RUTINA DE VALIDACION DE CTA. BANCARIA P/DEPOSITOS
   PROCEDURE VALICTABAN
      IF V_CTABAN = SPACE(10)
         WAIT "ATENCION !!!, NO HA SELECCIONADO CTA. BANCARIA P/DEPOSITO" WINDOW
         V_FLAK01 = "F"
         RETURN
      ENDIF
      V_FLAK01 = "T"
   RETURN

* RUTINA AUXILIAR TEMPORAL
  PROCEDURE AUXITEMP
       IF V_FECAUX <> CTOD([//])
           V_FECALT = CTOD([//])
           V_FECCOB = V_FECAUX
           V_FECAUX = CTOD([//])
       ENDIF
  RETURN

* FIN


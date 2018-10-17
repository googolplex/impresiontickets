*  PROGRAMA...: CBCOBVED.PRG (viene de CBCODVEG.prg)
*  OBJETIVO...: ACTUALIZA PAGOS EN VENTANILLA
*  FECHA......: 12/11/1997
*  ULT.MODIF..: 02/12/2013
*  AUTOR......: CARLOS LAMAS
*  CAMBIADO POR ROGER EL 8.5.2006
*  MODIFICADO POR CARLOS VALENTE 02-12-2013
*  MODIFICADO POR ROGER ARMOA 15.10.2018

*  SETEA AMBIENTE:
   set dele on
   set conf on
   set inte off
   set talk off
   set excl off
   set date brit
   set cent on
   set esca off
   set inte on
   program="cbcobved"
   @01,03 say program
   set color to i

*  DEFINE VARIABLES:
   v_feccob = date()                    && fecha de cobranza
   v_fectop = date()                    && fecha tope p/armar saldos
   v_codsuc = "01"                      && codigo de sucursal
   v_codpro = space(3)                  && codigo de producto
   v_nrocta = 0                         && NRO de cuenta
   v_nrocon = 0                         && NRO de contrato
   v_nomtit = []                        && nombre del titular
   v_nomben = []                        && nombre de beneficiario
   v_nrotal = 0                         && NRO de talon chequera
   v_cititu = []                        && NRO de C.I. de titular
   v_ructit = []                        && NRO de R.U.C. de titular
   v_tipacc = []                        && tipo de acceso
   v_canfil = 0                         && cantidad de filas en array
   v_cancon = 0                         && cantidad de contratos
   v_totido = 0                         && total de cuotas vencidas
   v_totcer = 0                         && total de cuotas a vencer
   v_totral = 0                         && total general
   v_totrgo = 0                         && total recargos
   v_subnto = 0                         && subtotal de cuotas p/contrato
   v_subrgo = 0                         && subtotal de recargos p/contrato
   v_subtal = 0                         && subtotal de totales p/contrato
   v_nrofac = 0                         && NRO de factura
   v_tipfac = []                        && tipo de factura
   v_serfac = []                        && serie de factura
   v_canval = 0                         && cantidad de valores de cobro
   v_codpro = []                        && codigo de producto o cenope
   v_totfac = 0                         && total de factura
   v_preiva = 0                         && total sin IVA
   v_valiva = 0                         && valor IVA
   v_imes   = []                        && nombre del mes de cobro
   valnel   = []                        && monto factura en letras
   v_nroci  = 0                         && NRO de cedula del titular
   v_nomben = []                        && nombre de beneficiario
   v_flak01 = []                        && bandera de control
   v_arcaux = []                        && nombre de archivo temporal
   v_codcaj = []                        && codigo de cajero
   v_vencim = []                        && variable aux. p/imprimir factura
   v_dmes   = []                        && variable aux. p/imprimir factura
   v_linea  = 0                         && variable aux. p/imprimir factura
   v_fecalt = ctod([//])                && fecha aux. alternativa de cobranza
   v_fecaux = ctod([//])                && fecha aux. de cobranza
   v_califa = 9                         && lineas de detalles por factura
   v_linfac = 0                         && auxiliar lineas de detalles por factura
   v_forpag = [         ]               && forma de pago  Mensual, Anual, Semestral
   v_detalle = []                       && detalle de imprecion de factura
   v_preuni = 0                         && precio unitario p/impresion de factura
   v_subtot = 0                         && subtotal de item p/impresion de factura
   v_totdescue = 0                      && total descuento en factura

*  DEFINE MATRICES Y VECTORES
   dimension estadcta(300,17)
   dimension saldoact(100,09)
   dimension detavalo(100,07)

*  DEFINE VENTANAS:
   define window interior  from 07,01 to 17,78 none color i
   define window pidepago  from 02,01 to 23,78 none color i
   define window titulares from 10,05 to 21,77 Title " " color i,,i,i,,i,
*  define window desvalcob from 07,01 to 18,78 none color i
*  define window mensajes  from 06,04 to 11,51 Title [ADVERTENCIA] color scheme 5
   define window acticobro from 05,01 to 17,78 none color scheme 5
   define window solmodif  from 06,04 to 23,79 Title [ SOLICITUD DE MODIFICACION DE DATOS ] color scheme 5
   define window tipomodi  from 10,27 to 12,50 Title [Que desea?] color i,i,i,i
   define window confirma  from 20,27 to 22,50 Title [Confirma?] color i,i,i,i
   define window estadcta  from 01,00 to 24,79 double color i,,i,i,,i,
   define window busqueda  from 16,06 to 22,76 Title [ BUSQUEDA ] color scheme 5
   define window detacobro from 12,05 to 22,75 Title [ DETALLE DE VALORES ]  color scheme 5
   define window con_tar   from 17,27 to 22,60 Title [ TARGETAS DE CREDITO ] double grow
   define window obser1    from 02,01 to 06,78 Title [Observaciones ] double
   define window obscajero from 07,01 to 23,78 Title [Detalle de Observaciones ] double Foot "<ESC> para Salir "

*  SELECCION DE ARCHIVOS:
   &&selec 1
   &&use client_m order titula
   selec 2
   use saldos_m order ias003 shared
   selec 3
   use modifi_m order modifi_num
   selec 4
   use talona_m order talona_pee
   selec 5
   use funcio_m order funcio_num
   selec 6
   use bancos_m order i01b01
   selec 7
   use derech_m order derech_hab
   selec 8
   use sucurs_m order sucursal
   selec 9
   use tarjet_m order tarjet_cod
   selec 10
   use movcaj_m order movcaj_num
   selec 11
   use valcaj_d order valcaj_fac
   selec 12
   use contra_m order con011
   &&set relation to (padr(contra_suc,2) + transform(contra_tit,[999999])) into client_m additive
   selec 13
   use benefi_d order benefi_con
   set relation to (benefi_pro + transform(benefi_con,[999999]));
                into contra_m additive
   selec 14
   use auxcob_d order auxcob_001
   selec 15
   use cobrad_m order cobrad_per
   sele 16
   use obscaj_d orde cuenta
   set filt to obscaj_fin=ctod('//')

*  CICLO PRINCIPAL:
   save screen to panta1
   store "t" to v_flak01
   do val_der with claveusu,capitulo,[Agregar]
   do valicaje
   do valictaban
   do while v_flak01 = "t" .and. habilita = space(02)
      @	06,04 clear to 23,78
      @ 06,04 to 23,78
      @ 06,05 say "Cobros en Ventanilla"
      do valifact
      if v_flak01 = "f"
         exit
      endif

******* Temporal para exonerar cuotas ******

      do auxitemp

******       Fin Temporal             ******

      do pantaini
      do pideinic
      if lastkey() = 27
         exit
      endif
      do while v_flak01 = "t"
          do eligeacc
          if lastkey() = 27
             exit
          endif
          if v_nrocta > 0 .and. v_nomtit <> space(35)
             do armaobsc
             do armaesta
          endif
          if v_totral > 0
             do despesta
          endif
          do limpiavar

******* Temporal para exonerar cuotas ******

      do auxitemp

******       Fin Temporal             ******

          on key
          do valifact
      enddo
      on key
   enddo
   restore screen from panta1
   close all
   return

*  ARMA OBSERVACIONES PARA EL CAJERO:
   proc armaobsc
   sele 16
   use obscaj_d orde cuenta
   set filt to obscaj_fin=ctod('//')
   go top
   seek tran(v_nrocta,"999999")
   if not eof()
      acti wind obser1
      @ 00,01 say [Mensaje para:]+[ ]+usuarios
      @ 01,01 say [La Cuenta NRO]+tran(v_nrocta," 999999")+[ perteneciente a: ]+trim(v_nomtit)
      @ 02,01 say [registra las siguientes observaciones:]
      do whil .t.
         acti wind obscajero
         brow key tran(v_nrocta,"999999"),tran(v_nrocta,"999999");
         fields;
         obscaj_fec     :h="Fecha",;
         obscaj_det     :h="Observacion",;
         obscaj_nom     :h="Operador";
         nomo;
         node;
         noap;
         nolg;
         norg;
         in wind obscajero
         if last()=27
            exit
         endi
      endd
   endi
   deac wind obscajero, obser1
   set filt to
   use
   retu

*  VALIDA NUMERO DE FACTURA
   PROCEDURE VALIFACT
   set exact off
   set near  off
   select talona_m
   go top
   seek trim(usuarper)+trim([EN USO])+trim([CONTADO])
   if eof()
      go top
      seek trim(usuarper)+trim([ENTREGADO])+trim([CONTADO])
      if eof()
         wait [No existe talonario habilitado para este usuario] window
         store "f" to v_flak01
         return
      else
         if talona_ult = 0
            store talona_des   to v_nrofac
         endif
      endif
   else
      if talona_ult < talona_has
            store talona_ult+1  to v_nrofac
      else
         go top
         seek trim(usuarper)+trim([ENTREGADO])+trim([CONTADO])
         if eof()
            wait [No existe talonario habilitado para este usuario] window
            store "f" to v_flak01
            return
         endif
         store talona_des      to v_nrofac
      endif
   endif
   store talona_tip            to v_tipfac
   store talona_ser            to v_serfac
   RETURN

*  VALIDA CAJERO CON EL USUARIO
   PROCEDURE VALICAJE
   select cobrad_m
   seek (usuarper)
   if .not. found()
*     seek trim(usuarper)+trim([ENTREGADO])
*     if eof()
         wait [ATENCION !!, USUARIO NO HABILITADO COMO COBRADOR ] window
         store    "f"     to v_flak01
         return
*     endif
   endif
   store cobrad_num to v_codcaj
   RETURN

* PANTALLA INICIAL
  PROCEDURE PANTAINI
     @ 07,06 SAY "Fecha de cobro......:"
     @ 07,45 SAY "Sucursal:"
     @ 08,06 SAY "Usuario.............: " + usuarios
     @ 08,40 SAY "Fecha alternativa...: "
  RETURN

* PIDE DATOS INICIALES
  PROCEDURE PIDEINIC
      @07,28 get v_feccob valid ! empty(v_feccob) error[NO PUEDE SER VACIO]
      &&@07,28 SAY v_feccob &&valid ! empty(v_feccob) error[NO PUEDE SER VACIO]
      @07,54 get v_codsuc valid ! empty(v_codsuc) error[NO PUEDE SER VACIO]
      @08,61 get v_fecalt
      READ
*     @07,28 say dtoc(v_feccob)
*     @07,54 say transform(v_codsuc,[99])
  RETURN

* ELIGE OPCION DE ACCESO
  PROCEDURE ELIGEACC
     @09,06 say "Ingresar por........:"
     @11,20 to 18,55
     @12,23 prompt [\<a - Nro. de contrato        ]
     @13,23 prompt [\<b - Nro. de cuenta          ]
     @14,23 prompt [\<c - Nro. de talon chequera  ]
     @15,23 prompt [\<d - Nro. de C.I. del titular]
     @16,23 prompt [\<e - Nombre del titular      ]
     @17,23 prompt [\<f - Nombre de beneficiario  ]
     store 1 to opcionacc
     menu    to opcionacc
     do case
        case opcionacc = 1
             do innrocon
        case opcionacc = 2
             do innrocta
        case opcionacc = 3
             do innrotal
        case opcionacc = 4
             do innroci
        case opcionacc = 5
             do innomtit
        case opcionacc = 6
             do innomben
     endcase
   RETURN

*  BUSQUEDA POR NRO. CTA.
   PROCEDURE INNROCTA
      @11,20 clear to 18,55
      @11,20 to 18,55
      do limpiavar
      do while lastkey() <> 27
         @13,22 say "Nro. de cuenta que desea:";
                get v_nrocta picture "999999" valid ! empty(v_nrocta) error [NO PUEDE SER CERO]
         read
         if v_nrocta > 0
         && set exact on
            select client_m
            set order to titula
            go top
            seek (padr(v_codsuc,2)+transform(v_nrocta,"999999"))
            if found()
               if client_est <> "B"
                  store client_tit to v_nrocta
                  store client_nom to v_nomtit
                  store contra_m.contra_nom to v_nomtit
                  store contra_m.contra_ruc to v_ructit
                  exit
               else
                  wait [Esta cuenta fue dado de baja el doa ] + dtoc(client_baj) + [, verifique !!] window
                  do limpiavar
               endif
            else
               if v_nrocta > 0
                  wait [ No existe ninguna cuenta con ese NROmero, favor verifique !!] window
               endif
               do limpiavar
            endif
         endif
      enddo
      @11,20 clear to 18,55
   RETURN

*  BUSQUEDA POR NRO. CONTRATO
   PROCEDURE INNROCON
      @11,20 clear to 18,58
      @11,20 to 18,58
      do limpiavar
      do while lastkey() <> 27
         @13     ,22 say "Producto que desea........:";
                     get v_codpro picture[@m PSM,PSV,UDS,CMS,SDS,INH,SDT,OTD,SDC,PSC,PSI]
         @row()+1,22 say "Nro. de contrato que desea:";
                     get v_nrocon picture "999999" valid ! empty(v_nrocon) error [NO PUEDE SER CERO]
         read
         if v_nrocon > 0
         && set exact on
            select contra_m
            set order to contrato
            go top
            seek (v_codsuc + v_codpro + transform(v_nrocon,[999999.99]))
            if found()
               if contra_est = "V"

                  if contra_aso > 0 .and. contra_est <> "B"
                     wait [Este contrato se paga por ASOC. EMPLEADOS, verifique !!] window
                  endif

                  store contra_num to v_nrocon
                  store contra_cen to v_codpro
                  store contra_tit to v_nrocta
                  store contra_nom to v_nomtit
                  store contra_ruc to v_ructit
                  do valisalcon
                  exit
               else
                  if contra_est = "B"
                     wait [Este contrato fue dado de baja el doa ] + dtoc(contra_baj) + [, verifique !!] window
                  endif
                  do limpiavar
               endif
            else
               if v_nrocon > 0
                  wait [ No existe ningun contrato con ese NROmero, favor verifique !!] window
               endif
               do limpiavar
            endif
         endif
         enddo
      @11,20 clear to 18,58
   RETURN

*  BUSQUEDA POR NRO. TALON DE CHEQUERA
   PROCEDURE INNROTAL
      @11,20 clear to 18,58
      @11,20 to 18,58
      do limpiavar
      do while lastkey() <> 27
         @13,22 say "Ingrese NRO de talon a cobrar:";
                get v_nrotal picture "999999" valid ! empty(v_nrotal) error [NO PUEDE SER CERO]
         read
         if v_nrotal > 0
        &&  set exact on
            select saldos_m
            set order to saldos_hoj
            go top
            seek (transform(v_nrotal,[999999]))
            if found()
               store saldos_num to v_nrocon
               store saldos_pro to v_codpro
               store saldos_tit to v_nrocta

               select contra_m
               set order to contra_001
               seek (v_codpro + transform(v_nrocon,[999999]))
               if found()
                  if contra_est = "V"
                     select saldos_m
                     if saldos_aso = 0 .and. saldos_est = " "
                    &&  set exact on
                        select client_m
                        set order to titula
                        go top
                        seek (padr(v_codsuc,2)+transform(v_nrocta,"999999"))
                        if found()
                           store client_nom to v_nomtit
                           store contra_m.contra_nom to v_nomtit
                           store contra_m.contra_ruc to v_ructit
                           exit
                        else
                           do limpiavar
                        endif
                     else
                        if saldos_est = "B"
                           wait [Este contrato fue dado de baja el doa ] + dtoc(contra_baj) + [, verifique !!] window
                        endif
                        if saldos_aso > 0 .and. saldos_est <> "B"
                           wait [Este contrato se paga por ASOC. EMP. NRO] + alltrim(saldos_aso) + [, verifique !!] window
                        endif
                        do limpiavar
                     endif
                  else
                     if contra_est = "B"
                        wait [Este contrato fue dado de baja el doa ] + dtoc(contra_m.contra_baj) + [, verifique !!] window
                     endif
                     do limpiavar
                  endif
               endif
            else
               if v_nrocon > 0
                  wait [ No existe ningun talon con ese NROmero, favor verifique !!] window
               endif
               do limpiavar
            endif
         endif
      enddo
      @11,20 clear to 18,58
   RETURN

*  BUSQUEDA POR NRO. C.I. DEL TITULAR
   PROCEDURE INNROCI
      @11,20 clear to 18,55
      @11,20 to 18,55
      do limpiavar
      do while lastkey() <> 27
         @13,22 say "Nro. de C.I. que desea:";
                get v_nroci  picture "99999999" valid ! empty(v_nroci) error [NO PUEDE SER CERO]
         read
         if v_nroci > 0
        &&  set exact on
            select client_m
            set order to client_ndo
            go top
            seek (padr(v_codsuc,2) + transform(v_nroci,"99999999"))
            if found()
               if client_est <> "B"
                  store client_tit to v_nrocta
                  store client_nom to v_nomtit
                  store contra_m.contra_nom to v_nomtit
                  store contra_m.contra_ruc to v_ructit
                  exit
               else
                  wait [Este contrato fue dado de baja el doa ] + dtoc(client_baj) + [, verifique !!] window
                  do limpiavar
               endif
            else
               if v_nroci > 0
                  wait [ No existe ningun titular con ese NRO de C.I., favor verifique !!] window
               endif
               do limpiavar
            endif
         endif
      enddo
      @11,20 clear to 18,55
   RETURN

*  BUSQUEDA POR NOMBRE DEL TITULAR
   PROCEDURE INNOMTIT
      save screen to panta5
      set colo to i
      @ 11,05 clea to 22,76
      set colo to w+/b
      @ 22,07 say [ <F2>Buscar     <F3>Actualizaciones     <Esc>Cancelar ]
      set colo to i
      &&sele client_m
      &&set orde to client_nom
      select contra_m
      set orde to nombre
      go top
      do limpiavar
      do tecla_cliente
      acti window titulares
      if .not. eof()
         brow fiel;
         contra_nom      :H="Apellido, Nombre" :35    ,;
         contra_ndo      :H="Identidad"  :P=[99999999],;
         contra_tit      :H="NRO cuenta"  :P=[999999]   ;
         nomodify			 						   ;
         noappend									   ;
         nodelete									   ;
         norgrid									   ;
         in window titulares
         if lastkey() <> 27
            if contra_est <> "B"
               store contra_m.contra_nom to v_nomtit
               store contra_tit to v_nrocta
               store contra_ruc to v_ructit
            else
               wait [ Esta cuenta fue dado de baja el doa ] + dtoc(contra_baj) + [ , favor verifique !!! ] window nowait
               do limpiavar
            endif
         endif
      else
         wait [ No existe ningun titular con ese nombre, favor verifique !!! ] window nowait
         if lastkey() = 27
            exit
         endif
      endif
      set order to nombre
      deactivate window titulares
      restore screen from panta5
      set color to I
      do tecla_pago
   RETURN

*  BUSQUEDA POR NOMBRE DEL BENEFICIARIO
   PROCEDURE INNOMBEN
      save screen to panta5
      set color to i
      @ 11,05 clear to 22,76
      set color to w+/b
      @ 22,15 say [ <F2>Buscar     <F3>Actualizaciones     <Esc>Cancelar ]
      set color to i
      select benefi_d
      set order to benefi_nom
      go top
      do limpiavar
      do tecla_benefi
      activate window titulares
      if .not. eof()
         browse fields;
         benefi_nom      :H="Apellido, Nombre" :35    ,;
         benefi_ndo      :H="Identidad"  :P=[99999999],;
         benefi_pro      :H=" "                       ,;
         benefi_con      :H="Contrato "  :P=[999999]   ;
         nomodify									   ;
         noappend									   ;
         nodelete									   ;
         norgrid									   ;
         in window titulares
         if lastkey() <> 27
            if  contra_m.contra_aso = 0   .and. ;
                contra_m.contra_est = "V"
                if benefi_est <> "B"
                   store contra_m.contra_tit to v_nrocta
                   store client_m.client_nom to v_nomtit
                   store contra_m.contra_nom to v_nomtit
                   store contra_m.contra_ruc to v_ructit
                   store benefi_pro          to v_codpro
                   store benefi_con          to v_nrocon
                else
                   if contra_m.contra_est = "B"
                       wait [Este beneficiario fue dado de baja el doa ] + dtoc(benefi_baj) + [, verifique !!] window
                   endif
                endif
            else
               if contra_m.contra_est = "B"
                    wait [Este contrato fue dado de baja el doa ] + dtoc(contra_m.contra_baj) + [, verifique !!] window
               endif
               if  contra_m.contra_aso > 0 .and. contra_m.contra_est <> "B"
                    wait [Este contrato se paga por ASOC. EMPLEADOS, verifique !!] window
               endif
               do limpiavar
            endif
         endif
      else
         wait [ No existe ningun beneficiario con ese nombre, favor verifique !!! ] window nowait
         if lastkey() = 27
            exit
         endif
      endif
      set order to benefi_nom
      deactivate window titulares
      restore screen from panta5
      set color to I
      do tecla_pago
   RETURN

*  CARGA ESTADO DE CUENTA DEL CLIENTE EN LA MATRIZ POR
   PROCEDURE ARMAESTA

   for i = 1 to 300

       store   0        to estadcta(i,01)     && posicion fisica registro saldos_m
       store space(03)  to estadcta(i,02)     && codigo del producto
       store   0        to estadcta(i,03)     && nro. contrato
       store space(05)  to estadcta(i,04)     && nro. cuota
       store ctod([//]) to estadcta(i,05)     && fecha vencimiento cuota
       store   0        to estadcta(i,06)     && cant. dias de atraso
       store   0        to estadcta(i,07)     && saldo de cuota
       store   0        to estadcta(i,08)     && monto recargo
       store   0        to estadcta(i,09)     && monto cuota + recargo
       store   0        to estadcta(i,10)     && acumulado a pagar
       store   0        to estadcta(i,11)     && numero de talon
       store   0        to estadcta(i,12)     && monto interes cobrado
       store   0        to estadcta(i,13)     && monto cuota cobrado
       store   0        to estadcta(i,14)     && monto cuota original
       store   0        to estadcta(i,15)     && nro. de factura asignada
       store ctod([//]) to estadcta(i,16)     && periodo inicial cms.
       store ctod([//]) to estadcta(i,17)     && periodo final cms.

   endfor

   for i = 1 to 100

       store space(03)  to saldoact(i,01)     && codigo producto
       store   0        to saldoact(i,02)     && nro. contrato
       store   0        to saldoact(i,03)     && monto cuota sin recargo
       store   0        to saldoact(i,04)     && monto a cobrar
       store   0        to saldoact(i,05)     && monto cobrado
       store   0        to saldoact(i,06)     && monto cuota con recargo
       store space(03)  to saldoact(i,07)     && marca de forma de pago
       store   0        to saldoact(i,08)     && monto de descuento p/Sem. o Anu
       store   0        to saldoact(i,09)     && porcentaje p/recargo por producto

       store space(03)  to detavalo(i,01)     && codigo del valor
       store   0        to detavalo(i,02)     && monto  del valor
       store ctod([//]) to detavalo(i,03)     && fecha de vencimiento
       store space(16)  to detavalo(i,04)     && nro. del valor
       store space(02)  to detavalo(i,05)     && codigo entidad emisora
       store space(02)  to detavalo(i,06)     && codigo tarjeta de credito
       store   0        to detavalo(i,07)     && cantidad de facturas utilizadas

   endfor

**************** Temporal para exonerar recargos **********

   if v_fecalt <> ctod([//])
      store v_feccob to v_fecaux
      store v_fecalt to v_feccob
   endif

***************  Fin Temporal  *****************************

   store  0            to v_nrocon
   store "   "         to v_codpro
   store v_feccob + 30 to v_fectop
   set exact off
   set near  on
   select saldos_m
   set order to sal001
   go top
   seek (v_codsuc+transform(v_nrocta,"999999")+[   ]+transform(000000,"999999")+dtos(v_feccob))
   if .not. eof() .and. saldos_tit = v_nrocta
      do while v_nrocta = saldos_tit
         if (transform(year(saldos_ven),"9999")+transform(month(saldos_ven),"99"));
         <= (transform(year(v_fectop),"9999")  +transform(month(v_fectop),"99")) .and.;
            (saldos_hab - saldos_deb) < 0 .and. saldos_est <> "B"

            store (v_canfil + 1)    to v_canfil
            if saldos_pro  <>  v_codpro  .or.  saldos_num  <>  v_nrocon
               if v_canfil > 1
                  stor v_codpro   to estadcta(v_canfil,02)
                  stor v_nrocon   to estadcta(v_canfil,03)
                  stor v_subnto   to estadcta(v_canfil,07)
                  stor v_subrgo   to estadcta(v_canfil,08)
                  stor v_subtal   to estadcta(v_canfil,09)
                  stor   0        to v_subnto, v_subrgo, v_subtal
                  v_canfil = v_canfil + 1
               endif
               v_cancon = v_cancon + 1
               store saldos_pro     to estadcta(v_canfil,02),;
                v_codpro,;
 								       saldoact(v_cancon,01)
               store saldos_num     to estadcta(v_canfil,03),;
					                   v_nrocon			    ,;
 								       saldoact(v_cancon,02)

*********** BUSCA INTERES P/CALCULO DE RECARGO POR MORA ***********
               sele contra_m
               set orde to sucursal
               seek v_codsuc+v_codpro+tran(v_nrocon,"999999")
               if found()
                  if contra_int > 0
                     store contra_int to saldoact(v_cancon,09)
                  else
                      wait "ATENCION !!!, El " + v_codpro + " " + transform(v_nrocon,"999999") + " no tiene cargado interes p/mora" window
                  endif
               endif
               sele saldos_m

***********          FIN DE BUSQUEDA DE INTERES P/MORA          *********

			endif
            estadcta(v_canfil,01)  =  recno()
            estadcta(v_canfil,02)  =  saldos_pro
            estadcta(v_canfil,03)  =  saldos_num
            estadcta(v_canfil,04)  =  saldos_ncu
            estadcta(v_canfil,05)  =  saldos_ven
            estadcta(v_canfil,06)  =  v_feccob   - saldos_ven
            estadcta(v_canfil,07)  =  saldos_deb - saldos_hab
            estadcta(v_canfil,14)  =  saldos_deb
            estadcta(v_canfil,16)  =  saldos_pde
            estadcta(v_canfil,17)  =  saldos_pha
            v_subnto               =  estadcta(v_canfil,07) +  v_subnto

************  CALCULO DE RECARGO DE CUOTAS VENCIDAS  **********

            if (v_feccob - saldos_ven) > 0
               if saldos_int = saldos_deb * ((saldoact(v_cancon,09)*estadcta(v_canfil,06)) / 100)
                  estadcta(v_canfil,08) = 0
               else
                  if v_codpro=[UDS] or v_codpro=[PSV] or v_codpro=[SDS] or v_codpro=[SDC] or v_codpro=[PSC] or v_codpro=[PSI]
                     if saldos_ncu # [GA/28] and saldos_ncu # [GA/29] and saldos_ncu # [GA/AD]
                        if saldos_fep # ctod('//')
                           v_mesatr = v_feccob-saldos_fep
                        else
                           v_mesatr = v_feccob-estadcta(v_canfil,05)
                        endi
                        v_porrec = v_mesatr*(saldoact(v_cancon,09)/30)
                        estadcta(v_canfil,08)=roun((saldos_deb - saldos_hab)*(v_porrec)/100,0)
                     endi
                  else
                     if saldos_ncu # [GA/28] and saldos_ncu # [GA/29] and saldos_ncu # [GA/AD]
                        estadcta(v_canfil,08) = round(((saldos_deb - saldos_hab) * (saldoact(v_cancon,09) / 100)),0)
                     endi
                  endi
               endif
               v_subrgo              = (v_subrgo + estadcta(v_canfil,08))
               v_totido              = (v_totido + estadcta(v_canfil,07))
               v_subtal              = (v_subtal + estadcta(v_canfil,07) + estadcta(v_canfil,08))
               v_totrgo              = (v_totrgo + estadcta(v_canfil,08))
               saldoact(iif(v_cancon > 0,v_cancon,1),03) = estadcta(v_canfil,07)
               saldoact(iif(v_cancon > 0,v_cancon,1),06) = estadcta(v_canfil,07) + estadcta(v_canfil,08)
               saldoact(iif(v_cancon > 0,v_cancon,1),07) = "***"
            else
               estadcta(v_canfil,08) = 0
               v_subtal              = (v_subtal + estadcta(v_canfil,07))
               v_totcer              = (v_totcer + saldos_deb)
            endif

************      FIN DE CALCULO RECARGO     ***********

            if v_cancon = 0
               v_cancon = 1
               store saldos_pro to saldoact(v_cancon,01)
               store saldos_num to saldoact(v_cancon,02)
            endif

            if saldoact(v_cancon,03) = 0
               saldoact(v_cancon,03) = estadcta(v_canfil,07)
               saldoact(v_cancon,06) = estadcta(v_canfil,07) + estadcta(v_canfil,08)
            endif

*           saldoact(v_cancon,03)     = saldoact(v_cancon,03) + estadcta(v_canfil,07)
            saldoact(v_cancon,04)     = saldoact(v_cancon,04) + (estadcta(v_canfil,07) + estadcta(v_canfil,08))

            estadcta(v_canfil,09)     = (estadcta(v_canfil,07) + estadcta(v_canfil,08))
            if estadcta(v_canfil,10)  =  0  .and.  estadcta(iif(v_canfil=1,1,v_canfil-1),10) = 0
               estadcta(v_canfil,10)  =  estadcta(v_canfil,09)
            else
               estadcta(v_canfil,10)  =  estadcta(v_canfil,09) + estadcta(iif(v_canfil=1,1,v_canfil-1),10)
            endif
            estadcta(v_canfil,11)     =  saldos_hoj
         else
            if (transform(year(saldos_ven),"9999")+transform(month(saldos_ven),"99"));
            >  (transform(year(v_fectop),"9999")  +transform(month(v_fectop),"99")) .and.;
               (saldos_hab - saldos_deb) < 0 .and. saldos_est <> "B"
               store (v_canfil + 1)    to v_canfil
               if saldos_pro  <>  v_codpro  .or.  saldos_num  <>  v_nrocon
                  if v_canfil > 1
                      store v_codpro   to estadcta(v_canfil,02)
                      store v_nrocon   to estadcta(v_canfil,03)
                      store v_subnto   to estadcta(v_canfil,07)
                      store v_subrgo   to estadcta(v_canfil,08)
                      store v_subtal   to estadcta(v_canfil,09)
                      v_subnto = 0
                      v_subrgo = 0
                      v_subtal = 0
                      v_canfil = v_canfil + 1
                  endif
                  v_cancon = v_cancon + 1
                  store saldos_pro to estadcta(v_canfil,02), v_codpro, saldoact(v_cancon,01)
                  store saldos_num to estadcta(v_canfil,03), v_nrocon, saldoact(v_cancon,02)
               endif
               estadcta(v_canfil,01)  =  recno()
               estadcta(v_canfil,02)  =  saldos_pro
               estadcta(v_canfil,03)  =  saldos_num
               estadcta(v_canfil,04)  =  saldos_ncu
               estadcta(v_canfil,05)  =  saldos_ven
               estadcta(v_canfil,06)  =  v_feccob   - saldos_ven
               estadcta(v_canfil,07)  =  saldos_deb - saldos_hab
               estadcta(v_canfil,14)  =  saldos_deb
               estadcta(v_canfil,16)  =  saldos_pde
               estadcta(v_canfil,17)  =  saldos_pha
               v_totcer               = (v_totcer + saldos_deb)

               if v_cancon = 0
                  v_cancon = 1
                  store saldos_pro to saldoact(v_cancon,01)
                  store saldos_num to saldoact(v_cancon,02)
               endi

               if saldoact(v_cancon,03) = 0
                  saldoact(v_cancon,03) = estadcta(v_canfil,07)
                  saldoact(v_cancon,06) = estadcta(v_canfil,07) + estadcta(v_canfil,08)
               endi
*              saldoact(v_cancon,03)     = saldoact(v_cancon,03) + estadcta(v_canfil,07)
               saldoact(v_cancon,04)     = saldoact(v_cancon,04) + (estadcta(v_canfil,07) + estadcta(v_canfil,08))
               estadcta(v_canfil,09)     = (estadcta(v_canfil,07) + estadcta(v_canfil,08))
               if estadcta(v_canfil,10)  =  0  .and.  estadcta(iif(v_canfil=1,1,v_canfil-1),10) = 0
                  estadcta(v_canfil,10)  =  estadcta(v_canfil,09)
               else
                  estadcta(v_canfil,10)  =  estadcta(v_canfil,09) + estadcta(iif(v_canfil=1,1,v_canfil-1),10)
               endi
               estadcta(v_canfil,11)     =  saldos_hoj
            endi
         endi
         skip
      endd
  &&  set exact on
      set near off
      v_canfil = v_canfil + 1
      estadcta(v_canfil,07)    = v_subnto
      estadcta(v_canfil,02)    = v_codpro
      estadcta(v_canfil,03)    = v_nrocon
      estadcta(v_canfil,08)    = v_subrgo
      estadcta(v_canfil,09)    = v_subtal
      v_totral 				   = v_totido + v_totcer
      if v_totral > 0
         do impestcta
      else
         wait "No tiene cuotas vencidas ni a vencer, favor verifique" window
      endi
   else
      wait "No existe ficha de ningun contrato para la cuenta " + transform(v_nrocta,[999999]) + " - " + trim(v_nomtit)+", verifique !!!" window
      do limpiavar
   endi
   RETU

*  DESPLIEGA ESTADO DE CTA. DEL CLIENTE
   PROCEDURE DESPESTA
      do tecla_pago
      save scre to panta2
      acti wind estadcta
      clear
      @00,00 say "CUENTA.: " + transform(v_nrocta,"999999")
      @00,60 say "FECHA: "   + dtoc(v_feccob)
      @01,00 say "TITULAR: " + alltrim(v_nomtit)
      @02,00 say replicate("o",78)
      @03,00 say " CONTRATO   CUOTA   VENCE   ATRASO  MONTO  RECARGO    TOTAL     ACUMUL.  TALON"
      @04,00 say replicate("o",78)
      @16,00 say replicate("o",78)
      @17,00 say space(16)+"TOTAL VENCIDO.:  " + transform(v_totido,[99,999,999]) + space(01) + transform(v_totrgo,[999,999]) + space(01)+transform(v_totido+v_totrgo,[9,999,999])
      @18,00 say space(16)+"TOTAL A VENCER:  " + transform(v_totcer,[99,999,999]) + space(09) + transform(v_totcer,[9,999,999])
      @19,00 say space(16)+"TOTAL GENERAL.:  " + transform(v_totral,[99,999,999]) + space(01) + transform(v_totrgo,[999,999]) + space(01)+transform(v_totral+v_totrgo,[9,999,999])
      @20,00 say replicate("o",78)
      set colo to w+/b
      @21,03 say " <F4>Cobrar  <F5>Act. Cobros  <F6>Reclamos  <F7>Mensajes  <Esc>Cancelar "
      set colo to i
      modi comm M:\-DATOS\SICMADAT\CCE\&v_arcaux noedit window interior
      dele file M:\-DATOS\SICMADAT\CCE\&v_arcaux
      deac wind estadcta
      rest scre from panta2
   RETU

* IMPRIME ESTADO DE CUENTA:
  PROCEDURE IMPESTCTA
  v_arcaux = left(usuarios,2) + substr(time(),1,2) + substr(time(),4,2) + substr(time(),7,2) + ".txt"
  set prin to M:\-DATOS\SICMADAT\CCE\&v_arcaux
  set devi to prin
  for i = 1 to v_canfil
      if (transform(year(estadcta(i,05)),"9999") + transform(month(estadcta(i,05)),"99")) <=  ;
         (transform(year(v_fectop),"9999")       + transform(month(v_fectop),"99"))      .or. ;
         estadcta(i,04) = space(05)
         if i > 1
            if estadcta(i,02) <>  estadcta(i-1,02) .or.;
               estadcta(i,03) <>  estadcta(i-1,03)
               @prow()+1,00   say iif(v_feccob - estadcta(i,05) > 0,""," ")
               @prow(),pcol() say estadcta(i,02)
               @prow(),pcol() say transform(estadcta(i,03),[999999])
            else
               @prow()+1,00   say iif(v_feccob - estadcta(i,05) > 0,""," ")
               @prow(),pcol() say space(03)
               @prow(),pcol() say space(06)
            endif
         else
            @prow()+1,00   say iif(v_feccob - estadcta(i,05) > 0,""," ")
            @prow(),pcol() say estadcta(i,02)
            @prow(),pcol() say transform(estadcta(i,03),[999999])
         endi
         @prow(),pcol()+1 say estadcta(i,04)
         @prow(),pcol()+1 say iif(estadcta(i,05)<>ctod([//]),dtoc(estadcta(i,05)),space(010))
         @prow(),pcol()+1 say iif(estadcta(i,06)>0,transform(estadcta(i,06),[9999]),space(04))
         @prow(),pcol()+1 say iif(estadcta(i,07)>0,transform(estadcta(i,07),[9,999,999]),space(09))
         @prow(),pcol()+1 say iif(estadcta(i,08)>0,transform(estadcta(i,08),[999,999]),space(07))
         @prow(),pcol()+1 say iif(estadcta(i,09)>0,transform(estadcta(i,09),[9,999,999]),space(09))
         @prow(),pcol()+1 say iif(estadcta(i,10)>0,transform(estadcta(i,10),[9,999,999]),space(09))
         @prow(),pcol()+1 say iif(estadcta(i,11)>0,transform(estadcta(i,11),[999999]),space(06))
      endi
  endf
  set prin off
  set cons on
  set devi to scre
  set prin to prn
  RETU

*  PIDE PAGOS POR CONTRATO
   PROCEDURE PIDEPAGO
      on key
      v_mensaje = []
      save screen to panta3
      clear
      activate window pidepago
      do while .t.
         clear
         @00,00 say "CUENTA.: " + transform(v_nrocta,[999999])
         @00,55 say "FECHA.....: " + dtoc(v_feccob)
         @01,00 say "TITULAR: " + alltrim(v_nomtit)
         @01,55 say "FACTURA NRO: " + transform(v_nrofac,[999999])
         @02,00 say replicate("o",77)
         @03,00 say "CONTRATO NRO  MONTO CUOTA  CON RECARGO  SALDO ACTUAL  FORMA PAGO  MONTO A COBRAR "
         @04,00 say replicate("o",77)
         v_totcobrar = 0
         v_brucobrar = 0
         v_netcobrar = 0
         v_totdescue = 0
         for i = 1 to v_cancon
             saldoact(i,08) = 0
             v_forpag       = "Mensual  "
             v_bandera      = .f.
             @row()+1,00     say saldoact(i,01)
             @row(),col()+1  say transform(saldoact(i,02),[999999])
             @row(),col()+5  say transform(saldoact(i,03),[9,999,999])
             @row(),col()+4  say transform(saldoact(i,06),[9,999,999])
             @row(),col()+5  say transform(saldoact(i,04),[9,999,999])
             do while v_bandera = .f. .and. lastkey() <> 27
                @row(),col()+2  get v_forpag picture[@m Mensual,Semestral,Anual] valid valforpag(v_forpag) error v_mensaje
                read
             enddo
             do case
                case saldoact(i,07) = "MEN"
                   v_moncob  = iif(saldoact(i,06)>0,saldoact(i,06),saldoact(i,04))
                case saldoact(i,07) = "SEM"
                   v_moncob  = saldoact(i,03) * 6
                   saldoact(i,08) = saldoact(i,03) * (-1)
                case saldoact(i,07) = "ANU"
                   v_moncob  = saldoact(i,03) * 12
                   saldoact(i,08) = saldoact(i,03) * (-2)
             endcase
             v_bandera = .f.
             v_totdescue = v_totdescue + saldoact(i,08)
             do while v_bandera = .f. .and. lastkey() <> 27
                @row(),col()+5  get v_moncob picture[99,999,999] valid veripago(v_moncob) error v_mensaje
                read
             enddo
             if lastkey() = 27
                exit
             endif
             if saldoact(i,05) > 0
                 v_brucobrar = v_brucobrar + saldoact(i,05)
             endif
         endfor
         v_netcobrar = v_brucobrar + v_totdescue
         set color to b+/w
         @row()+1,64 say "============="
         @row()+1,37 say "MONTO BRUTO A COBRAR ------>> " + transform(v_brucobrar,[99,999,999])
         @row()+1,37 say "DESCUENTO            ------>> " + transform(v_totdescue,[99,999,999])
         @row()+1,64 say "============="
         @row()+1,37 say "MONTO NETO A COBRAR  ------>> " + transform(v_netcobrar,[99,999,999])
         v_opcion = 1
         set color to n+/w,w/n
         @20,05 promp "\<Aceptar Pago"
         @20,30 promp "\<Corregir    "
         @20,50 promp "Ca\<ncelar    "
         menu to v_opcion
         set color to i
         do case
            case v_opcion = 1
               do aceptapago
               if v_netcobrar = 0
                  exit
               endi
            case v_opcion = 2
               loop
            case v_opcion = 3
               exit
         endc
      endd
      deactivate window pidepago
      restore screen from panta3
      do tecla_pago
   RETURN

*  CARGA DETALLES DE VALORES DE COBRO
   PROCEDURE ACEPTAPAGO
   save screen to panta4
   activate window detacobro
   v_canval = 1
   v_vuelto = 0
   v_auxnetcob = v_netcobrar
   v_tipvalor  = space(15)
   do while .t. .and. lastkey() <> 27 .and. v_netcobrar > 0
      v_vuelto = 0
      detavalo(v_canval,02) = v_netcobrar
      clear
      @00,00       say "MONTO A COBRAR......: " + transform(v_netcobrar,[99,999,999])
      @row()+2,00  say "TIPO DE VALOR.......: "
      @row(),col() get v_tipvalor picture[@m Efectivo,Cheque,Tarjeta] valid valtipval(v_tipvalor)
      @row()+1,00  say "MONTO VALOR.........: "
      @row(),col() get detavalo(v_canval,02) picture[99,999,999] valid valmonval(detavalo(v_canval,02));
                                      error v_mensaje
      read
      if detavalo(v_canval,01) <> "EFE" .and. lastkey() <> 27
         store date()  to detavalo(v_canval,03)
         @row()+1,00  say "FECHA VENCIMIENTO...: "
         @row(),col() get detavalo(v_canval,03) valid !empty(detavalo(v_canval,03)) error [DEBE CARGAR DE VENCIMIENTO DEL CHEQUE O TARJETA]
         @row()+1,00  say "NRO DEL VALOR........: "
         @row(),col() get detavalo(v_canval,04) picture[@!] valid !empty(detavalo(v_canval,04)) error [DEBE CARGAR EL NRO DEL CHEQUE O TARJETA]
         if detavalo(v_canval,01) = "CHE"
            @row()+1,00  say "ENTIDAD EMISORA.....: "
            @row(),col() get detavalo(v_canval,05) valid valban(detavalo(v_canval,05))
            read
         endif
         if detavalo(v_canval,01) = "TAR" .and. lastkey() <> 27
            @row()+1,00  say "TARJETA DE CREDITO..: "
            @row(),col() get detavalo(v_canval,06) valid valtar(detavalo(v_canval,06))
            read
         endif
      endif
      v_opcion2 = 1
      @08,15       promp "\<Aceptar "
      @08,col()+05 promp "\<Corregir"
      @08,col()+05 promp "Ca\<ncelar"
      menu to v_opcion2
      do case
         case v_opcion2 = 1
              v_netcobrar = v_netcobrar - (detavalo(v_canval,02) + v_vuelto)
              if v_netcobrar > 0
                 v_canval = v_canval + 1
              endif
              loop
  		 case v_opcion2 = 2
		      loop
		 case v_opcion2 = 3
		      v_netcobrar = v_auxnetcob
		      exit
	  endcase
   enddo

********** Si pago en cheque y tiene vuelto **********
   if v_vuelto < 0
      v_canval              = v_canval + 1
      detavalo(v_canval,01) = "EFE"
      detavalo(v_canval,02) = v_vuelto
      detavalo(v_canval,04) = "Vuelto"
   endif

**********     Fin Vuelto de cheque     **********

   if v_netcobrar = 0
      v_opcion2 = 1
      @08,10       say space(65)
      @08,15       promp "\<Grabar"
      @08,col()+10 promp "Ca\<ncelar"
      menu to v_opcion2
      do case
         case v_opcion2 = 1
			  *do imprifactu
			  do actuaficha
              do actuacaja
              do actuavalor
    	      do actuatalon
    	      do imprifactu
         case v_opcion2 = 2
              v_netcobrar = v_auxnetcob
      endcase
   endif
   deactivate window detacobro
   restore screen from panta4
   RETURN

* ACTUALIZA MATRIZ DE FICHA CON COBROS
  PROCEDURE ACTUAFICHA
	 **********************
	 * movimiento de caja *
	 **********************
     v_linfac  = 1
     v_canfac  = 1
     v_primero = ""
     v_pagcon  = 0
     for i = 1 to v_cancon
        if saldoact(i,05) > 0
           v_pagcon = saldoact(i,05)
           for j = 1 to v_canfil
              if estadcta(j,02) =  saldoact(i,01) .and.;
                 estadcta(j,03) =  saldoact(i,02) .and.;
                 estadcta(j,04) <> space(05)      .and.;
                 saldoact(i,05)  > 0

                 if saldoact(i,05) >= (estadcta(j,07) + estadcta(j,08))
                    estadcta(j,12) =  estadcta(j,08)
                    estadcta(j,13) =  estadcta(j,07)
                    saldoact(i,05) =  saldoact(i,05) - estadcta(j,09)
                 else
                    if saldoact(i,05) >= estadcta(j,08)
                       estadcta(j,12) =  estadcta(j,08)
                       saldoact(i,05) =  saldoact(i,05) - estadcta(j,08)
                       estadcta(j,13) =  saldoact(i,05)
                       saldoact(i,05) =  saldoact(i,05) - estadcta(j,13)
                    else
                       estadcta(j,12) =  saldoact(i,05)
                       saldoact(i,05) =  saldoact(i,05) - estadcta(j,12)
                    endif
                 endif
                    estadcta(j,15) = v_nrofac
                    if saldoact(i,7) = "MEN"
                       if v_linfac >= v_califa
                       do actuavalor
*                      do valifact
                       v_nrofac = v_nrofac + 1
                       v_canfac = v_canfac + 1
                       v_linfac = 0
                    else
                       v_linfac = v_linfac + 1
                       v_primero = "t"
                    endif
                 else
                    if v_primero = "t"
                       if v_linfac >= v_califa
                          do actuavalor
                          v_nrofac = v_nrofac + 1
                          v_canfac = v_canfac + 1
                          v_linfac = 0
                       else
                          v_linfac = v_linfac + 1
                          v_primero = "f"
                       endif
                    endif
                 endif
	          endif
 	       endfor
        endif
        saldoact(i,05) = v_pagcon
	 endfor
  RETURN

* ACTUALIZA MOVIMIENTO DE CAJA
  PROCEDURE ACTUACAJA
     v_privez = "SI"
     for h = 1 to v_cancon

************     Actualiza auxiliar de cobranza     **********
        do actuauxcob
************	     Fin de actualizacion           **********

        if saldoact(h,05) > 0
           for j = 1 to v_canfil
              if (estadcta(j,12) >  0 .or. estadcta(j,13) >  0) .and. ;
                 saldoact(h,01) = estadcta(j,02)                .and. ;
                 saldoact(h,02) = estadcta(j,03)

                 selec movcaj_m
                 scatter memvar blank
                 go bottom
                 sele sucurs_m
                 seek v_codsuc
                 m.movcaj_num = sucurs_ulm + 1
                 repl sucurs_ulm with sucurs_ulm+1
                 m.movcaj_suc = v_codsuc
                 sele movcaj_m

*************** Temporal para exonerar recargos *********

                 if v_fecalt <> ctod([//])
                    m.movcaj_fec = v_fecaux
                 else
                    m.movcaj_fec = v_feccob
                 endif

*************** fin temporal ***************

                 m.movcaj_cob = v_codcaj
                 m.movcaj_pna = usuarpna
                 m.movcaj_per = usuarper
                 m.movcaj_cli = v_nrocta
                 m.movcaj_fac = estadcta(j,15)
                 m.movcaj_tip = v_tipfac
                 m.movcaj_ser = v_serfac
                 m.movcaj_pro = estadcta(j,02)
                 m.movcaj_con = estadcta(j,03)
                 m.movcaj_cuo = estadcta(j,04)
                 m.movcaj_ven = estadcta(j,05)
                 m.movcaj_mon = estadcta(j,13)
                 m.movcaj_int = estadcta(j,12)
                 m.movcaj_tal = estadcta(j,11)
                 m.movcaj_dat = date()
                 m.movcaj_tim = time()
                 m.movcaj_tre = [1]
                 append blank
                 gather memvar
                 if estadcta(j,01) > 1
                    do actuasaldo with j
                 endif
              endif
           endfor

*********** Actualiza descuento p/pago semes. o anual en movim. de caja *********

           if saldoact(h,08) < 0
              do case
                 case uppe(left(v_forpag,3))=[SEM]
                    sele saldos_m
                    repl saldos_obh with [DESC.PAGO SEMESTRAL]
                    repl saldos_ads with 1
                 case uppe(left(v_forpag,3))=[ANU]
                    sele saldos_m
                    skip -1
                    repl saldos_obh with [DESC.PAGO ANUAL]
                    repl saldos_ads with 2
                    skip
                    repl saldos_obh with [DESC.PAGO ANUAL]
                    repl saldos_ads with 2
              endc
              sele movcaj_m
              go bott
              scat memv blan
              if v_fecalt <> ctod([//])
                 m.movcaj_fec = v_fecaux
              else
                 m.movcaj_fec = v_feccob
              endif
              m.movcaj_suc = v_codsuc
              m.movcaj_num = movcaj_num + 1
              m.movcaj_cob = v_codcaj
              m.movcaj_pna = usuarpna
              m.movcaj_per = usuarper
              m.movcaj_cli = v_nrocta
              m.movcaj_fac = v_nrofac
              m.movcaj_tip = v_tipfac
              m.movcaj_ser = v_serfac
              m.movcaj_pro = saldoact(h,01)
              m.movcaj_con = saldoact(h,02)
              m.movcaj_cuo = [DESCU]
              m.movcaj_mon = saldoact(h,08)
              m.movcaj_dat = date()
              m.movcaj_tim = time()
              m.movcaj_tre = [1]
              appe blan
              gath memv
           endif

***********       Fin actualizacion de descuento      *********

        endif
     endfor
    do actuatalon
   RETURN

* ACTUALIZA ARCHIVO DE SALDO DE CLIENTES
  PROCEDURE ACTUASALDO
  PARAMETERS j
     selec saldos_m
     go estadcta(j,01)
     scatter memvar
     if m.saldos_ncu = estadcta(j,04)  .and.;
        m.saldos_tit = v_nrocta        .and.;
        m.saldos_pro = estadcta(j,02)  .and.;
        m.saldos_num = estadcta(j,03)  .and.;
        m.saldos_ven = estadcta(j,05)
        m.saldos_suc  =  v_codsuc
        m.saldos_mov  =  movcaj_m.movcaj_num
        m.saldos_caj  =  v_codcaj
        m.saldos_int  =  (m.saldos_int + estadcta(j,12))
        m.saldos_fac  =  estadcta(j,15)
        m.saldos_tip  =  v_tipfac
        m.saldos_ser  =  v_serfac
*********** Temporal para exonerar recargos **********
        if v_fecalt <> ctod([//])
           m.saldos_fep  =  v_fecaux
        else
           m.saldos_fep  =  v_feccob
        endif
***********   Fin temporal **************
        m.saldos_hab  =  (m.saldos_hab + estadcta(j,13))
        m.saldos_cdc  =  v_codcaj
        m.saldos_pna  =  usuarpna
        m.saldos_per  =  usuarper
        m.saldos_dat  =  date()
        m.saldos_tim  =  time()
        gather memvar
     endif
   RETURN

* ACTUALIZA DETALLE VALORES DE CAJA
  PROCEDURE ACTUAVALOR
     selec valcaj_d
     go bottom
     for k = 1 to v_canval
         scatter memvar blank
         m.valcaj_suc = v_codsuc
         m.valcaj_fac = v_nrofac
         m.valcaj_tip = v_tipfac
         m.valcaj_ser = v_serfac
         m.valcaj_tva = detavalo(k,01)
         m.valcaj_imp = detavalo(k,02)
         m.valcaj_eem = detavalo(k,05)
         m.valcaj_tar = detavalo(k,06)
         m.valcaj_nro = detavalo(k,04)
         if detavalo(k,01) = "CHE" .or. detavalo(k,01) = "TAR"
            m.valcaj_ven = detavalo(k,03)
         else
            m.valcaj_ven = v_feccob
         endif
         if detavalo(k,07) > 1
            m.valcaj_dup ="S"
         endif
         m.valcaj_pna = usuarpna
         m.valcaj_per = usuarper
         m.valcaj_dat = date()
         m.valcaj_tim = time()
         m.valcaj_tre = [1]

*********** Temporal para exonerar recargos **********

         if v_fecalt <> ctod([//])
            m.valcaj_fec = v_fecaux
         else
            m.valcaj_fec = v_feccob
         endif

***********   Fin temporal **************

         m.valcaj_ncd = alltrim(v_ctaban)

         append blank
         gather memvar
     endfor
   RETURN

* ACTUALIZA NRO FACTURA EN ARCHIVO DE TALONARIOS
  PROCEDURE ACTUATALON
     selec talona_m

     do case
        case v_nrofac = talona_des
             replace talona_est with [EN USO]
        case v_nrofac = talona_has
             replace talona_est with [UTILIZADO]
     endcase

     replace talona_ult with v_nrofac

********** TEMPORAL PARA EXONERACION DE RECARGO **********

     if v_fecalt <> ctod([//])
        replace talona_fum with v_fecaux
     else
        replace talona_fum with v_feccob
     endif

**********       FIN TEMPORAL         **********

   RETURN

*  ACTUALIZA ARCHIVO AUXILIAR DE COBRANZA
   PROCEDURE ACTUAUXCOB
      selec auxcob_d
      go top

*********** Temporal para exonerar recargos **********

            if v_fecalt <> ctod([//])
                seek (dtos(v_fecaux)+v_codcaj+saldoact(h,01))
            else
                seek (dtos(v_feccob)+v_codcaj+saldoact(h,01))
            endif

***********   Fin temporal **************

      if found()
         scatter memvar
         m.auxcob_mop = m.auxcob_mop + saldoact(h,05) + saldoact(h,08)
         if v_privez = "SI"
             m.auxcob_cap = m.auxcob_cap + 1
             v_privez = "NO"
         endif
      else
         go bottom
         scatter memvar blank
         m.auxcob_cob = v_codcaj
         m.auxcob_cen = saldoact(h,01)
         m.auxcob_cap = 1
         m.auxcob_mop = saldoact(h,05) + saldoact(h,08)
         m.auxcob_caa = 0
         m.auxcob_moa = 0
         m.auxcob_cod = "C"
*********** Temporal para exonerar recargos **********

            if v_fecalt <> ctod([//])
                m.auxcob_fec = v_fecaux
            else
                m.auxcob_fec = v_feccob
            endif

***********   Fin temporal **************

         append blank
         v_privez = "NO"
      endif
      gather memvar
      selec movcaj_m
   RETURN

*  VERIFICA CARGA DE COBRO
   PROCEDURE VERIPAGO
   PARAMETERS v_moncob
      do case
         case saldoact(i,07) = "MEN"
              if v_moncob > saldoact(i,04) .and. v_moncob > saldoact(i,05)
                 v_moncob = saldoact(i,04)
                 v_mensaje = [MONTO A COBRAR NO PUEDE SER MAYOR AL VALOR TOTAL DE CUOTAS PENDIENTES]
                 return .f.
              endif
         case saldoact(i,07) = "SEM"
              if v_moncob > (saldoact(i,03) * 6) .or. v_moncob < (saldoact(i,03) * 6)
                 v_moncob = saldoact(i,03) * 6
                 v_mensaje = [MONTO A COBRAR NO PUEDE SER DIFERENTE AL VALOR DE 6 CUOTAS PENDIENTES]
                 return .f.
              endif
         case saldoact(i,07) = "ANU"
              if v_moncob > (saldoact(i,03) * 12) .or. v_moncob < (saldoact(i,03) * 12)
                 v_moncob = saldoact(i,03) * 12
                 v_mensaje = [MONTO A COBRAR NO PUEDE SER DIFERENTE AL VALOR DE 12 CUOTAS PENDIENTES]
                 return .f.
              endif
         endcase
         saldoact(i,05) = v_moncob
         v_bandera = .t.
   RETURN .t.

*  VALIDA TIPO DE VALOR
   PROCEDURE VALTIPVAL
   PARAMETERS v_tipovalor
      detavalo(v_canval,01) = (upper(left(v_tipovalor,3)))
   RETURN

*  VALIDA FORMA DE PAGO
   PROCEDURE VALFORPAG
   PARAMETERS v_forpag
      if v_forpag <> "Mensual  " .and. saldoact(i,07) = "***"
         v_mensaje = "ATENCION!!!, CONTRATO CON MORA, IMPOSIBLE COBRAR SEMESTRAL O ANUAL"
         return .f.
      else
        if v_forpag <> "Mensual  "
           if estadcta(i,02) = "UDS" .or. estadcta(i,02) = "PSV"
               v_mensaje = "ATENCION !!!, NO SE PUEDE REALIZAR PAGO SEMESTRAL O ANUAL A LOS 'PSV' Y 'UDS'"
               return .f.
           endif
           if v_forpag = "Semestral" .and. ((saldoact(i,03)*6) >  saldoact(i,04))
               v_mensaje = "ATENCION !!!, CONTRATO NO TIENE 6 CUOTAS PENDIENTES P/PAGO SEMESTRAL"
              return .f.
           endif
           if v_forpag = "Anual    " .and. ((saldoact(i,03)*12) >  saldoact(i,04))
              v_mensaje = "ATENCION !!!, CONTRATO NO TIENE 12 CUOTAS PENDIENTES P/PAGO ANUAL"
              return .f.
           endif
         endif
      endif
      saldoact(i,07) = (upper(left(v_forpag,3)))
      v_bandera = .t.
   RETURN

*  VALIDA MONTO DE VALOR
   PROCEDURE VALMONVAL
   PARAMETERS v_valormonto
      do case
         case detavalo(v_canval,01) = "EFE"
            if detavalo(v_canval,02) <= 0 .or. detavalo(v_canval,02) > v_netcobrar
               v_mensaje = [DEBE CARGAR MONTO A COBRAR O <ESC> PARA CANCELAR]
               return .f.
            endif
         case detavalo(v_canval,01) = "CHE"
            if detavalo(v_canval,02) > v_netcobrar
               v_vuelto = v_netcobrar - detavalo(v_canval,02)
            endif
         case detavalo(v_canval,01) = "TAR"
            if detavalo(v_canval,02) <= 0 .or. detavalo(v_canval,02) > v_netcobrar
               v_mensaje = [DEBE CARGAR MONTO A COBRAR O <ESC> PARA CANCELAR]
               return .f.
			endif
      endcase
   RETURN

*  VALIDA SALDO DE CONTRATO
   PROCEDURE VALISALCON
      v_totdeb = 0
      v_totcre = 0
      v_feccan = ctod("  /  /  ")
      set exact off
      select saldos_m
      set order to sal005
      go top
      seek v_codpro + transform(v_nrocon,[999999])
      if found()
         do while saldos_pro = v_codpro .and. saldos_num = v_nrocon
            store v_totdeb + saldos_deb to v_totdeb
            store v_totcre + saldos_hab to v_totcre
            store saldos_fep            to v_feccan
            if eof()
               exit
            endif
            skip
         enddo
      endif
      if v_totdeb - v_totcre = 0
         wait "El " + v_codpro + " " + transform(v_nrocon,[999999]) + " fue cancelado el " + dtoc(v_feccan) window
      endif
   RETURN

*  TECLAS PARA CARGA DE PAGOS
   PROCEDURE TECLA_PAGO
      on key
      on key label F4  do pidepago
      on key label F5  do activacobros
      on key label F6  do reclaclien
      on key label F7  do mensaclien
      on key label F8  do avisocobra
      on key label F11 on key
   RETURN

  PROCEDURE TECLA_CLIENTE
     on key
     on key label enter keyb chr(23)
*    on key label ins   do altas_person
*    on key label F1    do consu_histor
     on key label F2    do buscatitu
     on key label F3    do solic_modifi with [T]
     on key label F11   on key
  RETURN

  PROCEDURE TECLA_BENEFI
     on key
     on key label enter keyb chr(23)
     on key label F2    do buscabene
     on key label F3    do solic_modifi with [B]
     on key label F11   on key
  RETURN

*  LIMPIA VARIABLES DEL PROGRAMA
   PROCEDURE LIMPIAVAR
       store      0      to v_nrocta, v_cancon, v_canfil, v_canval, v_nrocon
       store   space(35) to v_nomtit, v_nomben
       store   space(03) to v_codpro
       store   space(10) to v_imes

       store      0      to v_subnto,v_subrgo,v_subtal,v_canfil,v_cancon
       store      0      to v_totido,v_totcer,v_totral,v_totrgo,v_totfac
       store      0      to v_preiva,v_valiva,v_nrotal,v_nroci

   RETURN

****************************************************************
*  PROCEDIMIENTO DE IMPRESION DE FACTURA:
****************************************************************
*  TICKETFELIZ 2018.10.15
   PROCEDURE IMPRIFACTU
   set device to printer
   set printer to lpt1
   set printer on

   *@ 1,1 say "impresion con arrobas"
     do valmes
      v_linea  = 08
     do impcabfac
      v_linea = v_linea + 4
      *@ prow()+4, 00  say [ ]

	  for k = 1 to v_cancon
         if saldoact(k,05) > 0
            do case
               case saldoact(k,07) = "SEM"
                  do imppagper
               case saldoact(k,07) = "ANU"
                  do imppagper
               case saldoact(k,07) = "MEN"
                  do imppagmen
            endcase
         endif
      endfor

      if v_totdescue < 0
         v_linea = v_linea  + 1
         @ prow()+1, 32  say "Descuento..."
         @ prow()  , 142 say v_totdescue pict  "999,999,999"
      endif

      if v_linea < 22
         for i = v_linea to 22
             @ prow()+1,00 say [ ]
         endfor
         v_linea = 22
      endif

*******************************************************************
*	IMPRIME TOTALES DE LA FACTURA
*******************************************************************
      do a0opeags with v_totfac
      v_preiva = round(v_totfac/1.1,0)
      v_valiva = v_totfac - v_preiva

      @ prow()+1,142 say v_totfac  pict "999,999,999"
      @ prow()+1, 38 say subs(valnel,1,60) &&( Importe en Letras )
      @ prow()  ,142 say v_totfac  pict "999,999,999"

      @ prow()+2,80 say v_valiva pict "99,999,999"
      @ prow()  ,113 say v_valiva pict "99,999,999"

      v_linea  = v_linea + 8
	  eject
      set device  to screen
      set printer off
      set printer to
   RETURN

********************************************************************
*  IMPRIME CABECERA DE FACTURA
********************************************************************
* IMPRESION DE TICKETS 2018.10.15a
   PROCEDURE IMPCABFAC
      *??? chr(27)+chr(64)
      *??? chr(27)+chr(67)+chr(33)
      ??? chr(15)
      ??? chr(27)+chr(77)
      do valmes
      * @ prow() + v_linea,5 say [PARQUE SERENIDAD S.R.L.]
      @ prow() + 1 ,0 say "VERSION 6"
      @ prow() + 1 ,0 say [PARQUE SERENIDAD S.R.L.]
      @ prow() + 1,0 say "CAPITAL: Gs 1.000.000 "
      @ prow() + 1,0 say [SERVICIOS FUNERALES Y CAJONERIA]
      @ prow() + 1,0 say "MATRIZ: Av.Espanha 693"
      @ prow() + 1,0 say "Esq.Boqueron"
      @ prow() + 1,0 say "tel1: 207013(R.A.)"
      @ prow() + 1,0 say "tel2: 211452 (R.A.)"
      @ prow() + 1,0 say "Suc1. Administracion"
      @ prow() + 1,0 say "Peru c/Espana"
      @ prow() + 1,0 say "tel: 207013 (RA)"
      @ prow() + 1,0 say "tel: 211452 (RA)"
      @ prow() + 1,0 say "Suc2. Memorial Mcal.Lopez"
      @ prow() + 1,0 say "Avda. Mcal Lopez 5353"
      @ prow() + 1,0 say "tel: 613767/9"
      @ prow() + 1,0 say "Suc3. Boqueron"
      @ prow() + 1,0 say "Calle Boqueron 491"
      @ prow() + 1,0 say "Juan de Salazar"
      @ prow() + 1,0 say "Suc4. Memorial San Lorenzo"
      @ prow() + 1,0 say "Ruta Mcal Estigarribia y Azara"
      @ prow() + 1,0 say "tel: 585030 - 585111"
      @ prow() + 1,0 say "Suc5. Parque Cementerio"
      @ prow() + 1,0 say "Av Tte. Americo Pico"
      @ prow() + 1,0 say "y Tte Ojeda 4300"
      @ prow() + 1,0 say "tel: 940260"
      @ prow() + 1,0 say "R.U.C.: 80001620-3"
      @ prow() + 1,0 say "Suc6. Memorial Sajonia"
      @ prow() + 1,0 say "Av.Carlos Antonio Lopez"
      @ prow() + 1,0 say "e/Paris y Dr. Coronel"
      @ prow() + 1,0 say "tel: 480481"
      @ prow() + 1,0 say "Dep. Fabrica M.R.Alonso"
      @ prow() + 1,0 say "Calle Campo Via 1850"
      @ prow() + 1,0 say "tel: 751305"
      @ prow() + 1,0 say "R.U.C.: 80001620-3"
      @ prow() + 1,0 say "TIMBRADO: 12506778"
      @ prow() + 1,0 say "VALIDO DESDE: 18/12/2017"
      @ prow() + 1,0 say "VALIDO HASTA: 31/12/2017"
      @ prow() + 1,0 say "CODIGO DESCRIPCION PRECIO"
      @ prow() + 1,0 say [FECHA ]
	If v_fecalt <> ctod("//")
		@ prow()+1 , 10  say day(date()) pict [99]
		@ prow()   , 12  say v_imes pict [@!]
		@ prow()   , 14  say year(date()) pict [9999]
	Else
		If v_feccob <> date()
			@ prow()+1 , 10  say day(v_feccob) pict [99]
			@ prow()   , 12  say v_imes pict [@!]
			@ prow()   , 14  say year(v_feccob) pict [9999]
		Else
			@ prow()+6 , 10  say day(date()) pict [99]
			@ prow()   , 12  say v_imes pict [@!]
			@ prow()   , 14  say year(date()) pict [9999]
		Endif
	Endif
 	@ prow()   , 135 say "X"

      v_linea  = v_linea  + 1
      @ prow()+2, 25  say trim(v_ructit)
      @ prow()+2, 47  say trim(v_nomtit)+[ - ]+[(]+tran(v_nrocta,"999,999")+[)]
      v_linea = v_linea + 1
      @ prow()+4, 21  say [ ]
   RETURN

*********************************************************************
*  IMPRIME LINEA DE DETALLE FACTURA
   PROCEDURE IMPRIDETA
      v_linea = v_linea  + 1
      @ prow()  , 27  say v_detalle
      @ prow()  , 92  say v_preuni  pict  "999,999,999"
      @ prow()  , 142 say v_subtot  pict  "999,999,999"
      @ prow()+1, 21  say [ ]
   RETURN


*********************************************************************
*  IMPRIME DETALLE DE FACTURA POR PAGO SEMESTRAL O ANUAL
   PROCEDURE IMPPAGPER
      v_detalle   = []
      v_cuomen    = space(05)
      v_anomen    = 0
      v_cuomay    = space(05)
      v_anomay    = 0
      v_totdescue = 0
      for q = 1 to v_canfil
         if estadcta(q,02) =  saldoact(k,01) .and.;
            estadcta(q,03) =  saldoact(k,02) .and.;
            estadcta(q,04) <> space(05)      .and.;
            estadcta(q,15) >   0

*************  Elige el primer mes del periodo a pagar  *************

            if v_cuomen = space(05)
               store estadcta(q,04)       to v_cuomen
               store year(estadcta(q,05)) to v_anomen
            else
               if val(SUBS(estadcta(q,04),1,2)) <  val(SUBS(v_cuomen,1,2)) .and. ;
                  year(estadcta(q,05))          <= v_anomen
                  store estadcta(q,04)       to v_cuomen
                  store year(estadcta(q,05)) to v_anomen
               endif
            endif

*************  Fin de eleccion del primer mes  **************

*************  Elige el ultimo mes del periodo a pagar  ************

            if v_cuomay = space(05)
               store estadcta(q,04)       to v_cuomay
               store year(estadcta(q,05)) to v_anomay
            else
               if val(SUBS(estadcta(q,04),1,2)) > val(SUBS(v_cuomay,1,2)) .or. ;
                  year(estadcta(q,05))          > v_anomay
                  store estadcta(q,04)       to v_cuomay
                  store year(estadcta(q,05)) to v_anomay
               endif
            endif
************   Fin de eleccion del ultimo mes  *************
            v_totfac       = v_totfac + estadcta(q,12) + estadcta(q,13)
         endif
      endfor
      v_preuni    = saldoact(k,05)
      v_subtot    = saldoact(k,05)
      v_totfac    = v_totfac + saldoact(k,08)
      v_totdescue = v_totdescue + saldoact(k,08)
      v_vencim    = v_cuomen
      do valmes
      v_cuomen    = alltrim(v_dmes)
      v_vencim    = v_cuomay
      do valmes
      v_cuomay    = alltrim(v_dmes)
      do case
         case saldoact(k,07) = "SEM"
            v_detalle = "Pago semestral " + saldoact(k,01) + transform(saldoact(k,02),[999999])  +;
                        ", de " + v_cuomen + "/" + transform(v_anomen,[9999]) + " a " + v_cuomay +;
                        "/" + transform(v_anomay,[9999])
         case saldoact(k,07) = "ANU"
            v_detalle = "Pago anual " + saldoact(k,01) + transform(saldoact(k,02),[999999])      +;
                        ", de " + v_cuomen + "/" + transform(v_anomen,[9999]) + " a " + v_cuomay +;
                        "/" + transform(v_anomay,[9999])
      endcase
      do imprideta
   RETURN

*************************************************
*  IMPRIME DETALLE DE FACTURA POR PAGO MENSUAL

   PROCEDURE IMPPAGMEN
      v_detalle = []
      for q = 1 to v_canfil
         if saldoact(k,01) = estadcta(q,02) .and. ;
            saldoact(k,02) = estadcta(q,03) .and. ;
            (estadcta(q,12) + estadcta(q,13)) > 0
               if estadcta(q,02) = "PSM"
                  v_vencim = estadcta(q,04)
                  do valmes
                  v_dmes = alltrim(v_dmes)+[/]+ transform(year(estadcta(q,05)),[9999])
               endif
               do case
                  case (estadcta(q,12) + estadcta(q,13)) <  estadcta(q,09)
                     v_detalle = [Pago ]+estadcta(q,02)+[ ]+tran(estadcta(q,03),"999999")+[, a cuenta de ]
                  case (estadcta(q,12) + estadcta(q,13)) =  estadcta(q,09)
                     if estadcta(q,09) < estadcta(q,14)
                        v_detalle = [Pago ]+estadcta(q,02)+[ ]+tran(estadcta(q,03),"999999")+[, complemento ]
                     else
                        v_detalle = [Pago ]+estadcta(q,02)+[ ]+tran(estadcta(q,03),"999999")+[, ]
                     endif
               endcase
               v_detalle = v_detalle + iif(estadcta(q,02)="PSM","cuota de " + v_dmes,"cuota NRO " + estadcta(q,04))
               if estadcta(q,02) = "CMS"
                  v_detalle = v_detalle +  " de " + transform(year(estadcta(q,05)),[9999])
               endif
               v_preuni  = estadcta(q,12)+estadcta(q,13)
               v_subtot  = estadcta(q,12)+estadcta(q,13)
               v_totfac  = v_totfac+estadcta(q,12)+estadcta(q,13)
               if estadcta(q,04)=[GA/29]
                  v_detalle = [Gastos Administrativos-029]
               endi
               if estadcta(q,04)=[GA/AD]
                  v_detalle = [Gastos Administrativos-Documentos]
               endi
               if estadcta(q,02)=[CMS] &&and left(estadcta(q,04),2)#[GA]
                  v_detalle = [Pago ]+estadcta(q,02)+" "+tran(estadcta(q,03),"999999")+" Per: "+dtoc(estadcta(q,16))+" a "+dtoc(estadcta(q,17))
               &&else
               &&   v_detalle = [Pago ]+estadcta(q,02)+" "+tran(estadcta(q,03),"999999")+" Gastos Administrativos "
               endi
               do imprideta
         endif
      endfor
      v_totfac    = v_totfac - saldoact(k,08)
      v_totdescue = v_totdescue + saldoact(k,08)
   RETURN


**************************************
*  VALIDA CODIGO DE BANCO:
   function valban
   parameters pban
   sele bancos_m
   seek padr(pban,2)
   if eof()
      define popup popbanco from 01,05 to 10,62;
      prompt fields;
      bancos_den+" "+;
      bancos_num;
      title " Bancos ";
      color  i,w+/b,w+/n,i,,b/w,
      on sele popu popbanco deac popu
      set orde to tag nombco
      go top
      on key label enter keyb chr(23)
      acti popu popbanco
      m.bancos_den=bancos_den
      codban      =bancos_num
   else
      m.bancos_den=bancos_den
      codban      =bancos_num
   endi
   on key
   m.bancos_den          = bancos_den
   detavalo(v_canval,05) = bancos_num
   set orde to i01b01
   @row(),col()+05 SAY m.bancos_den font fuente,12
   retu

****************************************
*  VALIDA CODIGO DE TARJETA DE CREDITO:
   function valtar
   para ptar
   sele tarjet_m
   seek padr(ptar,2)
   if eof()
      set orde to tarjet_nom
      go top
      on key label enter keyb chr(23)
      acti wind con_tar
      brow fields tarjet_den:H="Denominacion",tarjet_cod:H="Cod." in wind con_tar node nomo noap
      m.tarjet_den=tarjet_den
      codtar      =tarjet_cod
   endi
   on key
   m.tarjet_den          = tarjet_den
   detavalo(v_canval,06) = tarjet_cod
   deactivate window con_tar
   set order to tarjet_cod
   @row(),col()+05 say trim(m.tarjet_den)
   return

***** PROCEDIMIENTO DE VALIDACION DE MES:
  PROCEDURE VALMES
  DO CASE
     CASE SUBS(v_vencim,1,2)=[01]
        V_DMES=[ENERO    ]
     CASE SUBS(v_vencim,1,2)=[02]
        V_DMES=[FEBRERO  ]
     CASE SUBS(v_vencim,1,2)=[03]
        V_DMES=[MARZO    ]
     CASE SUBS(v_vencim,1,2)=[04]
        V_DMES=[ABRIL    ]
     CASE SUBS(v_vencim,1,2)=[05]
        V_DMES=[MAYO     ]
     CASE SUBS(v_vencim,1,2)=[06]
        V_DMES=[JUNIO    ]
     CASE SUBS(v_vencim,1,2)=[07]
        V_DMES=[JULIO    ]
     CASE SUBS(v_vencim,1,2)=[08]
        V_DMES=[AGOSTO   ]
     CASE SUBS(v_vencim,1,2)=[09]
        V_DMES=[SETIEMBRE]
     CASE SUBS(v_vencim,1,2)=[10]
        V_DMES=[OCTUBRE  ]
     CASE SUBS(v_vencim,1,2)=[11]
        V_DMES=[NOVIEMBRE]
     CASE SUBS(v_vencim,1,2)=[12]
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
If v_feccob<>date()
  DO CASE
     CASE MONT(v_feccob)=1
        V_IMES=[ENERO    ]
     CASE MONT(v_feccob)=2
        V_IMES=[FEBRERO  ]
     CASE MONT(v_feccob)=3
        V_IMES=[MARZO    ]
     CASE MONT(v_feccob)=4
        V_IMES=[ABRIL    ]
     CASE MONT(v_feccob)=5
        V_IMES=[MAYO     ]
     CASE MONT(v_feccob)=6
        V_IMES=[JUNIO    ]
     CASE MONT(v_feccob)=7
        V_IMES=[JULIO    ]
     CASE MONT(v_feccob)=8
        V_IMES=[AGOSTO   ]
     CASE MONT(v_feccob)=9
        V_IMES=[SETIEMBRE]
     CASE MONT(v_feccob)=10
        V_IMES=[OCTUBRE  ]
     CASE MONT(v_feccob)=11
        V_IMES=[NOVIEMBRE]
     CASE MONT(v_feccob)=12
        V_IMES=[DICIEMBRE]
  ENDCASE
Endif
If v_fecalt<>ctod("//")
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
Endif

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
   STORE MONTO1 TO valnel
   RETURN

* BUSQUEDA DE CLIENTES POR NOMBRE
  PROCEDURE BUSCATITU
     on key
     activate window busqueda
     @ 00     ,01 clear to 05,48
     if lastkey()<>27
        store space(35) to v_buscar
        @ 00     ,01 say [ Nombre?      :]
        @ row()+2,01 say [Esta opcion le encontrara el titular con el cual desea trabajar.]
        @ 00     ,16 get v_buscar pict[@!]
        read
        set order to nombre
        go top
        set near on
        set exact off
        seek allt(v_buscar)
     endif
     deactivate window busqueda
     set near off
  && set exact on
     do tecla_cliente
  RETURN

* BUSQUEDA DE BENEFICIARIOS POR NOMBRE
  PROCEDURE BUSCABENE
     on key
     activate window busqueda
     @ 00     ,01 clear to 05,48
     if lastkey()<>27
        store space(35) to v_buscar
        @ 00     ,01 say [ Nombre?      :]
        @ row()+2,01 say [Esta opcion le encontrara el beneficiario con el cual desea tra-    ]
        @ row()+1,01 say [bajar.                                                              ]
        @ 00     ,16 get v_buscar pict[@!]
        read
        set order to benefi_nom
        go top
        set near on
        set exact off
        seek (v_codsuc + v_buscar)
     endif
     deactivate window busqueda
     set near off
  && set exact on
     do tecla_benefi
  RETURN

* SOLICITUD DE MODIFICACION DE DATOS:
  procedure solic_modifi
  parameter oridatos
  store space(30)to solicita
  do case
     case oridatos =[T]
          store [el titular]        to itemmodi
          store []                  to origiagr
          &&store transform(client_tit,[999999])+[ ]+alltrim(client_nom)+ [ - ]+ [C.I.: ]+transform(client_ndo,[99999999]);
                                    to origicam
          store []                  to origicod
          store [client_m]          to salebusq,archimod
          store [tecla_cliente]     to teclbusq
     case oridatos =[B]
          store [el beneficiario]   to itemmodi
          store benefi_pro+[ ]+transform(benefi_con,[999999]);
                                    to origiagr
          store transform(benefi_num,[9999999])+[ ]+alltrim(benefi_nom)+[ - ]+origiagr;
                                    to origicam
          store [benefi_d]          to salebusq,archimod
          store [tecla_benefi]      to teclbusq
     case oridatos =[E]
          store [la calle]          to itemmodi
          store ciuciu+[, ]+ciupai  to origiagr
          store alltrim(calles_nom)+[, ]+ciuciu+[, ]+ciupai to origicam
          store [CALLES_M]          to salebusq,archimod
          store [tecla_calles]      to teclbusq
  endcase
  on key
  select modifi_m
  go bottom
  store val(modifi_num)+1 to regisnum
  store str(regisnum,5)   to regisnum
  store alltrim(regisnum) to regisnum
  store dtoc(date())      to fechasol
  activate window solmodif
  @ 00,00 say [Solicitud No. ]+regisnum+[ del ]+fechasol
  @ 01,00 say [Por medio de la presente, solicito...]
  @ 07,14 say [Haga su eleccion o si desiste, <ESC>Escape]
  activate window tipomodi
  @ 00,00 prompt [ \<Cambiar ]
  @ 00,11 say    [o]
  @ 00,13 prompt [ \<Agregar ]
  menu to opctip
  deactiva window tipomodi
  if lastkey()<>27
     @ 04,00 say [ADVERTENCIA: Por favor escriba exactamente como diroa el NUEVO dato]
     @ 07,14 say [Digote el dato  o  si desiste, <ESC>Escape]
     if opctip=1
        store [Cambiar] to tipomodi
        @ 01,34 say [ que se cambie ]+upper(itemmodi)
        @ 02,00 say [de: ]+origicam
        @ 03,00 say [a :]get solicita pict[@!!!!!!!!!!!!!!!!!!!!!!!!!!!!!]
        read
        @ 03,00 say space(72)
        @ 03,00 say [a : ]+solicita
        store tipomodi+[ ]+substr(itemmodi,4)+[ a ]+alltrim(solicita) to textmodi
        store origicam                                                to origmodi
     else
        store [Agregar] to tipomodi
        @ 01,34 say [ que se agrege ]+upper(itemmodi)
        if itemmodi<>[el titular]
           @ 02,00 say [dentro de: ]+origiagr
        endif
        @ 03,00 get solicita pict[@!]
        read
        @ 03,00 say space(72)
        if .not. empty (solicita)
           @ 03,00 say alltrim(solicita)
        else
           @ 03,00 say alltrim(solicita)+[, ]+origiagr
        endif
        store tipomodi+[ ]+substr(itemmodi,4)+[ ]+alltrim(solicita) to textmodi
        store origiagr                                              to origmodi
     endif
     if lastkey()<>27
        scatter memvar blank
        @ 04,00 say space(72)
        @ 04,00 say [Debido a que:]
        @ 12,00 say [ADVERTENCIA!: Si Usted no comenta justificando adecuadamente su solici-]
        @ 13,00 say [              tud osta no sero grabada asi como tampoco sero procesada.]
        @ 15,22 say [<CTRL-W>Graba <ESC>Escapa]
        scatter memvar memo blank
        @ 05,00 edit m.modifi_det size 06,72
        read
        if lastkey()<>27
           if empty(m.modifi_det)
              wait [ADVERTENCIA!: No puede ser vacio, su dato no sero procesado] wind nowait
           else
              @ 12,00 clear to 15,72
              activate window confirma
              @ 00,03 prompt [   \<No   ]
              @ 00,11 prompt [   \<Si   ]
              menu to opccon
              deactiva window confirma
              if opccon=2
                 store regisnum to m.modifi_num
                 store date()   to m.modifi_alt
                 store textmodi to m.modifi_txt
                 store origmodi to m.modifi_ori
                 store archimod to m.modifi_arc
                 store solicita to m.modifi_cam
                 store origicod to m.modifi_cod
                 store usuarios to m.modifi_usu
                 store usuarpai to m.modifi_upa
                 store usuarper to m.modifi_unu
                 append blank
                 gather memvar
                 gather memvar memo
              endif
           endif
        endif
     endif
  endif
  deactiva window solmodif
  select &salebusq
  do     &teclbusq
  RETURN

* LISTA DE LLAMADAS POR CLIENTES.
  PROCEDURE ACTIVACOBROS

  selec 15
  use activa_m

  ******************************************************
  *  Verifica si el cliente tiene llamados             *
  ******************************************************
  on key
  selec activa_m
  set order to activa_tit
  go top
  seek (v_codsuc + transform(v_nrocta,[999999]))
  if eof()
     wait " ESTE CLIENTE NO REGISTRA LLAMADOS - PRESIONE UNA TECLA... " window
     selec activa_m
     use
     do tecla_pago
     return
  endif
  store usuarios + [.txt] to v_disacco
  save screen to panta07
  set printer to M:\-DATOS\SICMADAT\CCE\&v_disacco
  set device  to printer

  ************************
  *  Inicio del listado  *
  ************************
  scatter memvar
  do titulo
  v_totlla = 0  									&& total de llamados.
  v_totaco = 0  									&& total a cobrar.
  do while .t.
     *********************
     *  Acumula totales  *
     *********************
     v_totlla = v_totlla + 1       					&& total de llamados.
     v_totaco = v_totaco + m.activa_mon  			&& total a cobrar.
     *******************
     *  Imprime linea  *
     *******************
     @prow()+1,01     say m.activa_pro  + ": " + TRAN(m.activa_con,"999999") + " "+ m.activa_nom
     @prow()  ,pcol() say space(05) + "(" + TRAN(m.activa_nll,"999999") + ")"
     @prow()+1,01     say "DIRECCION...: " + m.activa_dir
     @prow()+1,01     say "INTERSECCION: " + m.activa_int + "   (" + m.activa_cob + ")"
     @prow()+1,01     say "TELEFONOS: PARTICULAR: " + m.activa_tpa
     IF LEN(ALLT(m.activa_tcm)) # 0
        @prow()+1,01 say "           COMERCIAL.: " + m.activa_tcm
     ENDIF
     IF LEN(ALLT(m.activa_tcb)) # 0
        @prow()+1,01 say "           DE COBRO..: " + m.activa_tcb
     ENDIF
     @prow()+1,01 say  "VOLVER A LLAMAR EL DIA: " + DTOC(m.activa_lla)
     IF LEN(ALLT(m.activa_ob1)) # 0
        @prow()+1,01 say  SPAC(05) + m.activa_ob1
     ENDIF
     IF LEN(ALLT(m.activa_ob2)) # 0
        @prow()+1,01 say  SPAC(05) + m.activa_ob2
     ENDIF
     IF LEN(ALLT(m.activa_ob3)) # 0
        @prow()+1,01 say  SPAC(05) + m.activa_ob3
     ENDIF
     IF LEN(ALLT(m.activa_se1)) # 0 .OR. LEN(ALLT(m.activa_se2)) # 0;
        LEN(ALLT(m.activa_se3)) # 0 .OR. LEN(ALLT(m.activa_se4)) # 0;
        LEN(ALLT(m.activa_se5)) # 0 .OR. LEN(ALLT(m.activa_se6)) # 0
        @prow()+1,01 say space(01)
        @prow()+1,01 say "SEGUIMIENTO: "
        IF LEN(ALLT(m.activa_se1)) # 0
          @prow()+1,01 say  SPAC(05) + m.activa_se1
        ENDIF
        IF LEN(ALLT(m.activa_se2)) # 0
          @prow()+1,01 say  SPAC(05) + m.activa_se2
        ENDIF
        IF LEN(ALLT(m.activa_se3)) # 0
          @prow()+1,01 say  SPAC(05) + m.activa_se3
        ENDIF
        IF LEN(ALLT(m.activa_se4)) # 0
          @prow()+1,01 say  SPAC(05) + m.activa_se4
        ENDIF
        IF LEN(ALLT(m.activa_se5)) # 0
          @prow()+1,01 say  SPAC(05) + m.activa_se5
        ENDIF
        IF LEN(ALLT(m.activa_se6)) # 0
          @prow()+1,01 say  SPAC(05) + m.activa_se6
        ENDIF
     ENDIF
     @prow()+1,01  say  REPLI("o",79)
     if .not. eof()
        skip
        scatter memvar
     endif
     IF v_nrocta # m.activa_tit .OR. EOF()
        EXIT
     ENDIF
  ENDDO
  @prow()+1,01 say "o" + REPLI("o",77) + "o"
  @prow()+1,01 say "o" + SPAC(10) + "** " + "TOTAL DE LLAMADOS:" + TRAN(v_totlla,"9999") + " ** " + "TOTAL A COBRAR:" + TRAN(v_totaco,"999,999,999") + " **" + SPAC(09) + "o"
  @prow()+1,01 say "o" + REPLI("o",77) + "o"
  set device  to screen
  *set printer to prn
  set console on
  modify comm M:\-DATOS\SICMADAT\CCE\&v_disacco noedit window acticobro
  delete file M:\-DATOS\SICMADAT\CCE\&v_disacco
  set color to i
  restore screen from panta07
  selec activa_m
  use
  do tecla_pago
  RETURN

*  TITULO DEL LISTADO DEL ACTIVADOR DE COBROS
   PROCEDURE TITULO
      @00      ,01 say "o" + REPLI("o",77) + "o"
      @prow()+1,01 say "o  " + "LLAMADOS DEL ACTIVADOR DE COBROS AL " + DTOC(DATE()) + SPAC(28) + " o"
      @prow()+1,01 say "o" + REPLI("o",77) + "o"
   RETURN

* SEGUIMIENTO DE RECLAMOS DE CLIENTES.
  PROCEDURE RECLACLIEN

   selec 16
   use reclam_m
   selec 17
   use forrec_m order forrec_cod
   selec 18
   use tiprec_m order tiprec_cod

  ******************************************************
  *  Verifica si el cliente tiene reclamos             *
  ******************************************************
  on key
  selec reclam_m
  set order to reclam_tit
  go top
  seek (v_codsuc + transform(v_nrocta,[999999]))
  if eof()
     wait " ESTE CLIENTE NO TIENE RECLAMOS - PRESIONE UNA TECLA... " window
     do finreclamos
     return
  endif
  store usuarios + [.txt] to v_disacco
  save screen to panta07
  set printer to M:\-DATOS\SICMADAT\CCE\&v_disacco
  set device  to printer
  ************************
  *  Inicio del listado  *
  ************************
  scatter memvar
  v_totaco = 0  									&& total a cobrar.
  v_canrec = 0 							            && cantidad de reclamos.
  do titurecla
  do while .t.
     *********************
     *  Acumula totales  *
     *********************
     v_canrec = v_canrec + 1       					&& cantidad de reclamos.
     v_totaco = v_totaco + m.reclam_mon  			&& total a cobrar.
     *******************
     *  Imprime linea  *
     *******************
      @prow()+1,01 say  " RECLAMO NRO.: " + TRANSFORM(reclam_num,"999999")
      @prow()+1,01 say  " " + m.reclam_pro + "........: " + TRANSFORM(m.reclam_con,"999999") + " " + v_nomtit;
                                           + "  " + "COBRADOR..: " + m.reclam_cob
      selec forrec_m
      go top
      seek (transform(m.reclam_atr,[999]))
      @prow()+1,01 say " SE RECIBIO EL " + dtoc(m.reclam_fec) + " A LAS " + m.reclam_hor + " Hs. - " + forrec_des
      @prow()+1,01 say " DIRECCION...: " + m.reclam_dir
      @prow()+1,01 say " INTERSECCION: " + m.reclam_int
      selec tiprec_m
      go top
      seek (transform(m.reclam_tip,[999]))
      @prow()+1,01 say " RECLAMANDO..: " + tiprec_des + SPAC(08) + "A COBRAR...:" + TRAN(m.reclam_mon,"999,999,999")
      selec reclam_m
      if m.reclam_ob1 # SPACE(69)
         @prow()+1,01   say SPACE(10) + m.reclam_ob1
      endif
      if m.reclam_ob2 # SPACE(69)
         @prow()+1,01   say SPACE(10) + m.reclam_ob2
      endif
      if m.reclam_ob3 # SPACE(69)
         @prow()+1,01   say SPACE(10) + m.reclam_ob3
      endif
      if m.reclam_ob4 # SPACE(69)
         @prow()+1,01   say SPACE(10) + m.reclam_ob4
      endif
      if m.reclam_se1 # SPACE(69) .OR. m.reclam_se2 # SPACE(69);
         m.reclam_se3 # SPACE(69) .OR. m.reclam_se4 # SPACE(69);
         m.reclam_se5 # SPACE(69) .OR. m.reclam_se6 # SPACE(69)

         @prow()+1,01 say "SEGUIMIENTO: "
         if m.reclam_se1 # SPACE(69)
            @prow()+1,01 say  SPACE(10) + m.reclam_se1
         endif
         if m.reclam_se2 # SPACE(69)
            @prow()+1,01 say  SPACE(10) + m.reclam_se2
         endif
         if m.reclam_se3 # SPACE(69)
            @prow()+1,01 say  SPACE(10) + m.reclam_se3
         endif
         if m.reclam_se4 # SPACE(69)
            @prow()+1,01 say  SPACE(10) + m.reclam_se4
         endif
         if m.reclam_se5 # SPACE(69)
            @prow()+1,01 say  SPACE(10) + m.reclam_se5
         endif
         if m.reclam_se6 # SPACE(69)
            @prow()+1,01 say  SPACE(10) + m.reclam_se6
         endif
      endif
      @prow()+1,01 say " SE FINIQUITO EL " + dtoc(m.reclam_ffi) + " A LAS " + m.reclam_hfi + " Hs."
      @prow()+1,01 say replicate("o",79)
      IF .NOT. EOF()
         skip
         scatter memvar
      ENDIF
      IF v_nrocta # m.reclam_tit .OR. EOF()
         EXIT
      ENDIF
  enddo
  @prow()+1,01 say "o" + REPLI("o",77) + "o"
  @prow()+1,01 say "o" + SPAC(10) + "** " + "TOTAL DE RECLAMOS:" + TRAN(v_canrec,"9999") + " ** " + "TOTAL A COBRAR:" + TRAN(v_totaco,"999,999,999") + " **" + SPAC(09) + "o"
  @prow()+1,01 say "o" + REPLI("o",77) + "o"
  set device  to screen
  set printer to prn
  set console on

  modify comm M:\-DATOS\SICMADAT\CCE\&v_disacco noedit window acticobro
  delete file M:\-DATOS\SICMADAT\CCE\&v_disacco

  set color to i
  restore screen from panta07
  do finreclamos
  RETURN

* Titulo del listado de reclamos.
  PROCEDURE TITURECLA
     @00      ,01  say "o"   + REPLI("o",77) + "o"
     @prow()+1,01  say "o"   + "RECLAMOS DE CLIENTES" + space(57) + "o"
     @prow()+1,01  say "o"   + REPLI("o",77) + "o"
  RETURN

* Fin de reclamos
  PROCEDURE FINRECLAMOS
     selec reclam_m
     use
     selec forrec_m
     use
     selec tiprec_m
     use
     do tecla_pago
  RETURN

* SEGUIMIENTO DE MENSAJES A CLENTES.
  PROCEDURE MENSACLIEN

   selec 19
   use menmen_m
   set order to menmen_cod
   selec 20
   use menmot_m
   selec 21
   use menenc_m
   selec 22
   use mensaj_m
   selec 23
   use tipent_m

  ******************************************************
  *  Verifica si el cliente tiene mensajes             *
  ******************************************************
  on key
  selec mensaj_m
  set order to mensaj_tit
  go top
  seek (v_codsuc + transform(v_nrocta,[999999]))
  if eof()
     wait " ESTE CLIENTE NO REGISTRA MENSAJES - PRESIONE UNA TECLA... " window
     do finmensajes
     return
  endif
  store usuarios + [.txt] to v_disacco
  save screen to panta07
  set printer to M:\-DATOS\SICMADAT\CCE\&v_disacco
  set device  to printer
  ************************
  *  Inicio del listado  *
  ************************
  scatter memvar
  v_totent = 0  									&& cantidad de mensajes.
  do titumensa
  do while .t.
     *********************
     *  Acumula totales  *
     *********************
     v_totent = v_totent + 1       					&& cantidad de mensajes.
     selec menmot_m
     set order to menmot_cod
     go top
     seek (m.mensaj_mot)
     *******************
     *  Imprime linea  *
     *******************
     @prow()+1,01 say m.mensaj_pro + ": " + TRAN(m.mensaj_con,[999999]) + " " + m.mensaj_nom + " "
     if m.mensaj_urg = "Si"
        @prow(),pcol() say "! URGENTE ! "
     else
        @prow(),pcol() say "            "
     endif
     if (date() - m.mensaj_fen) > 1
        @prow(),pcol() say "ATRASO:" + TRAN((date() - m.mensaj_fen),[999]) + " DIAS"
     else
        @prow(),pcol() say "              "
     endif
     @prow(),pcol()    say "(" + TRAN(m.mensaj_enc,[99]) + ")"
     @prow()+1,01      say "CALLE: " + ALLT(m.mensaj_dir) + " " + m.mensaj_int
     IF LEN(ALLT(m.mensaj_ob1)) # 0
        @prow()+1,01   say SPAC(05) + m.mensaj_ob1
     ENDIF
     IF LEN(ALLT(m.mensaj_ob2)) # 0
        @prow()+1,01   say SPAC(05) + m.mensaj_ob2
     ENDIF
     IF LEN(ALLT(m.mensaj_ob3)) # 0
        @prow()+1,01   say SPAC(05) + m.mensaj_ob3
     ENDIF
     IF LEN(ALLT(m.mensaj_ob4)) # 0
        @prow()+1,01   say SPAC(05) + m.mensaj_ob4
     ENDIF
     selec tipent_m
     IF m.mensaj_l01 = "Si"
        go 1
        @prow()+1,01     say SPAC(17) + tipent_con + " " + m.mensaj_o01
     ENDIF
     IF m.mensaj_l02 = "Si"
        go 2
        @prow()+1,01     say SPAC(17) + tipent_con + " " + m.mensaj_o02
     ENDIF
     IF m.mensaj_l03 = "Si"
        go 3
        @prow()+1,01     say SPAC(17) + tipent_con + " " + m.mensaj_o03
     ENDIF
     IF m.mensaj_l04 = "Si"
        go 4
        @prow()+1,01     say SPAC(17) + tipent_con + " " + m.mensaj_o04
     ENDIF
     IF m.mensaj_l05 = "Si"
        go 5
        @prow()+1,01     say SPAC(17) + tipent_con + " " + m.mensaj_o05
     ENDIF
     IF m.mensaj_l06 = "Si"
        go 6
        @prow()+1,01      say SPAC(17) + tipent_con + " " + m.mensaj_o06
     ENDIF
     IF m.mensaj_l07 = "Si"
        go 7
        @prow()+1,01     say SPAC(17) + tipent_con + " " + m.mensaj_l07
     ENDIF
     IF m.mensaj_l08 = "Si"
        go 8
        @prow()+1,01     say SPAC(17) + tipent_con + " " + m.mensaj_l08
     ENDIF
     IF m.mensaj_l09 = "Si"
        go 9
        @prow()+1,01     say SPAC(17) + tipent_con + " " + m.mensaj_l09
     ENDIF
     IF m.mensaj_l10 = "Si"
        @prow()+1,01     say SPAC(17) + tipent_con + " " + m.mensaj_l10
     ENDIF
     selec mensaj_m
     IF .NOT. EOF()
        skip
        scatter memvar
     ENDIF
     IF v_nrocta # m.mensaj_tit .OR. EOF()
        EXIT
     ENDIF
  enddo
  @prow()+1,01 say  REPLI("o",79)
  @prow()+1,01 say  "o" + REPLI("o",77) + "o"
  @prow()+1,01 say  "o" + SPAC(25) + "** " + "TOTAL DE ENTREGAS:" + TRAN(v_totent,[9999]) + " **" + SPAC(24) + "o"
  @prow()+1,01 say  "o" + REPLI("o",77) + "o"
  set device  to screen
  *set printer to prn
  set console on
  modify comm M:\-DATOS\SICMADAT\CCE\&v_disacco noedit window acticobro
  delete file M:\-DATOS\SICMADAT\CCE\&v_disacco

  set color to i
  restore screen from panta07
  do finmensaje
  RETURN

* Titulo del listado de mensajes.
  PROCEDURE TITUMENSA
     @01,01       say "o"   + REPLI("o",77) + "o"
     @prow()+1,01 say "o  " + "ENTREGAS A CLIENTE  AL " + DTOC(DATE()) + SPAC(32)
     @prow()+1,01 say "o  " +  "TODOS LOS MENSAJEROS"   + SPAC(55) + "o"
     @prow()+1,01 say "o" + REPLI("o",77) + "o"
  RETURN

* Finalizar mensajes
  PROCEDURE FINMENSAJES
     selec menmen_m
     use
     selec menmot_m
     use
     selec menenc_m
     use
     selec mensaj_m
     use
     selec tipent_m
     use
     do tecla_pago
  RETURN

*****************************************************
* Verifica si el cliente tiene avisos de cobradores *
*****************************************************
PROCEDURE AVISOCOBRA
  selec 19
  use avisos_m
  set order to avisos_num
  selec 20
  use codavi_m

  ****************************************************
  *  Verifica si el cliente tiene avisos pendientes  *
  ****************************************************
  selec avisos_m
  set order to avisos_tit
  go top
  seek (v_codsuc + transform(v_nrocta,[999999]))
  if eof()
     wait " ESTE CLIENTE NO TIENE AVISOS PENDIENTES " window
     do finavisos
     return
  endif

  store usuarios + [.txt] to v_disacco
  save screen to panta07
  set printer to M:\-DATOS\SICMADAT\CCE\&v_disacco
  set device  to printer

  ************************
  *  Inicio del listado  *
  ************************
  v_totavi = 0  							&& total de avisos.
  v_totaco = 0  							&& total a cobrar.

  scatter memvar
  do tituaviso
  do while .t.
     *********************
     *  Acumula totales  *
     *********************
     v_totavi = v_totavi + 1   			    	&& total de avisos.
     v_monaco = m.avisos_psm + m.avisos_uds + m.avisos_cms + m.avisos_psv + m.avisos_otr
     v_totaco = v_totaco + v_monaco         	&& total a cobrar.
     *******************
     *  Imprime linea  *
     *******************
     @ prow()+1,01 say dtoc(m.avisos_fca)+space(01)+m.avisos_pro+": "+tran(m.avisos_con,"999999")+" "+m.avisos_nom
     @ prow()  ,pcol() say SPAC(08)+" "+m.avisos_cob + " (" + tran(m.avisos_num,"999999") + ")"
     @ prow()+1,01 say "  DIRECCION...: " + avisos_dir
     @ prow()+1,01 say "  INTERSECCION: " + avisos_int
     if m.avisos_psm # 0
        @ prow()+1,01 say SPAC(07) + "PSM " + tran(m.avisos_psm,"999,999,999") + " " + m.avisos_osm
     endif
     if m.avisos_uds # 0
        @ prow()+1,01 say SPAC(07) + "UDS " + tran(m.avisos_uds,"999,999,999") + " " + m.avisos_ods
     endif
     if m.avisos_cms # 0
        @ prow()+1,01 say SPAC(07) + "CMS " + tran(m.avisos_cms,"999,999,999") + " " + m.avisos_oms
     endif
     if m.avisos_psv # 0
        @ prow()+1,01 say SPAC(07) + "PSV " + tran(m.avisos_psv,"999,999,999") + " " + m.avisos_psv
     endif
     if m.avisos_otr # 0
        @ prow()+1,01 say SPAC(07) + "OTR " + tran(m.avisos_otr,"999,999,999") + " " + m.avisos_obs
     endif
     @ prow(),pcol()+1 say tran(v_monaco,"999,999,999")
     @ prow()+1,01     say REPLI("o",79)
     @ prow()+1,01     say REPLI("o",79)

     selec avisos_m

     if .not. eof()
        skip
        scatter memvar
     endif
     if v_nrocta # m.avisos_tit .or. eof()
        exit
     endif
  enddo
  @ prow()+1,01 say "o" + REPLI("o",78) + "o"
  @ prow()+1,01 say "o" + SPAC(11) + "** " + "TOTAL DE AVISOS:" + TRAN(v_totavi,"9999") + " ** " + "TOTAL A COBRAR:"
  @ prow(),pcol()+1 say TRAN(v_totaco,"999,999,999") + " **" + SPAC(10) + "o"
  @ prow()+1,01 say  "o" + REPLI("o",78) + "o"
  set device  to screen
  set printer to prn
  set console on

  modify comm M:\-DATOS\SICMADAT\CCE\&v_disacco noedit window acticobro
  delete file M:\-DATOS\SICMADAT\CCE\&v_disacco

  set color to i
  restore screen from panta07
  do finavisos
RETU

* Titulo del avisos.
  PROCEDURE TITUAVISO
     @ 01,01       say "o"   + REPLI("o",77) + "o"
     @ prow()+1,01 say "o  " + "AVISOS POR CLIENTES AL " + DTOC(DATE()) + SPAC(30) + "            o"
     @ prow()+1,01 say "o  " + "CLIENTE: " + m.avisos_pro + TRAN(m.avisos_con," 999999") + " "+ m.avisos_nom + SPAC(20) + "o"
     @ prow()+1,01 say "o" + REPLI("o",77) + "o"
  RETURN

* Finalizar avisos
  PROCEDURE FINAVISOS
     selec avisos_m
     use
     do tecla_pago
  RETURN

* RUTINA DE VALIDACION DE DERECHOS.
  procedure val_der
  parameter claveusu,capitulo,derechos
  store alltrim(claveusu)+alltrim(capitulo)+alltrim(derechos) to peticion
  store space(02) to habilita
  select derech_m
  seek peticion
  if eof()
     store [NO] to habilita
     wait [ATENCION !!, USUARIO NO HABILITADO COMO CAJERO ] window
  endif
  return

 * RUTINA DE VALIDACION DE CTA. BANCARIA P/DEPOSITOS
   PROCEDURE VALICTABAN
      if v_ctaban = space(10)
         wait "Atencion !!!, no ha seleccionado cta. bancaria p/deposito" window
         v_flak01 = "f"
         return
      endif
      v_flak01 = "t"
   RETURN

* Rutina auxiliar temporal
  procedure auxitemp
       if v_fecaux <> ctod([//])
           v_fecalt = ctod([//])
           v_feccob = v_fecaux
           v_fecaux = ctod([//])
       endif
  return

* FIN


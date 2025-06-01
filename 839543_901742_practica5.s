				AREA datos,DATA,readwrite
;vuestras variables y constantes
VICIntEnable 	EQU 0xFFFFF010 ;act. Irqs 
VICIntEnClear 	EQU 0xFFFFF014 ;desact. IRQs 
VICVectAddr0 	EQU 0xFFFFF100 ;vector interr. (VI)
VICVectAddr 	EQU 0xFFFFF030 ;reg. para EOI	
T0_IR			EQU 0xE0004000 ; reg. bajar IRQ[4]
timer_so		DCD 0		   ;var. para @RSI_timer_SO
RDAT 			EQU 0xE0010000 ;reg datos tec (UART1)
N               EQU 60 ;tamagno del rastro

vector1     SPACE 4*N; cada bit es (x,y)
vector2     SPACE 4*N; cada bit es (x,y)
tecl_so		    DCD 0		   ;var. para @RSI_tecl_SO
terminar	    DCD 0		   ;indicador terminacion

semilla 		EQU 0x7673A639 ; Semilla para generar numeros aleatorios

cabecera        EQU 0x40007E00 ; inicio de la cabecera
pantalla 		EQU 0x40007E20 ; inicio de la pantalla de juego
nFil	        EQU 15 ; numero de filas
nCol			EQU 32	; numero de columnas
espBlanco		EQU 32 ; Espacio en blanco (ASCII)



	
X				EQU 88; X (ASCII)
O				EQU 79; O (ASCII)
DP				EQU 58

x1				DCB 0; coordenada horizontal del jugador 1 [0-31]
y1				DCB 0; coordenada vertical del jugador 1 [0-14]
x2				DCB 0; coordenada horizontal del jugador 2 [0-31]
y2				DCB 0 ; coordenada vertical del jugador 2 [0-14]

dir1x DCB 0 ;mov. horizontal jugador1 (-1 izq,0 col fija,1 der)
dir1y DCB 0 ;mov. vertical jugador1 (-1 arriba,0 fila fija,1 abajo)
dir2x DCB 0 ;mov. horizontal jugador2 (-1 izq.,0 col fija,1 der)
dir2y DCB 0 ;mov. vertical jugador2 (-1 arriba,0 fila fija,1 abajo)
reloj		DCD 0		;contador de centesimas de segundo
max         DCD 32      ; frecuencia de movimiento
cont        DCD 0		;velocidad de movimiento (en centesimas de s.)
contador    DCD 0       ; cuenta los movimientos de la partida



puntos1				DCB '0'; Puntos jugador 1
puntos2             DCB '0'; Puntos jugador 2
cincoASCII			EQU 53; 5 (ASCII)
quieto1				DCB 0; 1 si el jugador 1 se ha quedado sin movimientos posibles
quieto2				DCB 0; 1 si el jugador 2 se ha quedado sin movimientos posibles
j1				DCB 'J','U','G','A','D','O','R',' ','1'
j2				DCB 'J','U','G','A','D','O','R',' ','2'
i1              DCB  1 ;para manejar los indices del vector1
i2              DCB  1 ; para manejar los indices del vector2

tamt1 			EQU 9; Para formato de cabecera

			
		AREA codigo,CODE,readonly
		EXPORT inicio			; forma de enlazar con el startup.s
		IMPORT srand			; para poder invocar SBR srand
		IMPORT rand				; para poder invocar SBR rand
inicio
		; Guarda VI[4] y VI[7] en timer_so y teclado_so
		LDR r0,= VICVectAddr0   ;r0 = @VI
		LDR r1,=timer_so		;r1=@timer_so
		add r0, r0, #16			; r0=@VI[4]
		ldr r3,[r0],#12         ;r3 = VI[4] = @RSI_timer_SO
								;r0= @VI[7]
		str r3,[r1]				;timer_so=@RSI_timer_SO
		LDR r1,=tecl_so		    ;r1=@teclado_so
		ldr r3, [r0],#-12		;r3 = VI[7]=@RSI_timer_SO
								;r0=@VI[4]
		str r3,[r1]				;teclado_so=@RSI_teclado_SO
		;VI almacenar en las correspondientes direcciones de VI
		;las direcciones de nuestras rutinas de servicios
		LDR r1,=RSI_timer		;r1=@RSI_timer
		str r1, [r0],#12        ;VI[4]=@RSI_timer
								;r0=@VI[7]
		LDR r1,=RSI_teclado		;r1=@RSI_teclado
		str r1,[r0]			    ;VI[7]=@RSI_teclado
		;
		LDR r0,=VICIntEnable	;r0=@VICIntEnable
		mov r1,#0x90			;r1=#2_10010000
		str r1,[r0]				;VICIntEnable[4]=1 
								;VICIntEnable[7]=1
		;GENERAR SEMILLA
		LDR r0,=semilla			;r0=semilla
		PUSH{r0}				;apilar par. (v. de semilla)
		bl srand				;llamada a subrutina srand
		add sp, sp, #4			;liberar parametros
		;Limpiar Cabecera
		LDR r0,=cabecera	  	;r0=@cabecera
		LDR r1,=nCol            ;r1=numElementos
		LDR r2,=espBlanco		;r2=' '
bLimp	strb r2,[r0],#1			;pantalla[i][j]=' '
		subs r1, r1, #1			;r1=r1-1
		bne bLimp				;salta si r1!=0				
		;Dibujar cabecera
		LDR r0,=cabecera	
		LDR r1,=j1
		mov r2, #tamt1				;r2=tamt1
bCJ1	ldrb r3,[r1],#1
		strb r3,[r0],#1
		subs r2, r2, #1
		bne bCJ1
		LDR r0,=cabecera+23
		LDR r1,=j2
		mov r2, #tamt1				;r2=tamt1
bCJ2	ldrb r3,[r1],#1
		strb r3,[r0],#1
		subs r2, r2, #1
		bne bCJ2
		LDR r0,=cabecera
		LDR r1,=puntos1
		LDR r2,=puntos2
		ldrb r1,[r1]
		ldrb r2,[r2]
		strb r1,[r0,#15]
		strb r2,[r0,#17]
		mov r2, #DP
		strb r2,[r0,#16]
		;Limpiar Pantalla de Juego
bPart   LDR r0,=pantalla		;r0=@cabecera
		LDR r1,=(nFil)*nCol   ;r1=numElementos
		LDR r2,=espBlanco		;r2=' '
bLimpJ	strb r2,[r0],#1			;pantalla[i][j]=' '
		subs r1, r1, #1			;r1=r1-1
		bne bLimpJ				;salta si r1!=0			
		;GENERAR  DIRECCIONES INICIALES
		;JUGADOR 1
		LDR r0,=dir1x			;r0=@dir1x
		LDR r1,=dir1y			;r1=@dir1y
		sub sp, sp, #4			;Espacio para resultado
		bl rand					;Llamada a num aleatorio
		POP{r2}
		movs r2, r2, LSR #1		; C=r2[0]
		bcs InJ1Vert			; Salta si C=1
		; JUGADOR 1 COMIENZA EN LA HORIZONTAL
InJ1Hor mov r3, #0				;r3=0
		strb r3,[r1]			;dir1y=0
		movs r2, r2, LSR #1		; C=r2[1]
		movcs r2, #1			;r2=1 si C=1
		movcc r2, #-1			;r2=-1 si C=0
		strb r2,[r0]			; dir1x=r2
		; JUGADOR 1 COMIENZA EN LA VERTICAL
InJ1Vert mov r3, #0				;r3=0
		strb r3,[r0]			;dir1x=0
		movs r2, r2, LSR #1		; C=r2[1]
		movcs r2, #1			;r2=1 si C=1
		movcc r2, #-1			;r2=-1 si C=0
		strb r2,[r1]			; dir1x=r2
		;JUGADOR 2
		LDR r0,=dir2x			;r0=@dir2x
		LDR r1,=dir2y			;r1=@dir2y
		sub sp, sp, #4			;Espacio para resultado
		bl rand					;Llamada a num aleatorio
		POP{r2}
		movs r2, r2, LSR #1		; C=r2[0]
		bcs InJ2Vert			; Salta si C=1
		; JUGADOR 2 COMIENZA EN LA HORIZONTAL
InJ2Hor mov r3, #0				;r3=0
		strb r3,[r1]			;dir2y=0
		movs r2, r2, LSR #1		; C=r2[1]
		movcs r2, #1			;r2=1 si C=1
		movcc r2, #-1			;r2=-1 si C=0
		strb r2,[r0]			; dir2x=r2
		; JUGADOR 2 COMIENZA EN LA VERTICAL
InJ2Vert mov r3, #0				;r3=0
		strb r3,[r0]			;dir2x=0
		movs r2, r2, LSR #1		; C=r2[1]
		movcs r2, #1			;r2=1 si C=1
		movcc r2, #-1			;r2=-1 si C=0
		strb r2,[r1]			; dir2x=r2		
		; POSICIONES INICIALES
		; X1	
		ldr r1,=x1
		sub sp, sp, #4			;Espacio para resultado
		bl rand					;Llamada a num aleatorio
		POP{r0}
		and r0, r0, #0x1F;			;Mantener bits [4:0] (nums del 0 al 31)
		strb r0, [r1]			;x1=r2[4:0]
		; Y1
		ldr r1,=y1
		sub sp, sp, #4			;Espacio para resultado
		bl rand					;Llamada a num aleatorio
		POP{r2}
		mov r3, #nFil-1		    ; r3=15
		mov r4,#0				;r4=0
bPIY1   cmp r3, #0
		beq GPIY1
		movs r2, r2, LSR #1		; r2=r2/2 C=r2[i]   
		addcc r4, r4, #1 
		sub r3, r3, #1
		b    bPIY1
GPIY1	strb r4, [r1]			;y1=r4(num del 0 al 14)
		;X2
		ldr r1,=x2
		ldr r5,=y2
pInJ2	sub sp, sp, #4			;Espacio para resultado
		bl rand					;Llamada a num aleatorio
		POP{r3}
		and r3, r3, #0x1F;		;Mantener bits [4:0] (nums del 0 al 31)
		; Y2
		sub sp, sp, #4			;Espacio para resultado
		bl rand					;Llamada a num aleatorio
		POP{r2}
		mov r3, #nFil-1		    ; r3=14
		mov r4,#0				;r4=0
bPIY2   cmp r3, #0
		beq cmpPI
		movs r2, r2, LSR #1		; r2=r2/2 C=r2[i]   
		addcc r4, r4, #1 
		sub r3, r3, #1
		b    bPIY2
		;Comparar si posiciones iniciales son iguales
cmpPI	cmp r0, r3	
		bne almP2
		cmp r2, r4
		beq pInJ2				;Salta si (x1,y1) = (x2,y2)
almP2	strb r3,[r1]				; Inicializar aleatoriamente x2
		strb r4,[r5]				; Inicializar aleatoriamente y2
		;DIBUJAR PANTALLA
		;Dibujar Jugador 1
		LDR r0,= pantalla		;r0=@pantalla
		LDR r1,=x1				;r1=@x1
		LDR r2,=y1				;r2=@y1
		ldrb r1,[r1]				;r1=x1
		ldrb r2,[r2]				;r2=y1
		LDR r3,= nCol			; r3=nCol
		mla r1, r2, r3, r1		;r1=y1*nCol+x1= @Pantalla[x1][y1]
		LDR r3,=X			    ;r3=X
		strb r3,[r0,r1]!			;Pantalla[x1][y1] = 'X'

		ldr r1,=vector1
		str r0,[r1]	; guarda el primer elemento en el rastro
		LDR r1,=i1
		mov r2,#1   ; reiniciar vector
		strb r2,[r1]
		




		;Dibujar Jugador 2
		LDR r0,= pantalla		;r0=@pantalla
		LDR r1,=x2				;r1=@x2
		LDR r2,=y2				;r2=@y2
		ldrb r1,[r1]				;r1=x2
		ldrb r2,[r2]				;r2=y2
		LDR r3,= nCol			; r3=nCol
		mla r1, r2, r3, r1		;r1=y2*nCol+x2= @Pantalla[x2][y2]
		LDR r3,= O			    ;r3=O
		strb r3,[r0,r1]!			;Pantalla[x2][y2] = 'O'
		ldr r1,=vector2
		str r0,[r1]	; guarda el primer elemento en el rastro
		LDR r1,=i2
		mov r2,#1   ; reiniciar vector
		strb r2,[r1]
		
		;reiniciar contador de juego
		LDR r1,=contador
		eor r2, r2, r2
		str r2,[r1]
		;JUEGO
bucJgo  LDR r0,=terminar		; r0=@terminar
		ldr r0,[r0]				; r0=terminar
		cmp r0,#1
		beq rest				; salta si terminar=1
		LDR r0,=cont		    ;r0=@cont
		ldr r2,[r0]				;r2=cont
		LDR r1,= max			; r1=@MAX
		ldr r1,[r1]				; r1=MAX
		cmp r2, r1				;
		blo	bucJgo; salta si cont<MAX
		eor r2, r2, r2			; r2=0
		str r2,[r0]				;cont=0
		LDR r5,=contador
		ldr r7,[r5]; para ver si han pasado x movimientos
		add r7,r7,#1
		str r7,[r5]
						    
		;MOVER JUGADORES
		LDR r0,=pantalla
		;MOVER JUGADOR 1
		LDR r2,=x1				
		ldrb r2,[r2]				;r2=x1
		LDR r3,=y1				
		ldrb r3,[r3]				;r3=y1
		LDR r4,=dir1x			
		ldrsb r4,[r4]				;r4=dir1x
		LDR r5,=dir1y				
		ldrsb r5,[r5]				;r5=dir1y
		;Casos de desbordamiento
		cmp r4, #0
		beq	 movVJ1					; salta si dir1x=0
		add r6, r2, r4				;r2=x1+dir1x
		cmp r6,#nCol				
		moveq r6, #0				;r6=0 si x1=31
		beq valPH1					;
		mov r7, #0					;r7=0
		sub r7, r7, #1				;r7=0xFFFFFFFF
		cmp r6, r7
		moveq r6, #nCol-1			;r6=nCol-1
valPH1  LDR r7,= nCol			; r3=nCol
		mla r7, r3, r7, r6		;r7=y2*nCol+x2= @Pantalla[x2][y2]
		ldrb r8,[r0,r7]			;r8=Pantalla[nuevax1][y1]
		cmp r8,#espBlanco		;
		bne altMH1
		mov  r1, #X			; r1='X'
		strb r1,[r0,r7]!		;Pantalla[nuevax1][y1]='X' es el movimiento horizontal del juegador 1
		;Guardar en el vector del jugador 1
		
		ldr r11,=vector1
		ldr r7,=i1
		ldrb r8,[r7]  ; R4=I1
		ldr r9,[r11,r8,LSL #2]; sacar el viejo que hay que borrar de la pantalla
		str r0,[r11,r8,LSL #2]; meter el nuevo
		add r8,r8,#1;actualiza indice
		cmp r8,#N
		moveq r8,#0; para que no se pase del indice		
		strb r8,[ r7]
		LDR r10,=contador
		ldr r11,[r10]
		cmp r11,#N
		blo nada
		ldr r8,=espBlanco
		strb r8,[r9]; borrar pantalla			
nada    LDR r0,=pantalla
		LDR r1,= x1			; r1=@x1
		strb r6, [r1]		;x1=x1+dir1x
		b movJ2
altMH1	mov r1, #0			;r1=0
		;MIRAR DIRECCION OPUESTA
		sub r6, r2, r4		;r2=x1-dir1x
		cmp r6,#nCol				
		moveq r6, #0				;r6=0 si x1=31
		beq altIM1					;
		mov r7, #0					;r7=0
		sub r7, r7, #1				;r7=0xFFFFFFFF
		cmp r6, r7
		moveq r6, #nCol-1			;r6=nCol-1
altIM1  LDR r7,= nCol			; r3=nCol
		mla r7, r3, r7, r6		;r7=y2*nCol+x2= @Pantalla[x2][y2]
		ldrb r8,[r0,r7]			;r8=Pantalla[nuevax1][y1]
		cmp r8,#espBlanco		;
		addeq r1, r1, #1		;
		subeq r4, r4, r4, LSL#1 ; r4 = -dir1x
		mov r9, #0
		mov r10, #-1			;
		; MIRAR ARRIBA
		add r6, r3, r10		    ;r2=y1-1
		cmp r6,#nFil				
		moveq r6, #0				;r6=0 si y1=14
		beq altIM1					;
		mov r7, #0					;r7=0
		sub r7, r7, #1				;r7=0xFFFFFFFF
		cmp r6, r7
		moveq r6, #nFil-1			;r6=nFil-1
altAM1  LDR r7,= nCol			; r3=nFil
		mla r7, r6, r7, r2		;r7=y1*nCol+x1= @Pantalla[x1][y1]
		ldrb r8,[r0,r7]			;r8=Pantalla[nuevax1][y1]
		cmp r8,#espBlanco		;
		bne altBM1
		cmp r1, #1
		beq posalH1				; SALTA SI YA HAY DOS HUECOS LIBRES
		mov r4, r9				;
		mov r5, r10				;
		add r1, r1, #1
		; Mirar ABAJO
altBM1 	mov r10,#1				;
        add r6, r3, r10		    ;r2=y1+1
		cmp r6,#nFil				
		moveq r6, #0				;r6=0 si y1=15
		beq sigBM1					;
		mov r7, #0					;r7=0
		sub r7, r7, #1				;r7=0xFFFFFFFF
		cmp r6, r7
		moveq r6, #nFil-1			;r6=nFil-1
sigBM1  LDR r7,= nCol			; r3=nFil
		mla r7, r6, r7, r2		;r7=y1*nCol+x1= @Pantalla[x1][y1]
		ldrb r8,[r0,r7]			;r8=Pantalla[nuevax1][y1]
		cmp r8,#espBlanco		;
		bne cmpPH1
		cmp r1,#1
		beq posalH1
		mov r4, r9
		mov r5, r10
        b   nDir
cmpPH1  LDR r11,=quieto1
		cmp r1, #0				;
		moveq r1,#1
		strbeq r1,[r11]			; quieto1=1 
	    beq movJ2				; salta si no quedan opciones
		b   nDir
posalH1 sub sp, sp, #4			;Espacio para resultado
		bl rand					;Llamada a num aleatorio
		POP{r11}
		movs r11, r11, LSR #1		; C=r11[0]
		movcs r4, r9			; Mod si C=1
        movcs r5, r10	
		; ACTUALIZAR TRAS CHOQUE
nDir	LDR r9,= dir1x
        LDR r10,=dir1y
		strb r4,[r9]			; Almacenar nuevas direcciones
		strb r5,[r10]
		LDR r9,=x1
		LDR r10,=y1
		add r2, r2, r4			; r2= x1 + dir1xAleatoria
		cmp r2,#nCol				
		moveq r2, #0				;r6=0 si x1=32
		beq H1nVDir						;
		mov r7, #0					;r7=0
		sub r7, r7, #1				;r7=0xFFFFFFFF
		cmp r2, r7
		moveq r2, #nCol-1			;r6=nCol-1
H1nVDir	add r3, r3, r5				;r3=y1+dir1yAleatoria
		cmp r3,#nFil				
		moveq r3, #0				;r6=0 si y1=15
		beq sgHDir1					;
		mov r7, #0					;r7=0
		sub r7, r7, #1				;r7=0xFFFFFFFF
		cmp r3, r7
		moveq r3, #nFil-1			;r6=nFil-1
sgHDir1	strb r2,[r9]					;x1=x1nuevo
		strb r3,[r10]				;y1=y1nuevo
		LDR r7,=nCol			
		mla r7, r3, r7, r2
		LDR r6,=X
		strb r6, [r0,r7]!
		;guardar en el vector del jugador 1
		ldr r11,=vector1
		ldr r7,=i1
		ldrb r8,[r7]  ; R4=I1
		ldr r9,[r11,r8,LSL #2]; sacar el viejo que hay que borrar de la pantalla
		str r0,[r11,r8,LSL #2]; meter el nuevo
		add r8,r8,#1;actualiza indice
		cmp r8,#N
		moveq r8,#0; para que no se pase del indice
		strb r8,[r7]
		LDR r10,=contador
		ldr r11,[r10]
		cmp r11,#N
		blo nada1
		ldr r8,=espBlanco
		strb r8,[r9]; borrar pantalla			
nada1 	LDR r0,=pantalla
		b movJ2
		
movVJ1	add r6, r3, r5				;r2=y1+dir1y
		cmp r6,#nFil				
		moveq r6, #0				;r6=0 si x1=nFil
		beq valPV1					;
		mov r7, #0					;r7=0
		sub r7, r7, #1				;r7=0xFFFFFFFF
		cmp r6, r7
		moveq r6, #nFil-1			;r6=nCol-1
valPV1  LDR r7,= nCol			; r3=nCol
		mla r7, r6, r7, r2		;r1=y1*nCol+x1= @Pantalla[x1][y1]
		ldrb r8,[r0,r7]			;r7=Pantalla[x1][nuevay1]
		cmp r8,#espBlanco		;
		bne altMV1
		mov r1, #X			; r1='X'
		strb r1,[r0,r7]!		;Pantalla[nuevax1][y1]='X'
		;guardar en el vector del jugador 1
		ldr r11,=vector1
		ldr r7,=i1
		ldrb r8,[r7]  ; R4=I1
		ldr r9,[r11,r8,LSL #2]; sacar el viejo que hay que borrar de la pantalla
		str r0,[r11,r8,LSL #2]; meter el nuevo
		add r8,r8,#1;actualiza indice
		cmp r8,#N
		moveq r8,#0; para que no se pase del indice
		strb r8,[ r7]
		LDR r10,=contador
		ldr r11,[r10]
		cmp r11,#N
		blo nada2
		ldr r8,=espBlanco
		strb r8,[r9]; borrar pantalla			
nada2   LDR r0,=pantalla
		LDR r1,= y1	
		strb r6,[r1]		;r1=@y1
		b movJ2
altMV1	mov r1, #0			;r1=0
		;MIRAR DIRECCION OPUESTA
		sub r6, r3, r5		;r2=y1-dir1y
		cmp r6,#nFil				
		moveq r6, #0				;r6=0 si x1=31
		beq altV1					;
		mov r7, #0					;r7=0
		sub r7, r7, #1				;r7=0xFFFFFFFF
		cmp r6, r7
		moveq r6, #nFil-1			;r6=nCol-1
altV1   LDR r7,= nCol			; r3=nCol
		mla r7, r6, r7, r2		;r7=y1*nCol+x1= @Pantalla[x1][y1]
		ldrb r8,[r0,r7]			;r8=Pantalla[nuevax1][y1]
		cmp r8,#espBlanco		;
		addeq r1, r1, #1		;
		subeq r5, r5, r5, LSL#1 ; r4 = -dir1x
		mov r9, #-1
		mov r10, #0			;
		; MIRAR Izquierda
		add r6, r2, r9		    ;r6=x1-1
		cmp r6,#nCol				
		moveq r6, #0				;r6=0 si x1=16
		beq altIV1					;
		mov r7, #0					;r7=0
		sub r7, r7, #1				;r7=0xFFFFFFFF
		cmp r6, r7
		moveq r6, #nCol-1			;r6=nCol-1
altIV1  LDR r7,= nCol			; r3=nCol
		mla r7, r3, r7, r6		;r7=y1*nCol+x1= @Pantalla[x1][y1]
		ldrb r8,[r0,r7]			;r8=Pantalla[nuevax1][y1]
		cmp r8,#espBlanco		;
		bne altDM1
		cmp r1, #1
		beq posalV1				; SALTA SI YA HAY DOS HUECOS LIBRES
		mov r4, r9				;
		mov r5, r10				;
		add r1, r1, #1
		; Mirar ABAJO
altDM1 	mov r9,#1				;
        add r6, r2, r9		    ;r6=X1+1
		cmp r6,#nCol				
		moveq r6, #0				;r6=0 si x1=32
		beq sigDM1					;
		mov r7, #0					;r7=0
		sub r7, r7, #1				;r7=0xFFFFFFFF
		cmp r6, r7
		moveq r6, #nCol-1			;r6=nFil-1
sigDM1  LDR r7,= nCol			; r3=nFil
		mla r7, r3, r7, r6		;r7=y1*nCol+x1= @Pantalla[x1][y1]
		ldrb r8,[r0,r7]			;r8=Pantalla[nuevax1][y1]
		cmp r8,#espBlanco		;
		bne cmpPV1
		cmp r1,#1
		beq posalV1
		mov r4, r9
		mov r5, r10
        b   nVDir1
cmpPV1  LDR r11,=quieto1
		cmp r1, #0				;
		moveq r1,#1
		strbeq r1,[r11]			; quieto1=1 
	    beq movJ2				; salta si no quedan opciones
		b   nVDir1
posalV1 sub sp, sp, #4			;Espacio para resultado
		bl rand					;Llamada a num aleatorio
		POP{r11}
		movs r11, r11, LSR #1		; C=r11[0]
		movcs r4, r9			; Mod si C=1
        movcs r5, r10	
		; ACTUALIZAR TRAS CHOQUE
nVDir1	LDR r9,= dir1x
        LDR r10,=dir1y
		strb r4,[r9]			; Almacenar nuevas direcciones
		strb r5,[r10]
		LDR r9,=x1
		LDR r10,=y1
		add r2, r2, r4			; r2= x1 + dir1xAleatoria
		cmp r2,#nCol				
		moveq r2, #0				;r6=0 si x1=32
		beq V1nVDir					;
		mov r7, #0					;r7=0
		sub r7, r7, #1				;r7=0xFFFFFFFF
		cmp r2, r7
		moveq r2, #nCol-1			;r6=nFil-1
V1nVDir	add r3, r3, r5				;r3=y1+dir1yAleatoria
		cmp r3,#nFil				
		moveq r3, #0				;r6=0 si y1=15
		beq sgVDir1					;
		mov r7, #0					;r7=0
		sub r7, r7, #1				;r7=0xFFFFFFFF
		cmp r3, r7
		moveq r3, #nFil-1			;r6=nFil-1
sgVDir1 strb r2,[r9]					;x1=x1nuevo
		strb r3,[r10]				;y1=y1nuevo
		LDR r7,=nCol			
		mla r7, r3, r7, r2
		LDR r6,=X
		strb r6, [r0,r7]!
		; guardar en el vector del jugador 1
		ldr r11,=vector1
		ldr r7,=i1
		ldrb r8,[r7]  ; R4=I1
		ldr r9,[r11,r8,LSL #2]; sacar el viejo que hay que borrar de la pantalla
		str r0,[r11,r8,LSL #2]; meter el nuevo
		add r8,r8,#1;actualiza indice
		cmp r8,#N
		moveq r8,#0; para que no se pase del indice
		strb r8,[ r7]
		LDR r10,=contador
		ldr r11,[r10]
		cmp r11,#N
		blo nad
		ldr r8,=espBlanco
		strb r8,[r9]; borrar pantalla
nad    LDR r0,=pantalla
		; MOVER JUGADOR 2
movJ2   LDR r2,=x2				
		ldrb r2,[r2]				;r2=x2
		LDR r3,=y2				
		ldrb r3,[r3]				;r3=y2
		LDR r4,=dir2x			
		ldrsb r4,[r4]				;r4=dir2x
		LDR r5,=dir2y				
		ldrsb r5,[r5]				;r5=dir2y
		;Casos de desbordamiento
		cmp r4, #0
		beq	 movVJ2					; salta si dir2x=0
		add r6, r2, r4				;r2=x2+dir2x
		cmp r6,#nCol				
		moveq r6, #0				;r6=0 si x2=31
		beq valPH2					;
		mov r7, #0					;r7=0
		sub r7, r7, #1				;r7=0xFFFFFFFF
		cmp r6, r7
		moveq r6, #nCol-1			;r6=nCol-1
valPH2  LDR r7,= nCol			; r3=nCol
		mla r7, r3, r7, r6		;r7=y2*nCol+x2= @Pantalla[x2][y2]
		ldrb r8,[r0,r7]			;r8=Pantalla[nuevax2][y2]
		cmp r8,#espBlanco		;
		bne altMH2
		mov  r1, #O			; r1='O'
		strb r1,[r0,r7]!	;Pantalla[nuevax1][y1]='O'
		;guardar en el vector del jugador 2
		LDR r11,=vector2
		LDR r7,=i2
		ldrb r8,[r7]  ; R4=I1
		ldr r9,[r11,r8,LSL #2]; sacar el viejo que hay que borrar de la pantalla
		str r0,[r11,r8,LSL #2]; meter el nuevo
		add r8,r8,#1;actualiza indice
		cmp r8,#N
		moveq r8,#0; para que no se pase del indice
		strb r8,[r7]
		LDR r10,=contador
		ldr r11,[r10]
		cmp r11,#N
		blo nada3
		ldr r8,=espBlanco
		strb r8,[r9]; borrar pantalla
nada3   LDR r0,=pantalla
		LDR r1,= x2			; r1=@x2
		strb r6, [r1]		;x2=x2+dir2x
		b finPart

altMH2	mov r1, #0			;r1=0
		;MIRAR DIRECCION OPUESTA
		sub r6, r2, r4		;r2=x2-dir2x
		cmp r6,#nCol				
		moveq r6, #0				;r6=0 si x2=31
		beq altIM2					;
		mov r7, #0					;r7=0
		sub r7, r7, #1				;r7=0xFFFFFFFF
		cmp r6, r7
		moveq r6, #nCol-1			;r6=nCol-1
altIM2  LDR r7,= nCol			; r3=nCol
		mla r7, r3, r7, r6		;r7=y2*nCol+x2= @Pantalla[x2][y2]
		ldrb r8,[r0,r7]			;r8=Pantalla[nuevax2][y2]
		cmp r8,#espBlanco		;
		addeq r1, r1, #1		;
		subeq r4, r4, r4, LSL#1 ; r4 = -dir2x
		mov r9, #0
		mov r10, #-1			;
		; MIRAR ARRIBA
		add r6, r3, r10		    ;r2=y1-1
		cmp r6,#nFil				
		moveq r6, #0				;r6=0 si y1=15
		beq altAM2					;
		mov r7, #0					;r7=0
		sub r7, r7, #1				;r7=0xFFFFFFFF
		cmp r6, r7
		moveq r6, #nFil-1			;r6=nFil-1
altAM2  LDR r7,= nCol			; r3=nFil
		mla r7, r6, r7, r2		;r7=y2*nCol+x2= @Pantalla[x2][y2]
		ldrb r8,[r0,r7]			;r8=Pantalla[nuevax2][y2]
		cmp r8,#espBlanco		;
		bne altBM2
		cmp r1, #1
		beq posalH2				; SALTA SI YA HAY DOS HUECOS LIBRES
		mov r4, r9				;
		mov r5, r10				;
		add r1, r1, #1
		; Mirar ABAJO
altBM2 	mov r10,#1				;
        add r6, r3, r10		    ;r2=y2+2
		cmp r6,#nFil				
		moveq r6, #0				;r6=0 si y1=15
		beq sigBM2					;
		mov r7, #0					;r7=0
		sub r7, r7, #1				;r7=0xFFFFFFFF
		cmp r6, r7
		moveq r6, #nFil-1			;r6=nFil-1
sigBM2  LDR r7,= nCol			; r3=nFil
		mla r7, r6, r7, r2		;r7=y1*nCol+x1= @Pantalla[x1][y1]
		ldrb r8,[r0,r7]			;r8=Pantalla[nuevax1][y1]
		cmp r8,#espBlanco		;
		bne cmpPH2
		cmp r1,#1
		beq posalH2
		mov r4, r9
		mov r5, r10
        b   nDir2
cmpPH2  LDR r11,=quieto2
		cmp r1, #0				;
		moveq r1,#1
		strbeq r1,[r11]			; quieto1=1 
	    beq finPart				; salta si no quedan opciones
		b   nDir2
posalH2 sub sp, sp, #4			;Espacio para resultado
		bl rand					;Llamada a num aleatorio
		POP{r11}
		movs r11, r11, LSR #1		; C=r11[0]
		movcs r4, r9			; Mod si C=1
        movcs r5, r10	
		; ACTUALIZAR TRAS CHOQUE
nDir2	LDR r9,= dir2x
        LDR r10,=dir2y
		strb r4,[r9]			; Almacenar nuevas direcciones
		strb r5,[r10]
		LDR r9,=x2
		LDR r10,=y2
		add r2, r2, r4			; r2= x1 + dir1xAleatoria
		cmp r2,#nCol				
		moveq r2, #0				;r6=0 si x1=32
		beq H2nVDir						;
		mov r7, #0					;r7=0
		sub r7, r7, #1				;r7=0xFFFFFFFF
		cmp r2, r7
		moveq r2, #nCol-1			;r6=nFil-1
H2nVDir	add r3, r3, r5				;r3=y1+dir1yAleatoria
		cmp r3,#nFil				
		moveq r3, #0				;r6=0 si y1=15
		beq sgHDir2					;
		mov r7, #0					;r7=0
		sub r7, r7, #1				;r7=0xFFFFFFFF
		cmp r3, r7
		moveq r3, #nFil-1			;r6=nFil-1
sgHDir2	strb r2,[r9]					;x1=x1nuevo
		strb r3,[r10]				;y1=y1nuevo
		LDR r7,=nCol			
		mla r7, r3, r7, r2
		LDR r6,=O
		strb r6, [r0,r7]!
		; guardar en el vector del jugador 2
		ldr r11,=vector2
		ldr r7,=i2
		ldrb r8,[r7]  ; R4=I1
		ldr r9,[r11,r8,LSL #2]; sacar el viejo que hay que borrar de la pantalla
		str r0,[r11,r8,LSL #2]; meter el nuevo
		add r8,r8,#1;actualiza indice
		cmp r8,#N
		moveq r8,#0; para que no se pase del indice
		strb r8,[r7]
		LDR r10,=contador
		ldr r11,[r10]
		cmp r11,#N
		blo nada4
		ldr r8,=espBlanco
		strb r8,[r9]; borrar pantalla			
nada4   LDR r0,=pantalla
		b finPart
	
movVJ2	add r6, r3, r5				;r2=y1+dir1y
		cmp r6,#nFil				
		moveq r6, #0				;r6=0 si x1=nFil
		beq valPV2					;
		mov r7, #0					;r7=0
		sub r7, r7, #1				;r7=0xFFFFFFFF
		cmp r6, r7
		moveq r6, #nFil-1			;r6=nCol-1
valPV2  LDR r7,= nCol			; r3=nCol
		mla r7, r6, r7, r2		;r1=y1*nCol+x1= @Pantalla[x1][y1]
		ldrb r8,[r0,r7]			;r7=Pantalla[x1][nuevay1]
		cmp r8,#espBlanco		;
		bne altMV2
		mov r1, #O			; r1='X'
    	strb r1,[r0,r7]!		;Pantalla[nuevax1][y1]='O'
		ldr r11,=vector2
		ldr r7,=i2
		ldrb r8,[r7]  ; R4=I1
		ldr r9,[r11,r8,LSL #2]; sacar el viejo que hay que borrar de la pantalla
		str r0,[r11,r8,LSL #2]; meter el nuevo
		add r8,r8,#1;actualiza indice
		cmp r8,#N
		moveq r8,#0; para que no se pase del indice
		strb r8,[r7]
		LDR r10,=contador
		ldr r11,[r10]
		cmp r11,#N
		blo nada5
		ldr r8,=espBlanco
		strb r8,[r9]; borrar pantalla		
nada5   LDR r0,=pantalla
		LDR r1,= y2			;r1=@y1
		strb r6,[r1]	
		b finPart	
altMV2	mov r1, #0			;r1=0
		;MIRAR DIRECCION OPUESTA
		sub r6, r3, r5		;r2=y1-dir1y
		cmp r6,#nFil				
		moveq r6, #0				;r6=0 si x1=31
		beq altV2					;
		mov r7, #0					;r7=0
		sub r7, r7, #1				;r7=0xFFFFFFFF
		cmp r6, r7
		moveq r6, #nFil-1			;r6=nCol-1
altV2   LDR r7,= nCol			; r3=nCol
		mla r7, r6, r7, r2		;r7=y1*nCol+x1= @Pantalla[x1][y1]
		ldrb r8,[r0,r7]			;r8=Pantalla[nuevax1][y1]
		cmp r8,#espBlanco		;
		addeq r1, r1, #1		;
		subeq r5, r5, r5, LSL#1 ; r4 = -dir1x
		mov r9, #-1
		mov r10, #0			;
		; MIRAR Izquierda
		add r6, r2, r9		    ;r6=x1-1
		cmp r6,#nCol				
		moveq r6, #0				;r6=0 si x1=16
		beq altIV2					;
		mov r7, #0					;r7=0
		sub r7, r7, #1				;r7=0xFFFFFFFF
		cmp r6, r7
		moveq r6, #nCol-1			;r6=nCol-1
altIV2  LDR r7,= nCol			; r3=nCol
		mla r7, r3, r7, r6		;r7=y1*nCol+x1= @Pantalla[x1][y1]
		ldrb r8,[r0,r7]			;r8=Pantalla[nuevax1][y1]
		cmp r8,#espBlanco		;
		bne altDM2
		cmp r1, #1
		beq posalV2				; SALTA SI YA HAY DOS HUECOS LIBRES
		mov r4, r9				;
		mov r5, r10				;
		add r1, r1, #1
		; Mirar ABAJO
altDM2 	mov r9,#1				;
        add r6, r2, r9		    ;r6=X1+1
		cmp r6,#nCol				
		moveq r6, #0				;r6=0 si x1=32
		beq sigDM2					;
		mov r7, #0					;r7=0
		sub r7, r7, #1				;r7=0xFFFFFFFF
		cmp r6, r7
		moveq r6, #nCol-1			;r6=nFil-1
sigDM2  LDR r7,= nCol			; r3=nFil
		mla r7, r3, r7, r6		;r7=y1*nCol+x1= @Pantalla[x1][y1]
		ldrb r8,[r0,r7]			;r8=Pantalla[nuevax1][y1]
		cmp r8,#espBlanco		;
		bne cmpPV2
		cmp r1,#1
		beq posalV2
		mov r4, r9
		mov r5, r10
        b   nVDir2
cmpPV2  LDR r11,=quieto2
		cmp r1, #0				;
		moveq r1,#1
		strbeq r1,[r11]			; quieto1=1 
	    beq finPart				; salta si no quedan opciones
		b   nVDir2
posalV2 sub sp, sp, #4			;Espacio para resultado
		bl rand					;Llamada a num aleatorio
		POP{r11}
		movs r11, r11, LSR #1		; C=r11[0]
		movcs r4, r9			; Mod si C=1
        movcs r5, r10	
		; ACTUALIZAR TRAS CHOQUE
nVDir2	LDR r9,= dir2x
        LDR r10,=dir2y
		strb r4,[r9]			; Almacenar nuevas direcciones
		strb r5,[r10]
		LDR r9,=x2
		LDR r10,=y2
		add r2, r2, r4			; r2= x1 + dir1xAleatoria
		cmp r2,#nCol				
		moveq r2, #0				;r6=0 si x1=32
		beq V2nVDir					;
		mov r7, #0					;r7=0
		sub r7, r7, #1				;r7=0xFFFFFFFF
		cmp r2, r7
		moveq r2, #nCol-1			;r6=nFil-1
V2nVDir	add r3, r3, r5				;r3=y1+dir1yAleatoria
		cmp r3,#nFil				
		moveq r3, #0				;r6=0 si y1=15
		beq sgVDir2					;
		mov r7, #0					;r7=0
		sub r7, r7, #1				;r7=0xFFFFFFFF
		cmp r3, r7
		moveq r3, #nFil-1			;r6=nFil-1
sgVDir2 strb r2,[r9]					;x1=x1nuevo
		strb r3,[r10]				;y1=y1nuevo
		LDR r7,=nCol			
		mla r7, r3, r7, r2
		LDR r6,=O
		strb r6, [r0,r7]!
		;guardar en el vector del jugador 2
		ldr r11,=vector2
		ldr r7,=i2
		ldrb r8,[r7]  ; R4=I1
		ldr r9,[r11,r8,LSL #2]; sacar el viejo que hay que borrar de la pantalla
		str r0,[r11,r8,LSL #2]; meter el nuevo
		add r8,r8,#1;actualiza indice
		cmp r8,#N
		moveq r8,#0; para que no se pase del indice
		strb r8,[ r7]
		LDR r10,=contador
		ldr r11,[r10]
		cmp r11,#N
		blo n1
		ldr r8,=espBlanco
		strb r8,[r9]; borrar pantalla
n1		LDR r0,=pantalla
		; Fin de una ronda de movimientos
		; BUSCAR SI ALGUN JUGADOR SE HA QUEDADO ATRAPADO
finPart LDR r0,=quieto1
		LDR r1,=quieto2
		ldrb r2,[r0]
		ldrb r3,[r1]
		cmp r2,#1
		beq j1qto	; Juagador 1 sin movimientos
		cmp r3,#1
		beq gana1   ; jugador 2 sin movimientos, gana el jugador 1
		b bucJgo
j1qto   cmp r3,#1
		bne gana2  ; Jugador 2 gana, se ha podido mover
		mov r3,#0
		strb r3,[r0]
		strb r3,[r1]
		b bPart
		; Actualizar cabecera con victoria del jugador 1
gana1   LDR r2,=cabecera
		LDR	r0,=puntos1 
		ldrb r1,[r0]
		add r1,r1,#1
		strb r1,[r0]
		strb r1,[r2,#15]
		cmp r1, #cincoASCII	
		beq rest
	    LDR r0,=quieto2
		mov r1, #0
		strb r1,[r0]
		b bPart
		; Actualizar cabecera con victoria del jugador 2
gana2   LDR r2,=cabecera
		LDR	r0,=puntos2 
		ldrb r1,[r0]
		add r1,r1,#1
		strb r1,[r0]
		strb r1,[r2,#17]
		cmp r1, #cincoASCII	
		beq rest
		LDR r0,=quieto1
		mov r1, #0
		strb r1,[r0]
		b bPart
		
		;deshabilitar IRQ4 y IRQ7
rest	LDR r0,=VICIntEnClear  	; r0=@VICIntEnClr
		mov r1,#0x90			; r1=#2_10010000
		str r1, [r0]				;VICIntEnable[4]=0
								;VICIntEnable[7]=0
		;Reestablecer VI[4] y VI[7]
		LDR r0,=VICVectAddr0    ; r0=@VICVectAdrr0
		LDR r1,=timer_so		; r1=@timer_so
		ldr r1,[r1]				; r1=timer_so
		add r0, r0, #16			; r0=@VI[4]
		str r1,[r0],#12		    ; VI[4]=timer_so
		LDR r1,=tecl_so		    ;r1=@teclado_so
		ldr r1,[r1]				;r1=teclado_so
		str r1,[r0]  			;VI[7]=teclado_so
fin     b   fin							
RSI_timer						;RSI timer (100 inter./s.)
		sub lr, lr,#4			; correccion @ret. (segm.)
		PUSH{lr}				;apilar @retorno
		mrs r14,spsr			;r14=cpsr prog interr.
		PUSH {r14}
		msr cpsr_c,#2_01010010  ;I=0 -> act.IRQs (modo IRQ)
		PUSH {r0-r1}			; apilar registros utilizados
		LDR r0,=T0_IR			;r0=@T0_IR
		mov r1,#1				;r1=1
		str r1,[r0]				;T0_IR = 1 (bajar peticion IRQ[4])
		LDR r0,=reloj			; r0=@reloj
		ldr r1,[r0]				;r1=reloj
		add r1, r1, #1			; r1 = r1 + 1
		str r1,[r0]				; reloj = reloj + 1
		LDR r0,=cont			; r0=@cont
		ldr r1,[r0]				;r1=cont
		add r1, r1, #1			;r1=cont + 1
		str r1,[r0]				;cont = cont +1
fintimer POP {r0-r1}				;desapilar reg. utilizados
		msr cpsr_c,#2_11010010	; I=1 -> desact. Interr. IRQ
		POP {r14}				;desapilar cpsr prog interrumpido
		msr spsr_fsxc,r14		;spsr=cpsr prog interrumpido
		LDR r14,=VICVectAddr	; EOI r14=@VICVectAdr
		str r14,[r14]			; EOI escribir VICVectAdrr
		POP {pc}^				;retorno a prog. interrumpido
		
RSI_teclado						;RSI teclado por UART1
		;Prologo para activar nuevas interrupciones (I=0)
		sub lr,lr,#4			;correcion @ret. (segmentacion)
		PUSH {lr}				; apilar @ret.
		mrs r14, spsr			;r14=cpsr prog interr.
		PUSH {r14}				;apilar estado de prog. interr.
		msr cpsr_c,#2_01010010  ;I=0 -> act.IRQs (modo IRQ)
		;
		PUSH{r0-r4}				;apilar registros utilizados
		;Transferencia de informacion
		LDR r1,=RDAT			;r1=@reg. datos teclado
		ldr r0,[r1]				;r0=codigo ASCII tecla
		;Tratratamiento de la informacion
		cmp r0, #45		
		bhi alfa				; salta si tecla no es '+' o '-'
		beq negat				; salta si tecla='-'
		cmp r0, #43
		bne epTec				; salta si tecla<'+'
		LDR r1,=max				;r1=@max
		ldr r2,[r1]				;r2=max
		cmp r2,#128				
		movne r2, r2, LSL #1 	;ejecuta si max<128
		strne r2, [r1]			;max=max*2
		b epTec
negat   LDR r1,=max				;r1=@max
		ldr r2,[r1]				;r2=max
		cmp r2, #1				
		movne r2, r2, LSR #1	; r2=max/2
		strne r2,[r1]			;max=max/2
		b epTec
alfa	bic r0,r0,#2_100000		;paso a MAYUSCULAS
		cmp r0,#81				
		bne sigue 				;salta si tecla!='Q'
		;tratamiento de la tecla 'Q' (fin de programa)
		LDR r1,=terminar		;r1=@terminar
		mov r0, #1				;r0=1
		str r0,[r1]				;terminar=1
sigue   cmp r0, #73
		blo j1x		; salta si tecla<73
		cmp r0, #76
		bhi j1y					;salta si tecla>76
		LDR r1,=dir2x			;r1=@dir2x
		LDR r2,=dir2y			;r2=@dir2y
		mov r3, #0				; r3=0
		cmp r0, #73
		moveq r4,#-1			; ejecuta si letra='I'
		strbeq r3,[r1]
		strbeq r4,[r2]
		beq epTec
		cmp r0,#74
		moveq r4,#-1			; ejecuta si letra='J'
		strbeq r3,[r2]
		strbeq r4,[r1]
		beq epTec
		cmp r0,#75
		moveq r4,#1				;ejecuta si letra='K'
		strbeq r3,[r1]
		strbeq r4,[r2]
		beq epTec
L		mov r4,#1				;ejecuta si letra='L'
		strb r3,[r2]
		strb r4,[r1]
		b epTec
j1x     LDR r1,=dir1x			;r1=@dir1x
		LDR r2,=dir1y			;r2=@dir1y
		mov r3, #0				;r3=0
		cmp r0, #65
		moveq r4, #-1			; ejecuta si letra='A'
		beq alm1x				
		cmp r0, #68				
		bne epTec
		mov r4,#1
alm1x   strb r3,[r2]				; dir1y = 0
		strb r4,[r1]				; dir2y=nuevaDir
		b   epTec
j1y		LDR r1,=dir1x			; r1=@dir1x
		LDR r2,=dir1y			;r2=@dir1y
		mov r3, #0				; r3=0
		cmp r0,#83				
		moveq r4, #1			; ejecuta si letra='S'
		beq alm1y
		cmp r0, #87				;
		bne epTec
		mov r4, #-1				; ejecuta si letra='W'
alm1y   strb r3,[r1]				; dir1y = nuevaDir
		strb r4,[r2]				;dir1x = 0
epTec	POP{r0-r4}				;desapilar regs. utilizados
		;Epilogo
		msr cpsr_c,#2_11010010  ; desactivar. Interr. Irq (I=1)
		POP {r14}				;desapilar cpsr prog. interrumpido
		msr spsr_fsxc,r14		;spsr=cpsr prog. interrumpido
		LDR r14,=VICVectAddr	;EOI r14=@VICVectAddr
		str r14,[r14]			;EOI escribir VICVectAddr
		POP {pc}^				;retorno a prog. interrumpido
		END  

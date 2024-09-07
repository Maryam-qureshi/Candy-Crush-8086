[org 0x0100]  
jmp start 
data:
	BOARD: DB 0,0,0,0,0,0,3,0,0,0,0,0,2,3,0,0,1,0,0,0,3,0,0,0,0,0,0,3,0,0,0,0,0,0,3,0,0,0,0,0,0,3,0,0,0,0,0,0,3
	SCORE: DW 0
	GETX: DW 0
	GETY: DW 0
	INDEX1: DB 0
	INDEX2: DB 0
	Name: dw 'NAME:','$' 
	pname: db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	welcome: db 'Welcome to MINI CANDY CRUSH ','$'
	gameName db '____________  CANDY CRUSH  _____________','$'
	m1: db 'RULES: ','$'
	m2: db '1)You have 10 turns.','$'
	m3: db '2)Make pattern of 3 candies to pop them','$'
	m4: db '3)Make a pattern of 4 or to form a bomb','$'
	m5: db '4)Swap the bomb with candy to delete it','$'
	e1: db ' Thank you for playing        ','$'
	e2: db ' Hope you enjoyed','$'
	player: db 'Enter name: ','$'
	random: db 0
	COUNT: DB 0
	X: DW 0
	Y: DW 0
	TOTAL_MOVES: DB '/15','$'
	y1: DB 0
	x1: DW 0
	x2: DW 0
	MOVES: DB 0
	SCORESTR: DB 'Score:','$'
	MOVESSTR: DB 'Moves:','$'
	INVALID: DB 'INVALID MOVE','$'
	CANDYCOUNT: DB 0
	CANDYTYPE: DB 0
	VALID: DB 0
	border: db "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^$"
	colorbomb: db 0
	colorbombtri: db 0
	bomb: db 0
	matches db 0      
;---------------------------------------------------------
;---------------------clear screen------------------------ 
;---------------------------------------------------------
clrscr:
		push bp
		mov bp,sp
		push es
		push ax
		push di
		push cx
		mov ax, 0xb800
		mov es, ax
		xor di, di
		mov ax, 0x0720
		mov cx, 2000
		cld
		rep stosw
		pop cx
		pop di
		pop ax
		pop es
		pop bp
		ret
;-------------------------------------------------------
;---------------GETS X AND Y COORDINATES ---------------
;-------------------------------------------------------
getcoordinates:
	PUSHA
	MOV AX,1		;puts cursor on the screen 
	INT 33H
	MOV CX, 20		;min position
	MOV DX, 440		;max position
	MOV AX, 7		;sets mouse horizontal position
	INT 33H
	MOV CX, 30
	MOV DX, 170
	MOV AX, 8		;sets mouse vertical positions
	INT 33H
	MOV BL,0
	click:
		MOV AX,3		;gets cursor co-ordinates
		INT 33H
		MOV [GETX],CX  	;moves x-coordinate into getx variable
		MOV [GETY],DX 	;moves y-coordinate into gety variable
		CALL delay
		CALL delay
		CMP BL,1
		JNE click 
	MOV DX,0
	MOV AX,[GETX]
	MOV BX,2			;in 320x200 mode, value of cx is doubled so we half it
	DIV BX
	MOV [GETX],AX
	POPA
	RET
;**************************************
;-----------PRINT NUMBER---------------
;**************************************
printnum:
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di
	mov ax, [bp+4] 
	mov bx, 10 
	mov cx, 0 
	nextdigit:
		mov dx, 0 
		div bx 
		add dl, 0x30 
		push dx 
		inc cx 
		cmp ax, 0 
		jnz nextdigit 
		mov di, 0
	nextpos:
		pop dx
		MOV AH,2H
		INT 21H
		loop nextpos 
		pop di
		pop dx
		pop cx
		pop bx
		pop ax
		pop es
		pop bp
		ret 2
;********************************************
;------------------ SCORE -------------------
;********************************************
SCORE_UPDATE:
	push AX
	cmp byte [colorbomb],1
	je move5
	cmp byte [colorbombtri],1
	je move4
	cmp byte [matches],1
	je move3
	cmp byte [CANDYCOUNT],3
	ja addscore
	move3:
	mov byte [CANDYCOUNT],3
	jmp addscore
	move4:
	mov byte [CANDYCOUNT],4
	jmp addscore
	move5:
	mov byte [CANDYCOUNT],5
	addscore:
	mov AX,0
	mov AL,[CANDYCOUNT]
	add [SCORE],AX
	pop AX
	RET
;______________________________________
;------------new_line------------------
;______________________________________
new_line:
	pusha
	mov dl, 10
	mov ah, 02h
	int 21h
	mov dl, 13
	mov ah, 02h
	int 21h
	popa
	ret
;_________________________________________
;------------welcome screen---------------
;_________________________________________
welcomescreen:
	pusha
	MOV AH,06h	;scroll-up window
	MOV AL,0
	MOV BH,25H		;text and background colour
	MOV CH,0		;upper row number	
	MOV CL,0		;left col
	MOV DH,24
	MOV DL,79
	INT 10H

	MOV AH,02H
	MOV BH, 0
	MOV DH,2		;displays message in line 2
	MOV DL,0		;starts display with a gap of 0 spaces
	INT 10H
	
	mov DX, border	;displays gamename
	MOV AH,09H
	INT 21H

	MOV AH,02H
	MOV BH,0
	MOV DH,24		;displays message in line 24
	MOV DL,0		;starts display with a gap of 0 spaces
	INT 10H
	
	MOV DX, border	;displays DESIGNED  STRIPE
	MOV AH,09H
	INT 21H
	MOV AH,02H
	MOV BH, 0
	MOV DH,2		;displays message in line 2
	MOV DL,0		;starts display with a gap of 0 spaces
	INT 10H
	call new_line
	call new_line
	MOV DX,gameName
	MOV AH,09H
	INT 21H
	call new_line
	call new_line
	MOV DX,m1
	MOV AH,09H
	INT 21H
	call new_line
	call new_line
	mov dx,m2
	mov ah,09H
	INT 21H
	call new_line
	call new_line
	MOV DX,m3
	MOV AH,09H
	INT 21H
	call new_line
	call new_line
	MOV DX,m4
	MOV AH,09H
	INT 21H
	call new_line
	call new_line
	MOV DX,m5
	MOV AH,09H
	INT 21H
	call new_line
	call new_line
	MOV DX,player
	MOV AH,09H
	INT 21H
	MOV SI,pname	;TAKING INPUT OF NAME 
	input:
	MOV ah,1
	INT 21h
	CMP al,13
	je display
	MOV [si],al
	inc si
	inc al
	jmp input
	display:
	MOV byte [si],'$'
	call new_line
	MOV DX, welcome	
	MOV AH,09H
	INT 21H
	MOV DX, pname	
	MOV AH,09H
	INT 21H
	MOV AH,02H
	MOV BH,0
	MOV DH,24		;displays message in line 24
	MOV DL,0		;starts display with a gap of 0 spaces
	INT 10H
	mov DX, border	
	MOV AH,09H
	INT 21H
	mov ah, 0
	int 0x16 		;await input
	popa
	RET
;_________________________________________
;------------ending  screen---------------
;_________________________________________
end_screen:
	PUSHA
	MOV AH,06h	;scroll-up window
	MOV AL,0
	MOV BH,25H		;text and background colour
	MOV CH,0		;upper row number	
	MOV CL,0		;left col
	MOV DH,24
	MOV DL,79
	INT 10H

	MOV AH,02H
	MOV BH, 0
	MOV DH,2		;displays message in line 2
	MOV DL,0		;starts display with a gap of 0 spaces
	INT 10H
	
	mov DX, border	;displays gamename
	MOV AH,09H
	INT 21H

	MOV AH,02H
	MOV BH,0
	MOV DH,24		;displays message in line 24
	MOV DL,0		;starts display with a gap of 0 spaces
	INT 10H
	
	mov DX, border	;displays DESIGNED  STRIPE
	MOV AH,09H
	INT 21H
	MOV AH,02H
	MOV BH, 0
	MOV DH,2		;displays message in line 2
	MOV DL,0		;starts display with a gap of 0 spaces
	INT 10H
	call new_line
	call new_line
	mov dx,gameName
	mov ah,09H
	INT 21H
	call new_line
	call new_line
	call new_line
	call new_line
	mov dx,Name
	mov ah,09H
	INT 21H
	mov DX, pname	
	MOV AH,09H
	INT 21H
	call new_line
	call new_line
	call new_line
	call new_line

	mov dx,e1
	mov ah,09H
	INT 21H
	call new_line
	call new_line
	mov dx,e2
	mov ah,09H
	INT 21H
	call new_line
	call new_line
	call new_line
	call new_line
	
	MOV AH,02H
	MOV BH,0
	MOV DH,24		;displays message in line 24
	MOV DL,0		;starts display with a gap of 0 spaces
	INT 10H
	mov DX, border	
	MOV AH,09H
	INT 21H
	mov ah, 0
	int 0x16 		;await input
	returnback:
	POPA
	RET
;________________________________________
;---------------- DELAY -----------------
;________________________________________
delay:
	push cx
	mov cx,0xFFFF
	delay1:
	loop delay1
	mov cx,0xFFFF
	delay2:
	loop delay2
	mov cx,0xFFFF
	delay3:
	loop delay3
	mov cx,0xFFFF
	pop cx
	ret
;____________________________________________
;------------ RANDOM NUMBER -----------------
;____________________________________________
GenRandNum:
	push bp
	mov bp,sp;
	push cx
	push ax
	push dx;
	call delay
	MOV AH, 00h 	;interrupts to get system time
	INT 1AH 		;CX:DX now hold number of clock ticks since midnight
	mov ax, dx
	xor dx, dx
	mov bx, 10
	div bx 			;here dx contains the remainder of the division - from 0 to 9
	mov byte [random],dl;
	mov ax,[random]
	xor dx,DX
	mov bx,4		;further randomized - from 0 to 3
	div bx
	mov byte [random],dl
	pop dx
	pop ax
	pop cx
	pop bp
	ret
;---------------------------------------------------------
;---------------------INITIALIZE_BOARD--------------------
;---------------------------------------------------------
INITIALIZE_BOARD:
	PUSHA
	MOV byte [COUNT],0
	MOV SI,BOARD		
	MOV AX,0
	MOV CX,49
	initialize:
		CALL GenRandNum			
		MOV AL,[random]
		MOV BYTE [SI],AL
		INC SI
		LOOP initialize
	POPA
	RET
;________________________________________________
;----------------TRIANGLE CANDY------------------
;________________________________________________

TRIANGLE:
	pusha
	mov ax,[X]
	mov word [x1], ax
	add word [x1],15
	mov BH,0
	mov BX,0
	mov DX,[Y]
	add DX,5
	mov byte [y1],DL
	add byte [y1],9
	mov AX,[x1]
	mov word [x2], AX
	mov AH,0CH		;drawing pixels
	mov AL,0CH		;light red
	
	loop1:
		mov cx,[x1]
		loop2:
			int 10H
			inc cx
			cmp cx,[x2]
			jle loop2
		dec word [x1]			;decrementing starting point
		inc word [x2]			;incrementing ending point
		inc dx
		cmp dl,[y1]
		jne loop1
	popa
	ret
;_________________________________________________________
;---------------------SQUARE CANDY----------------------
;_________________________________________________________
square: 
	pusha
	mov AH,0CH		;function call for drawing pixels
	mov AL,0EH		;selecting dark blue color
	mov BH,01
	mov BX,[X]
	add BX,20		;x-axis range
	mov CX,[X]
	add CX,10
	mov DX,[Y]
	add DX,5
	int 10H
	mov byte [y1],Dl
	add byte [y1],10		;y-axis range
	loop3: 
		mov DX,[Y]
		add DX,5
		int 10H
		loop4:	
			inc DX
			int 10H
			cmp byte dl,[y1]
			jl loop4
		inc cx
		cmp cx,bx
		jl loop3
	popa
	RET
;_________________________________________________________
;---------------------RECTANGLE CANDY---------------------
;_________________________________________________________
rectangle: 
	pusha
	mov AH,0CH		;function call for drawing pixels
	mov AL,0BH
	mov BH,01
	mov BX,[X]
	add BX,25		;x-axis range
	mov CX,[X]
	add CX,5
	mov DX,[Y]
	add DX,7
	mov byte [y1],Dl
	add byte [y1],7		;y-axis range
		
	loop5: 
		mov DX,[Y]
		add DX,7
		int 10H
		loop6:	
			inc DX
			int 10H
			cmp byte dl,[y1]
			jl loop6
		inc CX
		cmp cx,BX
		jl loop5
	popa
	RET
;_________________________________________________
;-----------------DIAMOND CANDY-------------------
;_________________________________________________
DIAMOND:
	pusha
	mov AX,[X]
	mov word [x1], ax
	add word [x1],15
	mov BX,0
	mov DX,[Y]
	add DX,5
	mov byte [y1],DL
	add byte [y1],5
	mov AX,[x1]
	mov word [x2], AX
	mov AH,0CH		;drawing pixels
	mov AL,0AH		;light green
	
	loop7:
		mov CX,[x1]
		loop8:
			INT 10H
			inc CX
			cmp cx,[x2]
			jle loop8
		dec word [x1]			;decrementing starting point
		inc word [x2]			;incrementing ending point
		inc DX
		cmp dl,[y1]
		jne loop7
		
	add byte [y1], 6
	
	loop9:
		mov CX,[x1]
		loop10:
			int 10H
			inc CX
			cmp cx,[x2]
			jle loop10
		inc word [x1]			;decrementing starting point
		dec word [x2]			;incrementing ending point
		inc DX
		cmp dl,[y1]
		jne loop9
	popa
	RET
;---------------------------------------
;--------------- BOMB ------------------
;---------------------------------------
BOMB_CANDY:
	pusha
	mov AH,0CH		;drawing pixels
	mov AL,00H
	mov BH,01
	mov BX,[X]
	add BX,20		;x-axis range
	mov CX,[X]
	add CX,10
	mov DX,[Y]
	add DX,5
	mov [y1],DL
	add BYTE [y1],10			;y-axis range
	loop11: 
		mov AH,0CH
		inc AL
		mov DX,[Y]
		add DX,5
		int 10H
		loop12:	
			inc DX
			int 10H
			cmp byte dl,[y1]
			jl loop12
		inc CX
		cmp cx,BX
		jl loop11
	popa
	ret
;-------------------------------------
;---------TRIANGLE BOMB---------------
;-------------------------------------
TRIANGLE_BOMB:
	pusha
	mov ax,[X]
	mov word [x1], ax
	add word [x1],15
	mov BH,0
	mov BX,0
	mov DX,[Y]
	add DX,5
	mov byte [y1],DL
	add byte [y1],9
	mov AX,[x1]
	mov word [x2], AX
	mov AH,0CH		
	mov AL,03H		
	
	loop13:
		mov ah,0CH
		inc al
		mov cx,[x1]
		loop14:
			int 10H
			inc cx
			cmp cx,[x2]
			jle loop14
		dec word [x1]			;decrementing starting point
		inc word [x2]			;incrementing ending point
		inc dx
		cmp dl,[y1]
		jne loop13
	popa
	ret
;-----------------------------------------------------
;---------------------BLOCK_BORDER--------------------
;-----------------------------------------------------

BLOCK_BORDER:
	pusha
	MOV AH,0CH
	MOV AL,0FH
	MOV BX,[X]
	ADD BX,30
	MOV CX,[X]
	loop15: 				;constructs two parallel horizontal nice
		MOV DX,[Y]
		INT 10H
		ADD DX,20
		INT 10H
		INC CX
		cmp cx,bx
		jle loop15
	
	MOV BX,[Y]
	ADD BX,20
	MOV DX,[Y]
	loop16: 		;constructs two vertical lines
		MOV CX,[X]
		INT 10H
		ADD CX,30
		INT 10H
		INC DX	
		cmp dx,bx
		jle loop16
	popa
	ret
;----------------------------------------
;----------- SELECT BLOCK ---------------
;----------------------------------------
highlight_block:
	pusha
	MOV AH,0CH
	MOV AL,0FH
	MOV BX,[X]
	ADD BX,23
	MOV CX,[X]
	add cx,2
	loop17: 				;constructs two parallel horizontal nice
		MOV DX,[Y]
		add dx,2
		INT 10H
		add dx,15
		INT 10H
		INC CX
		cmp cx,bx
		jle loop17
	
	MOV BX,[Y]
	ADD BX,16
	MOV word DX,[Y]
	add dx,2
	loop18: 		;constructs two vertical lines
		MOV CX,[X]
		add cx,2
		INT 10H
		ADD CX,25
		INT 10H
		INC DX	
		cmp dx,bx
		jle loop18

	popa
	ret
;----------------------------------------
;---------- ARRAY INDEX -----------------
;----------------------------------------
CHECK_BLOCK:
	pusha
	mov byte [COUNT],0				;translates coordinates into block
	mov word [X],10
	mov CX,[GETX]
	x_index:							;while x is less than mouse coordinates
		inc byte [COUNT]		;incrementing for index
		add word [X],30
		cmp word [X],cx
		jl x_index
	sub word [X],30
	mov word [Y],20
	mov CX,[GETY]			;yy-coordinate get by mouse
	mov word BX,0
	y_index:		;getting y index
		inc BX
		add word [Y],20
		cmp word [Y],cx
		jl y_index
	sub word [Y],10
	CALL highlight_block	;selected block is highlighted
	mov word AX,7
	dec BX
	mul BX
	add byte [COUNT],AL
	dec byte [COUNT]
	popa
	RET

;---------------------------------------------------------
;---------------------DISPLAY_BOARD-----------------------
;---------------------------------------------------------
DISPLAYBOARD:
	CALL clrscr
	PUSHA
	MOV byte[COUNT],0
	MOV DI,BOARD
	MOV word [X],10
	MOV word [Y],30
	CANDYDISPLAY: 
	    MOV Al, [DI]
		CALL BLOCK_BORDER
		C1:CMP AL,0
		JE CALL_SQUARE 
		C2:CMP AL,1
		JE CALL_TRIANLE
		C3:CMP AL,2
		JE CALL_RECTANGLE
		C4:CMP AL, 3
		JE CALL_DIAMOND
		C5:CMP AL,4
		JE CALL_BOMB
		C6:CMP AL,5
		JE CALL_BOMB_TRI
		NEXT_CANDY:
			INC DI
			ADD word [X],30
			CMP word [X],220
			JGE adds
			INCREMENT:
			INC byte [COUNT]
			CMP byte [COUNT],49
			JNE CANDYDISPLAY
			JE RETURN
	
	CALL_SQUARE:call square
	jmp NEXT_CANDY
	CALL_TRIANLE:call TRIANGLE
	jmp NEXT_CANDY
	CALL_RECTANGLE: call rectangle
	jmp NEXT_CANDY
	CALL_DIAMOND: call DIAMOND
	jmp NEXT_CANDY
	CALL_BOMB: call BOMB_CANDY
	jmp NEXT_CANDY
	CALL_BOMB_TRI:call TRIANGLE_BOMB
	jmp NEXT_CANDY
	adds:
	mov byte [X],10
	add byte [Y],20
	JMP INCREMENT
	RETURN
	POPA
	RET
;************************************
;-----------HORIZONTAL---------------
;************************************
HORIZONTAL_CANDY_MATCH:
	pusha
	mov byte[matches],0
	mov byte[colorbomb],0
	mov byte[colorbombtri],0
	mov ax,0
	mov bx,0
	mov cx,0
	mov si,BOARD
	mov al,[si]
	mov byte [CANDYTYPE],al
	mov byte [CANDYCOUNT],1
	mov cx,49
	horizontal_check:
			inc si
			mov al,[CANDYTYPE]
			cmp al,[si]
			je check_next
		iff2:
			cmp byte [CANDYCOUNT],3
			jl check_next2
			;CALL SCORE_UPDATE
			mov byte [matches],1
			mov di,si
			push CX
			mov cx,0
			mov cl,[CANDYCOUNT]
		popcandy:
			call GenRandNum
			mov ah,[random]
			dec di
			mov byte [di],ah
			loop popcandy
			pop cx
			CMP BYTE [CANDYCOUNT],5
			JNE check_tri1
			mov byte[colorbomb],1
			SUB SI,3
			MOV BYTE [SI],4
			ADD SI,3
			jmp check_next2
			check_tri1:
			CMP BYTE [CANDYCOUNT],4
			JNE check_next2
			mov byte [colorbombtri],1
			SUB SI,3
			MOV BYTE [SI],5
			ADD SI,3
			check_next2:
			MOV byte [CANDYCOUNT],1		;resetting count
			MOV AL,[SI]
			MOV byte [CANDYTYPE],AL
			jmp k
		check_next:
		add byte[CANDYCOUNT],1
		k:
		loop horizontal_check
	popa
	RET
;************************************
;-------------VERTICAL---------------
;************************************
VERTICAL_CANDY_MATCH:
	pusha 
	mov byte[matches],0
	mov byte[colorbomb],0
	mov byte[colorbombtri],0
	mov ax,0
	mov bx,0
	mov dx,0
	mov cx,0
	mov si,BOARD
	mov al,[si]
	mov byte [CANDYTYPE],al
	mov byte [CANDYCOUNT],1
	MOV AX,0
	vertical_check:
		;push cx
		mov si,BOARD
		add si,dx
		mov bl,[si]
		mov byte [CANDYTYPE],bl
		mov cx,7 
		mov ah,0
		mov bh,0
		sec_check:
			add si,7
			cmp bl,[si]
			je check_next1
			cmp byte [CANDYCOUNT],3
			jl check_next3
			;CALL SCORE_UPDATE
			mov byte [matches],1
			mov di,si
			mov cl,[CANDYCOUNT]
			popcandy1:
			call GenRandNum
			mov ah,[random]
			sub di,7
			mov byte [di],ah
			mov dl,[di]
			loop popcandy1
			CMP BYTE [CANDYCOUNT],5
			JNE check_tri
			mov byte[colorbomb],1
			SUB SI,21
			MOV BYTE [SI],4
			ADD SI,21
			jmp check_next3
			check_tri:
			CMP BYTE [CANDYCOUNT],4
			JNE check_next3
			mov byte [colorbombtri],1
			SUB SI,21
			MOV BYTE [SI],5
			ADD SI,21
			check_next3:
			MOV byte [CANDYCOUNT],1		;resetting count
			MOV BL,[SI]
			MOV byte [CANDYTYPE],BL
			jmp k1
			check_next1:
			add byte[CANDYCOUNT],1
			k1:
			add bh,1
			cmp bh,7
			jne sec_check
		;inc al
		;pop CX
		add dx,1
		cmp dx,7
		jne vertical_check
	
	popa
	ret
;___________________________________________
;-------------CHECKING MATCHES--------------
;___________________________________________

check_candy_match:
	CALL HORIZONTAL_CANDY_MATCH
	CALL VERTICAL_CANDY_MATCH
	
	RET
;*****************************************
;-------------- CHECK BOMB ---------------
;*****************************************
check_bomb:
	pusha
	mov byte [matches],0
	mov byte [colorbomb],0
	mov byte [colorbombtri],0
	mov byte [CANDYCOUNT],0
	mov si,BOARD
	mov bx,0
	mov bl,[INDEX1]
	add si,bx
	mov di,BOARD
	mov dx,0
	mov dl,[INDEX2]
	add di,DX
	cmp byte [si],4
	jne next
	mov al,[di]
	mov [si],al
	mov byte [CANDYCOUNT],0
	mov [CANDYTYPE],al
	jmp popcandies
	next:
	cmp byte [di],4
	jne over
	mov al,[si]
	mov [di],al
	mov [CANDYTYPE],AL
	mov byte [CANDYCOUNT],0
	popcandies:
		mov si,BOARD
		mov cx,50
		iterate:
		mov al,[CANDYTYPE]
		cmp [si],AL
		jne nextcandy
		call GenRandNum
		mov dh,[random]
		mov [si],DH
		inc byte [CANDYCOUNT]
		nextcandy:
			inc SI
		loop iterate
		call SCORE_UPDATE
	over:
	popa 
	ret
;-----------------------------------------
;------------ CHECK BOMB 2 ---------------
;-----------------------------------------
check_bomb_tri:
	pusha
	mov byte [matches],0
	mov byte [colorbomb],0
	mov byte [colorbombtri],0
	mov byte [CANDYCOUNT],0
	mov si,BOARD
	mov bx,0
	mov bl,[INDEX1]
	add si,bx
	mov di,BOARD
	mov dx,0
	mov dl,[INDEX2]
	add di,DX
	cmp byte [si],5
	jne next1
	mov al,[di]
	mov [si],al
	mov byte [CANDYCOUNT],0
	mov [CANDYTYPE],al
	jmp popcandies1
	next1:
	cmp byte [di],5
	jne over1
	mov al,[si]
	mov [di],al
	mov [CANDYTYPE],AL
	mov byte [CANDYCOUNT],0
	popcandies1:
		mov si,BOARD
		mov cx,50
		iterate1:
		mov al,[CANDYTYPE]
		cmp [si],AL
		jne nextcandy1
		call GenRandNum
		mov dh,[random]
		mov [si],DH
		inc byte [CANDYCOUNT]
		nextcandy1:
			inc SI
		loop iterate1
		call SCORE_UPDATE
	over1:
	popa 
	ret
;--------------------------------------
;----------- BOMB MOVE ----------------
;--------------------------------------
check_valid_bomb_move:
	pusha
	mov si,BOARD
	mov bx,0
	mov bl,[INDEX1]
	add si,bx
	mov di,BOARD
	mov dx,0
	mov dl,[INDEX2]
	add di,DX
	cmp byte [si],5
	je isbombvalid
	cmp byte [di],5
	je isbombvalid
	cmp byte [si],4
	je isbombvalid
	cmp byte [di],4
	je isbombvalid
	mov byte [bomb],0
	jmp returntovalid
	isbombvalid:
	mov byte [bomb],1
	returntovalid:
	popa 
	ret
;----------------------------------------------------
;------------------ SWAP CANDIES --------------------
;----------------------------------------------------
swap: 
	pusha
	MOV SI,BOARD	;swaps both indices using SI and DI 
	MOV BX,0
	MOV BL,[INDEX1]
	ADD SI,BX
	MOV AL,[SI]
	MOV DI,BOARD
	MOV BL,[INDEX2]
	ADD DI,BX
	MOV AH, [DI]
	MOV [SI],AH
	MOV [DI],AL
	popa
	RET
;-------------------------------------------------
;------------- CHECK VALID MOVE ------------------
;-------------------------------------------------
VALIDITY:
	PUSHA
	MOV AX,0
	MOV BX,0
	MOV AL,[INDEX1]
	MOV BL,[INDEX2]
	MOV DI,BOARD		;moving address of board
	MOV CX,0
	DEC BL
	CMP BL,AL
	JE other_check
	ADD BL,2
	CMP BL,AL
	JE other_check
	MOV BL,[INDEX2]
	ADD BL,7
	CMP BL,AL
	JE other_check
	SUB BL,14
	CMP BL,AL
	JE other_check
	JNE not_valid
	
	
	other_check:
	jmp VALID_CHECK
	;call swap
	;call HORIZONTAL_CANDY_MATCH
	;cmp byte [matches],0
	;jne VALID_CHECK
	;call VERTICAL_CANDY_MATCH
	;cmp byte [matches],0
	;jne VALID_CHECK
	;call check_valid_bomb_move
	;cmp byte [bomb],0
	;jne VALID_CHECK
	;call swap
	not_valid:
	MOV BYTE [VALID],1
	POPA
	RET
	VALID_CHECK:
		;call swap
		MOV BYTE [VALID],0
		POPA
		RET
;--------------------------------------
;----------- NEW SCREEN ---------------
;--------------------------------------
NEW_SCREEN:
	PUSHA
	MOV AH,02H
	MOV BH,0
	MOV DH,6		;displays message in line 6
	MOV DL,29		;starts display with a gap of 29 spaces
	INT 10H
	mov dx,Name
	mov ah,09H
	INT 21H
	mov DX, pname	
	MOV AH,09H
	INT 21H
	MOV AH,02H
	MOV BH,0
	MOV DH,8		;displays message in line 8
	MOV DL,29		;starts display with a gap of 29 spaces
	INT 10H
	MOV DX,MOVESSTR	
	MOV AH,09H
	INT 21H
	MOV AX,0
	MOV AL,[MOVES]	
	PUSH AX
	CALL printnum
	MOV DX,TOTAL_MOVES		
	MOV AH,09H
	INT 21H	
	MOV AH,02H
	MOV BH,0
	MOV DH,10		;displays message in line 10
	MOV DL,29		;starts display with a gap of 29 spaces
	INT 10H
	
	MOV DX,SCORESTR			;displays the word 'Score'
	MOV AH,09H
	INT 21H
	MOV AX,0
	MOV AL,[SCORE]	
	PUSH AX
	CALL printnum
	cmp byte[VALID],1
	jne returntodisplay
	MOV AH,00H     		;setting video mode
	MOV AL,0Dh   		;320x200 pixels and 16 colours
	INT 10H
	mov ah, 0x13 		; service 13 - print string
	mov al, 1 			; subservice 01 – update cursor
	mov bh, 0 			; output on page 0
	mov bl, 0x5F 		; normal attrib
	mov dx, 0x0F1C 		; row 10 column 3
	mov cx, 12 			; length of string
	push cs
	pop es 				; segment of string
	mov bp, INVALID 	; offset of string
	int 0x10
	
	returntodisplay:
	POPA
	RET
;----------------------------------
;--------- BEEP SOUND -------------
;----------------------------------
sound:
	push ax
	push bx
	push cx
	mov al,182
	out 43h,al
	mov ax,4560	
	out 42h,al
	mov al,ah
	out 42h,al
	in al,61h	
	or al,00000011b
	out 61h,al
	mov bx,25

	pause1:
	mov cx,6000

	pause2:
	dec cx
	jne pause2
	dec bx
	jne pause1
	in al,61h	
	and al,11111100b
	out 61h,al
		
	pop cx
	pop bx
	pop ax
	ret
;---------------------------------------------------------
;-------------------- START OF GAME ----------------------
;---------------------------------------------------------

start:
	MOV AH,00H     	;setting video mode
	MOV AL,0Dh   	;320x200 pixels and 16 colours
	INT 10H
	MOV AH,0BH
	MOV BH,00H
	MOV BL,05H 		;background colour
	INT 10H
	CALL welcomescreen
	call sound
	MOV AH,00H     
	MOV AL,0Dh   
	INT 10H
	CALL INITIALIZE_BOARD
	MOV AH,06h		;scroll-up window
	MOV AL,00
	MOV BH,25H		
	MOV CH,0		
	MOV CL,0		
	MOV DH,24
	MOV DL,79
	INT 10H
	MOV CX,15
	call check_candy_match
	call check_candy_match
	mov byte [SCORE],0
	numberofmoves:
		push cx
		AGAIN:
		call delay
		mov ax,00h
		int 33h
		MOV AH,00H     
		MOV AL,0Dh   	
		INT 10H
		MOV AH,00H
		MOV AL,13H
		INT 10H
		MOV AH,0BH
		MOV BH,00H
		MOV BL,00H
		INT 10H
		MOV AH,06h		;scroll-up window
		MOV AL,0
		MOV BH,23H		
		MOV CH,0			
		MOV CL,0		
		MOV DH,24
		MOV DL,79
		INT 10H
		MOV AH,02H
		CALL NEW_SCREEN
		MOV BYTE [VALID],0
		call DISPLAYBOARD
		CALL getcoordinates
		call CHECK_BLOCK
		MOV CL,[COUNT]
		MOV [INDEX1],CL
		CALL delay
		CALL delay
		MOV BL,00H
		mov ax,00h
		int 33h 
		CALL getcoordinates
		CALL CHECK_BLOCK
		MOV CL,[COUNT]
		MOV [INDEX2],CL 
		CALL VALIDITY
		CMP byte [VALID],1
		JE AGAIN
		call swap
		mov ax,00h
		int 33h
		call check_bomb
		call check_bomb_tri
		call check_candy_match
		call SCORE_UPDATE
		mov cx,6
		checking_matches:
			call check_candy_match
		loop checking_matches
	pop cx
	add byte [MOVES],1
	dec cx
	cmp cx,0
	jne numberofmoves
	
	CALL check_candy_match
	CALL check_candy_match
	
	MOV AH,00H
	MOV AL,13H
	INT 10H
	MOV AH,0BH
	MOV BH,00H
	MOV BL,00H
	INT 10H
	MOV AH,06h		;scroll-up window
	MOV AL,0
	MOV BH,23H		
	MOV CH,0			
	MOV CL,0		
	MOV DH,24
	MOV DL,79
	INT 10H
	CALL NEW_SCREEN
	call DISPLAYBOARD
	mov ah,00
	int 16h
	MOV AH,00H     	;setting video mode
	MOV AL,0Dh   	;320x200 pixels and 16 colours
	INT 10H
	MOV AH,0BH
	MOV BH,00H
	MOV BL,05H 	
	INT 10H
	call end_screen
END:
mov ax,0x4c00
int 0x21
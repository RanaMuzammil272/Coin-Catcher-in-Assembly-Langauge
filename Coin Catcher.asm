[org 0x0100]
jmp start
msg1: db'Score:0', 0
msg2: db'Time:0', 0
msg3: db'Welcome To Catch And Score Game', 0
msg4: db'Game Over', 0
coin_pos:  dw 20h,40h
coin_size:   dw  8h
bomb_x_pos:  dw  195h,200h
bomb_y_pos:  dw 100h,100h
bomb_radius: dw 8h
bucket_x1:dw 150
bucket_y1: dw 150
bucket_size:dw 30
oldisr:  dd 0
tickcount:  dw 0
digit:  dw   0
timerflag: dw 0
scoreget: dw 0
redraw_cont: dw 0
;********************START****************

start:
        ;setting graphics mode 
       
	 
	push 0a000h           ;Video memory graphics segment
	pop es

	mov ax, 0013h         ;320x200   (25x45)
	int 10h

    call clrscr
	call delay
	call welcome_screen
	 mov ah, 0 ; service 0 – get keystroke 
 	int 0x16 ; call BIOS keyboard service
	call delay	
	call clrscr
	mov bx,[bucket_x1]
	push bx
	mov bx,[bucket_y1]
	push bx
	call draw_bucket
	mov ax,[coin_pos]
	push ax
	mov ax,[coin_size]
	push ax
	call draw_coin
	push 0h             ;red
	mov ax,[bomb_x_pos]  ;Cx
	push ax
	mov ax,[bomb_y_pos]  ;Cy
	push ax
	mov ax,[bomb_radius] ;radius
	push ax
	call draw_bomb
    call color_bottom
	call score_print
	call time_print
	call delay
	
   
    xor ax, ax 
    mov es, ax ; point es to IVT base
   
	
   			
    mov ax, [es:9*4] 
    mov word[oldisr], ax ; save offset of old routine 
    mov ax, [es:9*4+2] 
    mov word[oldisr+2], ax 
	
    xor ax, ax
	mov es, ax ; point es to IVT base
	 ; disable interrupts
	sti
	 mov word [es:9*4], kbisr ; store offset at n*4
	mov [es:9*4+2], cs ; store segment at n*4+2

	 mov word [es:8*4], timer; store offset at n*4 
	mov [es:8*4+2], cs ; store segment at n*4+2
	cli
	
	 ; enable interrupts
    
    

 	
exit_game: mov ah, 0 ; service 0 – get keystroke 
     int 0x16 ; call BIOS keyboard service 
     cmp al, 27 ; is the Esc key pressed 
     jne exit_game ; if no, check for next key 
	;return
	call clrscr
	call End_screen
	 mov ah, 0 ; service 0 – get keystroke 
 	int 0x16 ; call BIOS keyboard service 
	
	;Restore text mode
	mov ax, 0003h
    	int 10h
    mov ax,0x4c00
	int 21h



;*********Welcome Screen********

welcome_screen:
	push ax
	push bx
	push cx
	push dx
	push es
	push ds
	push si
	push di
	push bp
	mov ah, 0x13 ; service 13 - print string
	mov al, 1 ; subservice 01 – update cursor
	mov bh, 0 ; output on page 0
	mov bl, 2; normal attrib
	mov dx, 0x0A06 ; row 10 column 6
	mov cx,31  ; length of string
	push cs
	pop es ; segment of string
	mov bp, msg3 ; offset of string
	int 0x10
	pop bp
	pop di
	pop si
	pop ds
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	ret

;******score*****

score_print:
	push ax
	push bx
	push cx
	push dx
	push es
	push ds
	push si
	push di
	push bp
	mov ah, 0x13 ; service 13 - print string
	mov al, 1 ; subservice 01 – update cursor
	mov bh, 0 ; output on page 0
	mov bl, 10; normal attrib
	mov dx, 0x0021 ; row 0 column 33
	mov cx, 7 ; length of string
	push cs
	pop es ; segment of string
	mov bp, msg1 ; offset of string
	int 0x10
	pop bp
	pop di
	pop si
	pop ds
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	ret
;*******time******

time_print:
	push ax
	push bx
	push cx
	push dx
	push es
	push ds
	push si
	push di
	push bp
	mov ah, 0x13 ; service 13 - print string
	mov al, 1 ; subservice 01 – update cursor
	mov bh, 0 ; output on page 0
	mov bl, 10 ; normal attrib
	mov dx, 0x000F ; row 0 column 15
	mov cx, 6 ; length of string
	push cs
	pop es ; segment of string
	mov bp, msg2 ; offset of string
	int 0x10
	pop bp
	pop di
	pop si
	pop ds
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	ret
    
        
;*****************CLEAR SCREEN********************************

; clear screen using string instructions 

; subroutine to clear the screen 
clrscr: 

	push ax
	push bx
	push cx
	push dx
	push es
	push ds
	push si
	push di
	push bp
	mov ax,0A000h
    mov es,ax
    xor di,di
    xor ax,ax
    mov ax,0x2007
    mov cx,64000
    cld
    rep stosb
	pop bp
	pop di
	pop si
	pop ds
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	ret
	
;----------------------------clearscreen for bucket-----------------------

clrscrb: 

	push ax
	push bx
	push cx
	push dx
	push es
	push ds
	push si
	push di
	push bp
	mov ax,0A000h
    mov es,ax
    mov di,48000
    xor ax,ax
    mov ax,0x2007
    mov cx,11200
    cld
    rep stosb
	pop bp
	pop di
	pop si
	pop ds
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	ret

color_bottom: 

	push ax
	push bx
	push cx
	push dx
	push es
	push ds
	push si
	push di
	push bp
	mov ax,0A000h
    mov es,ax
    mov di,59200
    xor ax,ax
    mov ax,0x200A
    mov cx,4800
    cld
    rep stosb
	pop bp
	pop di
	pop si
	pop ds
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	ret
;*******************DRAW COIN***********************************

draw_coin: 

    push bp
	mov bp,sp
    push ax
	push bx
	push cx
	push dx
	push es
	push ds
	push si
	push di

       
    mov cx,[bp+6]
	mov dx,[bp+6]
    
        ; ----------------drawing coin-----------------------
        ;------------------for upper portion----------------
        mov ah,0ch                ; mode 13h
        mov al,0eh                ; color
        mov bh,0h                 ; page_no
        int 10h                   ; drawing first pixel
repeat_0: inc dx                    ; next line
          mov si,dx
          sub si,[bp+6]
          sub cx,si

repeat_1:   int 10h              ;drawing lines
        inc cx
        cmp cx,dx                
        jng repeat_1
        mov cx,[bp+6]           ;drawing lines
        cmp si,[bp+4]            ;to check wether to draw next line or coin_size is reached 
        jne repeat_0             
         
       ;------------------for lower portion----------------
        mov dx,[bp+6]          
        add dx,[bp+4]          ;last printed line
        inc dx                 ;next line(y_pos) where to draw next printing

repeat_3:mov si,[bp+6]
        add si,[bp+4]
        mov cx,[bp+6]
        sub cx,[bp+4]              ;point at x_pos where to start printing

repeat_2: int 10h
        inc cx        
        cmp cx,si
        jng repeat_2                    ;check all pixel printed
        add dx,1                        ; moving next line
        sub word[bp+4],1
        jnz repeat_3                   ; check wether size is reached
        mov cx,[bp+6]            ; drawing last pixel
        int 10h
		
		pop di
		pop si
		pop ds
		pop es
		pop dx
		pop cx
		pop bx
		pop ax
		pop bp
        ret 4




;********************DRAW BUCKET****************************************


draw_bucket:
	
        ; ----------------drawing bucket-----------------------
     push bp
     mov bp,sp	 
     push ax
	push bx
	push cx
	push dx
	push es
	push ds
	push si
	push di
	mov ah,0ch
    mov al,Ah	; color
    mov bh,0h                 ; page_no
        

	mov cx,[bp+6]
	mov dx,[bp+4]
	int 10h
	mov bl,30
right_line:
	int 10h
	inc dx
	dec bl
	jnz right_line

	mov bl,30
bottom_line:
	int 10h
	dec cx
	dec bl
	jnz bottom_line

mov bl,30
left_line:
	int 10h
	dec dx
	dec bl
	jnz left_line

  

	
	pop di
	pop si
	pop ds
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 4


;******************DRAW BOMB ********************************************


draw_bomb:

	 push bp
 mov bp, sp

 sub sp, 02h

 mov cx, WORD [bp+04h]   ;R

 mov ax, cx              
 mul ax                  ;AX = R^2
 mov WORD [bp-02h], ax   ;[bp-02h] = R^2



 mov ax, WORD [bp+06h]
 sub ax, cx              ;i = cY-R
 mov bx, WORD [bp+08h]
 sub bx, cx              ;j = cX-R

 shl cx, 1
 mov dx, cx              ;DX = Copy of 2R

move_vertical:
 push cx
 push bx

 mov cx,  dx

move_horizontal:
  ;Save values
  push bx
  push ax
  push dx

  ;Compute (i-y) and (j-x)
  sub ax, WORD [bp+06h]
  sub bx, WORD [bp+08h]

  mul ax                  ;Compute (i-y)^2

  push ax
  mov ax, bx             
  mul ax
  pop bx                  ;Compute (j-x)^2 in ax, (i-y)^2 is in bx now

  add ax, bx              ;(j-x)^2 + (i-y)^2
  cmp ax, WORD [bp-02h]   ;;(j-x)^2 + (i-y)^2 <= R^2

  ;Restore values before jump
  pop dx
  pop ax
  pop bx

  ja continue            ;Skip pixel if (j-x)^2 + (i-y)^2 > R^2

  ;Write pixel
  push WORD [bp+0ah]
  push bx
  push ax
  call writePx


continue:

  ;Advance j
  inc bx
 loop move_horizontal

 ;Advance i
 inc ax


 pop bx            ;Restore j
 pop cx            ;Restore counter

loop move_vertical

 add sp, 02h


 pop bp
 ret 08h



;Color
;X
;Y
writePx:
 push bp
 mov bp, sp

 push ax
 push bx

 mov bx, WORD [bp+04h]
 mov ax, bx
 shl bx, 6
 shl ax, 8
 add bx, ax       ;320 = 256 + 64

 add bx, WORD [bp+06h]
 mov ax, WORD [bp+08h]

 

 mov BYTE [es:bx], al

 pop bx
 pop ax

 pop bp
 ret 06h
;********SCROLL DOWN***********
scrolldown:


mov ah,07h
mov al,1
mov bh,07h    ;no_of_lines_to_shift
mov cl,0    ;upper_coll_no
mov ch,1    ;upper_row_no
mov dl,39   ;lower_coll_no
mov dh,17   ;lower_row_no
 int 10h



ret
;*********DELAY************
delay:

push bx
push cx
mov bx,2
loop1:
sub bx,1

mov cx,0xffff
loop2:
loop loop2
cmp bx,0
jnz loop1
pop cx
pop bx
ret
;***************End screen*************



End_screen:
	push ax
	push bx
	push cx
	push dx
	push es
	push ds
	push si
	push di
	push bp
	mov ah, 0x13 ; service 13 - print string
	mov al, 1 ; subservice 01 – update cursor
	mov bh, 0 ; output on page 0
	mov bl, 2; normal attrib
	mov dx, 0x0A0F ; row 10 column 15
	mov cx,9  ; length of string
	push cs
	pop es ; segment of string
	mov bp, msg4 ; offset of string
	int 0x10
	pop bp
	pop di
	pop si
	pop ds
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	ret


;***********************MOVE BUCKET****************

kbisr: 
    
	push ax
	push es
	in al, 0x60 ; read a char from keyboard port
	cmp al,  0x4B ; left arrow key
	jne nextcmp ; no, try next comparison
	
	call clrscrb

	sub word[cs:bucket_x1],1
	mov bx,[cs:bucket_x1]
	push bx
	mov bx,[cs:bucket_y1]
	push bx
	
	
	call draw_bucket
	
	
	
	jmp nomatch ; leave interrupt routine
	
nextcmp: 
	cmp al, 0x4D ; right arrow key
	
	jne cmp_with_esc ; no, leave interrupt routine
	
	call clrscrb
	
	add word[cs:bucket_x1],1
	mov ax,[cs:bucket_x1]
	push ax
	mov ax,[cs:bucket_y1]
	push ax
	call draw_bucket
	
	
	 
cmp_with_esc:
    cmp al,0x81
	jne nomatch
	mov word[cs:timerflag],1
nomatch: 
	
	pop es
	pop ax
    jmp far [cs:oldisr]	
	
	
;************************************	
	
scrolldown2: push bp 
 push ax 
 push cx 
 push si 
 push di 
 push es 
 push ds 
 

repeat_0001:mov ax,320
 mov bx,1 
 
 mul word bx
 push ax 
 mov si, 47999 
 sub si, ax  
 mov cx, 44800
sub cx,ax 
 
 mov ax, 0xA000
 mov es, ax  
 mov ds, ax  
 mov di, 47999 
 std 
 rep movsb  
 mov ax, 0x2007 
 pop cx  
 rep stosb 
 pop ds 
 pop es 
 pop di 
 pop si 
 pop cx 
 pop ax 
 pop bp 
 ret 
 
 
 
 
 
 
 
 ;**************************timer**********************
 
 
 printtime: push bp 
 mov bp, sp 
 push es 
 push ax 
 push bx 
 push cx 
 push dx 
 push di 
 mov ax, 0xA000
 mov es, ax ; point es to video base 
 mov ax, [bp+4] ; load number in ax 
 mov bx, 10 ; use base 10 for division 
 mov si, 0 ; initialize count of digits 
nextdigit: mov dx, 0 ; zero upper half of dividend 
 div bx ; divide by 10 
 add dl, 0x30 ; convert digit into ascii value 
 push dx ; save ascii value on stack 
 inc si ; increment count of values 
 cmp ax, 0 ; is the quotient zero 
 jnz nextdigit ; if no divide it again 
   mov di,0
nextpos: pop dx
 mov word[cs:digit],dx
 mov ah,0x13
 mov al,1
 mov bh,0
 mov bl,10
 mov bp,digit ;  
mov dh,0h
mov dx,0x0014
add dx,di
 mov cx,1
 push cs
 pop es 
 int 10h
 inc di
 dec si ;  
 jnz nextpos 
 pop di 
 pop dx 
 pop cx 
 pop bx 
 pop ax
 pop es 
 pop bp 
 ret 2 
; timer interrupt service routine 

timer:
 
push ax
 cmp word[cs:timerflag],1
 je end
 inc word [cs:tickcount]; increment tick count
cmp word [cs:tickcount],2184
je  exit_game
 push word [cs:tickcount] 
 
 call printtime ; print tick count 
add word[cs:bomb_y_pos],1
add word[cs:bomb_x_pos],1
add word[cs:redraw_cont],1
 call scrolldown2
 add word[cs:redraw_cont],1
 cmp word[cs:redraw_cont],100
 jne end
 mov ax,[cs:coin_pos]
	push ax
	mov ax,[cs:coin_size]
	push ax
 call draw_coin
 mov ax,[cs:coin_pos+2]
	push ax
	mov ax,[cs:coin_size]
	push ax
 call draw_coin
 push 0h             ;red
	mov ax,[cs:bomb_x_pos+2]  ;Cx
	push ax
	mov ax,[cs:bomb_y_pos+2]  ;Cy
	push ax
	mov ax,[cs:bomb_radius] ;radius
	push ax
	call draw_bomb
 mov word[cs:redraw_cont],0
;call collusion
 end: mov al, 0x20 
 out 0x20, al ; end of interrupt 
 pop ax 
 iret



;********************COLLUSION***************

;collusion:




;mov ax,[coin_pos]
;add ax,17
;cmp ax,[bucket_y1]
;jne skip

;mov ax,[coin_pos]
;add ax,17
;cmp ax,[bucket_x1]
;jnb skip




;mov ax,0x4c00
;int 21h
;ret

;skip: 

;ret
	













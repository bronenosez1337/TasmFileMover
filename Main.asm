.386
SHORT_NAME_LENGTH       equ   11

data segment use16
;---------------ANOTHER PARAMS FOR PROGRAM LOGIC-----------
cluster_number  dw      ?
file_record     db      32 dup (?)
sector          db      512 dup (0)
adress          db      50,51 dup ('$')
names           db      7 dup(SHORT_NAME_LENGTH dup (' '))
numlog          db      ?
free_record_addr dw     ?
;filename        db      SHORT_NAME_LENGTH dup " "
;---------------FOR 25h INTERRUPT-------------------------
read_parameters dd      ?
		dw      1
adr_buf         dw      sector
adr_seg         dw      data
;---------------LOGICAL DISK PARAMETERS-------------------
cluster_size    db      ?
reserved_sec    dw      ?
root_size       dw      ?
FAT_size        dw      ?

FAT_beg_sec     dw      ?
root_beg_sec    dw      ?
first_clust_sec dw      ?
;----------------MESSAGES---------------------------------
invite_message   db     'Enter path of file', 0Ah, 0Dh, '$'
invite_copy_msg  db     0Ah, 0Dh,'Enter path of file with SAME FILENAME', 0Ah, 0Dh, '$'
letter_err_msg   db     0Ah, 0Dh,'Error in letter of disk. Please enter letter in upper case!$'
length_error_msg db     0Ah, 0Dh,'I found error in your path stroke. Restart me, pls :3$'
read_sec_error_msg db   0Ah, 0Dh,'Reading sector error$'  
search_error_msg   db   0Ah, 0Dh,'Error with file searching$'
file_exist_err_msg db   0Ah, 0Dh,'File exist in this directory, pls enter another path',0Ah,0Dh,'$'
path_incorrect_msg db   0Ah, 0Dh,'Path incorrect, pls enter path again',0Ah,0Dh,'$'   
;----------------FLAGS-------------------------------------
i_found_file_flag db    0
search_error_flag db      0
check_next_flag db      0
data ends   

main_code segment use16
    assume ds:data, cs:main_code
begin:
	mov     dx, data
	mov     ds,dx
	mov     es,dx
;                                               ClearConsole
	mov     ax,3
	int     10h
;                                               InviteMessage
	lea     dx, invite_message
	mov     ah, 9
	int     21h
;                                               EnterPath
	mov     ah, 0Ah
	lea     dx, adress
	int     21h
;                                               LetterToNum
	mov     al, adress+2
	call    letter_to_log_num
;                                               PathToNames
	lea     si, adress+5
	lea     di, names
	call    stroke_to_names
	cmp     bl,0
	jne     length_error

;                                               System structures read
	mov     read_parameters, 0
	call    read_sector
	cmp     ah,0
	je      main_exit
	mov     cl,             sector+0Dh        
	mov     cluster_size,   cl
	mov     cx,             word ptr sector+0Eh
	mov     reserved_sec,   cx    
	mov     FAT_beg_sec,    cx
	mov     bx,             word ptr sector+SHORT_NAME_LENGTH
	mov     root_size,      bx
	mov     cx,             word ptr sector+16h
	mov     FAT_size,       cx

	shl     cx, 1
	add     cx, reserved_sec
	mov     root_beg_sec, cx
	shr     bx,4              
	add     cx,bx           ;bx=Root size / 16 + cx= root begin
	mov     first_clust_sec, cx
;                                               Read root sector
	movzx   eax, root_beg_sec
	mov     read_parameters, eax
	mov     cl, cluster_size
	mov     bx, 0
main_read_loop:        
	call    read_sector
	call    search_file
	cmp     search_error_flag,1
	jz      search_error
	cmp     check_next_flag,0
	jnz     short check_next
	cmp     i_found_file_flag,1
	jz      next_step
	call    calculate_first_sec_of_cluster
	add     bx, SHORT_NAME_LENGTH
	jmp     main_read_loop

check_next:
	mov     check_next_flag,0
	dec     cl
	cmp     cl,0
	jz      search_error      
	add     read_parameters,1
	jmp     short main_read_loop

next_step:
;                                               InviteMessage
	lea     dx, invite_copy_msg
	mov     ah, 9
	int     21h
;                                               EnterPath
back_to_enter_path:        
	mov     ah, 0Ah
	lea     dx, adress
	int     21h
;                                               PathToNames
	lea     si, adress+5
	lea     di, names
	call    stroke_to_names
	cmp     bl,0
	je      correct_path
incorrect_path:        
	lea     dx, path_incorrect_msg
	mov     ah, 9
	int     21h
	jmp     next_step

;                                               Read root sector
correct_path:    
	movzx   eax, root_beg_sec
	mov     read_parameters, eax
	mov     cl, cluster_size
	mov     bx, 0
main_read_loop1:        
	call    read_sector
	call    search_file
	cmp     search_error_flag,1
	jz      short find_free_record
	cmp     check_next_flag,0
	jnz     short check_next1
	cmp     i_found_file_flag,1
	jz      file_exist_error
	call    calculate_first_sec_of_cluster
	add     bx, SHORT_NAME_LENGTH
	jmp     main_read_loop1

check_next1:
	mov     check_next_flag,0
	dec     cl
	cmp     cl,0
	jz      incorrect_path      
	add     read_parameters,1
	jmp     short main_read_loop1

find_free_record:
	lea     si, file_record
	mov     cx, 32
	rep movsb
	call write_sector

main_exit:
	mov     ah,4Ch
	int     21h

file_exist_error:
	lea     dx, file_exist_err_msg
	mov     ah, 9
	int     21h
	jmp     next_step
search_error:
	lea     dx, search_error_msg
	mov     ah, 9
	int     21h
	jmp     short main_exit
letter_error:
	lea     dx, letter_err_msg
	mov     ah, 9
	int     21h
	jmp     short main_exit
length_error:
	lea     dx, length_error_msg
	mov     ah, 9
	int     21h
	jmp     short main_exit
;++++++++++++++++++++++++++++++++++++++++
;+              PROCEDURES                                              +        
;++++++++++++++++++++++++++++++++++++++++
;========Find number of logical disk=========;
letter_to_log_num proc
	cmp     al, 41h
	jb      letter_error
	cmp     al, 5Ah
	ja      letter_error
	sub     al, 41h
	mov     numlog,al
	ret
letter_to_log_num endp

;=======Convert path string to different names=====
stroke_to_names proc
	mov     ah,0
next_name:
	cmp     ah,8    
	jz      str_to_nam_error
	mov     cx, 09h                
str_to_nam_loop:
	lodsb        
	cmp     al, 5Ch         ;backslash
	jz      short is_backslash
	cmp     al, 2Eh         ;dot
	jz      short is_dot
	stosb
	loop    str_to_nam_loop
str_to_nam_error:
	mov     bl,1
	ret     
is_backslash:
	inc     ah
	add     cx,2
	add     di, cx
	jmp     short next_name
is_dot:
	inc     ah
	add     di, cx
	dec     di
	mov     cx, 3
	rep movsb 
	mov     bl,0
	ret
stroke_to_names endp

;============Reading Sector in Logical Disk ============
read_sector proc
	push    cx
	push    bx
	mov al, numlog
	mov cx, 0FFFFh
	lea     bx,read_parameters
	int     25h
	jc      short read_sec_error
	pop     dx
	mov     ah,1
	pop     bx
	pop     cx
	ret        
read_sec_error:
	lea     dx, read_sec_error_msg
	mov     ah, 9
	int     21h
	pop     dx
	mov ah,0        
	ret
read_sector endp

;============Write Sector in Logical Disk ============
write_sector proc
	push    cx
	push    bx
	mov al, numlog
	mov cx, 0FFFFh
	lea     bx,read_parameters
	int     26h
	jc      short read_sec_error
	pop     dx
	mov     ah,1
	pop     bx
	pop     cx
	ret        
write_sec_error:
	lea     dx, read_sec_error_msg
	mov     ah, 9
	int     21h
	pop     dx
	mov ah,0        
	ret
write_sector endp

;===============FILE SEARCH PROCEDURE====================
search_file proc
	mov     di,0
	mov     si,0
	mov     bp,0
compare_loop:     
	mov     al, byte ptr names+si+bx        ;Reading first let in names
	cmp     al, byte ptr sector+bp+di       ;Compare with let in record
	jz      short i_found_letter
	mov     si,0                            ;Not Find Letter
	mov     bp,0
	add     di, 32                          ;Next record
	cmp     di, 512
	jz      short sector_is_over                 ;Sector over
	cmp     byte ptr sector+bp+di,0
	jz      short fail_to_find                    ;If last record
	jmp     short compare_loop
i_found_letter:
	inc     si
	inc     bp
	cmp     si,SHORT_NAME_LENGTH
	jnz     short compare_loop              ;compare next symbol
	mov     cx,word ptr sector+bp+di+17     ;File Length= record + 11 + 17
	cmp     cx,0                            ;File length = 0 means catalog
	jz      short is_catalog
;i found end file
	mov     i_found_file_flag,1
	push    cx
	push    si
	push    di
	lea     si, sector+bp+di-SHORT_NAME_LENGTH
	lea     di, file_record
	mov     cx,32                           ;File Record to memory
	rep movsb
	pop     di
	mov     sector+bp+di-SHORT_NAME_LENGTH, 0E5h
	CALL    write_sector
	pop     si
	pop     cx
	ret
is_catalog:
	mov     cx, word ptr sector+bp+di+15
	mov     cluster_number,cx           ;num of cluster
	mov     i_found_file_flag, 0        ;i found catalog
	ret
sector_is_over:
	mov     check_next_flag,1
	ret
fail_to_find:
	mov     search_error_flag,1
				     
	lea     di, word ptr sector+bp+di
	mov     free_record_addr, di
	ret
search_file endp








;=====CALCULATE FIRST CLUSTER'S SECTOR=========
calculate_first_sec_of_cluster proc
	mov     ax, cluster_number
	dec     ax
	dec     ax
	movzx   cx, cluster_size
	mul     cx
	movzx   edx, first_clust_sec
	add     eax, edx
	mov     read_parameters, eax
	ret
calculate_first_sec_of_cluster endp

main_code ends
     end begin

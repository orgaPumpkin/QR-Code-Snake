.386
.MODEL flat, stdcall

Beep PROTO STDCALL :DWORD,:DWORD

GetStdHandle PROTO STDCALL :DWORD


SystemFunction036 PROTO STDCALL :DWORD,:DWORD
  ; _Out_ PVOID RandomBuffer,
  ; _In_  ULONG RandomBufferLength

SetConsoleTextAttribute PROTO STDCALL :DWORD,:DWORD
	; _In_ HANDLE hConsoleOutput,
	; _In_ WORD   wAttributes

ReadConsoleOutputCharacter PROTO STDCALL :DWORD,:DWORD,:DWORD,:DWORD,:DWORD
	; _In_	HANDLE	hConsoleOutput,
	; _Out_	LPTSTR	lpCharacter,
	; _In_	DWORD	nLength,
	; _In_	COORD	dwReadCoord,
	; _Out_	LPDWORD	lpNumberOfCharsRead


SetConsoleCursorPosition PROTO STDCALL :DWORD,:DWORD
	; _In_ HANDLE hConsoleOutput,
	; _In_ COORD  dwCursorPosition


WriteConsoleW PROTO STDCALL :DWORD,:DWORD,:DWORD,:DWORD,:DWORD
	;_In_             HANDLE  hConsoleOutput,
	;_In_       const VOID    *lpBuffer,
	;_In_             DWORD   nNumberOfCharsToWrite,
	;_Out_opt_        LPDWORD lpNumberOfCharsWritten,
	;_Reserved_       LPVOID  lpReserved

ReadConsoleInputA PROTO STDCALL :DWORD,:DWORD,:DWORD,:DWORD
	;_In_  HANDLE        hConsoleInput,
	;_Out_ PINPUT_RECORD lpBuffer,
	;_In_  DWORD         nLength,
	;_Out_ LPDWORD       lpNumberOfEventsRead

GetNumberOfConsoleInputEvents PROTO STDCALL :DWORD,:DWORD
	;_In_  HANDLE  hConsoleInput,
	;_Out_ LPDWORD lpcNumberOfEvents

SetConsoleMode PROTO STDCALL :DWORD,:DWORD
	;_In_ HANDLE hConsoleHandle,
	;_In_ DWORD  dwMode

GetTickCount PROTO STDCALL

BORADSIZE EQU 20

.DATA
	CLS BYTE 1BH,0, "[",0,"H",0, 1BH, 0, "[",0,"J",0
	HANDLE DWORD ?
	HANDLEIN DWORD ?

	LEN DWORD ?
	BUFFER BYTE 32 DUP(' ')
	CHAR WORD 2 DUP(0)
	READLEN DWORD ?
	CHNGX DWORD ?
	CHNGY DWORD ?

	PREVKEY BYTE ?
	PREVTIME DWORD ?

	APPLE DWORD	000A000Dh
	HEAD DWORD 	000A000Ah
	TAIL DWORD 	000A0008h
	
.CODE
START:
	sub esp, BORADSIZE*BORADSIZE*4
	mov ebp, esp

	mov eax, TAIL
	mov edx, TAIL+1
	call STACK_POS
	mov SS:[eax], edx

	inc eax
	inc edx
	mov SS:[eax], edx

	;GET CONSOLE (OUT):
	push -11	;get STD_OUTPUT_HANDLE 
	call GetStdHandle	;RETURNS IN EAX
	mov HANDLE, eax

	;GET CONSOLE (IN):
	push -10	;get STD_INPUT_HANDLE 
	call GetStdHandle	;RETURNS IN EAX
	mov HANDLEIN, eax

	; clear screen	
	mov LEN, 6
	mov eax, offset CLS
	call WRITEBUFFER

	; build borders
	mov [CHAR], 2588h
	mov [CHAR+2], 2588h
		push 08h
		push HANDLE
	call SetConsoleTextAttribute

	xor edx, edx
	call WRITECHARAT

	mov ecx, BORADSIZE
	add ecx, 2

	BORDER:
		;bottom
		mov edx, BORADSIZE
		add dx, 2
		shl edx, 16
		mov dx, cx
			push ecx
		call WRITECHARAT
			pop ecx
			
		;top
		mov edx, 0
		mov dx, cx
			push ecx
		call WRITECHARAT
			pop ecx

		; right
		mov edx, ecx
		shl edx, 16
		mov dx, BORADSIZE
		add dx, 2
			push ecx
		call WRITECHARAT
			pop ecx

		; left
		mov edx, ecx
		shl edx, 16
			push ecx
		call WRITECHARAT
			pop ecx

		loop BORDER

	; apple
		push 4
		push HANDLE
	call SetConsoleTextAttribute
	mov edx, APPLE
	call WRITECHARAT

	; head
		push 0Ah
		push HANDLE
	call SetConsoleTextAttribute
	mov edx, HEAD
	call WRITECHARAT
	; body
		push 2h
		push HANDLE
	call SetConsoleTextAttribute
	mov edx, HEAD
	dec edx
	call WRITECHARAT
	mov edx, TAIL
	call WRITECHARAT

	; get time
	call GetTickCount
	mov PREVTIME, eax

	; main loop
	WAITFORKEY:
		; if enugh time passed, tick
		call GetTickCount
		sub eax, PREVTIME
		cmp eax, 500
		ja TICK

		call READKEY
		cmp al, 00H
		je WAITFORKEY	; if no key
		cmp al, PREVKEY
		je WAITFORKEY	; if same key

		; check key
		cmp al, '%'	; left
		je LEFT
		cmp al, "'"	; right
		je RIGHT
		cmp al, '&'	; up
		je UP
		cmp al, '('	; down
		je DOWN
		jmp WAITFORKEY ; invalid

		LEFT:
			mov CHNGX, -1
			mov CHNGY, 0
		jmp CONTINUE

		RIGHT:
			mov CHNGX, 1
			mov CHNGY, 0
		jmp CONTINUE

		UP:
			mov CHNGX, 0
			mov CHNGY, -1
		jmp CONTINUE

		DOWN:
			mov CHNGX, 0
			mov CHNGY, 1

		CONTINUE:
		mov PREVKEY, al
	
	jmp WAITFORKEY

	TICK:
	call GetTickCount
	mov PREVTIME, eax

	; beep
		push 100
		push 500
	call Beep

	jmp WAITFORKEY

	mov esp, ebp

STACK_POS PROC ; pos in eax ret in eax
	xor ebx, ebx
	mov bx, ax
	shr eax, 16
	; y in eax and x in ebx
	mov ecx, BORADSIZE
	mul ecx
	add eax, ebx
	shr eax, 2
	add eax, ebp
	ret
STACK_POS ENDP


GEN_APPLE PROC
	    push 1
    push offset CHAR
    call SystemFunction036
	mov eax, 0
    mov ax, [CHAR]
    xor edx, edx
	mov ecx, BORADSIZE
    div ecx
    mov ax, dx
	add ax, 1
	shl eax, 16

		push eax
	push 1
	push offset CHAR
	call SystemFunction036
	mov eax, 0
    mov ax, [CHAR]
	xor edx, edx
	mov ecx, BORADSIZE
    div ecx
		pop eax
	mov ax, dx
	add ax, 1
		push eax
	push 4
	push HANDLE
	call SetConsoleTextAttribute
		pop edx
	mov [CHAR], 2588h
	mov [CHAR+2], 2588h
	call WRITECHARAT
	ret
GEN_APPLE ENDP

;KERNEL32 FUNCTIONS:
WRITECHARAT PROC	;CHAR STORED IN CHAR AND POSITION IN EDX
	; mul x by 2
	add dx, dx

	;pos
	push edx
	push HANDLE
	call SetConsoleCursorPosition

	; print char
	PUSH 0
	PUSH 0
	PUSH 2
	PUSH offset CHAR
	PUSH HANDLE
	CALL WriteConsoleW
	RET
WRITECHARAT ENDP

WRITEBUFFER PROC	;STRING PNTR STORED IN EAX AND LENGTH TO WRITE IN LEN
	PUSH 0
	PUSH 0
	PUSH LEN
	PUSH EAX
	PUSH HANDLE
	CALL WriteConsoleW
	RET
WRITEBUFFER ENDP

READKEY PROC
	;CHECK IF THERE ARE EVENTS TO BE READ:
	LEA EAX, [READLEN]
	PUSH EAX
	PUSH HANDLEIN
	CALL GetNumberOfConsoleInputEvents
	MOV EAX, READLEN
	CMP EAX, 00H
	JE ENDREADKEY

	;READ EVENTS:
	LEA EBX, [BUFFER]
	LEA EAX, [READLEN]
	PUSH EAX
	PUSH 1
	PUSH EBX
	PUSH HANDLEIN
	CALL ReadConsoleInputA
	MOV EAX, READLEN
	CMP EAX, 00H
	JE ENDREADKEY
	MOV AL, BUFFER
		;EVENT TYPE:
		CMP AL, 01H	;KEY_EVENT_RECORD
		JE CHECKKEYDOWN
		XOR EAX, EAX
		JMP ENDREADKEY
			CHECKKEYDOWN:
			;LOAD KEY:
			;CHECK IF HELD DOWN
			MOV AL, [BUFFER+4]
			CMP AL, 00
			JE ENDREADKEY
			;GET KEY CODE
			MOV AL, BUFFER+10
	ENDREADKEY:
	RET
READKEY ENDP

END START
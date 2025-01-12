.386
.MODEL flat, stdcall

Beep PROTO STDCALL :DWORD,:DWORD

GetStdHandle PROTO STDCALL :DWORD


SystemFunction036 PROTO STDCALL :DWORD,:DWORD
  ; _Out_ PVOID RandomBuffer,
  ; _In_  ULONG RandomBufferLength


ReadConsoleOutputCharacter PROTO STDCALL :DWORD,:DWORD,:DWORD,:DWORD,:DWORD
	; _In_	HANDLE	hConsoleOutput,
	; _Out_	LPTSTR	lpCharacter,
	; _In_	DWORD	nLength,
	; _In_	COORD	dwReadCoord,
	; _Out_	LPDWORD	lpNumberOfCharsRead


SetConsoleCursorPosition PROTO STDCALL :DWORD,:DWORD
	; _In_ HANDLE hConsoleOutput,
	; _In_ COORD  dwCursorPosition


WriteConsoleA PROTO STDCALL :DWORD,:DWORD,:DWORD,:DWORD,:DWORD
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

BORADSIZE EQU 20

.DATA
	CLS BYTE 1BH, "[H", 1BH, "[J"
	HANDLE DWORD ?
	HANDLEIN DWORD ?

	LEN DWORD ?
	BUFFER BYTE 32 DUP(' ')
	CHAR WORD ?
	READLEN DWORD ?
	CHNGX DWORD ?
	CHNGY DWORD ?
	PREVKEY BYTE ?
	

.CODE
START:
	;GET CONSOLE (OUT):
	push -11	;get STD_OUTPUT_HANDLE 
	call GetStdHandle	;RETURNS IN EAX
	mov HANDLE, eax

	;GET CONSOLE (IN):
	push -10	;get STD_INPUT_HANDLE 
	call GetStdHandle	;RETURNS IN EAX
	mov HANDLEIN, eax

	; clear screen	
	lea eax, [CLS]
	mov LEN, 6
	call WRITEBUFFERA

	; build borders
	xor edx, edx
	mov eax, 2588h
	call WRITECHARAT

	mov ecx, BORADSIZE
	add ecx, 2

	BORDER:
		;bottom
		mov edx, BORADSIZE
		add dx, 2
		shl edx, 16
		mov dx, cx
		mov eax, 2588h
			push ecx
		call WRITECHARAT
			pop ecx
			
		;top
		mov edx, 0
		mov dx, cx
		mov eax, 2588h
			push ecx
		call WRITECHARAT
			pop ecx

		; right
		mov edx, ecx
		shl edx, 16
		mov dx, BORADSIZE
		add dx, 2
		mov eax, 2588h
			push ecx
		call WRITECHARAT
			pop ecx

		; left
		mov edx, ecx
		shl edx, 16
		mov eax, 2588h
			push ecx
		call WRITECHARAT
			pop ecx

		loop BORDER

	; apple
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
	
	mov edx, eax
	mov ax, 2665h
	call WRITECHARAT

	; beep
	push 100
	push 500
	call Beep

	; main loop
	WAITFORKEY:
		call READKEY
		cmp al, 00H
		je WAITFORKEY	; if no key, continue waiting
		cmp al, PREVKEY
		je WAITFORKEY	; if same key, continue waiting

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

		; beep
			push 100
			push 500
		call Beep
	
	jmp WAITFORKEY

;KERNEL32 FUNCTIONS:
WRITECHARAT PROC	;CHAR STORED IN AX AND POSITION IN EDX
	add dx, dx
	mov [CHAR], ax

	;pos
		push edx
	push edx
	push HANDLE
	call SetConsoleCursorPosition

	; print char
	PUSH 0
	PUSH 0
	PUSH 1
	PUSH offset CHAR
	PUSH HANDLE
	CALL WriteConsoleW
		pop edx

	inc edx
	push edx
	push HANDLE
	call SetConsoleCursorPosition

	PUSH 0
	PUSH 0
	PUSH 1
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

WRITEBUFFERA PROC	;STRING PNTR STORED IN EAX AND LENGTH TO WRITE IN LEN
	PUSH 0
	PUSH 0
	PUSH LEN
	PUSH EAX
	PUSH HANDLE
	CALL WriteConsoleA
	RET
WRITEBUFFERA ENDP

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
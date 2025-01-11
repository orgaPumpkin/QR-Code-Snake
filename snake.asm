.386
.MODEL flat, stdcall

Beep PROTO STDCALL :DWORD,:DWORD

GetStdHandle PROTO STDCALL :DWORD

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

BORADSIZE EQU 48

.DATA
	TITLEVAR DWORD 1BH, ']','0',';','S','N','A','K','E', 07H, 1BH, '[','1','9','6','m'
	HANDLE DWORD ?
	HANDLEIN DWORD ?

	LEN DWORD ?
	BUFFER BYTE 32 DUP(?)
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

	;SET CONSOLE MODE ENSURING ANSI INPUT IS ALLOWED
	mov eax, 05H	;ENABLE VIRTUAL TERMINAL
	push eax
	push HANDLE
	call SetConsoleMode

	;SET CONSOLE TITLE & CURSOR OFF WITHT ANSI STRING
	lea eax, [TITLEVAR]
	mov LEN, 19
	call WRITEBUFFER

		push 100
		push 500
	call Beep

	mov LEN, 1
	
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

		push 100
		push 500
		call Beep
	
	jmp WAITFORKEY

;KERNEL32 FUNCTIONS:
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
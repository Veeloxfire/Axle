segment .bss
stdout_handle:
 RESQ 1
segment .text
global main
extern HeapFree
extern WriteConsoleA
extern HeapAlloc
extern GetStdHandle
extern GetProcessHeap
_label_0000000000000009:
jmp HeapFree
_label_0000000000000008:
jmp WriteConsoleA
_label_000000000000000b:
jmp HeapAlloc
_label_0000000000000006:
jmp GetStdHandle
_label_0000000000000007:
jmp GetProcessHeap
_label_0000000000000017:
push rbp
mov rbp, rsp
push rbx
sub rsp, 0x0000000000000028
lea rbx, [stdout_handle]
mov ecx, 0xfffffff5
call _label_0000000000000006
mov QWORD [rbx], rax
_label_0000000000000018:
mov rbx, QWORD [rbp - 0x00000008]
mov rsp, rbp
pop rbp
ret
_label_0000000000000000:
push rbp
mov rbp, rsp
sub rsp, 0x0000000000000038
mov rax, 0x6f57206f6c6c6548
mov QWORD [rbp - 0x0000000d], rax
mov DWORD [rbp - 0x00000005], 0x21646c72
mov BYTE [rbp - 0x00000001], 0x00
lea rcx, [rbp - 0x0000000d]
mov rdx, 0x000000000000000d
mov rax, 0x0000000000000001
sub rdx, rax
call _label_0000000000000001
_label_0000000000000019:
mov rsp, rbp
pop rbp
ret
_label_0000000000000001:
push rbp
mov rbp, rsp
sub rsp, 0x0000000000000028
mov r9, rcx
mov r8, rdx
lea rax, [stdout_handle]
mov rcx, QWORD [rax]
mov rdx, r9
mov r9, 0x0000000000000000
mov QWORD [rsp - 0x00000008], 0x0000000000000000
call _label_0000000000000008
_label_000000000000001a:
mov rsp, rbp
pop rbp
ret
main:
call _label_0000000000000017
jmp _label_0000000000000000

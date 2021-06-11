.code

; void* __cdecl memcpy(void* _Dst, void const* _Src, size_t _Size);
extrn memcpy : proc

; RCX = void* func
; RDX = uint64_t[4] param_registers
; R8  = uint8_t* stack_top
; R9  = size_t stack_required
call_native_x64 PROC

  push rbp
  mov rbp, rsp
  sub rsp, 32 ; shadow space

  ; move the params to the shadow space for fun (saved for later)
  mov QWORD PTR [rbp + 16], rcx
  mov QWORD PTR [rbp + 24], rdx
  mov QWORD PTR [rbp + 32], r8
  mov QWORD PTR [rbp + 40], r9

  ; parameters are still in the registers though

  cmp r9, 0    ; do we need to copy the stack
  je REGISTERS ; jump to setting registers if we dont need stack

  ; setup the stack
  sub rsp, r9  ; extend the stack by that many bytes

  ; rcx = * dst
  ; rdx = * src
  ; r8  = u64 size

  mov rcx, rbp
  sub rcx, r9  ; to where the parameters will go

  mov rdx, r8
  mov r8, r9
  call memcpy ; copy the stack

REGISTERS:
  mov rax, QWORD PTR [rbp + 24] ; load param regs to rax

  mov rcx, [rax] ; first param 
  mov rdx, [rax + 8] ; second param 
  mov r8,  [rax + 16] ; third param
  mov r9,  [rax + 24] ; fourth param

  call QWORD PTR [rbp + 16] ; call the pointer

  ; implicity return rax 

  mov rsp, rbp
  pop rbp
  ret

call_native_x64 ENDP

END
  
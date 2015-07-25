using Cosmos.IL2CPU.Plugs;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VbeHackCore
{
    /*
    typedef struct __attribute__ ((packed)) {
	unsigned short di, si, bp, sp, bx, dx, cx, ax;
	unsigned short gs, fs, es, ds, eflags;
} regs16_t;
        */
    public struct regs16_t
        {
            public ushort di, si, bp, sp, bx, dx, cx, ax;
            public ushort gs, fs, es, ds, eflags;
        }

    public static class JumStatment
    {

        private class Jmp : AssemblerMethod
        {
            public override void AssembleNew(Cosmos.Assembler.Assembler aAssembler, object aMethodInfo)
            {

                new Cosmos.Assembler.LiteralAssemblerCode("; ");
                new Cosmos.Assembler.LiteralAssemblerCode("; Protected Mode BIOS Call Functionailty v2.0 - by Napalm");
                new Cosmos.Assembler.LiteralAssemblerCode("; -------------------------------------------------------");
                new Cosmos.Assembler.LiteralAssemblerCode("; ");
                new Cosmos.Assembler.LiteralAssemblerCode("; This is code shows how its POSSIBLE to execute BIOS interrupts");
                new Cosmos.Assembler.LiteralAssemblerCode("; by switch out to real-mode and then back into protected mode.");
                new Cosmos.Assembler.LiteralAssemblerCode("; ");
                new Cosmos.Assembler.LiteralAssemblerCode("; If you wish to use all or part of this code you must agree");
                new Cosmos.Assembler.LiteralAssemblerCode("; to the license at the following URL.");
                new Cosmos.Assembler.LiteralAssemblerCode("; ");
                new Cosmos.Assembler.LiteralAssemblerCode("; License: http://creativecommons.org/licenses/by-sa/2.0/uk/");
                new Cosmos.Assembler.LiteralAssemblerCode(";         ");
                new Cosmos.Assembler.LiteralAssemblerCode("; Notes: This file is in NASM syntax.");
                new Cosmos.Assembler.LiteralAssemblerCode(";        Turn off paging before calling these functions.");
                new Cosmos.Assembler.LiteralAssemblerCode(";        int32() resets all selectors.");
                new Cosmos.Assembler.LiteralAssemblerCode(";");
                new Cosmos.Assembler.LiteralAssemblerCode("; C Prototype:");
                new Cosmos.Assembler.LiteralAssemblerCode(";	void _cdelc int32(unsigned char intnum, regs16_t *regs);");
                new Cosmos.Assembler.LiteralAssemblerCode("; ");
                new Cosmos.Assembler.LiteralAssemblerCode("; Example of usage:");
                new Cosmos.Assembler.LiteralAssemblerCode(";   regs.ax = 0x0013;");
                new Cosmos.Assembler.LiteralAssemblerCode(";   int32(0x10, &regs);");
                new Cosmos.Assembler.LiteralAssemblerCode(";   memset((char *)0xA0000, 1, (320*200));");
                new Cosmos.Assembler.LiteralAssemblerCode(";   memset((char *)0xA0000 + (100*320+80), 14, 80);");
                new Cosmos.Assembler.LiteralAssemblerCode(";   regs.ax = 0x0000;");
                new Cosmos.Assembler.LiteralAssemblerCode(";   int32(0x16, &regs);");
                new Cosmos.Assembler.LiteralAssemblerCode(";   regs.ax = 0x0003;");
                new Cosmos.Assembler.LiteralAssemblerCode(";   int32(0x10, &regs);");
                new Cosmos.Assembler.LiteralAssemblerCode("; ");
                new Cosmos.Assembler.LiteralAssemblerCode("; ");
                new Cosmos.Assembler.LiteralAssemblerCode("[bits 32]");
                new Cosmos.Assembler.LiteralAssemblerCode("");
                new Cosmos.Assembler.LiteralAssemblerCode("global int32, _int32");
                new Cosmos.Assembler.LiteralAssemblerCode("");
                new Cosmos.Assembler.LiteralAssemblerCode("struc regs16_t");
                new Cosmos.Assembler.LiteralAssemblerCode("	.di	resw 1");
                new Cosmos.Assembler.LiteralAssemblerCode("	.si	resw 1");
                new Cosmos.Assembler.LiteralAssemblerCode("	.bp	resw 1");
                new Cosmos.Assembler.LiteralAssemblerCode("	.sp resw 1");
                new Cosmos.Assembler.LiteralAssemblerCode("	.bx	resw 1");
                new Cosmos.Assembler.LiteralAssemblerCode("	.dx	resw 1");
                new Cosmos.Assembler.LiteralAssemblerCode("	.cx	resw 1");
                new Cosmos.Assembler.LiteralAssemblerCode("	.ax	resw 1");
                new Cosmos.Assembler.LiteralAssemblerCode("	.gs	resw 1");
                new Cosmos.Assembler.LiteralAssemblerCode("	.fs	resw 1");
                new Cosmos.Assembler.LiteralAssemblerCode("	.es	resw 1");
                new Cosmos.Assembler.LiteralAssemblerCode("	.ds	resw 1");
                new Cosmos.Assembler.LiteralAssemblerCode("	.ef resw 1");
                new Cosmos.Assembler.LiteralAssemblerCode("endstruc");
                new Cosmos.Assembler.LiteralAssemblerCode("");
                new Cosmos.Assembler.LiteralAssemblerCode("%define INT32_BASE                             0x7C00");
                new Cosmos.Assembler.LiteralAssemblerCode("%define REBASE(x)                              (((x) - reloc) + INT32_BASE)");
                new Cosmos.Assembler.LiteralAssemblerCode("%define GDTENTRY(x)                            ((x) << 3)");
                new Cosmos.Assembler.LiteralAssemblerCode("%define CODE32                                 GDTENTRY(1)	; 0x08");
                new Cosmos.Assembler.LiteralAssemblerCode("%define DATA32                                 GDTENTRY(2)	; 0x10");
                new Cosmos.Assembler.LiteralAssemblerCode("%define CODE16                                 GDTENTRY(3)	; 0x18");
                new Cosmos.Assembler.LiteralAssemblerCode("%define DATA16                                 GDTENTRY(4)	; 0x20");
                new Cosmos.Assembler.LiteralAssemblerCode("%define STACK16                                (INT32_BASE - regs16_t_size)");
                new Cosmos.Assembler.LiteralAssemblerCode("");
                new Cosmos.Assembler.LiteralAssemblerCode("");
                new Cosmos.Assembler.LiteralAssemblerCode("section .text");
                new Cosmos.Assembler.LiteralAssemblerCode("	int32: use32                               ; by Napalm");
                new Cosmos.Assembler.LiteralAssemblerCode("	_int32:");
                new Cosmos.Assembler.LiteralAssemblerCode("		cli                                    ; disable interrupts");
                new Cosmos.Assembler.LiteralAssemblerCode("		pusha                                  ; save register state to 32bit stack");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  esi, reloc                        ; set source to code below");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  edi, INT32_BASE                   ; set destination to new base address");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  ecx, (int32_end - reloc)          ; set copy size to our codes size");
                new Cosmos.Assembler.LiteralAssemblerCode("		cld                                    ; clear direction flag (so we copy forward)");
                new Cosmos.Assembler.LiteralAssemblerCode("		rep  movsb                             ; do the actual copy (relocate code to low 16bit space)");
                new Cosmos.Assembler.LiteralAssemblerCode("		jmp INT32_BASE                         ; jump to new code location");
                new Cosmos.Assembler.LiteralAssemblerCode("	reloc: use32                               ; by Napalm");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  [REBASE(stack32_ptr)], esp        ; save 32bit stack pointer");
                new Cosmos.Assembler.LiteralAssemblerCode("		sidt [REBASE(idt32_ptr)]               ; save 32bit idt pointer");
                new Cosmos.Assembler.LiteralAssemblerCode("		sgdt [REBASE(gdt32_ptr)]               ; save 32bit gdt pointer");
                new Cosmos.Assembler.LiteralAssemblerCode("		lgdt [REBASE(gdt16_ptr)]               ; load 16bit gdt pointer");
                new Cosmos.Assembler.LiteralAssemblerCode("		lea  esi, [esp+0x24]                   ; set position of intnum on 32bit stack");
                new Cosmos.Assembler.LiteralAssemblerCode("		lodsd                                  ; read intnum into eax");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  [REBASE(ib)], al                  ; set intrrupt immediate byte from our arguments ");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  esi, [esi]                        ; read regs pointer in esi as source");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  edi, STACK16                      ; set destination to 16bit stack");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  ecx, regs16_t_size                ; set copy size to our struct size");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  esp, edi                          ; save destination to as 16bit stack offset");
                new Cosmos.Assembler.LiteralAssemblerCode("		rep  movsb                             ; do the actual copy (32bit stack to 16bit stack)");
                new Cosmos.Assembler.LiteralAssemblerCode("		jmp  word CODE16:REBASE(p_mode16)      ; switch to 16bit selector (16bit protected mode)");
                new Cosmos.Assembler.LiteralAssemblerCode("	p_mode16: use16");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  ax, DATA16                        ; get our 16bit data selector");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  ds, ax                            ; set ds to 16bit selector");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  es, ax                            ; set es to 16bit selector");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  fs, ax                            ; set fs to 16bit selector");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  gs, ax                            ; set gs to 16bit selector");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  ss, ax                            ; set ss to 16bit selector");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  eax, cr0                          ; get cr0 so we can modify it");
                new Cosmos.Assembler.LiteralAssemblerCode("		and  al,  ~0x01                        ; mask off PE bit to turn off protected mode");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  cr0, eax                          ; set cr0 to result");
                new Cosmos.Assembler.LiteralAssemblerCode("		jmp  word 0x0000:REBASE(r_mode16)      ; finally set cs:ip to enter real-mode");
                new Cosmos.Assembler.LiteralAssemblerCode("	r_mode16: use16");
                new Cosmos.Assembler.LiteralAssemblerCode("		xor  ax, ax                            ; set ax to zero");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  ds, ax                            ; set ds so we can access idt16");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  ss, ax                            ; set ss so they the stack is valid");
                new Cosmos.Assembler.LiteralAssemblerCode("		lidt [REBASE(idt16_ptr)]               ; load 16bit idt");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  bx, 0x0870                        ; master 8 and slave 112");
                new Cosmos.Assembler.LiteralAssemblerCode("		call resetpic                          ; set pic's the to real-mode settings");
                new Cosmos.Assembler.LiteralAssemblerCode("		popa                                   ; load general purpose registers from 16bit stack");
                new Cosmos.Assembler.LiteralAssemblerCode("		pop  gs                                ; load gs from 16bit stack");
                new Cosmos.Assembler.LiteralAssemblerCode("		pop  fs                                ; load fs from 16bit stack");
                new Cosmos.Assembler.LiteralAssemblerCode("		pop  es                                ; load es from 16bit stack");
                new Cosmos.Assembler.LiteralAssemblerCode("		pop  ds                                ; load ds from 16bit stack");
                new Cosmos.Assembler.LiteralAssemblerCode("		sti                                    ; enable interrupts");
                new Cosmos.Assembler.LiteralAssemblerCode("		db 0xCD                                ; opcode of INT instruction with immediate byte");
                new Cosmos.Assembler.LiteralAssemblerCode("	ib: db 0x00");
                new Cosmos.Assembler.LiteralAssemblerCode("		cli                                    ; disable interrupts");
                new Cosmos.Assembler.LiteralAssemblerCode("		xor  sp, sp                            ; zero sp so we can reuse it");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  ss, sp                            ; set ss so the stack is valid");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  sp, INT32_BASE                    ; set correct stack position so we can copy back");
                new Cosmos.Assembler.LiteralAssemblerCode("		pushf                                  ; save eflags to 16bit stack");
                new Cosmos.Assembler.LiteralAssemblerCode("		push ds                                ; save ds to 16bit stack");
                new Cosmos.Assembler.LiteralAssemblerCode("		push es                                ; save es to 16bit stack");
                new Cosmos.Assembler.LiteralAssemblerCode("		push fs                                ; save fs to 16bit stack");
                new Cosmos.Assembler.LiteralAssemblerCode("		push gs                                ; save gs to 16bit stack");
                new Cosmos.Assembler.LiteralAssemblerCode("		pusha                                  ; save general purpose registers to 16bit stack");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  bx, 0x2028                        ; master 32 and slave 40");
                new Cosmos.Assembler.LiteralAssemblerCode("		call resetpic                          ; restore the pic's to protected mode settings");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  eax, cr0                          ; get cr0 so we can modify it");
                new Cosmos.Assembler.LiteralAssemblerCode("		inc  eax                               ; set PE bit to turn on protected mode");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  cr0, eax                          ; set cr0 to result");
                new Cosmos.Assembler.LiteralAssemblerCode("		jmp  dword CODE32:REBASE(p_mode32)     ; switch to 32bit selector (32bit protected mode)");
                new Cosmos.Assembler.LiteralAssemblerCode("	p_mode32: use32");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  ax, DATA32                        ; get our 32bit data selector");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  ds, ax                            ; reset ds selector");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  es, ax                            ; reset es selector");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  fs, ax                            ; reset fs selector");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  gs, ax                            ; reset gs selector");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  ss, ax                            ; reset ss selector");
                new Cosmos.Assembler.LiteralAssemblerCode("		lgdt [REBASE(gdt32_ptr)]               ; restore 32bit gdt pointer");
                new Cosmos.Assembler.LiteralAssemblerCode("		lidt [REBASE(idt32_ptr)]               ; restore 32bit idt pointer");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  esp, [REBASE(stack32_ptr)]        ; restore 32bit stack pointer");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  esi, STACK16                      ; set copy source to 16bit stack");
                new Cosmos.Assembler.LiteralAssemblerCode("		lea  edi, [esp+0x28]                   ; set position of regs pointer on 32bit stack");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  edi, [edi]                        ; use regs pointer in edi as copy destination");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  ecx, regs16_t_size                ; set copy size to our struct size");
                new Cosmos.Assembler.LiteralAssemblerCode("		cld                                    ; clear direction flag (so we copy forward)");
                new Cosmos.Assembler.LiteralAssemblerCode("		rep  movsb                             ; do the actual copy (16bit stack to 32bit stack)");
                new Cosmos.Assembler.LiteralAssemblerCode("		popa                                   ; restore registers");
                new Cosmos.Assembler.LiteralAssemblerCode("		sti                                    ; enable interrupts");
                new Cosmos.Assembler.LiteralAssemblerCode("		ret                                    ; return to caller");
                new Cosmos.Assembler.LiteralAssemblerCode("		");
                new Cosmos.Assembler.LiteralAssemblerCode("	resetpic:                                  ; reset's 8259 master and slave pic vectors");
                new Cosmos.Assembler.LiteralAssemblerCode("		push ax                                ; expects bh = master vector, bl = slave vector");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  al, 0x11                          ; 0x11 = ICW1_INIT | ICW1_ICW4");
                new Cosmos.Assembler.LiteralAssemblerCode("		out  0x20, al                          ; send ICW1 to master pic");
                new Cosmos.Assembler.LiteralAssemblerCode("		out  0xA0, al                          ; send ICW1 to slave pic");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  al, bh                            ; get master pic vector param");
                new Cosmos.Assembler.LiteralAssemblerCode("		out  0x21, al                          ; send ICW2 aka vector to master pic");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  al, bl                            ; get slave pic vector param");
                new Cosmos.Assembler.LiteralAssemblerCode("		out  0xA1, al                          ; send ICW2 aka vector to slave pic");
                new Cosmos.Assembler.LiteralAssemblerCode("		mov  al, 0x04                          ; 0x04 = set slave to IRQ2");
                new Cosmos.Assembler.LiteralAssemblerCode("		out  0x21, al                          ; send ICW3 to master pic");
                new Cosmos.Assembler.LiteralAssemblerCode("		shr  al, 1                             ; 0x02 = tell slave its on IRQ2 of master");
                new Cosmos.Assembler.LiteralAssemblerCode("		out  0xA1, al                          ; send ICW3 to slave pic");
                new Cosmos.Assembler.LiteralAssemblerCode("		shr  al, 1                             ; 0x01 = ICW4_8086");
                new Cosmos.Assembler.LiteralAssemblerCode("		out  0x21, al                          ; send ICW4 to master pic");
                new Cosmos.Assembler.LiteralAssemblerCode("		out  0xA1, al                          ; send ICW4 to slave pic");
                new Cosmos.Assembler.LiteralAssemblerCode("		pop  ax                                ; restore ax from stack");
                new Cosmos.Assembler.LiteralAssemblerCode("		ret                                    ; return to caller");
                new Cosmos.Assembler.LiteralAssemblerCode("		");
                new Cosmos.Assembler.LiteralAssemblerCode("	stack32_ptr:                               ; address in 32bit stack after we");
                new Cosmos.Assembler.LiteralAssemblerCode("		dd 0x00000000                          ;   save all general purpose registers");
                new Cosmos.Assembler.LiteralAssemblerCode("		");
                new Cosmos.Assembler.LiteralAssemblerCode("	idt32_ptr:                                 ; IDT table pointer for 32bit access");
                new Cosmos.Assembler.LiteralAssemblerCode("		dw 0x0000                              ; table limit (size)");
                new Cosmos.Assembler.LiteralAssemblerCode("		dd 0x00000000                          ; table base address");
                new Cosmos.Assembler.LiteralAssemblerCode("		");
                new Cosmos.Assembler.LiteralAssemblerCode("	gdt32_ptr:                                 ; GDT table pointer for 32bit access");
                new Cosmos.Assembler.LiteralAssemblerCode("		dw 0x0000                              ; table limit (size)");
                new Cosmos.Assembler.LiteralAssemblerCode("		dd 0x00000000                          ; table base address");
                new Cosmos.Assembler.LiteralAssemblerCode("		");
                new Cosmos.Assembler.LiteralAssemblerCode("	idt16_ptr:                                 ; IDT table pointer for 16bit access");
                new Cosmos.Assembler.LiteralAssemblerCode("		dw 0x03FF                              ; table limit (size)");
                new Cosmos.Assembler.LiteralAssemblerCode("		dd 0x00000000                          ; table base address");
                new Cosmos.Assembler.LiteralAssemblerCode("		");
                new Cosmos.Assembler.LiteralAssemblerCode("	gdt16_base:                                ; GDT descriptor table");
                new Cosmos.Assembler.LiteralAssemblerCode("		.null:                                 ; 0x00 - null segment descriptor");
                new Cosmos.Assembler.LiteralAssemblerCode("			dd 0x00000000                      ; must be left zero'd");
                new Cosmos.Assembler.LiteralAssemblerCode("			dd 0x00000000                      ; must be left zero'd");
                new Cosmos.Assembler.LiteralAssemblerCode("			");
                new Cosmos.Assembler.LiteralAssemblerCode("		.code32:                               ; 0x01 - 32bit code segment descriptor 0xFFFFFFFF");
                new Cosmos.Assembler.LiteralAssemblerCode("			dw 0xFFFF                          ; limit  0:15");
                new Cosmos.Assembler.LiteralAssemblerCode("			dw 0x0000                          ; base   0:15");
                new Cosmos.Assembler.LiteralAssemblerCode("			db 0x00                            ; base  16:23");
                new Cosmos.Assembler.LiteralAssemblerCode("			db 0x9A                            ; present, iopl/0, code, execute/read");
                new Cosmos.Assembler.LiteralAssemblerCode("			db 0xCF                            ; 4Kbyte granularity, 32bit selector; limit 16:19");
                new Cosmos.Assembler.LiteralAssemblerCode("			db 0x00                            ; base  24:31");
                new Cosmos.Assembler.LiteralAssemblerCode("			");
                new Cosmos.Assembler.LiteralAssemblerCode("		.data32:                               ; 0x02 - 32bit data segment descriptor 0xFFFFFFFF");
                new Cosmos.Assembler.LiteralAssemblerCode("			dw 0xFFFF                          ; limit  0:15");
                new Cosmos.Assembler.LiteralAssemblerCode("			dw 0x0000                          ; base   0:15");
                new Cosmos.Assembler.LiteralAssemblerCode("			db 0x00                            ; base  16:23");
                new Cosmos.Assembler.LiteralAssemblerCode("			db 0x92                            ; present, iopl/0, data, read/write");
                new Cosmos.Assembler.LiteralAssemblerCode("			db 0xCF                            ; 4Kbyte granularity, 32bit selector; limit 16:19");
                new Cosmos.Assembler.LiteralAssemblerCode("			db 0x00                            ; base  24:31");
                new Cosmos.Assembler.LiteralAssemblerCode("			");
                new Cosmos.Assembler.LiteralAssemblerCode("		.code16:                               ; 0x03 - 16bit code segment descriptor 0x000FFFFF");
                new Cosmos.Assembler.LiteralAssemblerCode("			dw 0xFFFF                          ; limit  0:15");
                new Cosmos.Assembler.LiteralAssemblerCode("			dw 0x0000                          ; base   0:15");
                new Cosmos.Assembler.LiteralAssemblerCode("			db 0x00                            ; base  16:23");
                new Cosmos.Assembler.LiteralAssemblerCode("			db 0x9A                            ; present, iopl/0, code, execute/read");
                new Cosmos.Assembler.LiteralAssemblerCode("			db 0x0F                            ; 1Byte granularity, 16bit selector; limit 16:19");
                new Cosmos.Assembler.LiteralAssemblerCode("			db 0x00                            ; base  24:31");
                new Cosmos.Assembler.LiteralAssemblerCode("			");
                new Cosmos.Assembler.LiteralAssemblerCode("		.data16:                               ; 0x04 - 16bit data segment descriptor 0x000FFFFF");
                new Cosmos.Assembler.LiteralAssemblerCode("			dw 0xFFFF                          ; limit  0:15");
                new Cosmos.Assembler.LiteralAssemblerCode("			dw 0x0000                          ; base   0:15");
                new Cosmos.Assembler.LiteralAssemblerCode("			db 0x00                            ; base  16:23");
                new Cosmos.Assembler.LiteralAssemblerCode("			db 0x92                            ; present, iopl/0, data, read/write");
                new Cosmos.Assembler.LiteralAssemblerCode("			db 0x0F                            ; 1Byte granularity, 16bit selector; limit 16:19");
                new Cosmos.Assembler.LiteralAssemblerCode("			db 0x00                            ; base  24:31");
                new Cosmos.Assembler.LiteralAssemblerCode("			");
                new Cosmos.Assembler.LiteralAssemblerCode("	gdt16_ptr:                                 ; GDT table pointer for 16bit access");
                new Cosmos.Assembler.LiteralAssemblerCode("		dw gdt16_ptr - gdt16_base - 1          ; table limit (size)");
                new Cosmos.Assembler.LiteralAssemblerCode("		dd gdt16_base                          ; table base address");
                new Cosmos.Assembler.LiteralAssemblerCode("		");
                new Cosmos.Assembler.LiteralAssemblerCode("	int32_end:                                 ; end marker (so we can copy the code)");
                new Cosmos.Assembler.LiteralAssemblerCode("	");
                new Cosmos.Assembler.LiteralAssemblerCode("	");
            }
        }

        private static class InternalJumStatment
        {
            public static  void Jump(Byte intnum, regs16_t regs) { }
        }

        [Plug(Target = typeof(JumStatment.InternalJumStatment))]
        private static class InternalJumStatmentPluged
        {
            [PlugMethod(Assembler = typeof(JumStatment.Jmp))]
            public static  void Jump(Byte intnum, regs16_t regs) { }
        }

        public static  void Jump(Byte intnum, regs16_t regs)
        {
            InternalJumStatment.Jump(intnum, regs);
        }
    }

}
